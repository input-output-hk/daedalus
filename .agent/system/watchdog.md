# Watchdog Architecture

The watchdog is a small Rust binary (`cardano-watchdog`) that sits between the Electron main process and the two Cardano backends (cardano-node and cardano-wallet). Its job is to own the lifetimes of both backends, restart the wallet if it crashes, shut everything down cleanly when Daedalus exits, and surface runtime metrics to the frontend.

---

## Process tree

```
Electron main process (Node.js)
└── cardano-watchdog          ← Rust binary, child of Electron
    ├── cardano-node          ← child of watchdog
    └── cardano-wallet        ← child of watchdog
```

Electron spawns exactly one watchdog. The watchdog spawns node and wallet. Neither backend is a direct child of Electron, so Electron never needs to know their PIDs to send them signals — that is all the watchdog's responsibility.

---

## How Daedalus starts the watchdog

### 1. `CardanoNode.ts` calls `startWatchdog()`

`source/main/cardano/CardanoNode.ts` is the top-level state machine for the Cardano backends. When it enters the `STARTING` state it calls `startWatchdog()` (from `CardanoWatchdog.ts`).

`startWatchdog()` does three things before returning a promise:

1. Picks two free TCP ports (one for cardano-node P2P, one for cardano-wallet REST API) using an ephemeral `net.createServer()` bind.
2. Builds the full argument lists for both backends (`buildNodeArgs`, `buildWalletArgs`).
3. Assembles a `WatchdogConfig` JSON object and spawns the watchdog binary with `stdio: ['pipe', 'pipe', 'pipe']`.

### 2. Config is delivered via stdin

The very first line written to the watchdog's stdin is the JSON config:

```jsonc
{
  "node": {
    "exe": "/nix/store/.../cardano-node",
    "args": ["run", "--socket-path", "...", "--topology", "...", ...],
    "state_dir": "/state/mainnet",
    "socket_path": "/state/mainnet/cardano-node.socket"   // Unix
                // or "\\\\.\pipe\\cardano-node.socket"    // Windows
  },
  "wallet": {
    "exe": "/nix/store/.../cardano-wallet",
    "args": ["serve", "--port", "44123", "--node-socket", "...", ...],
    "state_dir": "/state/mainnet",
    "api_port": 44123,
    "restart_delay_ms": 1000,          // default
    "max_restart_attempts": 10         // default
  },
  "node_log_file": "/logs/node.log",
  "wallet_log_file": "/logs/cardano-wallet.log"
}
```

The watchdog reads this first line in `main.rs`, deserializes it, then hands it to `supervisor::run()`. Subsequent stdin lines are treated as commands (see below).

### 3. `startWatchdog()` returns a promise that resolves on `wallet_ready`

The TypeScript side reads newline-delimited JSON events from the watchdog's stdout. The promise returned by `startWatchdog()` resolves with a `WatchdogHandle` only when the watchdog emits `wallet_ready` — i.e., when the wallet's HTTP API is actually accepting connections. `CardanoNode.ts` stores this handle as `this._node` and then calls `this._handleCardanoNodeMessage({ ReplyPort: handle.walletPort })` to tell the renderer which port to use.

If the promise rejects (startup timeout, `wallet_unrecoverable`, fatal error), `CardanoNode.ts` transitions to `ERRORED` and schedules a restart.

---

## What the watchdog monitors and restarts

### cardano-node

The watchdog **does not restart** cardano-node. It monitors it with a watch channel; if node exits at any point, the watchdog stops the wallet, emits `node_exited`, and shuts down. Node exit is treated as unrecoverable — Daedalus will restart the whole stack via `CardanoNode.ts`.

The watchdog also waits for the node socket to appear before starting the wallet:
- **Unix**: polls `Path::exists()` every 500 ms (up to 120 s).
- **Windows**: returns immediately — cardano-node uses a Named Pipe (`\\.\pipe\cardano-node.socket`) which does not appear as a filesystem entry; cardano-wallet retries the connection internally.

### cardano-wallet

The watchdog **does restart** the wallet automatically. The supervisor loop (`'supervisor` in `supervisor.rs`) has two phases:

**Phase 1 — pre-ready**: starts wallet, races `wait_for_port(api_port)` against `wallet.wait()`. If the wallet exits before the port opens, it increments `attempt` and restarts after `restart_delay_ms` (default 1 s).

**Phase 2 — post-ready**: wallet is serving. Waits for the wallet to exit. On exit, restarts with the same circuit-breaker logic.

**Circuit breaker**: after `max_restart_attempts` (default 10) cumulative failures, the watchdog emits `wallet_unrecoverable` and breaks out of the supervisor loop, triggering a full node shutdown.

The exit info (`code`, `signal`) and the phase (`"pre_ready"` / `"post_ready"`) are included in the `wallet_exited` event so Daedalus can show a meaningful error.

---

## Log rotation

Both `node_log_file` and `wallet_log_file` are written by the watchdog using [`file-rotate`](https://crates.io/crates/file-rotate) (v0.8). Logs rotate at **5 MB** and keep up to **4 rotated files** alongside the current file:

```
node.log          ← current (actively written)
node.log.1        ← most recent rotation
node.log.2
node.log.3
node.log.4        ← oldest (deleted when a 5th would be created)
```

Both stdout and stderr of each child process are written to the same rotating file. The two reader tasks share one `Arc<Mutex<FileRotate<AppendCount>>>` so their writes are serialised without races.

Rotated file names are matched by the regexes in `source/main/config.ts`:
- `ALLOWED_NODE_LOGS`: `/^node\.log\.\d+$/`
- `ALLOWED_WALLET_LOGS`: `/^cardano-wallet\.log\.\d+$/`

---

## IPC protocol

Communication is newline-delimited JSON on stdin/stdout of the watchdog process.

### Daedalus → watchdog (stdin)

| JSON | Meaning |
|------|---------|
| `{"cmd":"stop"}` | Initiate graceful shutdown |
| `{"cmd":"start_node"}` | Start cardano-node/wallet without Mithril (after user declines bootstrap) |
| `{"cmd":"start_mithril", "force": bool}` | Begin Mithril lifecycle; `force: true` skips behind-ness check |
| `{"cmd":"cancel_mithril"}` | SIGKILL the in-flight mithril-client or snapshot-converter; clean up staging |
| `{"cmd":"probe_mithril"}` | Run behind-ness probe asynchronously; emits `mithril_significantly_behind` or `mithril_not_needed` |
| stdin EOF | Treated as `stop` |

`handle.stop()` in `CardanoWatchdog.ts` writes the stop command and then calls `proc.stdin?.end()` to close stdin so the Rust async reader sees EOF and unblocks.

`probe_mithril` is accepted during socket-wait and while node/wallet are running. Probe errors are swallowed as warnings — no event is emitted on failure.

### Watchdog → Daedalus (stdout)

All events have an `"event"` discriminant field (snake_case):

| Event | Payload | Meaning |
|-------|---------|---------|
| `watchdog_started` | `pid` | Watchdog process is running |
| `chain_status` | `has_chain: bool` | Emitted on startup; if `false`, watchdog waits for `start_node` or `start_mithril` |
| `node_started` | `pid`, `started_at_unix_ms` | cardano-node spawned |
| `node_socket_ready` | `waited_ms` | Node socket appeared (Unix); wallet is about to start |
| `wallet_started` | `pid`, `started_at_unix_ms` | cardano-wallet spawned |
| `wallet_ready` | `port`, `waited_ms` | Wallet API is accepting connections → promise resolves |
| `wallet_exited` | `code`, `signal`, `phase` | Wallet process exited (`phase`: `pre_ready` or `post_ready`) |
| `wallet_restarting` | `attempt`, `last_exit_code`, `last_exit_signal` | Watchdog is restarting the wallet |
| `wallet_unrecoverable` | `attempt` | Circuit breaker tripped; node shutdown follows |
| `node_force_killed` | — | Node didn't exit within 30 s; was SIGKILLed |
| `node_shutdown_ms` | `ms`, `force_killed` | Time taken from shutdown-pipe close to node exit |
| `node_exited` | `code`, `signal` | cardano-node exited |
| `node_block_sync_progress` | `kind`, `progress` | Block replay / ledger validation progress (see below) |
| `mithril_status` | `phase: string` | Mithril lifecycle phase: `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, `completed` |
| `mithril_progress` | `files_downloaded`, `files_total`, `bytes_downloaded`, `bytes_total`, `seconds_elapsed`, `step_num`, `total_steps` | Download progress; rate-limited to one event per 500 ms |
| `mithril_not_needed` | `local_immutable_count`, `latest_certified_immutable` | Behind-ness probe result: gap < threshold |
| `mithril_significantly_behind` | `local_immutable_count`, `latest_certified_immutable` | Behind-ness probe result: gap ≥ threshold |
| `mithril_error` | `code: string`, `message: string` | Fatal Mithril error |
| `stopped` | — | Watchdog is done; process is about to exit |
| `error` | `message` | Fatal error during startup |

Watchdog **stderr** is plain text (tracing log lines); Daedalus logs it at INFO level but does not parse it.

---

## Block sync progress

During the initial chain sync, cardano-node emits log lines like:

```
Replayed block Progress:  0.01%
Validating chunk Progress: 99.96%
Pushing ledger state Progress: 50.00%
```

The watchdog parses these directly from the node's stdout/stderr stream inside `pipe_to_log` (no file read or `tail` involved) and emits `node_block_sync_progress` events in real time:

```json
{ "event": "node_block_sync_progress", "kind": "replayedBlock", "progress": 0.01 }
```

`kind` is one of `"replayedBlock"`, `"validatingChunk"`, `"pushingLedger"`, matching the `BlockSyncType` enum on the TypeScript side. Both `"Validating chunk"` and `"Validated chunk"` map to `"validatingChunk"`.

`CardanoWatchdog.ts` updates `WatchdogHandle.blockSyncProgress` in-place on each event. The existing `getCachedCardanoStatus` poll (every ~2 s) carries the latest snapshot to the renderer via `CardanoNode.status → NetworkStatusStore.blockSyncProgress → SyncingProgress` component.

> **Do not** add a separate push channel for block sync progress. The poll path is sufficient — the UI shows a percentage bar, not a live ticker.

---

## Graceful node shutdown

The watchdog uses an anonymous pipe (`ShutdownPipe`) to signal cardano-node to exit:

- **Unix**: the read end is duped to fd 3 in the child via `pre_exec` + `dup2`. cardano-node is started with `--shutdown-ipc 3`. When the watchdog closes the write end, fd 3 sees EOF and cardano-node shuts down gracefully.
- **Windows**: the read handle is bound as child stdin (`Command::stdin(stdio)`). cardano-node is started with `--shutdown-ipc 0` (fd 0 = stdin). Closing the write handle from the parent sends EOF to the node's stdin, triggering shutdown.

After closing the write end the watchdog waits up to 30 s for node to exit. If it doesn't, it force-kills by PID and emits `node_force_killed`.

---

## How event data reaches the Daedalus frontend

```
watchdog stdout events
  ↓
CardanoWatchdog.ts (readline, updates WatchdogHandle fields in-place)
  ↓
WatchdogHandle stored as CardanoNode._node
  ↓
CardanoNode.status getter (assembles CardanoStatus from _node fields + _status)
  ↓
getCachedCardanoStatusChannel IPC (polled by renderer every ~2 s)
  ↓
NetworkStatusStore._requestCardanoStatus → Object.assign(this, status)
  ↓
DaedalusDiagnostics / SyncingProgress components (read observables from NetworkStatusStore)
```

### Fields surfaced from WatchdogHandle

| Handle field | CardanoStatus key | Consumer |
|---|---|---|
| `pid` | `cardanoNodePID` | Diagnostics |
| `wpid` | `cardanoWalletPID` | Diagnostics |
| `watchdogPid` | `watchdogPid` | Diagnostics (when > 0) |
| `nodeStartedAt` | `cardanoNodeStartedAt` | Diagnostics |
| `walletStartedAt` | `cardanoWalletStartedAt` | Diagnostics |
| `walletRestartCount` | `cardanoWalletRestartCount` | Diagnostics |
| `nodeForceKilled` | `nodeForceKilled` | Diagnostics |
| `lastWalletExitCode` | `lastWalletExitCode` | Diagnostics |
| `lastWalletExitSignal` | `lastWalletExitSignal` | Diagnostics |
| `nodeSocketWaitMs` | `nodeSocketWaitMs` | Diagnostics |
| `walletReadyWaitMs` | `walletReadyWaitMs` | Diagnostics |
| `blockSyncProgress` | `blockSyncProgress` | SyncingProgress component |

`watchdogPid` is available as soon as `watchdog_started` fires, which happens before `wallet_ready` (i.e., before the `startWatchdog()` promise resolves). So `CardanoNode._node` always has a non-zero `watchdogPid` by the time it is set.

`blockSyncProgress` defaults to `{ replayedBlock: 0, validatingChunk: 0, pushingLedger: 0 }` when `_node` is null, so `Object.assign` never clobbers the renderer observable with `undefined`.

---

## Shutdown sequence

**Controlled shutdown** (`Daedalus → stop`):
1. `CardanoNode.stop()` calls `handle.stop(timeoutSeconds)`.
2. `handle.stop()` writes `{"cmd":"stop"}` to stdin, then closes stdin (EOF).
3. Watchdog receives stop command (or EOF); stops the wallet gracefully (SIGTERM / CTRL_BREAK_EVENT, 10 s timeout).
4. Closes the node shutdown pipe → node receives EOF → graceful exit.
5. Waits up to 30 s; force-kills if needed.
6. Emits `node_shutdown_ms`, `stopped`; process exits.
7. `handle.stop()` promise resolves on `proc.once('exit')`.

**Wallet unrecoverable** (circuit breaker):
1. Watchdog emits `wallet_unrecoverable`, breaks out of supervisor loop.
2. Watchdog shuts down node (same as above).
3. Watchdog process exits.
4. `proc.on('exit')` fires in `CardanoWatchdog.ts` → promise rejects (deferred from `wallet_unrecoverable`).
5. `CardanoNode.ts` `.catch()` → `_handleCardanoNodeError()` → `restart()`.

The rejection is **deferred to process exit** (not fired immediately on `wallet_unrecoverable`) so that the new watchdog never races the old cardano-node for the chain database lock file.

---

## Platform notes

| Concern | Unix | Windows |
|---------|------|---------|
| Node socket wait | Poll `Path::exists()` every 500 ms | Immediate return; wallet retries |
| Node socket path | `<state_dir>/cardano-node.socket` | `\\.\pipe\cardano-node.socket` |
| Stale socket cleanup | `tokio::fs::remove_file` at startup | No-op (Named Pipes are not files) |
| Graceful node stop | Close fd → EOF on fd 3 | Close HANDLE → EOF on fd 0 (stdin) |
| Wallet/node signal | SIGTERM | `GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT)` |
| Process group | Default | `CREATE_NEW_PROCESS_GROUP` (so CTRL_BREAK targets only that process) |
| Log rotation | `file-rotate` 0.8, cross-platform | Same |

---

## Mithril sync

The watchdog owns the entire Mithril snapshot sync lifecycle. TypeScript only triggers it and displays events.

### Startup flow

On startup the watchdog emits `chain_status` before doing anything else. If `has_chain` is `false`, the supervisor waits for a `start_node` or `start_mithril` command instead of immediately spawning node/wallet. This gives the UI time to show the bootstrap decision dialog.

### Behind-ness probe

The probe runs `mithril-client cardano-db snapshot show latest --json` and compares the `beacon.immutable_file_number` field against the highest numeric prefix found among files in the chain's `immutable/` directory. The comparison is done in Rust; no TypeScript touches the result.

The probe is triggered two ways:
1. **Implicitly** by `start_mithril` — if the gap is below `behind_threshold` (default 20 chunks), the pipeline stops and emits `mithril_not_needed`.
2. **Explicitly** by `probe_mithril` — the probe runs asynchronously; result is `mithril_significantly_behind` (gap ≥ threshold) or `mithril_not_needed` (gap < threshold). Probe errors are logged as warnings and no event is emitted.

### Mithril status phases

| Phase | When |
|-------|------|
| `preparing` | Probe passed; staging directory being set up |
| `downloading` | First `mithril-client` progress event with `files_total > 0` received |
| `verifying` | Ledger step of the download (no `files_total`) |
| `converting` | `snapshot-converter` is running |
| `installing` | Staged files being moved into the chain directory |
| `finalizing` | Install complete; node/wallet about to start |
| `completed` | Wallet API is ready after Mithril-boosted startup |

### Cancellation

`cancel_mithril` sends SIGKILL directly to the in-flight `mithril-client` or `snapshot-converter` process. Graceful termination is not needed — these processes hold no locks requiring orderly release, and the watchdog cleans up the staging directory regardless of how they exit.

### Install modes

- **Bootstrap** (no local chain): removes old chain directory, renames staged `db/` to chain path.
- **Partial sync** (local chain exists, gap ≥ 1): moves new immutable files into existing chain directory; replaces `ledger/` and `lsm/` directories.
- **Ledger-only** (local chain at or ahead of certified tip): passes `--start <certified> --end <certified>`; only ledger state is downloaded; existing immutables are untouched.

### TypeScript relay

`CardanoWatchdog.ts` handles all Mithril events in its readline switch and calls callbacks on `WatchdogHandle`. `MithrilController.ts` is a pure event relay — it routes callbacks to either the bootstrap IPC channel or the partial sync IPC channel depending on startup mode. All Mithril orchestration (process management, staging, marker file) lives in Rust.

---

## Key source files

| File | Purpose |
|------|---------|
| `watchdog/src/main.rs` | Entry point: reads config from first stdin line, spawns command reader task, calls `supervisor::run()` |
| `watchdog/src/supervisor.rs` | All supervisor logic: spawn, monitor, restart, shutdown, log rotation, block sync parsing, Mithril command routing |
| `watchdog/src/mithril.rs` | Full Mithril lifecycle: probe, download, install, cancellation |
| `watchdog/src/protocol.rs` | `Event` and `Command` serde types; `emit()` helper |
| `watchdog/src/config.rs` | `WatchdogConfig` / `NodeConfig` / `WalletConfig` / `MithrilConfig` serde types |
| `source/main/cardano/CardanoWatchdog.ts` | TypeScript wrapper: builds args, spawns watchdog, parses events, exposes `WatchdogHandle` |
| `source/main/cardano/CardanoNode.ts` | State machine that owns `WatchdogHandle`; surfaces status via IPC |
| `source/main/mithril/MithrilController.ts` | Pure event relay: routes watchdog Mithril callbacks to renderer IPC channels |
| `source/main/ipc/mithrilBootstrapChannel.ts` | Bootstrap decision flow IPC channel |
| `source/common/types/cardano-node.types.ts` | `CardanoStatus` and `BlockSyncProgress` types (shared between main and renderer) |
| `source/renderer/app/stores/NetworkStatusStore.ts` | Polls `getCachedCardanoStatus` and stores results as MobX observables |
| `source/renderer/app/components/status/DaedalusDiagnostics.tsx` | Renders watchdog/node/wallet diagnostics rows |
| `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/` | Renders block sync progress bar using `blockSyncProgress` observable |
