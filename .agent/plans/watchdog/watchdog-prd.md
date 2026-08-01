# Watchdog & Mithril-in-Watchdog Architecture PRD

## Why We're Switching to Watchdog

The watchdog is not a bug fix or an incremental improvement — it is a deliberate architectural shift in how Daedalus relates to its backends. The Daedalus frontend is being rebuilt as part of a broader refresh, and moving backend process management out of Electron is the foundational step that makes the rest possible. Without a clean separation between "the UI" and "the backend supervisor," the two concerns remain entangled in ways that slow down every future change.

**This is a significant part of an architectural refresh.** The old design managed `cardano-node` and `cardano-wallet` as direct Node.js child processes inside the Electron main process. Process supervision, log parsing, Mithril orchestration, restart logic, and platform workarounds all lived in the same TypeScript layer as the application shell. That meant you couldn't test backend behavior without running Electron, couldn't change the startup sequence without risking UI regressions, and couldn't reason about process state separately from UI state. The watchdog breaks that coupling. Electron is now a thin event consumer: it writes a JSON config file, spawns a single binary, and receives structured status events on stdout. Everything that happens between "process started" and "wallet ready" is owned by the watchdog. This boundary is the prerequisite for the larger frontend rewrite — the new UI can be built against a stable, well-specified IPC contract rather than being tangled with process management internals.

**Easier to debug node and wallet issues.** When something goes wrong, structured JSON events tell you exactly what happened and when — `node_started`, `wallet_exited` with exit code, `wallet_restarting` with attempt count, `mithril_error` with message. Previously, debugging required correlating timestamps across multiple log files (node stdout, wallet stdout, Daedalus application log) that were written by competing handles and sometimes truncated during rotation. The watchdog streams all relevant events in sequence over a single channel, which can be replayed or inspected with any JSON tool. The watchdog binary can also be run standalone with a hand-crafted config file — no Electron needed — which makes reproducing backend bugs dramatically faster.

**Removes a ton of complexity.** The migration deletes or drastically simplifies several large, error-prone subsystems:
- The ~800-line `MithrilPartialSyncService.ts` TypeScript state machine is gone — Mithril lifecycle is now owned entirely by the watchdog.
- The log-tailing reader that parsed block sync progress from on-disk files is gone — progress events flow directly through the IPC stream.
- The platform-specific `wait_for_socket` workaround (which returned immediately on Windows because Named Pipes don't appear as filesystem entries) is gone — startup is now gated on the `chainDbReady` log event on all platforms.
- The competing `rfs` file handles that caused interleaved output and Windows locking issues are gone — log rotation happens in a single Rust writer.
- The restart race where a new watchdog could start before the old one finished shutting down `cardano-node` is gone — promise rejection is now deferred until the watchdog process exits.

---

## Overview

This document covers two related features shipped on the `sl/wallet-bump` branch:

1. **Rust Process Supervisor** — replace the Node.js/TypeScript process-management layer for `cardano-node` and `cardano-wallet` with a cross-platform Rust binary (`cardano-watchdog`) that owns process lifetimes, restart logic, log rotation, and runtime metric collection.

2. **Mithril Sync in Rust Watchdog** — move the entire Mithril snapshot sync lifecycle out of the ~800-line `MithrilPartialSyncService.ts` TypeScript state machine and into the watchdog binary. The watchdog owns `mithril-client` and `snapshot-converter` process management, staging directory, marker file, and behind-ness probe.

In both cases, Electron becomes a thin event consumer: it configures the watchdog via JSON on startup and receives structured status events over a newline-delimited JSON IPC protocol.

---

## Part 1: Rust Process Supervisor

### Problem Statement

The original Daedalus architecture managed both Cardano backends as direct Node.js child processes from the Electron main process. This created a set of compounding problems:

**Platform fragility.** Node.js signal handling, Named Pipe paths, and process group semantics differ significantly between Unix and Windows. The `--socket-path` and `--node-socket` arguments received file paths on Windows instead of `\\.\pipe\cardano-node.socket`. There was no abstraction layer to isolate these differences from the application logic.

**No restart circuit breaker.** The TypeScript state machine restarted the wallet indefinitely on failure. A permanently broken wallet would loop forever.

**Broken log rotation.** `pipe_to_file` in the Rust watchdog appended logs without bound. The TypeScript side had competing `rfs` file handles that never received data because the watchdog wrote to its own handle. Two processes holding competing handles to the same log file is unsound on Windows (exclusive locking) and produces interleaved output on Unix.

**Block sync progress depended on log tailing.** Daedalus parsed progress percentages by tailing the on-disk log file with a separate TypeScript reader. This reader competed with the Rust writer handle, missed lines during rotation, and broke entirely when file-rotate renamed the active log file. The data traveled through two IPC hops (file → tail → push channel → renderer) instead of flowing directly through the existing metrics path.

**Restart race on wallet_unrecoverable.** Immediately rejecting the startup promise on `wallet_unrecoverable` triggered a new watchdog before the old one had shut down cardano-node. The new node could race the old node for the chain DB lock file.

### Goals

- Own all process lifetimes (spawn, monitor, restart, kill) in the Rust watchdog.
- Implement a configurable restart circuit breaker for cardano-wallet.
- Implement cross-platform log rotation in Rust using the `file-rotate` crate.
- Parse block sync progress from the live node stdout/stderr stream in Rust and surface it through the existing IPC poll path.
- Produce correct Windows Named Pipe paths for both backends.
- Eliminate the restart race by deferring TypeScript promise rejection until the watchdog process fully exits.
- Keep the IPC protocol minimal: one config write on stdin, structured events on stdout, one `stop` command on stdin.

### Non-Goals

- Replacing the Electron main process entirely; Electron still owns application lifecycle, UI, and all non-backend IPC.
- Rewriting the `CardanoNode.ts` state machine beyond the changes needed to wire the watchdog handle.
- Adding a separate push channel for block sync metrics; the existing `getCachedCardanoStatus` poll is sufficient.
- Parsing wallet log lines; block sync events come from cardano-node only.
- Supporting multiple node socket paths; the Named Pipe name is fixed (`cardano-node.socket`).

### Inputs And Source Material

- `.agent/system/architecture.md`
- `.agent/system/watchdog.md` (created as part of this work)
- `.agent/workflows/ipc.md`
- `watchdog/src/main.rs`
- `watchdog/src/supervisor.rs`
- `watchdog/src/protocol.rs`
- `watchdog/src/config.rs`
- `watchdog/Cargo.toml`
- `source/main/cardano/CardanoWatchdog.ts`
- `source/main/cardano/CardanoNode.ts`
- `source/common/types/cardano-node.types.ts`
- `source/renderer/app/stores/NetworkStatusStore.ts`
- `source/main/config.ts`

### Locked Planning Decisions

- The watchdog is a separate Rust binary, not a Rust library linked into Electron.
- Config is delivered as a single JSON line on the watchdog's stdin at startup.
- All runtime events flow watchdog-stdout → TypeScript readline → in-place mutation of `WatchdogHandle` → `CardanoNode.status` getter → `getCachedCardanoStatus` IPC poll → renderer.
- No dedicated push channel for block sync progress; the 2-second poll is sufficient for a percentage bar.
- Log rotation uses the `file-rotate` 0.8 crate: 5 MB per file, 4 rotated files kept, `AppendCount` suffix strategy (`.1`..`.4`), no compression.
- Both stdout and stderr of each backend are written to the same rotating file, sharing one `Arc<Mutex<FileRotate<AppendCount>>>` to prevent interleaving.
- Block sync progress is parsed in `pipe_to_log` directly from the node's live stream, not from disk.
- `wallet_unrecoverable` does not immediately reject the TypeScript promise; rejection is deferred to `proc.on('exit')` so the old node fully releases the chain DB lock before restart.
- The circuit breaker limit is `max_restart_attempts = 10` by default, configurable in the `WatchdogConfig`.

### Requirements

#### Functional Requirements

- [x] Watchdog spawns cardano-node and cardano-wallet as child processes.
- [x] Watchdog waits for the node socket before starting the wallet (Unix: polls `Path::exists()`; Windows: immediate, wallet retries internally).
- [x] Watchdog restarts the wallet on exit with configurable delay and attempt limit.
- [x] Watchdog emits `wallet_unrecoverable` and shuts down after `max_restart_attempts` failures.
- [x] Watchdog shuts down gracefully on `{"cmd":"stop"}` or stdin EOF.
- [x] Watchdog force-kills cardano-node after 30 s if it doesn't exit, emits `node_force_killed`.
- [x] Watchdog rotates logs for both backends: 5 MB per file, 4 rotations, `AppendCount` suffix.
- [x] Watchdog parses block sync progress from node stdout/stderr and emits `node_block_sync_progress` events.
- [x] TypeScript defers startup promise rejection until watchdog process exit.
- [x] Windows Named Pipe paths are correct for both `--socket-path` (node) and `--node-socket` (wallet).

#### Non-Functional Requirements

- Cross-platform: all Rust code must compile and behave correctly on Linux, macOS, and Windows.
- No regex crate in the watchdog (avoids compile-time complexity); block sync parsing uses `str::contains` and manual substring extraction.
- Log rotation crate must not use `#[cfg(unix)]` in its construction path.
- TypeScript changes must not introduce new IPC channels or break existing poll timing.

### Technical Design

#### Process Tree

```
Electron main process (Node.js)
└── cardano-watchdog          ← Rust binary, child of Electron
    ├── cardano-node          ← child of watchdog
    └── cardano-wallet        ← child of watchdog
```

#### IPC Protocol

**Daedalus → watchdog (stdin):**

| Message | Meaning |
|---------|---------|
| First line: `WatchdogConfig` JSON | Full startup configuration |
| `{"cmd":"stop"}` | Graceful shutdown |
| stdin EOF | Treated as stop |

**Watchdog → Daedalus (stdout), newline-delimited JSON:**

| Event | Key fields |
|-------|-----------|
| `watchdog_started` | `pid` |
| `node_started` | `pid`, `started_at_unix_ms` |
| `node_socket_ready` | `waited_ms` |
| `wallet_started` | `pid`, `started_at_unix_ms` |
| `wallet_ready` | `port`, `waited_ms` |
| `wallet_exited` | `code`, `signal`, `phase` |
| `wallet_restarting` | `attempt`, `last_exit_code`, `last_exit_signal` |
| `wallet_unrecoverable` | `attempt` |
| `node_block_sync_progress` | `kind`, `progress` |
| `node_force_killed` | — |
| `node_shutdown_ms` | `ms`, `force_killed` |
| `node_exited` | `code`, `signal` |
| `stopped` | — |
| `error` | `message` |

#### Block Sync Progress

The Rust watchdog parses these log patterns from cardano-node's stdout/stderr stream in real time:

| Log keyword | Event `kind` |
|-------------|-------------|
| `"Replayed block"` | `"replayedBlock"` |
| `"Validating chunk"` or `"Validated chunk"` | `"validatingChunk"` |
| `"Pushing ledger state"` | `"pushingLedger"` |

Progress is extracted from the `Progress: X%` substring without the regex crate. The camelCase `kind` strings match the `BlockSyncType` enum values on the TypeScript side exactly.

#### Log Rotation

`file-rotate` 0.8 (`AppendCount::new(4)`, `ContentLimit::Bytes(5_242_880)`, `Compression::None`). Rotated files are named `node.log.1` through `node.log.4`. The `ALLOWED_NODE_LOGS` and `ALLOWED_WALLET_LOGS` regexes in `source/main/config.ts` are `^node\.log\.\d+$` and `^cardano-wallet\.log\.\d+$` respectively.

#### Deferred Rejection

```
wallet_unrecoverable → sets pendingRejectionMessage (does NOT reject)
watchdog proc.on('exit') → rejects with pendingRejectionMessage if set
```

This guarantees the chain DB lock is released before `CardanoNode.ts` triggers a restart.

#### Data Flow

```
watchdog stdout events
  ↓
CardanoWatchdog.ts (readline → WatchdogHandle fields updated in-place)
  ↓
CardanoNode._node (WatchdogHandle reference)
  ↓
CardanoNode.status getter (assembles CardanoStatus including blockSyncProgress)
  ↓
getCachedCardanoStatusChannel IPC (polled every ~2 s)
  ↓
NetworkStatusStore._requestCardanoStatus → Object.assign(this, status)
  ↓
SyncingProgress / DaedalusDiagnostics components
```

#### Components Affected

- `watchdog/src/supervisor.rs` — log rotation, block sync parsing, pipe_to_log
- `watchdog/src/protocol.rs` — `NodeBlockSyncProgress` event variant
- `watchdog/Cargo.toml` / `Cargo.lock` — `file-rotate = "0.8"` dependency
- `source/main/cardano/CardanoWatchdog.ts` — Windows paths, deferred rejection, blockSyncProgress handle field and event handler
- `source/main/cardano/CardanoNode.ts` — blockSyncProgress in status getter, dead rfs streams removed
- `source/common/types/cardano-node.types.ts` — `blockSyncProgress` field on `CardanoStatus`
- `source/renderer/app/stores/NetworkStatusStore.ts` — removed push channel, added blockSyncProgress to poll path
- `source/main/index.ts` — removed `handleCheckBlockReplayProgress` call
- `source/main/config.ts` — updated log rotation filename regexes

**Deleted:**
- `source/main/utils/handleCheckBlockReplayProgress.ts`
- `source/main/utils/blockSyncProgressHelpers.ts`
- `source/main/utils/blockSyncProgressHelpers.spec.ts`
- `source/main/ipc/get-block-sync-progress.ts`
- `source/renderer/app/ipc/getBlockSyncChannel.ts`

### Implementation Strategy

1. Fix Windows Named Pipe paths in `CardanoWatchdog.ts` (`buildNodeArgs`, `buildWalletArgs`, `watchdogConfig.node.socket_path`).
2. Fix the restart race: introduce `pendingRejectionMessage` and defer rejection to `proc.on('exit')`.
3. Implement log rotation in Rust: add `file-rotate` dependency, `open_log()` helper, `RotatingLog` type alias, update `pipe_to_log` signature.
4. Move block sync parsing into `pipe_to_log` with a `parse_sync_progress` flag; add `try_parse_block_sync()`.
5. Add `NodeBlockSyncProgress` to `protocol.rs` and emit it from `pipe_to_log`.
6. Wire `blockSyncProgress` through TypeScript: add field to `WatchdogHandle`, handle event in readline loop, add to `CardanoNode.status` getter with zero-filled default.
7. Add `blockSyncProgress` to `CardanoStatus` in shared types.
8. Remove the tail-based pipeline from TypeScript/IPC layers.
9. Update `ALLOWED_NODE_LOGS` / `ALLOWED_WALLET_LOGS` regexes to match new rotation suffix format.
10. Document architecture in `.agent/system/watchdog.md`.

### Testing Strategy

- **Clippy**: `nix build .#checks.x86_64-linux.watchdog-clippy` — catches unused imports, dead code, type errors.
- **treefmt**: formatting validation for Rust (rustfmt) and TypeScript (prettier).
- **TypeScript compile**: `yarn compile` — catches type mismatches in IPC contract changes.
- **Manual QA**: observe `node_block_sync_progress` events in Electron logs during initial chain sync; verify percentage bar advances; confirm logs rotate at 5 MB.
- **Platform QA**: Windows smoke test for Named Pipe path correctness and wallet connectivity.

### Rollout / Migration / Rollback

- No feature flag; the watchdog binary is already a hard dependency of the launcher config.
- Old TypeScript log tail pipeline is removed completely; no fallback path.
- Rollback: revert the branch. There is no data migration; log files are append-only and the new rotation suffix scheme (`.1`..`.4`) is backward-compatible with what the old `rfs` library produced.

---

## Part 2: Mithril Sync in Rust Watchdog

### Problem Statement

The Mithril implementation was an ~800-line TypeScript state machine (`MithrilPartialSyncService.ts`) that:

1. **Duplicates process management** already done in Rust — spawns child processes, captures their stdout, cancels them via `SIGTERM`, and tracks exit codes — all in TypeScript which has weaker platform guarantees than the Rust layer already managing `cardano-node` and `cardano-wallet`.
2. **Interleaves tightly with node startup** — the finalizing → starting-node transition is wired through `MithrilPartialSyncNodeStartup.ts`, which depends on `CardanoNode.ts` state and `handleDiskSpace.ts` fallback logic. This coupling caused a hang bug (node stop no-op during STARTING state) requiring multi-file workarounds.
3. **Requires 7 Electron IPC channels** for a lifecycle that is fundamentally a single operation with progress updates.
4. **Cannot easily be restarted safely** — TypeScript process groups and signal delivery are unreliable across platforms; the Rust supervisor already handles this for the two backends.
5. **Duplicates network config** — aggregator URLs and verification key fetching exist only in TypeScript, not in the Rust binary that will need them.

### Goals

- Move `mithril-client` subprocess management entirely to the Rust watchdog.
- Move `snapshot-converter` subprocess management entirely to the Rust watchdog.
- Move staging directory management, marker file, and behind-ness probe to Rust.
- Move network config (aggregator URLs, verification keys) to Rust via the watchdog startup config.
- Expose a single command to the TypeScript layer: `{"cmd":"start_mithril"}`.
- Emit structured progress events over the existing watchdog stdout channel.
- Reduce the TypeScript IPC surface to: one trigger channel + one status subscription.
- Delete all TypeScript Mithril orchestration code; keep only the renderer store and display logic.

### Non-Goals

- Rewriting the renderer Mithril UI components or i18n strings.
- Adding log rotation for `mithril-client` or `snapshot-converter` output (these are short-lived; inline logging to watchdog stderr is sufficient for debugging).
- Exposing detailed `snapshot-converter` sub-progress to the UI (a simple `converting` phase with no byte counter is acceptable).
- Changing the cardano-node/wallet restart flow beyond removing the Mithril startup coupling.
- Implementing Mithril behind-ness thresholds in the UI; the watchdog decides whether Mithril is beneficial.

### Inputs And Source Material

- `.agent/plans/watchdog/watchdog-prd.md` — Part 1 above
- `.agent/system/watchdog.md` — watchdog IPC protocol reference
- `.agent/plans/mithril-watchdog/research/01-typescript-mithril-survey.md` — full TypeScript Mithril code survey
- `source/main/mithril/MithrilPartialSyncService.ts` — ~800-line state machine to delete
- `source/main/mithril/MithrilController.ts` — orchestration/IPC bridge to simplify
- `source/main/ipc/mithrilPartialSyncChannel.ts` — 7 IPC channels to collapse
- `source/main/cardano/CardanoWatchdog.ts` — existing watchdog IPC TypeScript layer
- `watchdog/src/supervisor.rs` — existing Rust supervisor
- `watchdog/src/protocol.rs` — existing event enum

### Locked Planning Decisions

- Mithril lifecycle is owned 100% by the Rust watchdog; TypeScript only triggers and displays.
- The watchdog receives a `start_mithril` command on stdin; it decides internally whether Mithril is behind enough to be worthwhile.
- All Mithril progress events use the same stdout JSON-lines channel as node/wallet events.
- The marker file (`mithril-partial-sync.lock`) is read and written by Rust only; TypeScript no longer touches it.
- Cancellation is via `{"cmd":"cancel_mithril"}` on stdin; the watchdog sends SIGKILL directly to the mithril-client or snapshot-converter process. Graceful termination is not needed — these processes hold no locks that require orderly release, and the staging directory is cleaned up by the watchdog regardless.
- Network config (aggregator URL, verification key) is passed in the initial startup config JSON, not fetched at runtime by TypeScript.
- The `snapshot-converter` binary path is passed in the watchdog startup config alongside `mithril_bin`; it does not emit structured progress and the watchdog emits a flat `converting` phase with no sub-progress.
- The behind-ness probe runs in Rust via a `mithril-client` JSON query; threshold default is 20 immutable chunk numbers, watchdog-config-only (not user-configurable).
- The local immutable position is determined by parsing filenames in the chain's `immutable/` directory to find the highest chunk index, which matches `immutable_file_number` semantics from the Mithril beacon.
- Partial sync installs by merging new immutable files into the existing chain directory and replacing only the ledger/lsm directories; bootstrap installs by replacing the entire chain directory.
- TypeScript `MithrilPartialSyncService`, `mithrilCommandRunner`, `mithrilSnapshotConverter`, `mithrilPartialSyncStaging`, `mithrilPartialSyncMarker`, `mithrilNetworkConfig`, and `MithrilPartialSyncNodeStartup` are deleted.
- `MithrilController.ts` is simplified to an event relay; it no longer orchestrates anything.

### Requirements

#### Functional Requirements

- [x] Watchdog accepts `{"cmd":"start_mithril"}` on stdin and begins the Mithril sync lifecycle.
- [x] Watchdog accepts `{"cmd":"cancel_mithril"}` on stdin and SIGKILLs the in-flight mithril-client or snapshot-converter, then cleans up the staging directory.
- [x] Watchdog accepts `{"cmd":"probe_mithril"}` on stdin at any point while node/wallet are running (or during socket-wait); runs the behind-ness probe asynchronously and emits `mithril_significantly_behind` or `mithril_not_needed` with the result. Probe errors are logged as warnings and no event is emitted.
- [x] Watchdog performs a behind-ness probe: runs `mithril-client cardano-db snapshot show latest --json`, compares `beacon.immutable_file_number` to highest local immutable chunk index. If not behind by ≥ threshold, emits `mithril_not_needed` and stops.
- [x] Watchdog runs `mithril-client cardano-db download latest ...` with `--origin-tag DAEDALUS`, `--include-ancillary`, `AGGREGATOR_ENDPOINT` / `GENESIS_VERIFICATION_KEY` / `ANCILLARY_VERIFICATION_KEY` env vars from config.
- [x] When local chain data exists, watchdog passes `--start <local+1> --end <certified> --allow-override`; when at or ahead of certified tip, passes `--start <certified> --end <certified> --allow-override` (ledger-state-only download); on fresh bootstrap, passes no range flags.
- [x] Watchdog streams mithril-client stdout/stderr JSON lines and emits `mithril_progress` events, rate-limited to 500 ms.
- [x] Watchdog emits `mithril_status` phase events for: `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, `completed`.
- [x] The `downloading` status is deferred until the first progress event with `files_total > 0`; prior events use the current tracked status.
- [x] Watchdog manages staging directory at `<stateDir>/mithril-partial-sync/download/` — creates, validates, and removes on error or cancellation.
- [x] Watchdog writes and reads marker file at `<stateDir>/Logs/mithril-partial-sync.lock`.
- [x] For bootstrap: watchdog removes old chain directory and renames staging db to chain path.
- [x] For partial sync: watchdog moves new immutable files into existing chain directory and replaces ledger/lsm directories.
- [x] After install, watchdog emits `mithril_status { phase: "finalizing" }` and starts cardano-node and cardano-wallet via its normal supervisor flow.
- [x] Watchdog emits `mithril_error` with an error code and message on any failure.
- [x] TypeScript receives `mithril_status`, `mithril_progress`, `mithril_not_needed`, `mithril_significantly_behind`, and `mithril_error` events and forwards them to the renderer via the existing partial sync IPC channel.

#### Non-Functional Requirements

- Rust code must compile and behave correctly on Linux, macOS, and Windows.
- The `mithril-client` and `snapshot-converter` binary paths are passed in the watchdog startup config.
- Aggregator URL and verification keys are passed in the startup config JSON.
- Progress event rate-limiting: emit at most one `mithril_progress` event per 500 ms.

### Technical Design

#### Rust Module Layout

New file: `watchdog/src/mithril.rs`

```
MithrilConfig {
  mithril_bin: String,
  snapshot_converter_bin: String,
  converter_config: String,
  aggregator_url: String,
  genesis_vkey: String,
  ancillary_vkey: Option<String>,
  state_dir: String,
  chain_path: String,
  behind_threshold: u64,   // default 20
}
```

`watchdog/src/supervisor.rs` — handles `start_mithril` / `cancel_mithril` stdin commands; calls `mithril::run_pipeline()`; routes `MithrilEvent` → protocol event → stdout JSON line.

`watchdog/src/protocol.rs` — new variants: `MithrilStatus`, `MithrilProgress`, `MithrilNotNeeded`, `MithrilError`, `ChainStatus`.

`watchdog/src/config.rs` — `MithrilConfig` field on `WatchdogConfig` (optional; if absent, Mithril commands are rejected).

#### IPC Protocol — New Events (watchdog stdout)

| Event | Key fields |
|-------|-----------|
| `chain_status` | `has_chain: bool` — emitted on startup; if `false`, watchdog waits for `start_node` or `start_mithril` |
| `mithril_status` | `phase: string` — one of `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, `completed` |
| `mithril_progress` | `files_downloaded`, `files_total`, `bytes_downloaded`, `bytes_total`, `seconds_elapsed`, `step_num`, `total_steps` |
| `mithril_not_needed` | `local_immutable_count`, `latest_certified_immutable` — probe result when gap < threshold |
| `mithril_significantly_behind` | `local_immutable_count`, `latest_certified_immutable` — probe result when gap ≥ threshold |
| `mithril_error` | `code: string`, `message: string` |

The `completed` phase is emitted by the supervisor (not `mithril.rs`) once the wallet's HTTP API is ready after a Mithril-boosted startup, signalling to the UI that the full stack is live.

The `mithril_significantly_behind` and `mithril_not_needed` events are emitted both by the proactive `start_mithril` behind-ness check (where `mithril_not_needed` ends the pipeline) and in response to `probe_mithril` (where neither event triggers any action — the UI decides what to show).

#### IPC Protocol — New Commands (watchdog stdin)

| Command | Effect |
|---------|--------|
| `{"cmd":"start_node"}` | Start cardano-node/wallet without Mithril (used after user declines bootstrap) |
| `{"cmd":"start_mithril", "force": bool}` | Begin Mithril lifecycle; `force: true` skips behind-ness check |
| `{"cmd":"cancel_mithril"}` | SIGKILL the in-flight mithril-client or snapshot-converter and clean up staging |
| `{"cmd":"probe_mithril"}` | Run behind-ness probe asynchronously; emits `mithril_significantly_behind` or `mithril_not_needed`; accepted during socket-wait and while node/wallet are running |

#### TypeScript Changes

**`source/main/cardano/CardanoWatchdog.ts`**:
- Extend `WatchdogHandle` with `hasChain: boolean | null`, `startNode()`, `startMithril()`, `cancelMithril()`.
- Add `onChainStatus?` callback parameter to `startWatchdog()`.
- Handle `chain_status`, `mithril_status`, `mithril_progress`, `mithril_not_needed`, `mithril_significantly_behind`, `mithril_error` events in the readline switch.

**`source/main/ipc/mithrilBootstrapChannel.ts`** (new, for bootstrap flow):
- Handles the chain_status → decision dialog → start/decline/cancel flow for first-time bootstrap.

**`source/main/mithril/MithrilController.ts`**:
- Simplified to an event relay: receives watchdog callbacks → pushes to renderer via existing IPC channels.
- Routes events to either the partial sync channel or bootstrap channel based on mode.

#### Data Flow

```
Frontend action ("Use Mithril" / "Sync Normally")
  → bootstrap decision IPC or partial sync trigger IPC
  → MithrilController.ts
  → handle.startMithril() / handle.startNode()
  ↓
Rust watchdog mithril.rs
  → mithril-client subprocess
  → snapshot-converter subprocess
  → node/wallet start (via supervisor)
  ↓
mithril_status / mithril_progress events on watchdog stdout
  ↓
CardanoWatchdog.ts readline
  → MithrilController.ts callbacks
  ↓
IPC push to renderer
  ↓
MithrilBootstrapStore / MithrilPartialSyncStore
  → UI components
```

#### Components Affected

**Rust (watchdog)**:
- `watchdog/src/mithril.rs` — new file; full Mithril lifecycle
- `watchdog/src/supervisor.rs` — command routing; MithrilEvent → stdout; chain_status probe
- `watchdog/src/protocol.rs` — new event variants
- `watchdog/src/config.rs` — `MithrilConfig` added to `WatchdogConfig`

**TypeScript (main)**:
- `source/main/cardano/CardanoWatchdog.ts` — extend handle types + callbacks + methods
- `source/main/cardano/CardanoNode.ts` — wire chain_status callback
- `source/main/ipc/mithrilBootstrapChannel.ts` — new; bootstrap decision flow
- `source/main/mithril/MithrilController.ts` — simplified to event relay

**Deleted TypeScript files**:
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/mithrilCommandRunner.ts`
- `source/main/mithril/mithrilSnapshotConverter.ts`
- `source/main/mithril/mithrilPartialSyncStaging.ts`
- `source/main/mithril/mithrilPartialSyncMarker.ts`
- `source/main/mithril/mithrilNetworkConfig.ts`
- `source/main/mithril/MithrilPartialSyncNodeStartup.ts`
- `source/main/mithril/mithrilPartialSyncPreflight.ts`

### Implementation Strategy

#### Phase 1 — Rust Mithril State Machine

1. Add `MithrilConfig` to `watchdog/src/config.rs`.
2. Write `watchdog/src/mithril.rs` with:
   - Behind-ness probe (parse highest immutable chunk index from filenames vs `beacon.immutable_file_number`).
   - Download phase with `--start`/`--end` range computed from local chain state.
   - Staging directory creation, validation, and cleanup.
   - `install_staged()`: merge for partial sync, full replace for bootstrap.
   - Marker file read/write.
   - `snapshot-converter` spawn and wait.
   - Cancellation via `tokio::select!` on a cancel channel.
3. Add Mithril event variants to `watchdog/src/protocol.rs`.
4. Wire `start_mithril` / `cancel_mithril` / `start_node` stdin commands in `watchdog/src/supervisor.rs`.
5. Emit `chain_status` on startup to gate the decision flow.

#### Phase 2 — TypeScript Relay Layer

6. Extend `WatchdogHandle` and `startWatchdog()` in `CardanoWatchdog.ts`.
7. Wire `onChainStatus` callback in `CardanoNode.ts`.
8. Write `source/main/ipc/mithrilBootstrapChannel.ts`.
9. Simplify `MithrilController.ts` to a pure event relay with bootstrap/partial-sync mode routing.

#### Phase 3 — Cleanup

10. Delete the TypeScript Mithril orchestration files listed above.
11. Run `yarn compile` and `cargo check` to verify no dead code or type errors.
12. Update `.agent/system/watchdog.md` with Mithril IPC events.

### Testing Strategy

- **Rust clippy**: `nix build .#checks.x86_64-linux.watchdog-clippy` — catches type errors and unused code.
- **TypeScript compile**: `yarn compile` — verifies IPC contract changes.
- **Manual QA — bootstrap**: trigger from a clean chain directory; observe decision dialog → mithril_status events → node/wallet start.
- **Manual QA — partial sync**: trigger on a partially-synced node; confirm only delta chunks are downloaded and existing immutables are preserved.
- **Manual QA — ledger-only**: trigger when local chain is at or ahead of certified tip; confirm only ledger state is downloaded.
- **Manual QA — cancel**: start Mithril, cancel mid-download; confirm `cancelled` phase and clean staging directory.
- **Manual QA — not needed**: trigger on a node that is already synced; confirm `mithril_not_needed` event and no download.

### Rollout / Migration / Rollback

- No feature flag; the watchdog owns Mithril when `mithril` config is present.
- Existing marker files use the same JSON format; the Rust reader handles all marker states.
- Rollback: revert the branch. Old TypeScript Mithril code is deleted, so rollback restores it.
- The staging directory location is unchanged; a partially-downloaded staging dir will be cleaned up by the Rust layer on next run.

---

**Branch:** `sl/wallet-bump`  
**Author:** Sam Leathers
