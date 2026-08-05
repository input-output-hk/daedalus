# Watchdog Integration PRD

## Overview

This document describes the TypeScript/Electron integration layer for the `cardano-watchdog` Rust binary from first principles. There is no existing code to build on. The goal is the leanest possible layer that:

1. Spawns the watchdog with correct config
2. Relays events to the renderer
3. Relays commands from the renderer to the watchdog
4. Drives the loading screen through bootstrap, Mithril sync, and normal startup

The watchdog owns all process management, log rotation, restart logic, and Mithril orchestration. TypeScript owns application lifecycle, window management, and UI.

---

## Watchdog IPC Contract

### Config (first stdin line)

```jsonc
{
  "node": {
    "exe": "/absolute/path/to/cardano-node",
    "args": ["run", "--config", "...", ...],   // all args; watchdog appends --shutdown-ipc
    "state_dir": "/state/mainnet",
    "socket_path": "/state/mainnet/cardano-node.socket"
    //            or "\\\\.\pipe\\cardano-node.socket" on Windows
  },
  "wallet": {
    "exe": "/absolute/path/to/cardano-wallet",
    "args": ["serve", "--port", "44123", "--node-socket", "...", ...],
    "state_dir": "/state/mainnet",
    "api_port": 44123,             // required; wallet port is echoed back in wallet_ready
    "restart_delay_ms": 1000,      // optional, default 1000
    "max_restart_attempts": 10     // optional, default 10
  },
  "node_log_file": "/logs/node.log",
  "wallet_log_file": "/logs/cardano-wallet.log",
  "mithril": {                     // optional; omit to disable Mithril commands
    "mithril_bin": "/absolute/path/to/mithril-client",
    "snapshot_converter_bin": "/absolute/path/to/snapshot-converter",
    "converter_config": "/absolute/path/to/converter-config.json",
    "aggregator_url": "https://...",
    "genesis_vkey": "5b...",
    "ancillary_vkey": "5b...",     // optional
    "state_dir": "/state/mainnet",
    "chain_path": "/state/mainnet/chain",
    "behind_threshold": 20         // optional, default 20 immutable chunks
  }
}
```

### Commands (subsequent stdin lines)

| JSON | When to send |
|------|-------------|
| `{"cmd":"stop"}` | App is quitting or restarting the stack |
| `{"cmd":"start_node"}` | User chose normal sync after `chain_status { has_chain: false }` |
| `{"cmd":"start_mithril"}` | User chose Mithril bootstrap; watchdog probes first |
| `{"cmd":"start_mithril","force":true}` | Proactive prompt: skip probe (chain exists but ledger is behind) |
| `{"cmd":"start_mithril","wipe_chain":true}` | User explicitly requests a full re-bootstrap |
| `{"cmd":"cancel_mithril"}` | User cancels in-progress Mithril sync |
| `{"cmd":"probe_mithril"}` | Trigger an async behind-ness probe during normal sync |

After `stop`, close stdin with EOF so the Rust async reader unblocks immediately.

### Events (watchdog stdout, newline-delimited JSON)

All events have an `event` string discriminant.

**Startup / identity**

| event | fields | meaning |
|-------|--------|---------|
| `watchdog_started` | `pid: number` | Process is alive |
| `chain_status` | `has_chain: boolean` | `false` → wait for `start_node` or `start_mithril` before spawning backends |

**Node**

| event | fields |
|-------|--------|
| `node_started` | `pid`, `started_at_unix_ms` |
| `node_socket_ready` | `waited_ms` |
| `node_startup_status` | `phase: string` — one of `openingChainDb`, `openingImmutableDb`, `openedImmutableDb`, `openingVolatileDb`, `openedVolatileDb`, `openingLedgerDb`, `replayingLedger`, `openedLedgerDb`, `chainDbReady` |
| `node_block_sync_progress` | `kind: "replayedBlock" \| "validatingChunk" \| "pushingLedger"`, `progress: number` (0–100) |
| `node_force_killed` | — |
| `node_shutdown_ms` | `ms`, `force_killed: boolean` |
| `node_exited` | `code: number \| null`, `signal: string \| null` |

**Wallet**

| event | fields |
|-------|--------|
| `wallet_started` | `pid`, `started_at_unix_ms` |
| `wallet_ready` | `port: number`, `waited_ms` |
| `wallet_exited` | `code`, `signal`, `phase: "pre_ready" \| "post_ready"` |
| `wallet_restarting` | `attempt`, `last_exit_code`, `last_exit_signal` |
| `wallet_unrecoverable` | `attempt` |

**Mithril**

| event | fields | notes |
|-------|--------|-------|
| `mithril_status` | `phase: string` | `preparing \| downloading \| verifying \| converting \| installing \| finalizing \| completed \| cancelled` |
| `mithril_progress` | `files_downloaded`, `files_total`, `bytes_downloaded`, `bytes_total`, `seconds_elapsed`, `step_num`, `total_steps`, `phase: "snapshot" \| "ledger"` | rate-limited 500 ms by Rust |
| `mithril_not_needed` | `local_immutable_count`, `latest_certified_immutable` | probe result: gap < threshold |
| `mithril_significantly_behind` | `local_immutable_count`, `latest_certified_immutable` | probe result: gap ≥ threshold |
| `mithril_error` | `code: string`, `message: string` | |

**Terminal**

| event | fields |
|-------|--------|
| `stopped` | — |
| `error` | `message: string` |

---

## Main Process: WatchdogManager

One new file: `source/main/WatchdogManager.ts`

### Responsibilities

- Build and validate the `WatchdogConfig` JSON from Daedalus runtime config
- Spawn the watchdog binary with `stdio: ['pipe', 'pipe', 'pipe']`
- Write the config JSON as the first stdin line
- Read stdout as newline-delimited JSON events; dispatch each to registered handlers
- Expose `sendCommand(cmd: object)` — serialises to JSON, writes line to stdin
- Expose `stop()` — writes `{"cmd":"stop"}`, then closes stdin, returns a Promise resolving on process `exit`
- Maintain a `WatchdogState` snapshot updated by events (see below)
- Expose `onEvent(handler)` for the IPC bridge to subscribe
- Return a `Promise<number>` (`walletPort`) that resolves on `wallet_ready` and rejects on process exit if `wallet_unrecoverable` was previously received

### WatchdogState (plain object, mutated in-place)

```ts
interface WatchdogState {
  // Identity
  watchdogPid: number;
  nodePid: number;
  walletPid: number;
  nodeStartedAt: number | null;
  walletStartedAt: number | null;
  walletRestartCount: number;
  walletPort: number | null;

  // Chain/sync
  hasChain: boolean | null;          // null until chain_status received
  nodeStartupPhase: string | null;   // latest node_startup_status.phase
  blockSyncProgress: {
    replayedBlock: number;
    validatingChunk: number;
    pushingLedger: number;
  };

  // Mithril
  mithrilPhase: string | null;
  mithrilProgress: MithrilProgress | null;

  // Error
  lastError: string | null;
  walletUnrecoverable: boolean;

  // Diagnostics
  nodeSocketWaitMs: number | null;
  walletReadyWaitMs: number | null;
  nodeForceKilled: boolean;
  lastWalletExitCode: number | null;
  lastWalletExitSignal: string | null;
}
```

### Config building

WatchdogManager receives a `DaedlausRuntimeConfig` (assembled elsewhere from the nix-baked config files and user preferences). It:

1. Picks two free TCP ports (one for node P2P, one for wallet REST) using ephemeral `net.createServer()` bind-and-release.
2. Assembles `NodeConfig.args` from the node config file paths and the chosen P2P port.
3. Assembles `WalletConfig.args` from wallet config paths, the wallet port, and the node socket path.
4. On Windows: sets `socket_path` to `\\\\.\pipe\\cardano-node.socket`; on Unix: `<stateDir>/cardano-node.socket`.
5. If Mithril binaries and network config are present in `DaedlausRuntimeConfig`, builds `MithrilConfig`; otherwise omits the field.

### Deferred rejection

`wallet_unrecoverable` sets a `pendingRejection` string but does **not** immediately reject the startup promise. Rejection fires only in `proc.on('exit')`. This ensures the old cardano-node fully releases the chain DB lock before a new watchdog is spawned.

---

## Main Process: BackendLifecycle

One new file: `source/main/BackendLifecycle.ts`

This is the single owner of the WatchdogManager instance for the app's lifetime. It replaces whatever state machine previously managed cardano-node/wallet.

### Responsibilities

- On app `ready`: call `WatchdogManager.start()`, await the wallet port promise
- On wallet port resolve: tell the renderer which port to use (via `walletPortChannel`)
- On startup promise reject: schedule a restart after a brief delay
- On app `will-quit`: call `WatchdogManager.stop()`, block quit until `stopped` event received or 45 s timeout
- Expose `getState()` returning a snapshot of `WatchdogState` for IPC polling
- Expose `sendMithrilCommand(cmd)` for the IPC bridge

---

## IPC Channels

Minimal surface: one poll channel + two push channels + one command channel.

### `getCachedBackendStatus` (poll, renderer → main → renderer)

Renderer polls every 2 s. Main returns a serialised snapshot of `WatchdogState`. No push needed for status that changes slowly (PIDs, restart counts, sync phases).

Response type mirrors `WatchdogState` exactly — no mapping layer.

### `mithrilProgress` (push, main → renderer)

Main pushes each `mithril_progress` event directly to the renderer as it arrives (already rate-limited to 500 ms by Rust). No batching or transformation.

### `mithrilStatus` (push, main → renderer)

Main pushes each `mithril_status` event to the renderer immediately. Renderer uses this to transition loading screen views.

### `mithrilCommand` (invoke, renderer → main)

Renderer sends user decisions. Payload:

```ts
type MithrilCommand =
  | { cmd: 'start_mithril' }
  | { cmd: 'start_mithril'; force: true }
  | { cmd: 'start_mithril'; wipe_chain: true }
  | { cmd: 'start_node' }
  | { cmd: 'cancel_mithril' }
  | { cmd: 'probe_mithril' };
```

Main forwards directly to `WatchdogManager.sendCommand()`.

### `walletPort` (push, main → renderer, once)

Sent once when the startup promise resolves with the wallet port. Renderer uses it to initialise the wallet API client.

---

## Renderer: BackendStore (MobX)

One new MobX store: `source/renderer/app/stores/BackendStore.ts`

### Observables

```ts
class BackendStore {
  // From WatchdogState poll
  @observable hasChain: boolean | null = null;
  @observable nodeStartupPhase: string | null = null;
  @observable blockSyncProgress = { replayedBlock: 0, validatingChunk: 0, pushingLedger: 0 };
  @observable walletRestartCount = 0;
  @observable nodeForceKilled = false;
  @observable lastWalletExitCode: number | null = null;

  // From mithril_status push
  @observable mithrilPhase: string | null = null;

  // From mithril_progress push
  @observable mithrilProgress: MithrilProgress | null = null;

  // Derived
  @computed get loadingPhase(): LoadingPhase { ... }
}
```

### LoadingPhase (derived)

```ts
type LoadingPhase =
  | 'starting'               // watchdog spawned, no chain_status yet
  | 'bootstrap-decision'     // chain_status { has_chain: false }
  | 'mithril-syncing'        // mithril_status received, not yet completed/cancelled
  | 'node-starting'          // node spawned; wallet not yet ready
  | 'ready'                  // wallet_ready; app fully usable
  | 'error';                 // wallet_unrecoverable
```

### Actions

```ts
startMithril(): void        // sends { cmd: 'start_mithril' } via mithrilCommand channel
startMithrilForce(): void   // sends { cmd: 'start_mithril', force: true }
startNode(): void           // sends { cmd: 'start_node' }
cancelMithril(): void       // sends { cmd: 'cancel_mithril' }
probeMithril(): void        // sends { cmd: 'probe_mithril' }
```

---

## Loading Screen Integration

Reuse existing screen components and styling; replace all logic wiring.

### What drives which screen

| `loadingPhase` | `mithrilPhase` | Screen shown |
|---------------|---------------|-------------|
| `starting` | any | Spinner only |
| `bootstrap-decision` | — | `MithrilDecisionView` (simplified — no snapshot list; watchdog selects latest automatically) |
| `mithril-syncing` | `preparing \| downloading \| verifying \| converting \| installing \| finalizing` | `MithrilProgressView` |
| `mithril-syncing` | `completed` | `MithrilProgressView` (completed transition frame) |
| `mithril-syncing` | `cancelled` | return to `bootstrap-decision` |
| `node-starting` | — | `SyncingConnecting` with block sync progress bar |
| `node-starting` + `mithril_significantly_behind` | — | `SyncingConnecting` + `SyncingConnectingMithrilPrompt` overlay |
| `ready` | — | Normal app |
| `error` | — | Error screen |

### MithrilDecisionView simplification

The existing `MithrilDecisionView` has snapshot selection (fetched from the Mithril aggregator by TypeScript). Drop that entirely — the watchdog picks the latest snapshot internally. The simplified version shows only:

- Title and description
- Accept ("Mithril Sync") button → `BackendStore.startMithril()`
- Decline ("Blockchain Sync from genesis") button → `BackendStore.startNode()`

Reuse `MithrilDecisionView.scss` and the existing button layout.

### MithrilProgressView

Connect directly to `BackendStore.mithrilPhase` and `BackendStore.mithrilProgress`. Cancel button → `BackendStore.cancelMithril()`. No changes to the component itself.

### SyncingConnectingMithrilPrompt

Shown as an overlay on the node-starting screen when a `mithril_significantly_behind` probe result is received. The `behindByEpochs` prop is derived from `(latest_certified_immutable - local_immutable_count) / ~2160` (approximate chunks per epoch). Start → `BackendStore.startMithrilForce()`. Dismiss → hide prompt for this session.

### Proactive probe trigger

During `node-starting` phase, after the node is confirmed running (node_started received), fire `BackendStore.probeMithril()` once. Show the prompt if `mithril_significantly_behind` arrives. Do not repeat within a session.

---

## Startup Flows

### Flow A: Chain exists (normal startup)

```
spawn watchdog
  ↓ chain_status { has_chain: true }   → loadingPhase = 'node-starting'
  ↓ node_started
  ↓ node_socket_ready
  ↓ node_startup_status (phases)       → update nodeStartupPhase
  ↓ wallet_started
  ↓ wallet_ready { port }              → loadingPhase = 'ready'; push walletPort to renderer
```

### Flow B: No chain (bootstrap decision)

```
spawn watchdog
  ↓ chain_status { has_chain: false }  → loadingPhase = 'bootstrap-decision'

User clicks "Mithril Sync":
  → send { cmd: 'start_mithril' }
  ↓ mithril_status { phase: 'preparing' }   → loadingPhase = 'mithril-syncing'
  ↓ mithril_status / mithril_progress ...  → update mithrilPhase / mithrilProgress
  ↓ mithril_status { phase: 'finalizing' }
  ↓ node_started, wallet_started, wallet_ready → loadingPhase = 'ready'
  ↓ mithril_status { phase: 'completed' }    (informational)

User clicks "Blockchain Sync":
  → send { cmd: 'start_node' }
  ↓ node_started, ... wallet_ready → loadingPhase = 'ready'

User clicks Cancel during Mithril:
  → send { cmd: 'cancel_mithril' }
  ↓ mithril_status { phase: 'cancelled' }  → return to loadingPhase = 'bootstrap-decision'
```

### Flow C: Proactive Mithril prompt during ledger replay

```
loadingPhase = 'node-starting'
  → send { cmd: 'probe_mithril' } (once, after node_started)
  ↓ mithril_significantly_behind → show SyncingConnectingMithrilPrompt

User confirms:
  → send { cmd: 'start_mithril', force: true }
  ↓ mithril_status { phase: 'preparing' } → loadingPhase = 'mithril-syncing'
  ... (same as Flow B Mithril path)

User dismisses: hide prompt; no command sent
```

---

## Shutdown

```
app will-quit event
  → BackendLifecycle.stop()
  → WatchdogManager.sendCommand({ cmd: 'stop' })
  → WatchdogManager stdin.end()           ← EOF unblocks Rust async reader immediately
  ← watchdog emits stopped, process exits
  → block quit for up to 45 s; force-proceed on timeout
```

---

## Error handling

| Scenario | Behaviour |
|----------|-----------|
| `wallet_unrecoverable` | set `pendingRejection`; do not reject yet |
| watchdog process exits, `pendingRejection` set | reject startup promise → BackendLifecycle schedules restart after 3 s |
| watchdog process exits unexpectedly (no `pendingRejection`) | same: restart after 3 s |
| `mithril_error` | set `BackendStore.mithrilPhase = 'error'`; show `MithrilErrorView` with `code` and `message`; offer "Retry" (→ `start_mithril`) and "Skip" (→ `start_node`) |
| `error` event | log to main process log; treat as unrecoverable |

---

## Files to create

| File | Purpose |
|------|---------|
| `source/main/WatchdogManager.ts` | Spawn, config, event dispatch, command send, WatchdogState |
| `source/main/BackendLifecycle.ts` | App-level owner of WatchdogManager; handles restarts, quit blocking |
| `source/main/ipc/backendStatusChannel.ts` | Poll handler returning WatchdogState snapshot |
| `source/main/ipc/mithrilCommandChannel.ts` | Invoke handler forwarding renderer commands to watchdog |
| `source/main/ipc/mithrilPushChannel.ts` | Push senders for `mithril_status` and `mithril_progress` |
| `source/renderer/app/stores/BackendStore.ts` | MobX store: state, computed loadingPhase, action methods |
| `source/common/types/watchdog.types.ts` | Shared TS types: WatchdogState, MithrilProgress, LoadingPhase, MithrilCommand |

---

## Files to delete

All existing files that duplicate any of the above responsibilities. The exact list is determined during implementation; the principle is: if it managed a cardano-node or wallet process, handled Mithril orchestration, or relayed node/wallet status to the renderer, it is replaced by one of the files above.

---

## Non-goals

- TypeScript-side process health probing (the watchdog owns all health checks)
- Mithril network config fetching (aggregator URL and keys come from the Daedalus nix config, passed in WatchdogConfig at startup)
- Log rotation (owned by Rust)
- Block sync progress parsing (owned by Rust)
- Any push channel for block sync progress (the 2 s poll is sufficient for a percentage bar)
