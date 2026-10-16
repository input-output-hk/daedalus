# Code Paths at 11.5.0

Everything below was read at `22440487d`, the commit tagged `11.5.0`. Paths are repository-relative.

## Rebuild: `start_mithril` with `wipe_chain: true`

The command is parsed at `watchdog/src/protocol.rs:16-26` and typed for the renderer at `source/common/types/watchdog.types.ts:89`. Nothing sends it: `BackendStore.startMithril` and `startMithrilForce` send the plain and `force` forms only (`source/renderer/app/stores/BackendStore.ts:312-318`). The main process forwards renderer commands to the watchdog verbatim (`source/main/ipc/index.ts:100-103`, `source/main/BackendLifecycle.ts:275-282`, `source/main/WatchdogManager.ts:236-259`), so no main-process change is needed to send it.

`mithril::run_pipeline` (`watchdog/src/mithril.rs:728`) handles it in this order:

1. Fails fast without the ancillary verification key (`mithril.rs:739-745`).
2. Refuses a chain directory that belongs to another network, `CHAIN_DIR_OTHER_NETWORK` (`mithril.rs:750`, guard at `:445-459`).
3. If `wipe_chain`: refuses unless the canonicalized chain path lies inside `state_dir`, emitting `INVALID_CHAIN_PATH` (`mithril.rs:754-789`), then `remove_dir_all(chain_path)` (`mithril.rs:791`).
4. Only then emits `preparing` and probes the aggregator (`mithril.rs:793-808`). With the chain gone, the probe finds no local chunks and the pipeline runs as a full bootstrap (`mithril.rs:812`, `:838`).

Consequences:

- **A failure after the wipe leaves no chain.** A probe failure (`mithril.rs:802-808`) or download failure (`:862-870`) returns `Cancelled`, and in the running path the supervisor then restarts the node and wallet (`watchdog/src/supervisor.rs:1203-1213`). The node starts on an empty directory and syncs from genesis without asking. Aggregator reachability is not checked before the delete.
- **Custom chain locations cannot be rebuilt.** A folder the user picked outside `state_dir` (configured through `watchdog/src/state.rs:184-199`) always fails the check with `INVALID_CHAIN_PATH`. A link inside `state_dir` that points outside canonicalizes outside and is refused too. Full bootstrap already handles both through `full_install_target` and `check_replaceable` (`mithril.rs:463-475`); the wipe uses neither, so it also deletes a default-location directory whatever it contains.
- The wallet database is outside the chain directory and is not touched. The default chain path is `<state_dir>/chain` (`watchdog/src/state.rs:91-103`, `source/main/index.ts:246`).

The renderer sees `node_shutdown_ms`, then `mithril_status` phases `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, then `completed` after `node_started` (`watchdog/tests/integration.rs:1182-1218` asserts this sequence), and `mithril_progress` throughout.

## Where `start_mithril` is accepted

| Supervisor state | Accepted | Effect on the node |
|---|---|---|
| No chain, choosing (`supervisor.rs:914`) | Yes, always forced | None running |
| Node starting or replaying, before `chainDbReady` (`:1523`) | Yes | `stop_node`: closes the shutdown pipe, kills after `stop_timeout_secs` (default 60) (`:383-414`) |
| Wallet starting (`:1857`) or ready (`:2046`) | Yes | `stop_wallet`, then `stop_node` |
| Wallet unrecoverable, node up (`:1734`) | Yes | `stop_node` |
| Node stopping, or inside the crash-restart delay | Held, runs instead of the restart | None running |
| Node unrecoverable (`:470-490`), after a cancelled or failed pipeline (`:1221`, `:1266`) | Yes | None running |
| Pipeline already running (`mithril.rs:288`, `:611`, `:937`) | Dropped | n/a |

So every state the three damage signatures leave the supervisor in already accepts the command. A node that hangs after the critical invariant violation is killed by the 60-second stop timeout.

## Log parsing

Node stdout and stderr both go through `pipe_to_log` with parsing on (`supervisor.rs:1409-1422`), written to `node.log` in the public logs directory. The parsers are plain substring matches:

- `try_parse_startup_status` (`supervisor.rs:556-579`) emits `node_startup_status` for the ImmutableDB, VolatileDB, LedgerDB and ChainDB open phases and for replay from genesis.
- `try_parse_block_sync` (`supervisor.rs:585-606`) emits `node_block_sync_progress` from `Replayed block`, `Validating chunk`, `Validated chunk` and `Pushing ledger state` lines.

There are no other parsers. Wallet stdout and stderr go to `cardano-wallet.log` with parsing off (`supervisor.rs:1805-1816`, `pipe_to_log(..., false, None)`). Lines are capped at 1 MiB (`supervisor.rs:618`). Parser unit tests feed `pipe_to_log` a `Cursor` (`supervisor.rs:2211-2463`).

## Events and state

`Event` is a serde enum tagged `event`, snake_case (`watchdog/src/protocol.rs:85-241`), written as JSON lines by `emit` (`protocol.rs:255-284`). `MithrilError { code, message }` is at `protocol.rs:170-173`. Codes in use include `CHAIN_DIR_OTHER_NETWORK`, `INVALID_CHAIN_PATH`, `ANCILLARY_VKEY_MISSING` and `PROBE_FAILED` (`mithril.rs:432`, `:454`, `:741`, `:783`, `:805`, `:853`, `:866`, `:890`, `:907`, `:975`, `:988`).

The main process parses events in `WatchdogManager.handleEvent` (`source/main/WatchdogManager.ts:358-570`). **It drops the `code` of `mithril_error`** and keeps only the message as `lastError` (`WatchdogManager.ts:505-508`); the renderer then labels every Mithril failure `code: 'mithril_error'` (`source/renderer/app/containers/loading/MithrilSyncContainer.tsx:55`). Node exits set only `_nodeRunning = false` and the exit code is not kept (`WatchdogManager.ts:474-477`).

A new field on the polled state has to be added in four places: `source/common/types/watchdog.types.ts:2-44`, `source/main/WatchdogManager.ts:25-75` and `:125-157`, and the fallback in `source/main/ipc/index.ts:62-97`. The renderer polls it every 2 s (`BackendStore._pollBackendStatus`, `BackendStore.ts:133-171`). The closest precedent for a new blocking state is `nodeUnrecoverable`: set by `node_unrecoverable` (`WatchdogManager.ts:468-472`), reset by `node_started` (`:397`), copied by the poll (`BackendStore.ts:152`), and mapped to the `error` loading phase (`BackendStore.ts:272-274`).

## Screen selection

`Root` renders `LoadingPage` whenever `loadingPhase !== 'ready'` (`source/renderer/app/containers/Root.tsx:85-92`). `BackendStore.loadingPhase` (`BackendStore.ts:265-308`) checks, in order: stopping, unrecoverable error, requested restart, no chain status yet, no chain (storage picker or bootstrap decision), Mithril in progress, wallet not ready, ready. `LoadingPage` (`source/renderer/app/containers/loading/LoadingPage.tsx:43-94`) renders a full-screen backdrop with `ChainStorageContainer` or `MithrilSyncContainer` for those phases and `SyncingConnectingPage` otherwise. A new full-screen state is a new `LoadingPhase` member (`watchdog.types.ts:75-83`) with its own branch there.

`MithrilSyncContainer` shows `MithrilDecisionView`, `MithrilErrorView` or `MithrilProgressView` (`MithrilSyncContainer.tsx:39-98`). Its error buttons ("Retry", "Skip to blockchain sync") are hard-coded English (`MithrilSyncContainer.tsx:50-70`). A failed rebuild lands on this view, so Japanese users would see English there.

## Existing pieces to reuse

- Choice-then-confirm layout: `SyncingConnectingMithrilPrompt` switches from a choice view to a confirm view (`source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:118-147`, `:199-230`).
- `MithrilErrorView` already has an `onWipeRetry` prop and a "Wipe chain & retry" button (`components/loading/mithril/MithrilErrorView.tsx:32`, `:103-114`), used only in Storybook. Message `partialSyncWipeAndFullSync` (`MithrilBootstrap.messages.ts:502-504`) is defined and never imported.
- Diagnostics has Restart node and Restart wallet buttons (`components/status/DaedalusDiagnostics.tsx:901-935`) and a Mithril Sync action that calls `startMithrilForce` (`containers/status/DaedalusDiagnosticsDialog.tsx:24-26`). It shows the current Mithril state only, with no history (`mithrilDiagnosticsState.ts:22-40`).
- i18n: Mithril loading screens keep messages in `components/loading/mithril/MithrilBootstrap.messages.ts` with IDs such as `loading.mithrilBootstrap.wipeAndRetry`; prompts use inline `defineMessages`. New `defaultMessage` values start with `!!!` (`.agent/skills/i18n-messaging/SKILL.md:33`, `:161`).

## Tests to extend

- Watchdog: mock binaries in `watchdog/src/bin/mock_*.rs`, registered at `watchdog/Cargo.toml:11-76`. `mock_node` prints hard-coded lines and has no way to emit arbitrary ones (`watchdog/src/bin/mock_node.rs:19-30`). Relevant integration tests: `wipe_chain_full_bootstrap` (`watchdog/tests/integration.rs:1182`), `partial_sync_while_running` (`:507`), download and conversion failures (`:1331`, `:1359`), `startup_log_phase_parsing` (`:877`). Chain storage guards: `watchdog/tests/chain_storage.rs:308`, `:524`, `:566`.
- Renderer: `source/renderer/app/stores/BackendStore.spec.ts` (mocks `mithrilCommandChannel.send`, calls handlers directly), `source/main/WatchdogManager.spec.ts` (emits JSON event lines on a fake channel, `:27-35`), `SyncingConnectingMithrilPrompt.spec.tsx`.
- Storybook: `storybook/stories/loading/mithril/*.stories.tsx`, one `.add()` per state, with fixtures in `storybook/stories/loading/_support/mithrilFixtures.ts` and `storybook/stories/loading/_support/mithrilHarness.tsx`.
