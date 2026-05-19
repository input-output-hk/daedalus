# Task 101 IPC Wrapper Notes

## Status

- State: complete
- Task: `task-101`
- Purpose: record the durable IPC seam decisions for diagnostics-launched Mithril partial sync

## Durable Decisions

- Partial sync uses five dedicated IPC channels in `source/common/ipc/api.ts`:
  - `MITHRIL_PARTIAL_SYNC_START_CHANNEL`
  - `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL`
  - `MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL`
  - `MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL`
  - `MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL`
- All partial-sync action requests are payload-free (`void`); snapshot selection, range derivation, and recovery eligibility stay backend-owned.
- Partial-sync status transport follows the existing Mithril bootstrap shape: renderer code can request the latest cached status and also subscribe to push updates.
- Until later tasks provide real backend delegates, partial-sync action handlers must reject explicitly rather than return successful no-op responses.

## Safety Boundaries Preserved

- `mithrilPartialSyncChannel.ts` must not copy bootstrap-only setup side effects such as `chainStorageCoordinator.syncMithrilWorkDir()` during IPC registration.
- Partial-sync cached status is full-replacement state, not a `Partial<>` merge helper. Every update must carry `allowedRecoveryActions`, including `[]` when nothing is safe.
- Best-effort renderer notification remains outside the critical path for internal listeners, matching the hardened Mithril startup rule that renderer IPC must not block authoritative runtime state transitions.

## Scope Boundary Preserved

- Task-101 stopped at shared IPC contracts, wrapper files, handler registration, focused wrapper tests, and the required IPC inventory doc update.
- It did not add coordinator wiring, service orchestration, node-start suppression hooks, renderer stores, or diagnostics UI behavior.

## Verification Notes

- Focused Jest coverage passed in `source/main/ipc/mithrilPartialSyncChannel.spec.ts`.
- The spec locks idle status shape, reject-until-implemented action semantics, best-effort broadcast behavior, stale-window rebinding protection, and no-merge recovery-action clearing.
- A raw `yarn tsc --noEmit ...` file-list invocation surfaced unrelated repo-wide renderer/config issues, so Jest plus live diff inspection was the truthful verification signal for this task.
