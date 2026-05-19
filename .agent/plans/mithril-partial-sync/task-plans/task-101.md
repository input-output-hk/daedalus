# Task task-101: Define Partial Sync IPC Channels And Add Main/Renderer Wrappers

## Task

- Task ID: `task-101`
- Title: `Define partial sync IPC channels and add main/renderer wrappers`

## Why This Task Now

- `task-100` is complete and the shared partial-sync type layer now exists in `source/common/types/mithril-partial-sync.types.ts`.
- `task-101` is the smallest next step that lets later coordinator, service, startup-suppression, and renderer-store work bind to dedicated partial-sync IPC seams without overloading bootstrap channels.

## Interaction Mode

- `autonomous`

## Scope

- Add dedicated partial-sync IPC channel names and request/response aliases in `source/common/ipc/api.ts`.
- Add main-process IPC wrapper setup for partial sync actions and status fanout in `source/main/ipc/`.
- Add renderer-side IPC wrapper exports in `source/renderer/app/ipc/`.
- Register the new main-process handler so later tasks can consume the wrappers through the normal IPC initialization path.
- Keep request payloads minimal and backend-owned.

## Non-Goals

- Implementing partial-sync orchestration, range derivation, node stop/start logic, or failure-boundary enforcement; those belong to `task-102` and later backend tasks.
- Adding renderer stores, diagnostics UI, or modal flows; those belong to phase 3 tasks.
- Reusing or widening bootstrap IPC channels with partial-sync-only semantics.
- Updating bootstrap shared contracts beyond any import-level adjustment needed to compile the new wrappers.
- Adding coordinator/service orchestration semantics, bootstrap workdir setup side effects, or startup-suppression helpers that belong to `task-102`, `task-103`, or `task-200`.

## Relevant Dependencies

- Required completed dependency:
  - `task-100`
- This task unblocks:
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-204`
  - `task-300`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/03-task-100-shared-contract-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/system/api-endpoints.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `understand`

## Live Repo Findings

- `source/common/ipc/api.ts` currently defines five bootstrap-specific Mithril channels: decision, start, status, cancel, and snapshots.
- `source/main/ipc/mithrilBootstrapChannel.ts` is the live pattern to mirror: one file owns channel instances, last-status caching, listener registration, best-effort renderer status broadcast, idempotent request-handler setup, and exported helper accessors.
- `source/main/ipc/index.ts` is the real registration seam; it calls `handleMithrilBootstrapRequests(window)` and `handleChainStorageRequests()` during IPC initialization.
- `source/renderer/app/ipc/mithrilBootstrapChannel.ts` is intentionally thin and exports only typed `RendererIpcChannel` instances, with store behavior layered elsewhere.
- `source/renderer/app/stores/MithrilBootstrapStore.ts` uses `mithrilBootstrapStatusChannel.onReceive(...)` plus `request()` for initial sync, which means partial sync likely needs the same wrapper shape for a future store.
- `task-100` already locked that partial-sync status transport should use `MithrilPartialSyncStatusUpdate` and that every update must carry `allowedRecoveryActions`, including `[]` when nothing is safe.
- `source/main/ipc/mithrilBootstrapChannel.ts` also contains two bootstrap-only behaviors that must not be copied into partial sync: setup-time `chainStorageCoordinator.syncMithrilWorkDir()` side effects during IPC registration, and `setMithrilBootstrapStatus(update: Partial<...>)`, which would violate the task-100 rule that `allowedRecoveryActions` is always present on every partial-sync update.
- Existing Mithril and chain-storage hardening work depends on idempotent main IPC setup and on best-effort renderer notification that does not become a startup or runtime critical path.
- There is no existing partial-sync IPC surface in the live repo, so `task-101` can still choose clean names without migration burden.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-101.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-101-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-101-impl-review.md`
- `source/common/ipc/api.ts`
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/index.ts`
- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `.agent/system/api-endpoints.md`

## Implementation Approach

1. Add dedicated partial-sync IPC contracts in `source/common/ipc/api.ts`.
   - Add channel constants for:
     - `MITHRIL_PARTIAL_SYNC_START_CHANNEL`
     - `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL`
     - `MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL`
     - `MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL`
     - `MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL`
   - Keep request payloads minimal:
     - `start`: `void`
     - `status`: `void`
     - `cancel`: `void`
     - `restart-normal`: `void`
     - `wipe-and-full-sync`: `void`
   - Use `MithrilPartialSyncStatusUpdate` as the status response type.
   - Do not add renderer-provided snapshot metadata, immutable ranges, thresholds, or recovery-boundary hints.

2. Create a dedicated main IPC wrapper file rather than widening the bootstrap one.
   - Add `source/main/ipc/mithrilPartialSyncChannel.ts` alongside the existing bootstrap channel file.
   - Mirror the bootstrap structure closely enough to stay familiar:
     - typed `MainIpcChannel` instances
     - cached `lastStatus` initialized to a truthful idle partial-sync update with `allowedRecoveryActions: []`
     - status listeners and exported `onMithrilPartialSyncStatus(...)`
     - best-effort renderer send function rebound to the current window
     - idempotent `handleMithrilPartialSyncRequests(window)` registration
      - exported helpers limited to the wrapper seam, such as `getMithrilPartialSyncStatus()`
    - Explicitly do not copy bootstrap-only registration side effects such as `chainStorageCoordinator.syncMithrilWorkDir()` into this task; partial-sync setup coordination belongs to `task-102`.
    - Explicitly do not add a `Partial<MithrilPartialSyncStatusUpdate>` merge helper. Partial-sync status writers in this module must replace the full cached status object and must provide `allowedRecoveryActions` on every emitted update, including `[]`.
    - Keep the wrapper delegations intentionally skeletal in this task, but fail closed: until `task-102` or `task-200` provides concrete coordinator or service delegates, each action request handler (`start`, `cancel`, `restart-normal`, `wipe-and-full-sync`) must reject with a deliberate not-implemented error instead of returning a successful no-op response.

3. Keep status transport as a push-plus-initial-request channel.
   - Match the existing bootstrap pattern where renderer code can call `request()` to fetch the latest cached status and also subscribe to `onReceive(...)` for long-running updates.
   - Avoid inventing a conversation channel unless live implementation constraints prove `RendererIpcChannel` is insufficient.

4. Add thin renderer wrappers in a dedicated file.
   - Add `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` exporting typed `RendererIpcChannel` instances for all five actions.
   - Keep the file wrapper-only; no store logic, derived helpers, or renderer-side safety interpretation.

5. Register the new main handler in the real IPC bootstrap path.
   - Update `source/main/ipc/index.ts` to call `handleMithrilPartialSyncRequests(window)` alongside the existing Mithril bootstrap registration.
   - Preserve the idempotent registration pattern so window recreation rebinds status delivery without duplicating request listeners.

6. Update IPC documentation in the same task if channels are added in code.
   - Update `.agent/system/api-endpoints.md` to inventory the new partial-sync channels under the Electron IPC section.
   - Keep the doc update limited to truthful channel inventory and purpose; do not document coordinator/service behavior that does not exist yet.

7. Test the wrapper seam directly.
   - Add a focused spec modeled on `source/main/ipc/mithrilBootstrapChannel.spec.ts`.
   - Verify at minimum:
      - initial status request returns the cached idle partial-sync status with `allowedRecoveryActions: []`
      - service-originated status listeners still fire when renderer send rejects
      - a replacement window rebinds outgoing status delivery without duplicating request-handler registration
      - each unimplemented action channel rejects with an explicit not-implemented error rather than false-success no-op behavior
      - the module never relies on `Partial<>` merge behavior to retain `allowedRecoveryActions` across updates
    - Keep task-101 tests at the wrapper level, not coordinator/service behavior.

## Acceptance Criteria

- New partial-sync channel names and request/response aliases are defined in `source/common/ipc/api.ts`.
- Main and renderer wrapper files exist for start, status, cancel, restart-normal, and wipe-plus-full-sync.
- Partial sync uses dedicated IPC channels and does not overload bootstrap channels with partial-sync-only meaning.
- Status transport is modeled as an initial request plus ongoing push updates using `MithrilPartialSyncStatusUpdate`.
- The main IPC handler is registered through `source/main/ipc/index.ts`.
- Idle partial-sync status defaults are truthful and include `allowedRecoveryActions: []`.
- Unimplemented action handlers reject explicitly until later tasks provide real delegates; none of the new channels false-success with a no-op response.
- The partial-sync IPC wrapper does not copy bootstrap-only registration side effects or `Partial<>` status-merge helpers.
- `.agent/system/api-endpoints.md` is updated in the same task if the channels are added in code.
- Tests cover the main wrapper registration and status-broadcast seam closely enough to catch duplicate-registration or stale-window regressions.

## Verification Plan

- Run targeted Jest tests for:
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - any touched existing Mithril IPC spec if implementation refactors shared bootstrap utilities
- Run targeted typechecking or repo compile as truthfully possible after edits.
- Verify the new contracts import from `source/common/types/mithril-partial-sync.types.ts` rather than broadening bootstrap-only contracts.
- Verify `source/main/ipc/index.ts` is the only registration change needed for the new handler.
- Verify `.agent/system/api-endpoints.md` is updated if the task lands new IPC channels in code.
- Verify the plan remains aligned with `task-102` by keeping coordinator/service semantics out of this task except for minimal delegation seams.
- If repo-wide `yarn compile` is still blocked by the known unrelated Trezor typing failures, record that truthfully and use targeted verification for touched IPC files instead.

## Risks And Open Questions

- The main uncertainty is how far `task-101` should go in wiring handlers to concrete coordinator methods before `task-102` lands. The preferred answer is to keep this task at the wrapper/registration seam and reject unimplemented actions rather than inventing fake backend behavior.
- If later implementation shows both Mithril IPC files need a shared best-effort broadcast helper, that should remain a tiny extraction rather than a broad refactor in this task.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-101` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` once implementation and review are approved.
- Update `.agent/system/api-endpoints.md` in the same task if the task adds the channels in code.
- Add a short implementation note only if a durable IPC-specific decision emerges, such as reusing the bootstrap push/request pattern versus introducing a conversation channel.

## Review Logs

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-101-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-101-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Completed 2026-05-19 after the required planning loop, one implementation iteration, and approved code review.
- Added dedicated partial-sync IPC contracts in `source/common/ipc/api.ts` for start, status, cancel, restart-normal, and wipe-and-full-sync.
- Added `source/main/ipc/mithrilPartialSyncChannel.ts` with cached idle status, best-effort renderer fanout, idempotent window rebinding, and explicit reject-until-implemented action handlers.
- Registered the new main-process handler in `source/main/ipc/index.ts` and added thin renderer wrappers in `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`.
- Kept the task at the wrapper seam: no coordinator wiring, startup suppression, bootstrap workdir sync, or bootstrap-style partial-status merge helper was introduced.
- Updated `.agent/system/api-endpoints.md` in the same task so the IPC inventory now includes both Mithril bootstrap and partial-sync channels.

## Verification Outcome

- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` passed.
- Focused wrapper coverage now verifies idle cached status shape, best-effort status broadcast, window rebinding without duplicate handler registration, explicit rejection semantics for unimplemented actions, and full-status replacement instead of merge-based stale recovery-action retention.
- A raw `yarn tsc --noEmit ...` file-list invocation was attempted but is not a truthful repo compile path here; it surfaced broad pre-existing TypeScript/config noise in unrelated renderer files rather than task-101-specific failures.
- No user interaction or manual validation was required for this task.

## Self-Review

- Scope-creep check: keeps coordinator, service, startup suppression, renderer store, and diagnostics UI out of task-101.
- Workflow freshness check: matches the live IPC registration pattern in `source/main/ipc/index.ts`, the current IPC workflow, and the known compile/test constraints.
- Tests/docs check: includes focused wrapper tests for idle status shape, best-effort broadcast, window rebinding, and explicit rejection semantics, and requires the `.agent/system/api-endpoints.md` update when channels land in code.
- Consistency check: uses the completed task-100 partial-sync contract, preserves dedicated channels, forbids bootstrap-only setup side effects and merge helpers, and stays aligned with later tasks that own real orchestration and safety rules.
