# Task task-103: Integrate Node Lifecycle Suppression And Active-State Reporting

## Task

- Task ID: `task-103`
- Title: `Integrate node lifecycle suppression and active-state reporting`

## Why This Task Now

- `task-101` and `task-102` are complete, so the repo now has both a dedicated partial-sync IPC seam and a coordinator-owned partial-sync active boundary.
- `task-103` is the next unblocked backend step on the remaining critical path after `task-001` through `task-102`.
- The next backend tasks need the main process to treat diagnostics-launched partial sync like bootstrap for crash-restart safety, without waiting for renderer-owned state or for the full partial-sync service to exist.

## Interaction Mode

- `autonomous`

## Scope

- Update main-process node lifecycle handling so generic `cardano-node` crash restart is suppressed while Mithril partial sync is active.
- Add the smallest main-process-owned active-state reporting seam needed for that suppression.
- Keep the active-state source authoritative in the main process and independent of renderer subscriptions or cached renderer UI state.
- Mirror the existing Mithril bootstrap restart-suppression pattern where it fits, while keeping bootstrap and partial-sync state separate.

## Non-Goals

- Implementing the real `MithrilPartialSyncService`, range derivation, download flow, cutover, or recovery actions; those belong to `task-200` through `task-204`.
- Reusing bootstrap status or bootstrap-only state machines for partial sync.
- Expanding partial-sync IPC contracts or renderer behavior.
- Introducing a broad shared Mithril runtime abstraction before later tasks prove it is needed.
- Adding user-facing docs or renderer/UI changes in this task.

## Relevant Dependencies

- Required completed dependencies:
  - `task-101`
  - `task-102`
- Design inputs already locked and carried forward:
  - `task-002`
  - `task-003`
  - `task-004`
- This task directly unblocks:
  - `task-200`
  - `task-204`
  - `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/04-task-101-ipc-wrapper-notes.md`
- `.agent/plans/mithril-partial-sync/research/05-task-102-coordinator-seam-notes.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand`

## Live Repo Findings Verified For Planning

- `source/main/cardano/setup.ts` currently suppresses automatic crash restart only for Mithril bootstrap via `isMithrilBootstrapNodeStartBlocked()`.
- `source/main/ipc/mithrilBootstrapChannel.ts` already exposes the pattern to mirror: a tiny exported accessor plus a provider setter wired from `source/main/index.ts`.
- `source/main/ipc/mithrilPartialSyncChannel.ts` currently has cached status helpers, but no authoritative active-state accessor and no link to the coordinator-owned partial-sync boundary.
- `source/main/utils/chainStorageCoordinator.ts` now owns the authoritative partial-sync activity flag through `_partialSyncInProgress`, but does not yet expose that state for startup or node-lifecycle integration.
- `source/main/index.ts` already wires bootstrap node-state access through `setMithrilBootstrapNodeStateProvider(...)`, so a matching partial-sync provider seam fits the live main-process composition style.
- `task-102` explicitly handed off that `task-103` should consume the new active partial-sync state boundary for node lifecycle suppression without inventing renderer-owned state.
- No partial-sync service exists yet, so status-cache-only detection would be premature and weaker than the coordinator-owned in-progress flag.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-103.md`
- `source/main/cardano/setup.ts`
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `source/main/index.ts`
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts` if the active-state getter needs direct locking coverage
- `source/main/cardano/setup.spec.ts` if no existing startup-lifecycle spec can cover the new suppression path cleanly

## Implementation Approach

1. Expose the authoritative partial-sync active state from the coordinator.
   - Add a narrow read-only accessor on `ChainStorageCoordinator` for whether partial sync is currently in progress.
   - Keep the source of truth on the coordinator flag introduced by `task-102` instead of deriving activity from renderer state or future service status events.
   - Avoid widening the coordinator contract beyond a getter; this task does not need new mutation APIs.

2. Add a partial-sync active-state seam in the main IPC layer.
   - Extend `source/main/ipc/mithrilPartialSyncChannel.ts` with the same style of tiny exported helper used by bootstrap.
   - Prefer a provider setter plus accessor so `setup.ts` can ask one narrow question such as whether partial sync restart suppression should apply, without importing the coordinator directly.
   - Keep this helper authoritative by sourcing it from the coordinator-backed provider, not from `lastStatus` alone.
   - Preserve the existing cached status helpers because later service tasks still need them for renderer updates, but do not make them the safety source of truth here.

3. Wire the active-state provider during main-process setup.
   - In `source/main/index.ts`, register the partial-sync active-state provider after the node and coordinator are available, mirroring the bootstrap provider wiring pattern already used in this file.
   - Keep the wiring local to main-process bootstrap so the suppression path stays independent of renderer startup timing.

4. Broaden `cardano-node` crash handling to respect partial sync.
   - Update `source/main/cardano/setup.ts` so `onCrashed` suppresses generic automatic restart when either:
     - Mithril bootstrap is blocking node start, or
     - Mithril partial sync is active
   - Keep the existing bootstrap behavior unchanged.
   - Log the partial-sync suppression path explicitly so support logs can distinguish bootstrap from diagnostics-launched partial sync.
   - Do not add broader node stop/start orchestration here; the task is only about suppressing generic crash retries while Mithril-managed work is active.

5. Verify the seam with focused tests.
   - Extend `mithrilPartialSyncChannel` tests to cover the new provider/accessor behavior.
   - Add or extend a focused main-process lifecycle test so `setup.ts` proves restart suppression while partial sync is active and preserves normal restart behavior otherwise.
   - Add coordinator getter coverage only if the implementation adds enough logic there to justify direct unit tests; otherwise rely on the existing coordinator tests plus the setup-level suppression spec.

## Acceptance Criteria

- Automatic `cardano-node` crash restarts are suppressed during active Mithril partial sync.
- Main-process restart suppression can determine partial-sync activity without depending on renderer state.
- The authoritative partial-sync activity source is the coordinator-owned main-process boundary from `task-102`, not renderer subscriptions.
- Existing empty-chain Mithril bootstrap restart suppression remains unchanged.
- The implementation keeps bootstrap and partial-sync state reporting separate rather than merging them into one vague Mithril flag.
- Focused tests cover the new active-state seam and crash-restart suppression behavior closely enough to catch regressions.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - the focused cardano setup lifecycle spec that covers `onCrashed` suppression behavior
  - `source/main/utils/chainStorageCoordinator.spec.ts` only if the getter implementation adds direct logic worth testing
- Verify these behaviors directly:
  - partial-sync active-state accessor reflects the provider-backed main-process state
  - `setup.ts` suppresses restart when partial sync is active
  - `setup.ts` still suppresses restart when bootstrap blocks node start
  - `setup.ts` still schedules restart normally when neither Mithril flow is active
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` remains blocked by known unrelated failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The main design risk is accidentally treating partial-sync status cache as authoritative before the real service exists. This plan avoids that by preferring the coordinator-owned active flag.
- If later service tasks introduce their own active-process state, they must stay aligned with the coordinator boundary rather than replacing it silently.
- The exact helper shape can stay small, but it should not force `setup.ts` to import coordinator internals directly unless the provider pattern proves impossible.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-103` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review are approved.
- Add a short research note only if implementation lands a durable active-state or node-lifecycle seam that later tasks must preserve.
- No `.agent/system/` doc update is expected unless implementation changes the durable startup or IPC architecture more than this narrow suppression seam currently suggests.

## Implementation Outcome

- Implemented the approved narrow seam without widening partial-sync scope beyond task-103.
- `ChainStorageCoordinator` now exposes a read-only `isPartialSyncInProgress()` accessor so later main-process integrations can consume the coordinator-owned active boundary from task-102 without new mutation APIs.
- `source/main/ipc/mithrilPartialSyncChannel.ts` now exports a bootstrap-style partial-sync active-state provider/accessor pair, but cached status remains separate from that authoritative safety seam.
- `source/main/index.ts` wires the partial-sync active-state provider from `chainStorageCoordinator.isPartialSyncInProgress()`.
- `source/main/cardano/setup.ts` now suppresses generic automatic restart while partial sync is active and logs the current partial-sync status distinctly from bootstrap.
- Added focused Jest coverage in `source/main/ipc/mithrilPartialSyncChannel.spec.ts`, `source/main/utils/chainStorageCoordinator.spec.ts`, and new `source/main/cardano/setup.spec.ts`.

## Final Verification Outcome

- Passed: `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- Passed: `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts`
- Passed: `yarn test:jest source/main/cardano/setup.spec.ts`
- `understand` was consulted at role startup, but the repo-local knowledge graph was unavailable, so implementation and review findings were verified directly against live files.

## Final Outcome

- Status: completed
- Review result: approved on implementation review iteration 1 with no blocking findings.
- User interaction: none required.
- Research outcome: durable seam recorded in a task-specific research note.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-103-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-103-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Approved Plan

- Keep `task-103` narrowly focused on main-process crash-restart suppression and authoritative active-state reporting.
- Use the coordinator-owned partial-sync in-progress boundary from `task-102` as the source of truth.
- Add a minimal partial-sync provider/accessor seam in `source/main/ipc/mithrilPartialSyncChannel.ts`, wire it from `source/main/index.ts`, and consume it in `source/main/cardano/setup.ts` alongside the existing bootstrap suppression helper.
- Prefer focused main-process tests over broader orchestration work, and leave real partial-sync service status emission to later tasks.

## Self-Review

- Scope-creep check: keeps service orchestration, recovery actions, renderer work, and bootstrap refactors out of `task-103`.
- Dependency check: uses the completed IPC and coordinator seams from `task-101` and `task-102` rather than inventing a new state source.
- Required-fields check: includes task ID/title, why now, interaction mode, scope, non-goals, dependencies, research consulted, docs/workflows/skills consulted, expected files, implementation approach, acceptance criteria, verification plan, risks/open questions, docs/tracking updates, review-log paths, planning status, and build status.
