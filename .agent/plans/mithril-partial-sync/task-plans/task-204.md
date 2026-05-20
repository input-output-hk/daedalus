# Task task-204: Implement Cancellation, Cleanup, And Failure Branching

## Task

- Task ID: `task-204`
- Title: `Implement cancellation, cleanup, and failure branching`

## Why This Task Now

- `task-203` is complete, so the backend now reaches staged conversion, validated cutover, persisted partial-sync markers, and startup-owned blocking for interrupted Boundary B and C1 states.
- The repo still stops short of the PRD's required actionable recovery surface: diagnostics-launched partial sync has no implemented `restart-normal` or `wipe-and-full-sync` handlers, and cancellation currently clears only in-memory state without explicit staged cleanup or boundary-aware enforcement beyond a late install/finalizing guard.
- `task-204` is therefore the next smallest truthful backend task on the critical path. It should finish the already-planned service and coordinator behavior for Boundary A cancellation and for boundary-dependent recovery actions, while leaving renderer wiring to `task-300` and later.
- The canonical task plan doc for this task did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this planning pass also fills that tracking gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual test steps before implementation can proceed: none.
- Required evidence back from the user during implementation: none.
- Implementation can proceed before any user interaction: yes.

## Scope

- Finish `MithrilPartialSyncService` cancellation behavior so Boundary A cancellation performs explicit staged cleanup, clears service runtime state truthfully, and preserves the untouched managed chain target.
- Add backend-owned recovery action execution for:
  - `retry`
  - `restart-normal`
  - `wipe-and-full-sync`
- Enforce the task-004 boundary rules in main-process code so unsafe actions are rejected even if requested over IPC.
- Extend the coordinator and IPC seams with only the minimal new action delegates needed for recovery execution.
- Keep startup-owned Boundary B and C1 interruption blocking from `task-203`, but align it with task-204 recovery execution so diagnostics-launched actions and startup-owned recovery do not drift.
- Add focused Jest coverage for cancellation cleanup, recovery-action eligibility, denied unsafe actions, and post-terminal state clearing.

## Non-Goals

- Renderer store, diagnostics CTA, confirmation modal, or overlay work; that belongs to `task-300` through `task-303`.
- Reworking the staged restore, conversion, or validated cutover semantics already locked by `task-202` and `task-203`.
- Replacing the startup-owned interrupted-install recovery seam with a diagnostics-owned flow.
- Generalizing bootstrap and partial-sync recovery behavior into one shared abstraction unless a very small helper extraction is clearly the smallest correct change.
- Manual supported-network QA or rollout-enablement decisions; those remain later-phase work.
- Broader rollout UX beyond the required backend LauncherConfig kill switch and its direct main-process enforcement.

## Relevant Dependencies

- Required completed dependencies:
  - `task-004`
  - `task-103`
  - `task-203`
- Supporting completed prerequisites used directly by this plan:
  - `task-001`
  - `task-002`
  - `task-003`
  - `task-100`
  - `task-101`
  - `task-102`
  - `task-200`
  - `task-201`
  - `task-202`
- This task directly unblocks:
  - `task-300`
  - `task-303`
  - backend portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/03-task-100-shared-contract-notes.md`
- `.agent/plans/mithril-partial-sync/research/04-task-101-ipc-wrapper-notes.md`
- `.agent/plans/mithril-partial-sync/research/05-task-102-coordinator-seam-notes.md`
- `.agent/plans/mithril-partial-sync/research/06-task-103-node-lifecycle-seam-notes.md`
- `.agent/plans/mithril-partial-sync/research/07-task-200-service-skeleton-notes.md`
- `.agent/plans/mithril-partial-sync/research/08-task-201-range-and-staging-notes.md`
- `.agent/plans/mithril-partial-sync/research/09-task-202-download-and-verification-notes.md`
- `.agent/plans/mithril-partial-sync/research/10-task-203-conversion-and-cutover-notes.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/chain-storage-pr-review-fixes.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was consulted first, then important findings were verified against live files before inclusion.

## Live Repo Findings Verified For Planning

- `source/main/mithril/MithrilPartialSyncService.ts` already completes staged download, verification, LSM conversion, allowlist validation, validated cutover, marker writes for `cutover-in-progress` and `installed-awaiting-node-start`, and then stops at a `finalizing` boundary with `allowedRecoveryActions: ['wipe-and-full-sync']` instead of owning post-install recovery actions.
- That same service currently treats cancellation as an in-memory state reset plus child-process kill. It does not explicitly remove `stateDir/mithril-partial-sync/` on cancel, does not clear the durable partial-sync marker, and only blocks cancel once status is `installing` or `finalizing`.
- `task-001` spike results proved interrupted download leaves partial `db/` and ancillary temp artifacts behind, so Boundary A cancellation cannot be treated as restart-safe without explicit staged cleanup.
- `source/main/mithril/mithrilPartialSyncMarker.ts` persists only `cutover-in-progress`, `installed-awaiting-node-start`, and dormant `node-start-verified`; `task-203` currently clears the marker after the first successful post-cutover node start instead of using `node-start-verified`.
- `source/main/utils/handleDiskSpace.ts` already owns startup recovery for interrupted Boundary B and C1 states. It blocks normal startup for `cutover-in-progress`, allows one truthful start attempt for `installed-awaiting-node-start`, clears the marker after first successful start, and suppresses normal fallback if that first-start proof fails. It does not implement diagnostics-launched recovery actions.
- `source/main/ipc/mithrilPartialSyncChannel.ts` still wires only `start` and `cancel`. `restart-normal` and `wipe-and-full-sync` channels deliberately reject with the fixed `Mithril partial sync action is not implemented yet.` placeholder.
- `source/main/utils/chainStorageCoordinator.ts` currently has a narrow partial-sync handler seam with only `start(context)` and `cancel()`. It has no recovery-action APIs yet, so task-204 needs either a minimal seam extension or a similarly narrow neighboring seam.
- `source/main/cardano/setup.ts` already suppresses generic automatic crash restart while partial sync is active, using the coordinator-backed active provider rather than cached status. Task-204 should preserve that main-process-owned safety seam.
- `source/main/config.ts` still has no partial-sync-specific LauncherConfig kill-switch field even though task-004 locked LauncherConfig as the primary rollout guard. The current backend task graph through `task-203` has not implemented that guard yet, so task-204 must add that kill switch as a required deliverable now rather than deferring it. The PRD still requires a fast disable path before renderer rollout continues.
- Existing tests already lock the seams task-204 must evolve carefully:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` covers successful cutover, install-stage allowlist failure, and the current late cancellation denial.
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts` locks the current placeholder rejection behavior for `restart-normal` and `wipe-and-full-sync`.
  - `source/main/utils/chainStorageCoordinator.spec.ts` locks prompt cancel delegation and the current partial-sync preflight seam.
  - `source/main/utils/handleDiskSpace.spec.ts` locks startup-owned wipe-only behavior for interrupted unsafe installs and the current first-start-proof path.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-204.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts`
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`
- `source/main/index.ts`
- `source/main/config.ts` and launcher-config readers
- `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- `.agent/system/architecture.md` only if implementation lands a durable new partial-sync recovery or kill-switch runtime seam that later tasks must preserve
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation/review approval to mark task completion

## Implementation Approach

1. Keep task-204 inside the existing service and coordinator seams.
   - Extend `MithrilPartialSyncService` rather than adding another orchestrator.
   - Extend `ChainStorageCoordinator` with the smallest explicit recovery-action methods instead of tunneling recovery through generic cardano restart or bootstrap APIs.
   - Keep the authoritative safety decision in main-process code, not in IPC wrappers or renderer callers.

2. Finish Boundary A cancellation truthfully.
   - Treat cancellation as allowed only before live cutover starts, matching the task-004 rule.
   - Replace the current kill-and-reset-only implementation with explicit staged cleanup for `stateDir/mithril-partial-sync/` and any known in-memory staging references.
   - Keep cancellation from touching the managed chain target or bootstrap lock files.
   - Clear transient status fields and service-owned runtime references only after cleanup is attempted, and surface a truthful `cancelled` status if cleanup succeeded.
   - If staged cleanup itself fails in Boundary A, convert that into a failed Boundary A outcome that still allows `retry`, `restart-normal`, and `wipe-and-full-sync`, because the live DB remains untouched even if scratch artifacts need manual cleanup.

3. Encode boundary-dependent recovery eligibility in one backend-owned place.
   - Prefer a small internal helper in `MithrilPartialSyncService` or a small shared main-process utility that maps current durable/in-memory state to allowed actions.
   - Keep the rule aligned with `task-004` and the PRD:
     - Boundary A: `retry`, `restart-normal`, `wipe-and-full-sync`
     - Boundary B: `wipe-and-full-sync` only
     - Boundary C1: `wipe-and-full-sync` only
     - Boundary C2: `restart-normal`, `wipe-and-full-sync`
   - Do not let IPC handlers or renderer code infer this from visible status strings alone.
    - Lock Boundary C2 up front for task-204 as a same-session, in-memory terminal state only.
    - Keep task-203's current marker-clear-after-first-successful-node-start behavior.
    - Do not adopt the dormant `node-start-verified` marker for this task.
    - Treat C2 as reachable only after `handleDiskSpace` has completed the first proven post-cutover node-start handoff via `startNodeAfterPartialSyncInstall(...)` and `finalizeInstalledPartialSyncAfterNodeStart(...)`, the durable marker has already been cleared, and the current service/coordinator session then fails during renderer/app-return cleanup.
    - Record this locked representation in tests and research so later tasks do not assume a durable C2 marker exists.

4. Add explicit recovery-action execution paths.
   - `retry`:
     - Allowed only in Boundary A terminal failures.
     - Must clean staged artifacts first so the next run starts from a known-empty staging area.
     - Must not rely on renderer to request a separate cleanup step.
     - Should reuse the existing coordinator `startPartialSync(...)` path after cleanup rather than duplicating preflight logic.
    - `restart-normal`:
     - Allowed only when the backend boundary says the current DB is safe to start normally.
       - For Boundary A, this means clearing staged artifacts and any partial-sync terminal status, then reusing the existing startup-owned chain-storage/cardano-node start seam through the `handleCheckDiskSpace` function returned by `handleDiskSpace(...)` in `source/main/utils/handleDiskSpace.ts`, which is created in `source/main/index.ts` as `handleCheckDiskSpace`, not by introducing a direct renderer-facing restart path.
     - For Boundary C2, this means leaving the installed DB in place, clearing any remaining partial-sync status state if needed, and starting normally if the node is stopped.
     - It must be rejected automatically in Boundary B and C1.
    - `wipe-and-full-sync`:
     - Must remain available from every terminal boundary, including startup-owned unsafe-install states.
       - Reuse existing chain wipe plus normal bootstrap startup gating rather than inventing a second full-sync implementation.
       - The exact reuse point is locked for this task: diagnostics-triggered `wipe-and-full-sync` must hand off back into the startup-owned `handleCheckDiskSpace` closure returned by `handleDiskSpace(...)` in `source/main/utils/handleDiskSpace.ts` and instantiated in `source/main/index.ts` as `const handleCheckDiskSpace = handleDiskSpace(mainWindow, cardanoNode);`.
       - Task-204 may add a narrow coordinator or channel path to request that handoff, but it must not bypass `handleCheckDiskSpace`, `startNodeAfterPartialSyncInstall(...)`, `finalizeInstalledPartialSyncAfterNodeStart(...)`, or the generation checks and startup suppression logic that already live there.

5. Implement the missing coordinator and IPC action seams narrowly.
   - Extend `ChainStorageCoordinator` with explicit methods for partial-sync recovery actions instead of hiding them inside channel files.
   - Extend the partial-sync handler registration seam only as far as necessary, likely to include `restartNormal()` and `wipeAndFullSync()` or a minimal action dispatcher.
   - Update `mithrilPartialSyncChannel.ts` so `restart-normal` and `wipe-and-full-sync` delegate to real backend logic instead of the placeholder rejection.
   - Keep payloads `void`; boundary and cleanup decisions stay backend-owned.

6. Keep startup-owned recovery and diagnostics-owned recovery aligned.
   - Do not remove the startup-native wipe-or-quit prompt for interrupted Boundary B or C1 states.
   - Make sure any shared wipe path used by startup and diagnostics clears both staged partial-sync artifacts and the durable marker in the same way.
    - Use the startup-owned `handleCheckDiskSpace` closure from `handleDiskSpace(...)` as the concrete source of truth for both `restart-normal` and post-wipe full-sync handoff, and for the existing generation/marker safety rules; task-204 may add a narrow coordinator entry point into that seam, but must not bypass it.
   - If startup currently clears the marker after first-start proof and task-204 keeps that behavior, document it explicitly in tests and the task-specific research note so later tasks do not assume `node-start-verified` exists.

7. Add the required LauncherConfig kill switch in the smallest backend-first form.
    - Because the PRD and task-004 locked LauncherConfig as the primary disable path, task-204 must add that guard now as a required deliverable, not optional polish.
    - Add one narrow `LauncherConfig` boolean plus main-process enforcement that rejects diagnostics-launched partial-sync start, `retry`, and `restart-normal` entry points when disabled.
    - Keep startup-owned interrupted-install recovery and diagnostics-triggered `wipe-and-full-sync` available even when the kill switch disables new partial-sync attempts, matching the PRD rollback contract.
    - Keep this backend-first. Renderer gating and user-facing copy can still land later, but the live backend must already have a fast disable path before task-300 and later rollout work.

8. Clear backend state fully after terminal outcomes and chosen recovery actions.
   - Ensure service `_activeWorkDir`, `_currentProcess`, `_stagedDbPath`, progress items, cached error, and cached allowed actions do not leak across runs.
   - Ensure channel-level cached status is replaced with a truthful post-action state instead of merge-retaining stale allowed actions.
   - Ensure retry and restart-normal do not leave the backend stuck in a pseudo-active state that would continue suppressing automatic node restart or block future partial-sync starts.

9. Add focused tests for every backend-owned recovery branch.
   - Extend `MithrilPartialSyncService.spec.ts` to cover:
     - Boundary A cancellation cleanup success
     - Boundary A cancellation cleanup failure behavior
     - denied cancellation after cutover begins
     - recovery-action eligibility mapping for Boundary A versus install/finalizing failures
   - Extend `chainStorageCoordinator.spec.ts` to cover new recovery-action seams and mutual exclusion behavior.
   - Extend `mithrilPartialSyncChannel.spec.ts` so start, cancel, restart-normal, and wipe-and-full-sync all delegate to the coordinator correctly and no longer use the placeholder error.
   - Extend `handleDiskSpace.spec.ts` only where task-204 changes startup-owned wipe paths or marker-clearing behavior.

## Acceptance Criteria

- Boundary A cancellation performs explicit staged cleanup and leaves the live managed chain target untouched.
- Cancellation remains denied automatically once live cutover begins.
- Backend-owned recovery eligibility exactly matches the locked safety rules from `task-004` and the PRD.
- Boundary C2 is implemented only as the locked same-session in-memory post-proof state, not as a new durable marker representation.
- `retry`, `restart-normal`, and `wipe-and-full-sync` are executed only when the backend says they are safe for the current boundary.
- `restart-normal` is rejected in Boundary B and C1 without depending on renderer restraint.
- A partial-sync `LauncherConfig` kill switch exists and is enforced in main-process entry points for diagnostics-launched start, retry, and restart-normal flows while preserving startup-owned unsafe-install recovery and wipe/full-sync recovery.
- `wipe-and-full-sync` reuses the existing full-bootstrap recovery path only after chain cleanup and marker clearing are handled.
- `restart-normal` and `wipe-and-full-sync` handoff reuses the startup-owned `handleCheckDiskSpace` seam rather than bypassing it with direct cardano-node start logic.
- Backend partial-sync state fully clears after cancellation and after chosen recovery actions, so future runs start from a well-defined state.
- Partial-sync IPC handlers for `restart-normal` and `wipe-and-full-sync` no longer reject as unimplemented.
- Startup-owned interrupted Boundary B and C1 recovery remains intact and aligned with any shared wipe path used by diagnostics-launched recovery.
- Focused Jest coverage exists for cancellation cleanup, denied unsafe actions, action delegation, and boundary-dependent recovery branching.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/handleDiskSpace.spec.ts` if startup wipe/marker behavior changes materially
- Verify these behaviors directly:
  - cancelling a Boundary A run removes staged partial-sync artifacts and emits a truthful terminal cancelled state
  - cancelling after `installing` begins is rejected
  - a Boundary A failure exposes `retry`, `restart-normal`, and `wipe-and-full-sync`
  - an install/finalizing/starting-node failure exposes `wipe-and-full-sync` only unless the DB has already passed the one proven successful post-cutover node-start handoff
  - `restart-normal` IPC is rejected automatically for Boundary B and C1 states
  - `retry` reuses the normal partial-sync start path only after cleanup succeeds
- `wipe-and-full-sync` clears partial-sync marker/status and returns the app to the ordinary empty-chain bootstrap path instead of leaving partial-sync state behind
- disabling the LauncherConfig kill switch blocks new diagnostics-launched start/retry/restart-normal requests but still allows startup-owned unsafe-install recovery and wipe/full-sync recovery
- channel-level cached status replacement does not retain stale allowed actions after recovery actions complete
  - startup-owned interrupted unsafe-install recovery still blocks normal startup and still wipes successfully after the task-204 changes
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` remains blocked by unrelated pre-existing failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The main risk is accidentally implementing `restart-normal` as a generic cardano restart path without first proving the current boundary is safe. That would violate the locked task-004 rules.
- A second risk is under-cleaning Boundary A cancellation, which the spike already showed can leave scratch artifacts behind even when the live DB is untouched.
- The current repo still lacks the LauncherConfig kill switch locked by task-004, so task-204 must add it as a mandatory backend deliverable without widening into renderer rollout work.
- Boundary C2 is now intentionally locked to a same-session in-memory state. That keeps task-203's marker-clear-after-proof behavior, but it also means recovery actions after app restart continue to rely on startup-owned `installed-awaiting-node-start` protection rather than a durable post-proof marker.
- If diagnostics-launched `restart-normal` or `wipe-and-full-sync` need direct `cardanoNode` control, the smallest safe injection seam must avoid widening partial-sync state ownership into the renderer.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-204.md`.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-204` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- Add `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md` capturing the final cancellation cleanup rule, recovery-action execution seam, boundary-to-action mapping as implemented, and the chosen marker/C2 posture.
- Update `.agent/system/architecture.md` because implementation is expected to land a durable partial-sync LauncherConfig kill switch and a named main-process handoff back into the startup-owned `handleCheckDiskSpace` seam that later tasks must preserve.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-204-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-204-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Approved Plan

- Boundary A cancellation now performs explicit staged cleanup and marker clearing before reporting `cancelled`, while cleanup failure remains a Boundary A `failed` outcome with restart-safe recovery actions.
- Backend-owned recovery eligibility remains boundary-based: Boundary A allows `retry`, `restart-normal`, and `wipe-and-full-sync`; wipe-only boundaries keep `retry` blocked at the backend start seam.
- Diagnostics-triggered `restart-normal` and `wipe-and-full-sync` re-enter the startup-owned `handleCheckDiskSpace` seam through a narrow coordinator callback from `source/main/index.ts` instead of starting `cardano-node` directly.
- `wipe-and-full-sync` preserves the partial-sync marker until the actual chain wipe completes so startup blocking remains intact across crashes during recovery.
- `LauncherConfig.mithrilPartialSyncEnabled` is now the backend rollout guard for new diagnostics-launched partial sync entry and restart-normal recovery without disabling startup-owned unsafe-install recovery.

## Final Outcome

- Implemented backend cancellation cleanup, restart-normal and wipe/full-sync recovery delegates, backend retry gating, and live IPC wiring for diagnostics-launched partial sync recovery actions.
- Added the required launcher-config kill switch seam and documented the startup handoff seam plus the finalized recovery behavior in architecture/research docs.
- Focused Jest coverage passed for the touched service, coordinator, and IPC seams. `yarn compile` remains blocked by unrelated pre-existing repo errors outside task-204 in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`.
- Research brain update completed in `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`.

## Self-Review

- Scope-creep check: kept renderer/store/modal work, broader rollout UX, and manual QA out of task-204; the plan stays on backend cancellation, cleanup, IPC action wiring, and boundary enforcement.
- Workflow freshness check: `ipc.md`, `test.md`, and `update-doc.md` were consulted and the plan only includes IPC/doc work where the live repo still lacks required seams.
- Missing-tests/docs/manifests check: added focused service/coordinator/channel/startup test expectations, a task-specific research note, and conditional architecture/config updates if the kill switch or runtime seams land.
- Consistency check: the plan matches the PRD and task-004 boundary rules, the task-203 startup marker behavior, and the live repo fact that `restart-normal` and `wipe-and-full-sync` are still unimplemented placeholder IPC actions.
