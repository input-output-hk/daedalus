Implementation: Iteration 1
Timestamp: 2026-05-20T18:58:41Z
- Changes made:
  - Added a backend LauncherConfig kill switch field (`mithrilPartialSyncEnabled`) and enforced it in the coordinator for diagnostics-launched partial-sync start and restart-normal entry paths.
  - Extended `ChainStorageCoordinator` with narrow recovery delegates (`restartNormalFromPartialSync`, `wipeAndFullSyncFromPartialSync`) plus a startup handoff provider so diagnostics-triggered recovery re-enters the startup-owned `handleCheckDiskSpace` seam instead of bypassing it.
  - Implemented `MithrilPartialSyncService` cancellation cleanup for Boundary A by removing staged partial-sync artifacts and clearing the durable marker before reporting `cancelled`, and converting cleanup failure into a Boundary A `failed` state with restart-safe recovery actions.
  - Implemented service-side `restartNormal()` and `wipeAndFullSync()` cleanup/reset behavior for allowed terminal states.
  - Replaced the IPC placeholder handlers for `restart-normal` and `wipe-and-full-sync` with real coordinator delegation.
  - Added focused Jest coverage for service cancellation/recovery cleanup, coordinator recovery-action orchestration, and IPC action delegation.
- Files touched:
  - `source/main/config.ts`
  - `source/main/index.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
- Verification run:
  - `yarn test:jest "source/main/mithril/MithrilPartialSyncService.spec.ts" "source/main/utils/chainStorageCoordinator.spec.ts" "source/main/ipc/mithrilPartialSyncChannel.spec.ts"` ✅
  - `yarn compile` ⚠️ failed on unrelated pre-existing repo issues outside task-204 (`source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, `source/main/trezor/manifest.ts`).
- Deviations from approved plan:
  - No `handleDiskSpace.ts` production changes were required because the smallest truthful startup handoff was to reuse the existing returned `handleCheckDiskSpace` closure from `index.ts` via a narrow coordinator callback instead of widening startup logic.
  - Boundary C2 remained the approved same-session in-memory contract; no durable `node-start-verified` adoption was introduced.
- User interaction required now: no.
Outcome: Task-204 implementation is functionally in place for backend cancellation cleanup, recovery branching, IPC wiring, startup handoff reuse, and LauncherConfig rollout guarding, with focused tests passing and only unrelated repo-wide compile failures remaining.

Code Review: Iteration 1
Timestamp: 2026-05-20T19:04:01Z

Reviewed the required planning/workflow docs, the task-204 review logs, relevant Mithril partial-sync and historical chain-storage research, and then verified the implementation against the live diff in the touched main-process files.

Blocking findings:
1. `wipe-and-full-sync` clears `mithril-partial-sync.lock` before the live chain is actually wiped, which opens an unsafe startup gap if Daedalus exits between those two steps. `MithrilPartialSyncService.wipeAndFullSync()` calls `_cleanupPartialSyncArtifacts()`, and that helper always removes the marker (`source/main/mithril/MithrilPartialSyncService.ts:383-388,646-649`) before `ChainStorageCoordinator.wipeAndFullSyncFromPartialSync()` performs the real chain wipe (`source/main/utils/chainStorageCoordinator.ts:318-334`). Startup currently relies on that marker to keep Boundary B/C1 installs wipe-only (`source/main/utils/handleDiskSpace.ts:156-208,317-360`). As written, a mid-recovery crash can strand an unsafe DB with no startup block.
2. Unsafe retry is still backend-allowed from wipe-only boundaries. `startPartialSync()` only checks the feature flag, node state, and layout preflight (`source/main/utils/chainStorageCoordinator.ts:212-266`); it never consults the current partial-sync recovery boundary/status. After an `installing`, `finalizing`, or `starting-node` failure where the backend advertises `[wipe-and-full-sync]` only (`source/main/mithril/MithrilPartialSyncService.ts:615-628`), a caller can still invoke the normal start channel again and re-enter partial sync. The approved task contract allowed "retry via start" only if backend gating enforced the boundary; right now that restriction still depends on renderer restraint.
3. Required task-204 documentation/tracking updates are still missing. This change adds a launcher kill switch and a coordinator-to-`handleCheckDiskSpace` recovery handoff seam, but there is no corresponding `.agent/system/architecture.md` update and no task-204 research note capturing the finalized behavior. The approved task plan called those updates out as required for this seam (`.agent/plans/mithril-partial-sync/task-plans/task-204.md:253-259`).

Non-blocking observations:
1. The implementation correctly reuses the startup-owned `handleCheckDiskSpace` seam instead of introducing a direct `cardanoNode.start()` bypass.
2. Focused Jest coverage improved cancellation and IPC delegation confidence, but it does not yet lock the two boundary regressions above. Add explicit tests for "start is rejected from wipe-only status" and "marker is retained until wipe completes".

Approval bar:
1. Keep the partial-sync marker in place until wipe/full-sync cleanup has completed successfully, or move marker clearing into the same post-wipe boundary that startup recovery already trusts.
2. Add backend-owned gating so `MITHRIL_PARTIAL_SYNC_START_CHANNEL` is rejected unless the current terminal state actually allows retry; Boundary B/C1/C2 must not be able to re-enter through the normal start path.
3. Land the missing task-204 documentation updates for the launcher kill switch and startup handoff seam, then extend focused Jest coverage for the guarded paths above.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-05-20T19:04:01Z
- Changes made:
  - Kept the partial-sync marker in place during diagnostics-triggered `wipe-and-full-sync` until the real chain wipe completes by splitting service cleanup into a pre-wipe stage (`wipeAndFullSync`) and a post-wipe finalizer (`finalizeWipeAndFullSync`).
  - Added backend retry gating through `MithrilPartialSyncService.assertStartAllowed()` and coordinator start-path enforcement so wipe-only boundaries can no longer re-enter via the normal partial-sync start channel.
  - Updated IPC handler registration to include the new service guards/finalizer delegates.
  - Added focused Jest coverage for marker retention during wipe/full-sync finalization and for rejecting start reuse from wipe-only boundaries.
  - Added the required task-204 documentation updates: architecture doc entries for the startup handoff seam and rollout guard, plus a new research note capturing the finalized recovery behavior.
- Files touched:
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `.agent/system/architecture.md`
  - `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- Verification run:
  - `yarn test:jest "source/main/mithril/MithrilPartialSyncService.spec.ts" "source/main/utils/chainStorageCoordinator.spec.ts" "source/main/ipc/mithrilPartialSyncChannel.spec.ts"` ✅
- Deviations from approved plan:
  - No deviation from the approved recovery ownership. The fix narrowed the implementation further by keeping marker clearing coupled to the post-wipe boundary instead of widening startup logic.
- User interaction required now: no.
Outcome: The task-204 implementation now preserves startup blocking through wipe/full-sync recovery, enforces backend-owned retry gating for wipe-only boundaries, and includes the required architecture/research updates with focused coverage for the reviewed edge cases.

Code Review: Iteration 2
Timestamp: 2026-05-20T19:06:06Z

Reviewed the required planning and workflow docs, the updated task-204 review history, the new task-204 research note, and the live touched implementation/files called out for this pass.

Blocking findings:
- None. The iteration-1 blockers are resolved in the live implementation:
- marker retention now stays in place until wipe/full-sync cleanup actually completes, with marker clearing deferred to the post-wipe finalizer (`source/main/mithril/MithrilPartialSyncService.ts:383-397`, `source/main/utils/chainStorageCoordinator.ts:321-338`, `source/main/mithril/MithrilPartialSyncService.spec.ts:781-800`)
- backend retry gating now blocks re-entry through the normal start path from wipe-only failed boundaries (`source/main/mithril/MithrilPartialSyncService.ts:400-416`, `source/main/utils/chainStorageCoordinator.ts:223-229`, `source/main/mithril/MithrilPartialSyncService.spec.ts:802-817`)
- the required documentation updates for the startup handoff seam and launcher kill switch are now present, along with the task-204 recovery note (`.agent/system/architecture.md:241-250`, `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md:8-24`)

Non-blocking observations:
- Focused coverage now locks the two reviewed regressions well, but there is still no explicit spec for the launcher-config-disabled rejection path on `startPartialSync()` or `restartNormalFromPartialSync()` (`source/main/utils/chainStorageCoordinator.ts:226,293-303,385-389`).
- `wipeAndFullSync()` resets partial-sync status to `idle` before the bootstrap wipe completes (`source/main/mithril/MithrilPartialSyncService.ts:383-397`). Safety is still preserved because the durable marker remains until `finalizeWipeAndFullSync()`, but later renderer work should treat this as a handoff into startup-owned recovery rather than as a status-rich partial-sync subflow.

Approval bar:
- The previously raised blockers are resolved, and this pass did not uncover a new materially blocking issue in the updated task-204 scope.

Decision: approved

