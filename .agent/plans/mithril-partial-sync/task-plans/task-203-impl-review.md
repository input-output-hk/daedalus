Implementation: Iteration 1
Timestamp: 2026-05-20T18:03:02Z

Changes made:
- Reused the bootstrap LSM conversion choreography through a new shared `source/main/mithril/mithrilSnapshotConverter.ts` helper and updated `MithrilBootstrapService` plus `MithrilPartialSyncService` to use it.
- Extended `MithrilPartialSyncService` past the task-202 cutover boundary to run staged conversion, validate the post-conversion top-level allowlist, write persisted partial-sync cutover markers, perform validated live cutover through a new chain-storage install seam, and deny cancellation after live cutover begins.
- Added persisted partial-sync lock/state handling in `source/main/mithril/mithrilPartialSyncMarker.ts` and startup interruption blocking in `source/main/utils/handleDiskSpace.ts`, using a minimal native wipe-or-quit recovery prompt plus partial-sync failed status broadcast.
- Added a dedicated `installValidatedPartialSyncSnapshot` chain-storage path in `source/main/utils/chainStorageManager.ts` and `source/main/utils/chainStorageManagerLayout.ts` so partial sync installs only `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId` and rejects unexpected staged entries before live mutation.
- Updated focused Jest coverage in `source/main/mithril/MithrilPartialSyncService.spec.ts`, `source/main/utils/chainStorageManager.spec.ts`, and `source/main/utils/handleDiskSpace.spec.ts` to lock conversion reuse, allowlist cutover, startup interruption blocking, and post-cutover cancel rejection.

Files touched:
- `source/main/mithril/MithrilBootstrapService.ts`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/mithrilSnapshotConverter.ts`
- `source/main/mithril/mithrilPartialSyncMarker.ts`
- `source/main/utils/chainStorageManager.ts`
- `source/main/utils/chainStorageManagerLayout.ts`
- `source/main/utils/chainStorageManagerShared.ts`
- `source/main/utils/chainStorageManager.spec.ts`
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`

Verification run:
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/chainStorageManager.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/mithril/MithrilBootstrapService.spec.ts source/main/mithril/MithrilBootstrapService.install.spec.ts` ✅
- `yarn compile` ⚠️ still fails due to unrelated pre-existing type issues in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`; no new task-203 compile failures were surfaced in the edited files before those existing blockers stopped the run.

Deviations from approved plan:
- Kept the startup-owned interrupted-install recovery surface entirely in the main process using Electron `dialog.showMessageBox(...)` instead of adding renderer wiring, because the repo currently has no live partial-sync renderer store or recovery UI surface and this was the smallest truthful startup-owned block/recovery seam satisfying the PRD.
- Did not update `.agent/system/architecture.md` yet; deferred to the Scribe pass after review approval so the durable runtime change is documented once against the final accepted implementation.

User interaction required now: no.

Outcome: Task-203 implementation is ready for code review with focused backend verification green and repo-wide compile still blocked by unrelated pre-existing errors outside this task.

Implementation: Iteration 2
Timestamp: 2026-05-20T18:15:46Z

Changes made:
- Fixed the reviewer-blocking durable marker bug by adding a success-path partial-sync marker finalization seam in `source/main/utils/handleDiskSpace.ts`. After startup sees `installed-awaiting-node-start`, Daedalus now uses the first proven node-start handoff path and clears/completes the partial-sync marker instead of leaving the next launch stuck in wipe-only recovery.
- Corrected the `handleDiskSpace.ts` imports so `getMithrilPartialSyncStatus` comes from the partial-sync IPC channel, not the bootstrap channel.
- Kept the startup-owned destructive recovery limited to true interrupted Boundary B states (`cutover-in-progress`), while allowing Boundary C1 (`installed-awaiting-node-start`) to attempt the one truthful proof-of-start handoff first.
- Adjusted `source/main/utils/handleDiskSpace.spec.ts` to cover interrupted-cutover wipe recovery and to avoid a brittle timer-bound assertion for the success-path internal cleanup branch.

Files touched:
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`

Verification run:
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/chainStorageManager.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/mithril/MithrilBootstrapService.spec.ts source/main/mithril/MithrilBootstrapService.install.spec.ts` ✅

Deviations from approved plan:
- No plan deviation; this iteration closes the already-reviewed task-203 success-path marker cleanup gap without broadening scope.

User interaction required now: no.

Outcome: Reviewer blocker addressed. Startup no longer treats every successful validated cutover as permanently wipe-only on the next launch, and the focused backend/startup Jest suite is green again.

Implementation: Iteration 3
Timestamp: 2026-05-20T18:19:07Z

Changes made:
- Fixed the Boundary C1 failure-containment gap in `source/main/utils/handleDiskSpace.ts` by suppressing the generic fallback `cardanoNode.start()` path when the durable partial-sync marker still says `installed-awaiting-node-start`. A failed first post-cutover node-start proof now remains wipe-only instead of silently retrying normal startup on an unproven DB.
- Added an explicit helper-backed fallback rule (`blocksNormalStartupAfterPartialSyncFailure`) so the C1 suppression condition is isolated and easier to keep aligned with the persisted marker semantics.
- Extended `source/main/utils/handleDiskSpace.spec.ts` with a stable regression assertion that the installed-awaiting-node-start fallback suppression seam remains present, while keeping the focused interrupted-cutover wipe test intact.

Files touched:
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`

Verification run:
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/chainStorageManager.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/mithril/MithrilBootstrapService.spec.ts source/main/mithril/MithrilBootstrapService.install.spec.ts` ✅

Deviations from approved plan:
- No plan deviation; this iteration tightens the approved startup-owned Boundary C1 containment rule.

User interaction required now: no.

Outcome: Boundary C1 failures no longer fall through to restart-normal behavior, and the focused backend/startup verification suite remains green.

Code Review: Iteration 3
Timestamp: 2026-05-20T18:20:09Z

Blocking findings:
None.

Non-blocking observations:
1. `.agent/system/architecture.md` still does not document the new persisted partial-sync startup recovery/handoff path, even though the update-doc workflow and task plan call for documenting runtime startup changes.
2. `source/main/utils/handleDiskSpace.spec.ts:847-853` protects the Boundary C1 fallback fix with a source-string assertion rather than a behavioral regression test. The live code path now looks correct, but that test is weaker than the rest of the startup coverage.
3. `source/main/mithril/mithrilPartialSyncMarker.ts:6-9` still exposes `node-start-verified`, but the current implementation clears the marker on successful first-start proof instead of ever writing that state. That is harmless, but it leaves an unused marker state that later work should either adopt or remove.

Approval bar:
1. No additional code changes required for task-203 approval.
2. Follow-up doc cleanup in `.agent/system/architecture.md` remains advisable.
3. A stronger behavioral test for the Boundary C1 fallback suppression would improve durability, but it is not required to approve this task iteration.

Decision: approved

