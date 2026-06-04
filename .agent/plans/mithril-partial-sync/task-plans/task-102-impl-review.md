Implementation: Iteration 1
Timestamp: 2026-05-20T01:20:51Z

Changes made:
- Added a coordinator-owned partial-sync seam in source/main/utils/chainStorageCoordinator.ts with exported PartialSyncPreflightContext, setPartialSyncHandlers, startPartialSync, and cancelPartialSync.
- Added separate _partialSyncInProgress tracking and symmetric mutual-exclusion checks so partial sync cannot overlap with bootstrap or chain-storage mutations, and bootstrap now also rejects while partial sync is active.
- Kept the workdir boundary partial-sync-specific by resolving layoutResult plus mithrilWorkDir and passing them to registered partial-sync handlers instead of mutating bootstrap service workdir state.
- Enforced coordinator preflight rejection for non-STOPPED node state and isRecoveryFallback layouts before any partial-sync delegate runs.
- Extended source/main/utils/chainStorageCoordinator.spec.ts with focused coverage for partial-sync preflight context handoff, recovery-fallback rejection, node-state rejection, bootstrap/partial-sync mutual exclusion in both directions, prompt cancel delegation, and mutation blocking while partial sync is active.

Files touched:
- source/main/utils/chainStorageCoordinator.ts
- source/main/utils/chainStorageCoordinator.spec.ts

Verification run:
- yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts: passed
- ./node_modules/.bin/tsc --noEmit --pretty false --skipLibCheck source/main/utils/chainStorageCoordinator.ts source/main/utils/chainStorageCoordinator.spec.ts: not a truthful focused signal in this repo; it pulled in unrelated repo-wide TypeScript/decorator configuration errors outside task-102 touched files.

Deviations from the approved plan:
- No behavior deviation. I tightened startBootstrap() with the same partial-sync overlap guard to satisfy the plan-required mutual exclusion in both directions; that was an implementation-time symmetry fix, not scope expansion.

Whether user interaction is now required:
- No. This task remains autonomous at the current implementation stage.

Outcome: task-102 implementation iteration 1 is ready for code review with focused passing coordinator coverage and no IPC or service-scope widening.

Code Review: Iteration 1
Timestamp: 2026-05-20T01:23:30Z

- Reviewed the required docs, approved task plan/review artifacts, partial-sync and historical Mithril chain-storage research, then verified the implementation against the live diff in source/main/utils/chainStorageCoordinator.ts, source/main/utils/chainStorageCoordinator.spec.ts, source/main/utils/chainStorageManager.ts, source/main/utils/chainStorageManagerLayout.ts, source/main/cardano/setup.ts, and source/main/ipc/mithrilPartialSyncChannel.ts.
- The implementation stays within the intended coordinator-only scope and keeps the partial-sync delegate boundary separate from bootstrap service state, but one chain-storage safety regression is still present in the new preflight ordering.

Blocking findings:
- source/main/utils/chainStorageCoordinator.ts: startPartialSync() called ensureManagedChainLayout() before enforcing the strict STOPPED guard. That is not just a read-only preflight: existing layout helpers can create the default managed chain directory, adopt an inconsistent custom layout, or fall back from a broken custom target before _assertNodeStopped() ever runs. Task-102 locked the node-stopped check as a coordinator-owned precondition before partial-sync-related chain-storage work begins, so this ordering reintroduced filesystem mutation while cardano-node may still be running or stopping. The non-stopped test only proved the delegate was not called; it did not prove layout normalization and workdir resolution were skipped.

Non-blocking observations:
- The partial-sync workdir handoff stayed on the approved boundary: the coordinator resolves layoutResult plus mithrilWorkDir and passes them through a dedicated delegate seam instead of mutating MithrilBootstrapService state.
- Mutual exclusion was broadened in the right direction: bootstrap now rejects while partial sync is active, and destructive chain-storage mutations reject while partial sync is in progress.
- Focused coordinator coverage is otherwise aligned with the plan: bootstrap/partial-sync overlap, recovery-fallback rejection, prompt cancel delegation, and mutation blocking are all exercised.

Approval bar:
- Move the strict _assertNodeStopped(..., start Mithril partial sync) check ahead of any ensureManagedChainLayout() call in startPartialSync(), so no chain-storage normalization or fallback runs unless the node is already stopped.
- Extend the non-stopped partial-sync tests to prove that ensureManagedChainLayout() and resolveMithrilWorkDir() are not touched for running and stopping inputs.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-05-20T01:23:37Z

Changes made:
- Moved the strict node-stopped guard to the top of startPartialSync() before any ensureManagedChainLayout() call, so coordinator preflight no longer allows layout normalization, recovery fallback, or other chain-storage mutations to run while cardano-node is running or stopping.
- Strengthened partial-sync node-state regression coverage in source/main/utils/chainStorageCoordinator.spec.ts to assert ensureManagedChainLayout() and resolveMithrilWorkDir() are not touched for running or stopping inputs.

Files touched:
- source/main/utils/chainStorageCoordinator.ts
- source/main/utils/chainStorageCoordinator.spec.ts

Verification run:
- yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts: passed (31 tests)

Deviations from the approved plan:
- None. This iteration only corrected the coordinator preflight ordering to match the approved node-stopped-before-layout safety rule.

Whether user interaction is now required:
- No. This task remains autonomous.

Outcome: reviewer-identified preflight-order regression is fixed, and the focused coordinator tests now prove no layout or workdir work runs for non-stopped partial-sync starts.

Code Review: Iteration 2
Timestamp: 2026-05-20T01:24:16Z

- Re-read the approved task-102 plan and the implementation review log, then verified the updated live files in source/main/utils/chainStorageCoordinator.ts and source/main/utils/chainStorageCoordinator.spec.ts against the previously reported blocker and the original coordinator-scope acceptance bar.
- The previously reported blocker is resolved. startPartialSync() now enforces _assertNodeStopped(..., start Mithril partial sync) before any ensureManagedChainLayout() call, so partial-sync preflight no longer allows layout normalization, recovery fallback, or other chain-storage mutations to begin while cardano-node is running or stopping.
- The focused regression coverage now proves that behavior directly: the non-stopped partial-sync tests assert that both ensureManagedChainLayout() and resolveMithrilWorkDir() are untouched for running and stopping inputs, which closes the earlier test gap.
- The rest of the iteration remains within the approved task boundary: the partial-sync delegate seam is still separate from bootstrap service state, mutual exclusion remains symmetric between bootstrap and partial sync, and no recovery-action APIs or IPC/service-scope widening were introduced.

Blocking findings:
- None.

Non-blocking observations:
- The implementation review log for Iteration 2 accurately describes the fix and matches the live code and tests.
- The coordinator tests now cover the key task-102 contract well: explicit preflight context handoff, recovery-fallback rejection, strict non-stopped rejection before layout/workdir work, symmetric bootstrap/partial-sync exclusion, prompt cancel delegation, and mutation blocking while partial sync is active.

Approval bar:
- Approved as implemented for task-102. Later tasks still need to wire the delegate into a real partial-sync service and node-lifecycle suppression, but that is the intended follow-on scope rather than missing work in this task.

Decision: approved

