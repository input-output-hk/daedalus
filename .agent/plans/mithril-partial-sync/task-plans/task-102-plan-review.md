Planner: Iteration 1
Timestamp: 2026-05-20T01:15:21Z

- Reviewed the required planning/docs set in order, consulted the `understand` skill first for repo grounding, and verified the important findings against live files in `source/main/utils/chainStorageCoordinator.ts`, `source/main/utils/chainStorageCoordinator.spec.ts`, `source/main/ipc/mithrilPartialSyncChannel.ts`, `source/main/cardano/setup.ts`, `source/main/ipc/index.ts`, `source/main/utils/chainStorageManager.ts`, and related chain-storage layout helpers.
- Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-102.md`.
- Classified the task as `autonomous` and kept the plan narrowly scoped to coordinator work: add explicit partial-sync coordinator APIs, add separate partial-sync operation locking instead of reusing bootstrap entry points, enforce strict `STOPPED` + managed-layout + workdir-sync preconditions in one place, and fail closed on `isRecoveryFallback`.
- Locked the implementation approach to a minimal coordinator seam that unblocks later tasks without inventing the partial-sync engine early: coordinator-owned preflight and serialization now, real `MithrilPartialSyncService` orchestration and node restart suppression later.
- Preserved the existing bootstrap contract explicitly: `startBootstrap()` keeps its empty-chain invariant, and task-102 only broadens mutual-exclusion rules so bootstrap, partial sync, and destructive chain-storage mutations cannot overlap.
- Added truthful acceptance criteria and verification focused on coordinator tests, especially overlap protection, node-stopped rejection, `isRecoveryFallback` rejection, and workdir sync before delegation.

Outcome: approved draft plan is in place, minimal, and aligned with the locked staged-only restore and failure-containment decisions; no review log file was written.

Critiquer: Iteration 1
Timestamp: 2026-05-20T01:18:49Z

- Reviewed the required docs, task artifacts, partial-sync research, and historical Mithril/chain-storage hardening notes, then verified the key assumptions against the live main-process files in `source/main/utils/chainStorageCoordinator.ts`, `source/main/utils/chainStorageManager.ts`, `source/main/utils/chainStorageManagerLayout.ts`, `source/main/ipc/mithrilPartialSyncChannel.ts`, `source/main/ipc/mithrilBootstrapChannel.ts`, `source/main/ipc/chainStorageChannel.ts`, `source/main/ipc/index.ts`, and `source/main/cardano/setup.ts`.
- The plan is close, but it still leaves two material implementation traps open that are knowable from the current tree and should be closed before approval.

Blocking findings:
- The plan does not lock the workdir handoff boundary tightly enough. In the live repo, `ChainStorageCoordinator._syncMithrilWorkDir()` is bootstrap-specific and mutates `MithrilBootstrapService` directly (`source/main/utils/chainStorageCoordinator.ts:117-120`). Task-102 currently says partial-sync start should "sync the Mithril workdir" through the existing seam, but unless the plan explicitly says "resolve the workdir and pass it through a partial-sync-specific delegate/context," an implementation can easily wire partial sync through bootstrap service state by mistake. That is the wrong boundary for this task and would couple partial sync preflight to bootstrap internals.
- The plan still leaves room for coordinator-owned failure-action APIs in task-102 (`restartNormalAfterPartialSyncFailure`, `wipeAndFullSyncAfterPartialSyncFailure`, or equivalents), but the task definition in `mithril-partial-sync-tasks.json` only requires partial-sync entry points plus serialization and preflight checks. Boundary-dependent recovery eligibility is intentionally deferred to `task-204` by the PRD/task graph. Pulling those APIs into task-102 now would widen scope, force premature naming/behavior decisions before the durable-marker and Boundary A/B/C rules are implemented, and make the task more complex than needed.

Non-blocking observations:
- The live coordinator pattern already passes node state in from callers instead of owning a node-state provider itself (`prepareForLocationChange`, `setDirectory`, `wipeChainAndSnapshots`). Keeping that parameter-passing pattern for partial sync is simpler and matches the existing hardening direction.
- `isRecoveryFallback` must be checked from the immediate `ensureManagedChainLayout()` result, not from sticky config metadata alone, because `ChainStorageManager._isRecoveryFallback` is session-sticky until reset/repoint.
- No `.agent/system/` doc update is needed if task-102 stays coordinator-only. If the task also changes `mithrilPartialSyncChannel.ts` from explicit reject-until-implemented behavior to real handler delegation, the IPC doc inventory should be updated per workflow.

Approval bar:
- Update the task-102 plan so the coordinator resolves partial-sync preflight context and passes `layoutResult` plus resolved workdir through a partial-sync-specific delegate/context seam, instead of implying reuse of the bootstrap service workdir setter.
- Narrow task-102 scope explicitly to `startPartialSync`, `cancelPartialSync`, mutual-exclusion state/locking, and the minimal delegate-registration/context seam needed by task-200.
- Defer recovery-action coordinator APIs to task-204 unless one concrete chain-storage mutation is proven necessary now and its safety rules are fully specified in this task plan.
- Keep verification focused on coordinator behavior: `STOPPED`-only rejection, `isRecoveryFallback` fail-closed behavior, workdir/context preparation before delegation, bootstrap/partial-sync mutual exclusion, and mutation blocking while partial sync is active.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-20T01:18:55Z

- Re-read the canonical `task-102` plan and the approved critique, then revised `.agent/plans/mithril-partial-sync/task-plans/task-102.md` only.
- Fixed the workdir boundary explicitly: the plan now locks `task-102` to resolve partial-sync preflight context in the coordinator and pass `layoutResult` plus resolved Mithril workdir through a partial-sync-specific delegate/context seam, without implying bootstrap-service state reuse or `_syncMithrilWorkDir()`.
- Narrowed task scope explicitly to `startPartialSync`, `cancelPartialSync`, mutual-exclusion state or locking, and the minimal delegate-registration/context seam needed by `task-200`.
- Deferred recovery-action coordinator APIs to `task-204` instead of planning restart-normal or wipe-and-full-sync coordinator methods in `task-102`.
- Tightened verification back to coordinator behavior only: targeted `chainStorageCoordinator.spec.ts`, with assertions for node-stopped rejection, `isRecoveryFallback` rejection, explicit preflight-context handoff, mutual exclusion with bootstrap and chain-storage mutations, and prompt cancel delegation.
- Preserved the prior approved constraints: bootstrap keeps its empty-chain invariant, partial sync remains coordinator-only at this stage, and service orchestration plus node restart suppression stay deferred to later tasks.

Outcome: final approved `task-102` plan now cleanly locks the partial-sync preflight-context boundary, keeps scope minimal and truthful, and aligns directly with the critique without writing the review log.

