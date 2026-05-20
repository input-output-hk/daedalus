Planner: Iteration 1
Timestamp: 2026-05-20T18:44:40Z
- Reviewed required inputs: `.agent/readme.md`, `.agent/system/architecture.md`, Mithril partial-sync PRD, tasks graph, all existing task-100 through task-203 canonical plans as needed for continuity, task-specific research `03` through `10`, historical Mithril docs/research, and workflows `ipc.md`, `test.md`, and `update-doc.md`. `frontend.md` was intentionally not used because task-204 remains backend/IPC/startup-safety scoped. `understand` skill guidance was consulted, then important findings were verified against live files.
- Current repo state confirms `task-203` already owns staged restore, LSM conversion, validated cutover, durable partial-sync markers, and startup-owned Boundary B/C1 blocking, but `task-204` is still required because:
  - `MithrilPartialSyncService.cancel()` only resets in-memory state and kills the child process; it does not explicitly clean staged partial-sync artifacts.
  - `restart-normal` and `wipe-and-full-sync` IPC actions still reject as unimplemented in `source/main/ipc/mithrilPartialSyncChannel.ts`.
  - `ChainStorageCoordinator` has only `start` and `cancel` partial-sync handlers, so there is no backend recovery-action execution seam yet.
  - The PRD/task-004 safety contract still needs enforcement in code so unsafe recovery actions are rejected by backend boundary, not renderer restraint.
- Classified interaction mode as `autonomous`.
  - Required user inputs before implementation: none.
  - Required manual test steps before implementation: none.
  - Required evidence back from user during implementation: none.
  - Implementation can proceed before user interaction: yes.
- Planned smallest truthful implementation:
  - Keep work inside existing main-process seams: extend `MithrilPartialSyncService`, `ChainStorageCoordinator`, and `mithrilPartialSyncChannel` rather than adding a second orchestrator.
  - Finish Boundary A cancellation truthfully by cleaning staged partial-sync artifacts before emitting terminal `cancelled`, while preserving the untouched live managed chain target.
  - Keep cancellation denied once live cutover begins.
  - Add backend-owned recovery execution for `retry`, `restart-normal`, and `wipe-and-full-sync`, with action eligibility mapped directly from the locked Boundary A/B/C rules.
  - Reuse existing chain wipe/bootstrap startup flow for `wipe-and-full-sync` after explicit marker/status cleanup, rather than inventing a second full-sync implementation.
  - Keep startup-owned interrupted Boundary B/C1 recovery from task-203 intact and align any shared wipe path with it.
  - Decide explicitly during implementation whether to keep current marker-clear-after-first-start-proof behavior or adopt the dormant `node-start-verified` state for Boundary C2; document whichever is chosen.
- Expected files to change:
  - `.agent/plans/mithril-partial-sync/task-plans/task-204.md`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/handleDiskSpace.ts` and spec only if shared wipe/marker behavior changes materially
  - `source/main/config.ts`, launcher-config readers, and `source/main/index.ts` only if task-204 also lands the smallest truthful LauncherConfig kill-switch seam
  - task-specific research note `11-task-204-cancellation-and-recovery-notes.md`
  - `.agent/system/architecture.md` only if a durable runtime seam is added
- Verification plan:
  - Targeted Jest for `MithrilPartialSyncService.spec.ts`, `chainStorageCoordinator.spec.ts`, `mithrilPartialSyncChannel.spec.ts`, and `handleDiskSpace.spec.ts` if startup recovery paths change.
  - Verify Boundary A cancel cleanup, denied cancel after cutover, boundary-dependent recovery-action exposure, backend rejection of unsafe `restart-normal`, reuse of normal partial-sync start for `retry`, and proper marker/status cleanup for `wipe-and-full-sync`.
  - Attempt truthful TypeScript verification; if `yarn compile` is still blocked by unrelated pre-existing repo issues, record that and rely on focused Jest plus live diff inspection.
- Required docs/tracking updates after implementation:
  - keep canonical task plan/review logs append-only
  - add task-204 research note capturing cancellation cleanup, recovery-action seam, implemented boundary mapping, and chosen marker/C2 posture
  - update tasks graph only after implementation/review approval
  - update architecture docs only if a durable new runtime seam is added
- Self-review:
  - Scope creep avoided: no renderer/store/modal work pulled in.
  - Workflow text remained current relative to live repo state.
  - Tests/docs/tracking requirements are explicitly captured.
  - Plan is consistent with PRD, task-004 safety rules, task-203 startup marker behavior, and current live file constraints.
Outcome: Approved creation of canonical plan `.agent/plans/mithril-partial-sync/task-plans/task-204.md` with `autonomous` execution, backend-only task-204 scope, explicit expected file set, verification plan, and required research/docs follow-ups.

Critiquer: Iteration 1
Timestamp: 2026-05-20T18:49:55Z

Blocking findings:
1. The plan leaves the LauncherConfig kill switch as optional implementation-time judgment instead of a required deliverable. `task-204.md` says to "decide the smallest truthful kill-switch step" and maybe defer it, but the PRD already locks a fast disable path as required for live-chain mutation work (`mithril-partial-sync-prd.md`: rollout/rollback and NFRs) and the live repo still has no partial-sync flag in `source/main/config.ts`. This needs to be either in scope for `task-204` or split into a prerequisite backend task before renderer rollout work continues. Shipping recovery actions without the promised disable path is plan drift on a safety-critical feature.
2. The plan does not lock a source of truth for Boundary C2, even though recovery eligibility depends on it. `task-204.md` explicitly defers whether to keep "clear marker after first-start proof" or adopt the dormant `node-start-verified` marker. Live code currently clears the marker on first proven start in `source/main/utils/handleDiskSpace.ts`, while `source/main/mithril/mithrilPartialSyncMarker.ts` still exposes `node-start-verified` but does not use it. Because backend-owned safety rules are a core requirement, this boundary cannot stay as an implementation-time choice; the plan must first state exactly how C2 is represented and how `restart-normal` is distinguished from ordinary idle.
3. The recovery re-entry point into node startup/bootstrap orchestration is underspecified and too permissive. The plan allows "asking `cardanoNode.start()` through a main-process-owned path" and says wipe/full-sync should "allow the ordinary empty-chain Mithril bootstrap decision flow to resume", but it does not lock which existing startup-owned seam must own that transition. Live `handleDiskSpace.ts` already owns generation checks, startup gating, marker clearing, and bootstrap decision flow; historical chain-storage hardening also explicitly warns against bypassing startup orchestration. The plan needs to require one concrete reuse point for normal restart and wipe-to-bootstrap handoff, otherwise task-204 can regress node-lifecycle or chain-storage safety while still appearing to satisfy the happy path.

Non-blocking observations:
1. Interaction mode is correct: `autonomous` matches the current backend-only scope and does not hide any user/manual checkpoint.
2. The plan can simplify `retry`: do not add a separate retry IPC/coordinator action if the renderer can truthfully call the existing start path again after backend cleanup, gated by `allowedRecoveryActions`.
3. The verification plan is mostly solid, but add one explicit check that recovery-status broadcasts remain best-effort and do not become part of the startup critical path, matching the existing Mithril/bootstrap hardening rule.

Approval bar:
1. Make the LauncherConfig kill switch a required output of this task, or explicitly carve it into an earlier required blocker task before `task-300`.
2. Lock the Boundary C2 representation up front: either adopt durable `node-start-verified` or explicitly document a narrower same-session-only contract and remove the unused durable state.
3. Name the exact main-process seam that owns `restart-normal` and `wipe-and-full-sync` handoff so task-204 cannot bypass `handleDiskSpace` startup safeguards.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-20T18:49:55Z
- Re-read the required planning context: `.agent/readme.md`, `.agent/system/architecture.md`, Mithril partial-sync PRD, tasks graph, canonical task plan, planning review log, and workflows `ipc.md`, `test.md`, and `update-doc.md`. Re-checked live main-process files to verify the critique-sensitive seams before revising the plan.
- Revised only `.agent/plans/mithril-partial-sync/task-plans/task-204.md`.
- Addressed blocker 1 by promoting the LauncherConfig guard from a planning note into a required task deliverable:
  - the plan now states task-204 must add a partial-sync-specific `LauncherConfig` boolean now
  - main-process enforcement is required for diagnostics-launched `start`, `retry`, and `restart-normal`
  - acceptance criteria now require that kill switch to exist and preserve startup-owned unsafe-install recovery plus wipe/full-sync recovery
- Addressed blocker 2 by locking Boundary C2 up front instead of leaving it as an implementation-time choice:
  - C2 is now explicitly fixed as a same-session, in-memory post-proof state only
  - task-204 must keep task-203’s current marker-clear-after-first-successful-node-start behavior
  - task-204 must not adopt the dormant durable `node-start-verified` representation
  - the plan now names the exact condition for entering C2: after `handleDiskSpace` completes the first proven post-cutover node-start handoff and clears the durable marker, followed by a later same-session failure during app-return cleanup
- Addressed blocker 3 by naming the exact startup/main-process seam that owns the `restart-normal` and `wipe-and-full-sync` handoff:
  - the canonical owner is now explicitly the startup-owned `handleCheckDiskSpace` closure returned by `handleDiskSpace(...)` in `source/main/utils/handleDiskSpace.ts`
  - the instantiation point is explicitly named in `source/main/index.ts` as `const handleCheckDiskSpace = handleDiskSpace(mainWindow, cardanoNode);`
  - the plan now forbids bypassing that seam with direct cardano-node start logic and explicitly preserves `startNodeAfterPartialSyncInstall(...)`, `finalizeInstalledPartialSyncAfterNodeStart(...)`, generation checks, and startup suppression logic as the governing safeguards
- Tightened acceptance and verification criteria so review can fail task-204 if:
  - the LauncherConfig kill switch is missing or only partially enforced
  - Boundary C2 is implemented as a new durable marker
  - recovery actions bypass the startup-owned `handleCheckDiskSpace` seam
- Kept the rest of the task intentionally narrow: no renderer work added, no broader infrastructure expansion, and no review log edits.
Outcome: Revised `task-204.md` now fully resolves the critique blockers by making the LauncherConfig kill switch mandatory, locking Boundary C2 representation before implementation, and naming the exact startup-owned `handleCheckDiskSpace` seam that must own `restart-normal` and `wipe-and-full-sync` handoff.

