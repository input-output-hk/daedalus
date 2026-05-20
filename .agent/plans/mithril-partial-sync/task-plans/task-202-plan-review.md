Planner: Iteration 1
Timestamp: 2026-05-20T15:24:04Z

Summary:
Read the required planning sources in order: `.agent/readme.md`, `.agent/system/architecture.md`, the Mithril partial-sync PRD and task graph, the relevant partial-sync research notes, historical Mithril research for bootstrap/download/verification/chain-storage safety context, and the required workflows `test.md` and `update-doc.md`. I consulted the `understand` skill first as requested, then verified the important planning claims against live files including `source/main/mithril/MithrilPartialSyncService.ts`, `source/main/mithril/MithrilPartialSyncService.spec.ts`, `source/main/mithril/mithrilProgress.ts`, `source/main/mithril/mithrilCommandRunner.ts`, `source/main/ipc/mithrilPartialSyncChannel.ts`, `source/common/types/mithril-partial-sync.types.ts`, and `source/main/utils/chainStorageCoordinator.ts`. Based on that verified state, I created the canonical plan for `task-202` and created the empty companion review-log files.

Plan updates:
Created `.agent/plans/mithril-partial-sync/task-plans/task-202.md` as the canonical plan doc with:
- task id/title, why now, interaction mode, scope/non-goals
- dependency chain from tasks 001/200/201 and unblocks into 203/204
- truthful list of consulted docs, workflows, research, and the `understand` skill
- live-repo findings grounded in the current partial-sync service and progress/parser seams
- expected file changes centered on `MithrilPartialSyncService.ts`, its spec, and only narrow parser/research updates if needed
- implementation approach that keeps task-202 limited to staged `cardano-db download` execution, latest-drift handling, progress/status mapping, verification-stage reporting, and pre-cutover-safe failure semantics
- acceptance criteria and verification plan focused on targeted Jest coverage and truthful compile verification
- risks/open questions around latest drift, coarse verification telemetry, cancellation semantics, and avoiding overlap with task-203 cutover validation
- required docs/tracking updates, review-log paths, planning status `approved`, and build status `in_progress`
Created empty append-ready review-log files:
- `.agent/plans/mithril-partial-sync/task-plans/task-202-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-202-impl-review.md`
Deliberately did not consult `ipc.md` or `frontend.md` because the live repo and task scope show task-202 is still a backend Mithril-service task and does not truthfully require IPC/shared-contract or renderer/UI changes.

Self-review:
Scope creep check passed: the plan stays within staged download and verification and explicitly defers LSM conversion, live cutover, restart handoff, and recovery-action implementation to tasks 203 and 204. Workflow staleness check passed: only `test.md` and `update-doc.md` were needed for this task. Missing manifests/tests/docs check passed: the canonical plan exists, both review-log paths now exist as empty files, and focused verification expectations are included. Consistency check passed: the plan matches the live repo state where partial sync already has coordinator and IPC seams, the service still stops at `PARTIAL_SYNC_NOT_READY`, and `mithrilProgress.ts` is available but not yet wired into partial-sync status updates.

Outcome: `task-202` planning artifacts are now in place and aligned with the PRD, accepted research, and current repo state.

Critiquer: Iteration 1
Timestamp: 2026-05-20T15:28:42Z

Blocking findings:
- None. The plan is aligned with the PRD’s staged-only safety model, keeps live-chain cutover out of scope for `task-202`, matches the completed spike decisions on `latest` + `--start/--end` + staged `--allow-override`, and fits the live service/coordinator shape in `source/main/mithril/MithrilPartialSyncService.ts:168-236`, `source/main/utils/chainStorageCoordinator.ts:197-258`, and `source/common/types/mithril-partial-sync.types.ts:3-72`.

Non-blocking observations:
- The plan’s choice to keep `task-202` inside the existing `MithrilPartialSyncService.start()` path is the right simplification. It avoids reopening the task-201 boundary problem where `_partialSyncInProgress` would otherwise clear too early if the service returned before terminal state (`task-202.md:100-103`, `source/main/utils/chainStorageCoordinator.ts:245-249`).
- The proposed status mapping is truthfully narrower than bootstrap and matches the research better: step 1-3 as `downloading`, step 4-7 as `verifying`, with no invented verification percentage (`task-202.md:111-117`, `.agent/plans/mithril/research/mithril-bootstrap-process-output.md:7-25`). That is a good deliberate divergence from the startup bootstrap UX.
- The plan’s “post-verification but not yet converted/installed” stop boundary is acceptable for this task, because it is explicitly temporary and does not claim restart readiness (`task-202.md:131-134`). Just keep that failure/status wording obviously implementation-boundary-driven so it cannot be mistaken for a real Mithril verification failure.
- There is an existing bootstrap compatibility seam for `cardano-db download` vs `cardano-db snapshot download` in `source/main/mithril/MithrilBootstrapService.ts:666-677`. Reusing or consciously not reusing that seam is an implementation choice, not a planning blocker, because the spike and current pinned artifact already lock the shipped partial-download shape (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:101-209`).
- Cancellation cleanup is correctly deferred out of full recovery-branching scope, but implementation should avoid quietly expanding that into task-204. If any cleanup is added here, keep it limited to Daedalus-owned staging artifacts only, consistent with the plan text (`task-202.md:118-123`).

Approval bar:
- Preserve the staged-only boundary: `--download-dir` must stay on the Daedalus-owned staging parent, never the managed chain path.
- Keep lifecycle semantics truthful: no `completed`, `starting-node`, or live-install implications from task-202 alone.
- Verify with focused Jest/service coverage for command shape, drift handling, status mapping, telemetry propagation, and pre-cutover failure attribution; best-effort TypeScript verification is sufficient if unrelated repo compile debt still blocks full `yarn compile`.

Decision: approved

