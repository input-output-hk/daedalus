Planner: Iteration 1
Timestamp: 2026-05-20T17:45:02Z

Task: `task-203` - `Implement LSM conversion and validated staged cutover`
Status: approved draft plan recorded in `.agent/plans/mithril-partial-sync/task-plans/task-203.md`
Interaction mode: `autonomous`
Why now: `task-202` already restores and verifies the staged partial snapshot but still ends at `PARTIAL_SYNC_CUTOVER_NOT_READY`; the next smallest truthful backend step is staged ledger LSM conversion plus fixed-allowlist live cutover, while leaving boundary-dependent recovery handling to `task-204`.
Consulted materials: `.agent/readme.md`, `.agent/system/architecture.md`, Mithril partial-sync PRD and tasks JSON, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, partial-sync research notes `01`, `02`, `08`, `09`, historical Mithril docs including `bootstrap-cardano-node.md`, `mithril-chain-storage-hardening-summary.md`, `mithril-bootstrap-ui-model.md`, and `understand` skill guidance; important claims were verified against live files in `source/main/mithril/`, `source/main/utils/`, and `source/common/types/`.
Approved implementation direction: extend `MithrilPartialSyncService` in place, reuse or narrowly extract the proven bootstrap `snapshot-converter` flow for staged `db/` output, and add a dedicated chain-storage validated-cutover seam that empties the managed target then installs only `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`; reject any unexpected staged top-level entry, including `volatile`, and do not rely on the current broad `installSnapshot()` behavior that moves every top-level staged entry.
Expected files: `source/main/mithril/MithrilPartialSyncService.ts`, `source/main/mithril/MithrilPartialSyncService.spec.ts`, `source/main/utils/chainStorageManager.ts`, `source/main/utils/chainStorageManagerLayout.ts`, `source/main/utils/chainStorageManager.spec.ts`, optional narrow bootstrap converter/install seam files if extraction is the smallest safe path, and a task-specific research note `research/10-task-203-conversion-and-cutover-notes.md`.
Verification intent: targeted Jest for partial-sync service and chain-storage cutover behavior, plus bootstrap specs only if shared converter/install seams change; verify converter argument shape, staged `lsm/` creation, rejection of staged `volatile/` or any unexpected top-level entry, fixed-allowlist cutover into the managed target, live `volatile/` discard, and truthful install-stage failure reporting. Attempt `yarn compile`, but treat existing unrelated compile failures as non-blocking if unchanged.
Key risks/open questions: current `ChainStorageManager.installSnapshot()` is too broad for the locked allowlist rule; legacy managed-entry constants are not valid cutover truth because they include `volatile` and omit `lsm`/`protocolMagicId`; live cutover crosses Boundary B/C semantics, so any minimal durable marker added here must stay narrowly scoped and full interrupted-install recovery remains deferred to `task-204`; if automatic restart cannot be made truthful without task-204 recovery work, task-203 should stop at the narrowest honest post-cutover boundary instead of implying full completion.
Outcome: plan approved, canonical task plan doc created, no IPC/shared-contract workflow expansion needed for this task, build status truthfully left `not_started`, review-log paths confirmed as `.agent/plans/mithril-partial-sync/task-plans/task-203-plan-review.md` and `.agent/plans/mithril-partial-sync/task-plans/task-203-impl-review.md`.

Critiquer: Iteration 1
Timestamp: 2026-05-20T17:46:52Z

Blocking findings:
1. The plan does not truthfully cover the first persisted unsafe-install boundary it proposes to cross. `task-203.md` treats a durable cutover marker as optional and defers startup-owned interrupted-install recovery to `task-204` (`.agent/plans/mithril-partial-sync/task-plans/task-203.md:135-143,180-186`), but the live code has no persisted partial-sync marker or startup gate at all. `MithrilPartialSyncService` still keeps all state in memory and just clears `_activeWorkDir` in `finally` (`source/main/mithril/MithrilPartialSyncService.ts:172-276`), `setup.ts` suppresses crash restart only while that in-memory flag is active (`source/main/cardano/setup.ts:138-147`), and startup recovery in `handleDiskSpace.ts` only knows about `mithril-bootstrap.lock`, not partial sync (`source/main/utils/handleDiskSpace.ts:135-146,415-430`). If task-203 performs live target empty-and-reinstall without also planning the minimal persisted marker plus startup blocking path, a crash/quit/power loss during Boundary B/C1 can come back through normal startup with no way to distinguish “safe to restart” from “wipe/full-sync only,” which violates the PRD’s locked boundary rules (`.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md:167-185`). This is the main blocking gap.

Non-blocking observations:
1. The plan is otherwise appropriately narrow: extending `MithrilPartialSyncService`, reusing the proven bootstrap converter seam, and avoiding `installSnapshot()`’s broad move-all behavior matches the live code (`source/main/mithril/MithrilBootstrapService.ts:805-885`, `source/main/utils/chainStorageManagerLayout.ts:524-568`).
2. The “no IPC/shared-contract work expected” claim is acceptable only if task-203 does not emit `completed` or `starting-node`. Current shared contracts already support later phases (`source/common/types/mithril-partial-sync.types.ts:3-72`), so task-203 should stay at `converting`/`installing`/`finalizing` or fail, not imply post-cutover restart-safe completion.
3. If the task is broadened to include the minimal persisted marker/startup gate required above, the plan should also widen its required docs note from “research only” to include the relevant planning/runtime tracking update, because that changes durable Mithril runtime behavior rather than just an internal helper seam.

Approval bar:
- Update the plan so task-203 does one of these two things explicitly:
- Narrower option: stop at staged LSM conversion only and leave all live cutover for task-204.
- Preferred option: keep live cutover in task-203, but add the minimal persisted partial-sync operation marker and startup-owned block/recovery seam required to make Boundary B/C1 interruptions truthful.
- In either case, make the post-task terminal state explicit: task-203 must not report a restart-safe completion unless the startup/node-handoff semantics for that boundary are actually implemented.
- Add verification for interruption semantics around the first live mutation boundary, not just happy-path cutover and install failure.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-20T17:50:52Z

Task: `task-203` - `Implement LSM conversion and validated staged cutover`
Status: approved revised plan recorded in `.agent/plans/mithril-partial-sync/task-plans/task-203.md`
Interaction mode: `autonomous`
Why revised: the critique blocker was valid. The live repo still has no persisted partial-sync marker or startup-owned unsafe-install recovery path, so a plan that performed live target empty-and-reinstall while deferring that work to `task-204` was not truthful against the PRD’s Boundary B/C1 rules.
Consulted materials: `.agent/readme.md`, `.agent/system/architecture.md`, Mithril partial-sync PRD and tasks JSON, revised `task-203.md`, `task-203-plan-review.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, partial-sync research notes `01`, `02`, `05`, `06`, `07`, `08`, `09`, and historical Mithril docs including `bootstrap-cardano-node.md`, `mithril-chain-storage-hardening-summary.md`, `mithril-bootstrap-ui-model.md`, and `mithril-bootstrap-process-output.md`; `understand` skill guidance was loaded and all planning claims were re-verified against live files in `source/main/mithril/`, `source/main/utils/handleDiskSpace.ts`, `source/main/cardano/setup.ts`, `source/main/ipc/mithrilPartialSyncChannel.ts`, and `source/common/types/mithril-partial-sync.types.ts`.
Approved fix: keep live cutover in `task-203`, but widen it only by the minimum required safety closure. The revised plan now includes:
- staged LSM conversion reuse from the proven bootstrap converter seam
- fixed-allowlist staged cutover into the managed chain target
- a persisted partial-sync operation marker written before the first live mutation boundary and updated across Boundary B/C1
- a startup-owned block/recovery seam in `handleDiskSpace.ts` so interrupted Boundary B/C1 installs cannot fall through to ordinary startup
- an explicit post-task terminal-state rule: `task-203` must not emit restart-safe `completed` or `starting-node` semantics unless that node-handoff boundary is actually implemented
Scope boundaries preserved: `task-203` still does not take on full retry/restart-normal policy, diagnostics-launched recovery UX, or the complete boundary-dependent recovery matrix. Those remain in `task-204`. The startup recovery surface added here is intentionally minimal: enough to block normal node start and expose wipe-and-full-sync plus existing non-recovery affordances after interrupted unsafe installs.
Expected files now include: `source/main/mithril/MithrilPartialSyncService.ts`, `source/main/mithril/MithrilPartialSyncService.spec.ts`, `source/main/utils/chainStorageManager.ts`, `source/main/utils/chainStorageManagerLayout.ts`, `source/main/utils/chainStorageManager.spec.ts`, `source/main/utils/handleDiskSpace.ts`, `source/main/utils/handleDiskSpace.spec.ts`, optional narrow bootstrap converter/install seam files, optional narrow `mithrilPartialSyncChannel` helper/spec changes only if needed for the startup seam, research note `research/10-task-203-conversion-and-cutover-notes.md`, and `.agent/system/architecture.md` if the durable startup-recovery path lands as planned.
Verification intent: targeted Jest must now cover not only conversion and allowlist cutover, but also interruption semantics:
- persisted marker transitions before live mutation and after validated staged install
- startup detection of interrupted Boundary B/C1 states
- blocked normal node start on interrupted unsafe installs
- minimal wipe-and-full-sync startup-owned recovery exposure
- explicit non-emission of `completed`/`starting-node` unless those semantics are genuinely implemented
Key risks/open questions: the largest remaining risk is under-building the persisted marker/startup gate and accidentally letting interrupted unsafe installs resume as normal startup; `installSnapshot()` is still too broad to reuse as-is; legacy managed-entry constants remain the wrong truth for partial-sync cutover because they include `volatile` and omit `lsm`/`protocolMagicId`; if automatic node restart still cannot be made truthful after cutover, `task-203` should stop at a narrow post-cutover boundary rather than claim completion.
Docs/tracking outcome: the revised plan now explicitly calls for a task-specific research note covering the persisted marker/startup blocking rule and for updating `.agent/system/architecture.md` if the runtime startup-recovery path is implemented, because that becomes durable system behavior rather than an internal helper detail.
Outcome: blocker resolved with a single plan-fix pass. The canonical task plan now matches the PRD’s Boundary B/C1 interruption requirements without de-scoping away live cutover, and review-log paths remain `.agent/plans/mithril-partial-sync/task-plans/task-203-plan-review.md` and `.agent/plans/mithril-partial-sync/task-plans/task-203-impl-review.md`.

