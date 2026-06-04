Planner: Iteration 1
Timestamp: 2026-05-19T18:13:31Z

Summary:
- Reviewed required docs, workflows, PRD, task graph, Mithril partial-sync research, and relevant historical Mithril contract/status notes.
- Used `understand` first for repo understanding, then verified key findings against live files including `source/common/types/mithril-bootstrap.types.ts`, `source/common/ipc/api.ts`, `source/main/ipc/mithrilBootstrapChannel.ts`, `source/renderer/app/stores/MithrilBootstrapStore.ts`, and chain-storage imports.
- Updated `.agent/plans/mithril-partial-sync/task-plans/task-100.md` with an autonomous, minimal plan centered on dedicated shared partial-sync contracts.

Plan:
- Add `source/common/types/mithril-partial-sync.types.ts` for partial-sync-specific statuses, status updates, failure actions, and error contracts.
- Keep `MithrilBootstrapStatus` and `MithrilBootstrapStatusUpdate` bootstrap-specific instead of broadening them for diagnostics-launched partial sync.
- Reuse only genuinely generic Mithril progress shapes if needed and avoid opportunistic relocation of `ChainStorageConfig` or `ChainStorageValidation`.
- Encode backend-owned recovery actions (`retry`, `restart-normal`, `wipe-and-full-sync`) in the shared contract layer so later renderer work does not infer safety heuristically.

Self-Review:
- Scope kept limited to shared contracts and task-doc planning.
- Workflow guidance and locked PRD decisions remain consistent.
- No missing manifest, test, or documentation work was identified for the planning step beyond the required task docs.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-19T18:16:12Z

Summary:
- Reviewed the required docs, PRD, task graph, task plan, prior review, partial-sync research, and relevant historical Mithril notes.
- Used `understand` first, then verified key assumptions against live files including `source/common/types/mithril-bootstrap.types.ts`, `source/common/ipc/api.ts`, `source/main/ipc/mithrilBootstrapChannel.ts`, `source/main/mithril/mithrilErrors.ts`, `source/renderer/app/stores/MithrilBootstrapStore.ts`, and `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`.
- The plan is close, but it still leaves one safety-sensitive contract ambiguous and carries one avoidable scope expansion from PRD wording into shared types.

Blocking findings:
- `allowedRecoveryActions` is still underspecified in a way that can produce stale or unsafe renderer actions. `task-100.md:111-117` says to add the field “on the terminal or status payload,” but the live store/update pattern is merge-based and optional-field-driven (`source/renderer/app/stores/MithrilBootstrapStore.ts:173-205`). If partial-sync updates omit that field on later statuses, old actions can persist in store state and the renderer can keep showing recovery affordances that are no longer safe. The plan needs one exact contract now: either emit `allowedRecoveryActions` on every update with `[]` when none are allowed, or define explicit clearing semantics that later stores/IPC handlers must implement.
- The proposed shared status union includes `confirming` (`task-100.md:94-109`), but the current task graph says the confirmation modal is renderer-owned and backend work starts only after user confirmation (`task-302` and PRD confirmation flow). That makes `confirming` an avoidable cross-process state that blurs renderer-local UI state with main-owned long-running operation state. The simpler and safer contract is to keep confirmation local to the renderer/store and start shared status at the first backend-owned lifecycle step.
- The plan makes shared helper predicates optional (`task-100.md:124-126`), but live code already relies on shared status-group helpers for real behavior, not just convenience: node-start suppression uses `isMithrilBootstrapBlockingNodeStart` in `source/main/ipc/mithrilBootstrapChannel.ts:26-27,107-108`, startup logic uses the same bootstrap helpers in `handleDiskSpace.ts`, and the reused progress UI depends on `isMithrilBootstrapRestoreCompleteStatus` in `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx:12,109-115`. If task-100 does not add the minimal partial-sync equivalents now, task-103 and task-303 will almost certainly recreate those status groupings ad hoc, which is exactly the contract drift this task is supposed to prevent.

Non-blocking observations:
- The dedicated `source/common/types/mithril-partial-sync.types.ts` seam is the right simplification. Keeping bootstrap unions bootstrap-specific matches the live import surface in `source/common/ipc/api.ts:74-80` and avoids forcing existing bootstrap consumers to accept partial-sync-only states.
- The plan is right to avoid moving `ChainStorageConfig` and `ChainStorageValidation`; those types are still imported broadly from `mithril-bootstrap.types.ts`, including `source/main/utils/chainStorageCoordinator.ts:3-8`.
- The task doc lists `task-004` as a required dependency, but the task graph still declares only `task-002` for `task-100`. That mismatch is stale planning metadata, not a design blocker, because `task-004` is already complete.
- Verification is too vague for a contract task. “TypeScript or Jest verification” (`task-100.md:155-161`) should be replaced with at least one named command so later reviewers can tell whether shared-type consumers were actually exercised.

Approval bar:
- Define one exact `allowedRecoveryActions` field contract, including clearing behavior across non-terminal updates.
- Remove `confirming` from the shared backend status union unless a real cross-process need is identified and justified.
- Add the minimal shared partial-sync helper predicates needed for node-start suppression and reused progress/error UI, or explicitly narrow later tasks so they do not rely on bootstrap-style helper reuse.
- Tighten verification to at least one concrete compile/test command that covers existing shared-type consumers.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-19T18:16:59Z

Summary:
- Revised `.agent/plans/mithril-partial-sync/task-plans/task-100.md` against the full blocker list from critique.
- Tightened the contract to keep confirmation renderer-local, require explicit `allowedRecoveryActions` clearing on every status update, and add only the minimal shared helper predicates needed by likely downstream consumers.
- Replaced vague verification wording with a concrete baseline compile command.

Plan Revisions:
- Removed `confirming` from the shared backend status union so the contract starts at backend-owned lifecycle steps only.
- Specified `allowedRecoveryActions` as a required field on every `MithrilPartialSyncStatusUpdate`, with `[]` emitted whenever no recovery action is safe.
- Elevated minimal helper predicates into scope: `isMithrilPartialSyncTerminalStatus`, `isMithrilPartialSyncBlockingNodeStart`, and `isMithrilPartialSyncRestoreCompleteStatus` if progress/terminal UI reuse needs the shared grouping.
- Corrected dependency wording to reflect `task-002` as the required graph dependency while still noting `task-004` as an already-completed design input.
- Tightened verification to `yarn compile`, plus targeted Jest coverage only if helper additions introduce unit-testable logic in this task.

Self-Review:
- Revisions narrowed scope rather than broadening it.
- The updated plan now aligns with merge-based store update behavior and existing shared helper usage patterns.
- No further plan inconsistencies or hidden manual checkpoints remain.

Outcome: Plan revised after critique and approved for implementation

