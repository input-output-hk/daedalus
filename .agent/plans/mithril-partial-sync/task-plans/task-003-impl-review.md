Implementation: Iteration 1
Timestamp: 2026-05-19T17:21:51Z

Changes made:
- Updated the partial-sync PRD to make backend-only immutable range derivation explicit: local immutable position now comes from the highest parseable immutable file number in the resolved managed chain `immutable/` directory, renderer sync data remains context only, `ManagedChainLayoutResult.isRecoveryFallback` is a fail-closed blocker, and latest-snapshot drift now has an explicit bounded handling rule.
- Updated `task-003` in the task graph so it explicitly depends on completed `task-002`, tightened its implementation notes and acceptance criteria around concrete preflight checks, and recorded completion notes that capture the final design outcome.
- Appended a durable research addendum to the validation-spike results note so later implementation inherits the new range-derivation and fallback-blocking constraints alongside the earlier staged-only restore decision.

Files touched:
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-003.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-003-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-003-impl-review.md`

Verification run:
- Re-read the updated PRD, task graph, research note, and canonical task plan against live chain-storage seams in `chainStorageManagerLayout.ts`, `chainStoragePathResolver.ts`, and `chainStorageCoordinator.ts`.
- Verified that the final wording stays docs/spec-first, uses concrete repo-validatable checks only, and propagates the `isRecoveryFallback` fail-closed rule plus latest-snapshot-drift handling across the required planning artifacts.
- No automated build or test run was required because this task completed as planning/documentation hardening only.

Deviations from approved plan:
- None. The task remained docs/spec-first with no product-code changes.

User interaction required:
- No

Outcome:
- Completed the agent-executable work for `task-003` by making immutable range derivation, concrete preflight blockers, dependency ordering, and fallback/drift rules explicit in the canonical task artifacts before backend implementation begins.

Implementation: Iteration 2
Timestamp: 2026-05-19T17:23:23Z

Changes made:
- Corrected task-graph bookkeeping drift found in code review: restored `task-002` to its proper dependency set, marked `task-003` as completed, and made `task-003` explicitly depend on both `task-001` and `task-002` so the graph now matches the canonical task plan and completion notes.

Files touched:
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/task-plans/task-003-impl-review.md`

Verification run:
- Re-read the `task-002` and `task-003` entries in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` to confirm the dependency DAG and completion state now match the canonical task plan and review outcome.
- Verified that `task-002` no longer has a self-dependency and `task-003` now records both its explicit safety-gate dependency on `task-002` and its completed status.

Deviations from approved plan:
- None. This was a tracker-alignment fix required by review.

User interaction required:
- No

Outcome:
- Resolved the remaining review blockers by making the task graph internally consistent with the approved `task-003` plan and completion state.

Code Review: Iteration 2
Timestamp: 2026-05-19T17:24:16Z

Implementation iteration 2 resolves the prior blockers. The task graph now correctly records `task-003` as completed with explicit dependencies on both `task-001` and `task-002` (`.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json:89-115`), matching the canonical task plan’s safety-gate ordering (`.agent/plans/mithril-partial-sync/task-plans/task-003.md:31-39`). The documentation set also now consistently carries the concrete preflight rules and fail-closed fallback rule that were previously missing: `ManagedChainLayoutResult.isRecoveryFallback` is explicitly treated as a blocker in the canonical task plan (`.agent/plans/mithril-partial-sync/task-plans/task-003.md:81-103`), the PRD (`.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md:167-178,255`), and the research addendum (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:318-326`). Those requirements are grounded in the live repo seams, where `ensureManagedChainLayout()` can return a recovery fallback after broken custom-storage recovery (`source/main/utils/chainStorageManagerLayout.ts:42-50,90-99`) and `chainStorageCoordinator` simply propagates that result today (`source/main/utils/chainStorageCoordinator.ts:140-143,212-219`). I did not find any new blocker introduced by the iteration-2 bookkeeping fix.
Blocking findings:
- None.
Non-blocking observations:
- `source/main/utils/chainStoragePathResolver.ts:124-125` still returns the managed chain path as the Mithril work dir today, so later implementation should stay careful to keep staging work separate from the live managed target as the PRD now requires.
- `source/main/utils/chainStorageManagerLayout.ts:61-64` still treats `inconsistent` layout as adoptable for current storage management purposes; the new docs correctly avoid using that as a broader corruption heuristic and instead limit partial-sync blockers to the concrete checks now listed.
Approval bar:
- Keep later product-code work aligned with the now-locked docs contract: fail closed on `isRecoveryFallback`, derive local immutable position only from parseable filenames in the resolved managed `immutable/` directory, and handle latest-snapshot drift by re-resolving `latest` immediately before command issuance or surfacing a bounded retriable preflight failure.
Decision: approved

