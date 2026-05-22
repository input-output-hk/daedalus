Planner: Iteration 1
Timestamp: 2026-05-21T21:04:28Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`
Action: Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-401.md`.

Summary:
- Read the required planning inputs in the requested order: `.agent/readme.md`, `.agent/system/architecture.md`, the Mithril partial-sync PRD, tasks graph, `test.md`, `update-doc.md`, relevant partial-sync research, relevant historical Mithril research, and checked for existing `task-401` artifacts.
- Loaded `understand` for repository-understanding guidance, then verified all important claims against live files before writing the plan.
- Confirmed there were no existing `task-401` canonical plan, planning review log, implementation review log, or manual-QA results doc.
- Classified the task truthfully as `manual_execution` because completion depends on human-run supported-network QA, platform/build selection, operator-owned environment setup, evidence capture, and a user rollout decision the agent cannot produce alone.
- Verified the live rollout and validation seams that matter for this task:
  - supported Mithril networks remain `mainnet`, `preprod`, and `preview` in `source/main/mithril/mithrilNetworkConfig.ts`,
  - the launcher rollout guard still exists as `LauncherConfig.mithrilPartialSyncEnabled` in `source/main/config.ts`,
  - `ChainStorageCoordinator` still blocks diagnostics-launched partial sync when that flag is not `true`,
  - diagnostics entry remains manual and confirmation-first,
  - app-shell overlay ownership and backend-owned recovery action rendering are still live in `App.tsx` and `MithrilPartialSyncStore.ts`.
- Wrote the plan to keep scope narrow and truthful: no new implementation work is assumed; the task is framed around the supported-network QA matrix, required user/manual checkpoints, required evidence back, rollout decision gates, rollback checklist, and the existing kill-switch posture.
- Included consulted docs/workflows/skills, research consulted, expected files to change, implementation approach, verification plan, risks, required user inputs, manual steps, expected evidence, and review-log paths.
- Performed a brief self-review for scope creep, interaction-mode correctness, stale workflow assumptions, missing docs, and plan consistency.

Artifacts:
- Created: `.agent/plans/mithril-partial-sync/task-plans/task-401.md`

Notes:
- The plan explicitly says documentation preparation can proceed before manual QA, but the rollout decision itself cannot be finalized truthfully until the user/operator supplies supported-network evidence.
- Required review-log paths were preserved in the canonical plan, but no review-log files were written directly.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-21T21:05:34Z

Blocking findings:
- `task-401.md:140` misstates the current interaction truth: it says the diagnostics CTA is visible only when the feature is intentionally enabled for QA, but the live renderer always renders the recommendation and `Mithril Partial Sync` button and only disables it while Mithril work is already active (`source/renderer/app/components/status/DaedalusDiagnostics.tsx:870-893`). The actual rollout guard is backend-only in `ChainStorageCoordinator`, which rejects diagnostics-launched `startPartialSync` and `restartNormalFromPartialSync` when `LauncherConfig.mithrilPartialSyncEnabled !== true` (`source/main/utils/chainStorageCoordinator.ts:223-305`, `:386-389`). As drafted, the plan skips an essential manual checkpoint: verifying guard-off behavior. Replace the UI-gating assumption with explicit QA for guard-on and guard-off states, including that rollback disables new start/restart-normal while preserving wipe/startup-owned recovery.
- The planned results artifact path collides with the existing research numbering scheme. The draft reserves `.agent/plans/mithril-partial-sync/research/03-manual-qa-results.md` in multiple places (`task-401.md:103`, `:118`, `:180`, `:228`), but `03` is already taken by `.agent/plans/mithril-partial-sync/research/03-task-100-shared-contract-notes.md`. That will create confusing tracking history for the exact task whose output is supposed to support release review. Rename this to the next available sequence or a task-specific filename before approval.
- The rollout decision bar is too loose on build representativeness. The interaction section leaves `packaged build, dev build, or both` as operator choice (`task-401.md:22-24`), but the release decision depends on launcher-config behavior and packaged Mithril artifact wiring across OSes, which the repo research explicitly treats as part of shipped behavior (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:13-18`). A dev-only run cannot truthfully justify “enabled by default.” Require at least one release-equivalent packaged-build pass per release OS in scope, or state that the only truthful outcomes without that evidence are `keep guarded` or `hold`.

Non-blocking observations:
- The matrix is broader than it needs to be. `Implementation Approach` step 3 reads like every major scenario must run for every network/storage/platform combination, which is likely larger than necessary for task-401. Since the live network-specific seam is mainly endpoint/key selection (`source/main/mithril/mithrilNetworkConfig.ts:13-38`), a narrower truthful matrix is: success path on all supported networks and both storage modes, with recovery-branch coverage concentrated on one representative supported network per release OS/storage mode unless QA finds network-specific divergence.
- The plan is otherwise honest about manual checkpoints. It explicitly marks `manual_execution`, lists operator-owned steps, and does not hide human dependencies inside an autonomous-looking plan.
- The rollback posture is directionally correct and matches the live task-204 contract: launcher guard disables new diagnostics entry and restart-normal, but not startup-owned unsafe recovery or wipe-and-full-sync (`.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md:20-24`).

Approval bar:
- Update the plan so guard-off behavior is a named QA scenario instead of assuming the CTA disappears when the launcher flag is off.
- Rename the manual-QA results artifact to a non-colliding research path and align all references.
- Tighten the rollout decision criteria so release-equivalent packaged-build evidence is required for each release OS in scope before “enabled by default” is allowed.
- Prefer a smaller, explicitly scoped QA matrix instead of implying a full Cartesian product unless that extra breadth is truly required.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-21T21:21:35Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`
Action: Revised the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-401.md` to address the critique with the smallest truthful plan.

Summary:
- Re-read the critique, then re-verified the blocking claims against live repo files before revising:
  - `DaedalusDiagnostics.tsx` still renders the Mithril partial-sync recommendation and button in the diagnostics UI rather than hiding it behind the launcher flag.
  - `ChainStorageCoordinator.ts` still owns the real rollout guard by rejecting diagnostics-launched `startPartialSync` and `restartNormalFromPartialSync` when `LauncherConfig.mithrilPartialSyncEnabled !== true`.
  - `MithrilPartialSyncStore.ts`, `App.tsx`, and the task-204/task-400 research notes still confirm backend-owned recovery-action truth and startup-owned recovery preservation.
  - packaged Mithril artifact wiring remains part of shipped behavior across Linux, macOS, and Windows per the spike note and the packaging files it cites.
- Replaced the stale UI-gating assumption with explicit manual-QA requirements for both launcher-guard states:
  - guard-on QA now verifies the normal diagnostics-launched confirmation and execution flow,
  - guard-off QA now verifies that the diagnostics CTA may still be present in the renderer, but backend start and `restart-normal` are rejected while `wipe-and-full-sync` and startup-owned unsafe-install recovery remain available where allowed.
- Renamed the planned manual-QA results artifact to the next non-colliding research path and aligned all references:
  - from `.agent/plans/mithril-partial-sync/research/03-manual-qa-results.md`
  - to `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`
- Tightened the rollout-decision bar so `enabled by default` is no longer a possible conclusion from dev-only evidence:
  - at least one release-equivalent packaged-build pass is now required for each release OS in scope before that recommendation can be made truthfully,
  - dev-build runs are now optional supplemental evidence only,
  - without packaged-build evidence per release OS, the only truthful outcomes are `keep guarded by default` or `hold for follow-up fixes`.
- Narrowed the QA matrix so the plan no longer implies a full Cartesian product:
  - success-path coverage is required across all supported networks and both storage modes,
  - recovery-branch coverage is concentrated on one representative supported network per release OS and storage mode unless QA finds network-specific divergence,
  - guard-off rollback verification is called out as its own targeted scenario instead of being spread across every combination.
- Kept the rest of the plan intentionally narrow:
  - `manual_execution` remains the correct interaction mode,
  - no new implementation work is assumed,
  - the task still ends in a documented rollout posture tied only to `LauncherConfig.mithrilPartialSyncEnabled` plus a release-review rollback checklist.

Artifacts:
- Updated: `.agent/plans/mithril-partial-sync/task-plans/task-401.md`

Notes:
- The revised plan now treats backend-only gating, non-colliding QA evidence storage, packaged-build release evidence, and matrix minimization as explicit acceptance requirements rather than informal guidance.
- No planning review log was written in this revision step.

Outcome: Plan revised and ready for re-review

