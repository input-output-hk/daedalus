# Task task-401: Run Supported-Network Manual QA And Finalize Rollout Decision

## Task

- Task ID: `task-401`
- Title: `Run supported-network manual QA and finalize rollout decision`

## Why This Task Now

- `task-400` is complete, so the backend, IPC, renderer, Storybook, and focused automated Jest coverage for Mithril partial sync are in place.
- The remaining truth gap is no longer implementation. It is supported-network, human-run validation and release judgment for a feature that mutates live chain storage.
- The smallest truthful plan is therefore to keep this task focused on:
  - collecting manual QA evidence on supported networks and chain-storage modes,
  - confirming the existing launcher kill-switch posture against those results,
  - documenting the final rollout or hold decision and rollback checklist.

## Interaction Mode

- `manual_execution`
- Required user input before completion:
- the platform matrix to run now, at minimum across the release-relevant supported OS set,
- the release-equivalent packaged builds to validate for each release OS in scope, with any optional dev-build runs treated as supplemental evidence only,
- the launcher configuration used for the run, including whether `mithrilPartialSyncEnabled` is set to `true` for manual validation,
- the state directories or test environments to use for default and custom chain-storage runs.
- Required manual steps from the user or operator:
  - execute the supported-network QA passes on real or release-representative environments,
  - collect screenshots, logs, and observed recovery behavior for each pass,
  - report pass/fail results and rollout recommendation evidence back into the repo-local task artifacts.
- Evidence expected back from the user before the task can be closed:
- per-run environment details: OS, build type, commit/build identifier, network, chain-storage mode, launcher guard setting,
- success/failure result for each required scenario,
- paths or attachments for relevant logs, especially Mithril partial-sync logs and any startup-recovery evidence,
- explicit recommendation on whether the feature should remain kill-switched by default or be enabled for rollout, with `enabled by default` only allowed when release-equivalent packaged-build evidence exists for every release OS in scope.
- Can implementation proceed before that interaction:
  - no additional product implementation is required for this task,
  - documentation preparation can proceed before manual QA,
  - the rollout decision itself cannot be finalized truthfully until the user/operator supplies manual QA evidence.

## Scope

- Prepare the canonical manual-QA and rollout-decision plan for Mithril partial sync.
- Define the minimum supported-network QA matrix required to make a truthful rollout decision.
- Record the exact user-run scenarios, expected evidence, and pass/fail decision gates.
- Tie the rollout decision to the existing launcher kill switch `LauncherConfig.mithrilPartialSyncEnabled` rather than inventing a new rollout mechanism.
- Finalize the rollback checklist release reviewers need if QA reveals safety issues.

## Non-Goals

- Do not expand this task into new backend, IPC, renderer, or Storybook implementation unless manual QA reveals a concrete defect and a follow-up task is created.
- Do not claim feature enablement based only on automated tests, Storybook review, or static inspection.
- Do not rewrite the full Mithril partial-sync PRD or earlier task plans beyond any small tracking updates required after QA concludes.
- Do not write the planning or implementation review logs during planning.

## Dependencies

- Required completed dependency:
  - `task-400`
- Supporting completed work used directly by this plan:
  - `task-004`
  - `task-100`
  - `task-101`
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-201`
  - `task-202`
  - `task-203`
  - `task-204`
  - `task-300`
  - `task-301`
  - `task-302`
  - `task-303`
  - `task-304`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- `.agent/plans/mithril-partial-sync/research/17-task-400-automated-test-coverage-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was loaded for repository-understanding guidance, and all material claims below were verified against live files before writing the plan.

## Existing Task-401 Artifacts

- No existing `task-401` canonical plan, planning review log, implementation review log, or manual-QA results doc was present under `.agent/plans/mithril-partial-sync/` at planning time.

## Current Task Artifacts

- Canonical task plan: `.agent/plans/mithril-partial-sync/task-plans/task-401.md`
- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-401-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-401-impl-review.md`
- Expected manual-QA results doc: `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`

## Live Repo Findings Verified For Planning

- Supported Mithril network configuration is still explicitly limited to `mainnet`, `preprod`, and `preview` in `source/main/mithril/mithrilNetworkConfig.ts`, matching the PRD and tasks graph.
- The rollout guard still exists live as `LauncherConfig.mithrilPartialSyncEnabled` in `source/main/config.ts`, and `ChainStorageCoordinator` still blocks diagnostics-launched partial sync start when that flag is not `true` in `source/main/utils/chainStorageCoordinator.ts`.
- The diagnostics entry point remains manual and confirmation-first in `source/renderer/app/components/status/DaedalusDiagnostics.tsx`; the user must review confirmation copy before start.
- Global overlay ownership still lives in `source/renderer/app/App.tsx`, which mounts `MithrilPartialSyncOverlay` from `mithrilPartialSync.shouldShowOverlay` and forwards cancel, retry, restart-normal, wipe-and-full-sync, and completed-dismiss callbacks from the store.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` still derives UI recovery affordances from backend-owned `allowedRecoveryActions` rather than renderer-side safety inference.
- `source/main/mithril/MithrilPartialSyncService.ts` still exposes boundary-dependent recovery behavior, including wipe-only paths after unsafe boundaries and retry/restart-normal only where explicitly allowed.
- `task-400` closed the planned automated-coverage gaps, but that evidence remains code-level only; supported-network, platform, and live-environment validation is still outstanding.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-401.md`
- `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- possibly `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` only if the final rollout posture or rollback wording needs a factual post-QA update

## Implementation Approach

1. Treat this as a manual-validation and release-decision task, not an implementation task.
   - Keep the existing code as the subject under test.
   - Prepare the repo-local checklist and evidence expectations before asking for any rollout conclusion.

2. Run the minimum truthful QA matrix across supported networks and storage modes.
   - Networks:
     - `mainnet`
     - `preprod`
     - `preview`
   - Chain-storage modes:
     - default managed chain storage
     - custom managed chain storage
   - Platforms:
     - release-relevant supported desktop OSes the user intends to ship from this branch.
   - Build evidence required for rollout enablement:
     - at least one release-equivalent packaged-build pass per release OS in scope,
     - optional dev-build runs may supplement diagnosis but cannot by themselves justify `enabled by default`.

3. Cover the required user-visible and safety-sensitive scenarios without implying a full Cartesian product.
   - Run the success path across all supported networks and both storage modes.
   - Concentrate recovery-path coverage on one representative supported network per release OS and storage mode unless QA finds network-specific divergence.
   - Verify both launcher-guard states explicitly.
     - Guard on: diagnostics-launched confirmation and execution flow works normally.
     - Guard off: the renderer may still surface the diagnostics CTA, but backend start and `restart-normal` are rejected while wipe/startup-owned recovery remains available where allowed.
   - Confirmation step appears before the node is stopped.
   - Success path: start partial sync, observe progress, hand off back to normal sync, verify no misleading recovery actions remain.
   - Boundary A cancellation path: cancel before cutover, verify cleanup, verify restart-normal remains available if expected.
   - Retry path from a backend-approved terminal failure where `retry` is exposed.
   - Restart-normal path from a backend-approved terminal failure where `restart-normal` is exposed.
   - Wipe-and-full-sync path from a wipe-only terminal state or startup-owned unsafe recovery state.
   - Startup recovery behavior after an intentionally interrupted unsafe boundary if the user/operator can exercise that safely in the QA environment.

4. Require evidence capture for every run.
   - Record environment metadata, scenario, expected result, actual result, and whether the result is a blocker.
   - Capture log locations, especially `mithril-partial-sync` logs and any startup recovery evidence.
   - Capture screenshots or short notes for diagnostics confirmation, overlay progress, terminal recovery state, and post-success return to normal sync.

5. Keep rollout judgment tied to the existing kill switch.
   - If supported-network QA is incomplete or produces unresolved safety concerns, keep `mithrilPartialSyncEnabled` disabled by default.
   - If supported-network QA is complete and clean across the intended release matrix, and release-equivalent packaged-build evidence exists for every release OS in scope, document whether enabling by default is now justified or whether guarded rollout is still preferred.
   - If packaged-build evidence is missing for any release OS in scope, the only truthful outcomes are `keep guarded by default` or `hold for follow-up fixes`.
   - Do not invent a second rollout lever beyond the existing launcher-controlled guard unless QA reveals a concrete missing operational need.

6. Finalize the rollback checklist from the observed QA reality.
   - Release reviewers should be able to answer one question quickly: if field issues appear, can we disable new diagnostics-launched partial sync without stranding already-unsafe installs?
   - Confirm the checklist still matches the task-004/task-204 contract: disable new entry through the launcher guard, preserve startup-owned unsafe-install recovery, and leave empty-chain bootstrap unaffected.

## Required User / Manual Checkpoints

- Checkpoint 1: QA environment confirmation.
  - User must confirm which OS/build combinations are in scope for this rollout decision.
  - User must confirm the launcher configuration used for both guard-on and guard-off validation, especially `mithrilPartialSyncEnabled`.
- Checkpoint 2: Supported-network run execution.
  - User or operator must execute the manual runs because the agent cannot truthfully perform supported-network, operator-owned app validation in this environment.
- Checkpoint 3: Evidence handoff.
  - User must provide the recorded results, logs, and any screenshots/notes needed to distinguish pass, fail, or blocked outcomes.
- Checkpoint 4: Rollout decision.
  - User must confirm whether release posture should be:
    - enabled by default,
    - guarded by kill switch by default,
    - or held for follow-up fixes.

## Acceptance Criteria

- Manual QA results are captured in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md` with enough detail to support release review.
- The captured results cover `mainnet`, `preprod`, and `preview` across both default and custom chain-storage targets, or clearly explain any blocked combinations.
- The captured results include release-relevant platform coverage or an explicit statement of the remaining platform gap.
- The recorded evidence covers the major behavior paths named in the tasks graph:
  - success
  - cancellation
  - retry
  - restart-normal
  - wipe-and-full-sync
- The recorded evidence explicitly covers both launcher-guard states and shows that guard-off blocks new diagnostics-launched start/restart-normal while preserving the intended wipe/startup-owned recovery posture.
- Any recommendation to enable by default is backed by release-equivalent packaged-build evidence for every release OS in scope.
- The final plan outcome states a clear rollout posture for `mithrilPartialSyncEnabled`.
- The final plan outcome states the rollback checklist release reviewers should follow if post-release safety concerns appear.
- No rollout enablement claim is made without human-supplied manual QA evidence.

## Verification Plan

- Before manual QA starts, verify the implementation evidence baseline is still the current repo truth:
  - `task-400` automated coverage notes
  - live rollout guard in `source/main/config.ts` and `source/main/utils/chainStorageCoordinator.ts`
  - live manual diagnostics entry and overlay ownership in renderer files.
- During manual QA, require the user/operator to record for each run:
  - OS/platform
  - build type and identifier
  - network
  - chain-storage mode
  - launcher guard setting
  - scenario executed
  - expected result
  - actual result
  - logs/evidence paths
- After manual QA, verify the repo-local results document supports one of only three truthful conclusions:
  - enable by default,
  - keep guarded by default,
  - hold for follow-up fixes.
- If `enable by default` is proposed, verify that release-equivalent packaged-build evidence exists for every release OS in scope.
- If any code changes are required as a direct outcome of QA, create or update a follow-up task instead of silently widening `task-401` scope.

## Risks And Open Questions

- The main risk is pretending automated tests are enough for a live-chain-data rollout decision; they are not.
- Another risk is incomplete matrix coverage, especially if one supported network, one chain-storage mode, or one release-relevant platform is skipped without being documented as blocked.
- A third risk is conflating a technically functional happy path with a safe release posture; rollout must also consider recovery behavior, logs, and rollback confidence.
- Open question for execution:
  - which platform/build combinations does the user want this rollout decision to cover now?
  - this must be answered by the user before the QA checklist can be treated as complete.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-401.md`.
- Preserve the expected review-log paths, but do not write the review-log files during planning.
- During execution, create `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md` with the full supported-network QA record and rollout recommendation evidence.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after manual QA evidence is captured and the rollout decision is approved.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` only if the final rollout posture or rollback wording needs a factual post-QA status update.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-401-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-401-impl-review.md`

## Planning Self-Review

- Scope creep check: kept this plan on manual QA, evidence capture, and rollout judgment instead of reopening implementation.
- Interaction-mode check: `manual_execution` is required because truthful completion depends on user/operator-run supported-network validation and release judgment.
- Stale workflow text check: verification references current automated evidence as a baseline, not as a substitute for manual QA.
- Missing docs/tests check: called out the expected manual-QA results document, task tracking update, and possible PRD status follow-through.
- Plan consistency check: required user checkpoints, acceptance, rollout decision gates, and rollback posture all align with the live launcher guard and earlier task-004/task-204 safety contracts.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Manual QA evidence was captured for the latest fixed `preview` / Linux / `yarn dev` flow, including an end-to-end passing run after the live QA fixes discovered during task execution.
- Prior guard-off validation performed earlier in the same QA effort is accepted for this task checkpoint by explicit user direction.
- By explicit user direction, broader supported-network matrix coverage, custom chain-storage coverage, and additional recovery-path re-validation are deferred to later manual QA rather than blocking this task closeout.
- Rollout posture for this task checkpoint is recorded as `enabled by default` by explicit user decision.
