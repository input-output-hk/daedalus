# Task task-304: Add i18n, Theme, And Storybook Coverage

## Task

- Task ID: `task-304`
- Title: `Add i18n, theme, and Storybook coverage`

## Why This Task Now

- `task-303` is complete, so the renderer now has the real diagnostics CTA, confirmation step, and diagnostics-launched partial-sync overlay that this task needs to document and visually review.
- Live-file verification shows the remaining gap is mostly coverage, not new feature wiring:
  - runtime partial-sync copy already exists in diagnostics and overlay message files,
  - Mithril overlay theme tokens already exist in `createTheme` and generated theme outputs,
  - but Storybook still has no partial-sync overlay story coverage,
  - and the existing diagnostics story is stale against the current `DaedalusDiagnostics` props and partial-sync states.
- That makes `task-304` the next smallest truthful slice: add the missing review surface for copy and state coverage, keep i18n/theme edits only where a real gap is found, and avoid inventing more renderer abstraction.

## Interaction Mode

- `interactive_validation`
- Required user input before implementation: none.
- Required manual validation before completion: Storybook locale/theme validation for the refreshed diagnostics states and the new partial-sync overlay states.
- Required evidence back from the user during implementation: confirmation that the touched stories render correctly under `en-US` and `ja-JP` and across theme switching.
- Manual QA remains later `task-401` work.

## Scope

- Add Storybook coverage for the diagnostics-launched Mithril partial-sync UX that reviewers actually need to inspect.
- Refresh the stale diagnostics story so it matches the current `DaedalusDiagnostics` API and can demonstrate:
  - recommendation CTA state,
  - blocked CTA state,
  - confirmation-visible state.
- Keep diagnostics Storybook props truthful to the live component contract, especially action-shaped props such as `onRestartNode.trigger()`, so the story demonstrates the real interaction surface instead of a brittle plain-function approximation.
- Add a dedicated Storybook story for `MithrilPartialSyncOverlay` that covers at least:
  - active progress,
  - cancelled terminal state,
  - failed state with safe restart allowed,
  - failed state where wipe-and-full-sync is the only allowed recovery,
  - completed success state if the smallest story matrix can include it without widening helpers too far.
- Verify that all user-facing strings used by these states are sourced from the existing `react-intl` pipeline, and add only the minimum new messages if Storybook coverage exposes a real wording gap.
- Verify that the diagnostics and overlay states remain fully theme-driven across Daedalus themes, and add theme variables only if Storybook review exposes a real hard-coded or semantically missing visual state.
- Keep implementation aligned with backend-owned recovery-action truth already exposed by `allowedRecoveryActions`.

## Non-Goals

- Do not redesign the diagnostics UI, confirmation copy structure, or partial-sync overlay flow.
- Do not add new runtime states, IPC seams, store state, or backend contracts.
- Do not split the current diagnostics or Mithril overlay components into new abstractions just to serve Storybook.
- Do not add new theme tokens unless live Storybook coverage proves an existing token gap.
- Do not reopen bootstrap/partial-sync renderer integration solved in `task-303`.
- Do not write manual QA notes or rollout decisions; those remain later `task-401` work.
- Do not write the planning or implementation review-log files for this task.

## Dependencies

- Required completed dependency:
  - `task-303`
- Supporting completed prerequisites used directly by this plan:
  - `task-300`
  - `task-301`
  - `task-302`
  - `task-204`
  - `task-100`
- This task directly supports:
  - `task-400`
  - visual/manual review readiness before `task-401`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
- `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/frontend.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was loaded for the required repository-understanding pass, and all material claims below were verified against live files.
- `storybook-creation` skill guidance materially affects this plan because Storybook coverage is the primary output of the task and Daedalus still uses `storiesOf()` rather than CSF.
- `theme-management` skill guidance materially affects this plan because theme coverage must be validated truthfully, but the live repo already contains Mithril overlay theme tokens, so the plan intentionally treats new token creation as conditional rather than assumed.
- `i18n-messaging` skill guidance materially affects this plan because runtime diagnostics and overlay copy must remain polished in `en-US` and `ja-JP`, not just extracted into catalogs.

## Live Repo Findings Verified For Planning

- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` already localizes the recommendation CTA and confirmation copy with `defineMessages`; there is no remaining hard-coded partial-sync runtime copy in that component.
- `DaedalusDiagnostics` still expects a mixed prop surface that includes plain callbacks and action-shaped props. In particular, restart uses `this.props.onRestartNode.trigger()`, so Storybook must pass an object with a `trigger` function rather than a bare callback.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` already contains localized partial-sync overlay title, subtitle, completion, and recovery-action labels.
- `source/renderer/app/i18n/locales/en-US.json` and `ja-JP.json` already contain polished partial-sync runtime strings for diagnostics and overlay states; they are not currently seeded with `!!!` placeholders.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` is now the real diagnostics-launched overlay seam and already renders backend-owned recovery actions from `canRetry`, `canRestartNormally`, and `canWipeAndFullSync`.
- `source/renderer/app/App.tsx` mounts `MithrilPartialSyncOverlay` globally, so Storybook should cover the overlay directly rather than trying to recreate the full app shell.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss` already uses dedicated Mithril theme variables such as `--theme-mithril-overlay-backdrop-start`, `--theme-mithril-card-background`, `--theme-mithril-card-shadow`, and `--theme-mithril-card-text-color`.
- `source/renderer/app/themes/utils/createTheme.ts` already defines those Mithril overlay variables, and generated theme outputs already include them for all shipped themes.
- `source/renderer/app/components/status/DaedalusDiagnostics.scss` currently styles the CTA and confirmation view from existing `network-window` theme variables; no obvious hard-coded color gap is visible in the live file.
- Storybook already contains bootstrap-related stories and support fixtures under `storybook/stories/loading/mithril/` and `storybook/stories/loading/_support/`, so this task can extend the existing loading domain instead of creating a new Storybook domain.
- `storybook/stories/loading/mithril/index.ts` currently imports only bootstrap stories. There is no `MithrilPartialSyncOverlay` story registered today.
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` is stale against the current `DaedalusDiagnostics` prop surface. It still passes obsolete props and does not cover the new partial-sync CTA or confirmation states.
- Focused Jest coverage already exists for `DaedalusDiagnostics` and `MithrilPartialSyncOverlay`, so the smallest truthful Storybook plan is to mirror those already-proven states rather than inventing broader matrices.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-304.md`
- `storybook/main.ts`
- `storybook/stories/_support/environment.ts`
- `storybook/stories/index.ts`
- `storybook/stories/loading/index.ts`
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`
- `storybook/stories/loading/mithril/index.ts`
- `storybook/stories/nodes/status/Diagnostics.stories.tsx`
- possibly `storybook/stories/loading/_support/mithrilFixtures.ts` only if local inline overlay fixtures become clearly repetitive after the story file is drafted
- only if real gaps are discovered during implementation:
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
  - `translations/messages.json`
  - `source/renderer/app/i18n/locales/defaultMessages.json`
  - `source/renderer/app/i18n/locales/en-US.json`
  - `source/renderer/app/i18n/locales/ja-JP.json`
  - `source/renderer/app/themes/utils/createTheme.ts`
  - generated theme files under `source/renderer/app/themes/daedalus/*.ts`

## Implementation Approach

1. Refresh the diagnostics story instead of leaving a stale review surface.
   - Update `storybook/stories/nodes/status/Diagnostics.stories.tsx` to the current `DaedalusDiagnostics` props.
   - Keep the story focused on partial-sync-relevant diagnostics states rather than trying to reproduce the entire diagnostics matrix.
   - Pass truthful action-shaped props for live interaction contracts, especially `onRestartNode={{ trigger: action('onRestartNode.trigger') }}`, instead of simplifying them into plain callbacks that the component does not actually use.
   - Cover the smallest set that makes dangerous-action wording reviewable:
     - ready CTA state,
     - blocked CTA state,
     - confirmation-visible state.

2. Add one dedicated partial-sync overlay story file in the existing loading domain.
   - Create `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` using the repo’s `storiesOf()` pattern.
   - Reuse the existing loading overlay frame and Storybook decorators so the visual review surface matches existing Mithril loading stories.
   - Keep the story file close to the real component seam rather than routing through `App.tsx`.

3. Reuse existing fixtures before adding helpers.
   - Start with local inline partial-sync fixtures inside the new story files, because this task is coverage work and should not widen shared Storybook helpers by default.
   - Reuse existing global helpers only where they already fit cleanly, such as the loading overlay frame and common Storybook decorators.
   - Widen `storybook/stories/loading/_support/mithrilFixtures.ts` only if repeated partial-sync fixture setup becomes clearly worse than one tiny shared addition.
   - Do not touch `mithrilHarness.tsx` unless implementation proves a real repeated need that cannot stay local without more noise.

4. Mirror backend-owned recovery truth directly in Storybook states.
   - Model failed and cancelled stories through `canRetry`, `canRestartNormally`, and `canWipeAndFullSync` combinations instead of hand-wavy labels.
   - Ensure at least one story demonstrates a safe-restart path and one story demonstrates a wipe-only path.
   - Keep cancel and failure surfaces visually distinct so wording review is straightforward.

5. Treat i18n follow-through as validation-first, edit-second.
   - Use the existing diagnostics and overlay messages when they already cover the story states truthfully.
   - Add or revise messages only if Storybook coverage exposes a real wording gap, confusing button label, or inconsistent dangerous-action copy.
   - If any user-facing strings change, run the standard i18n pipeline and manually polish `en-US` and `ja-JP` outputs so no touched runtime strings ship with `!!!` placeholders.
   - Even if no runtime strings change, verify the touched stories render truthfully in both `en-US` and `ja-JP` so Storybook review is not implicitly English-only.

6. Treat theme follow-through the same way.
   - Validate the touched stories across Storybook theme switching using the existing Mithril and network-window variables.
   - Do not invent new theme variables for review-only coverage.
   - If Storybook reveals a real styling gap for a newly covered state, add the minimum semantic variable through `createTheme` and sync generated theme outputs using the theme workflow.

7. Keep registration and story organization conventional.
   - Register the new overlay story through `storybook/stories/loading/mithril/index.ts`.
   - Keep the story under the existing loading/mithril domain rather than adding a separate partial-sync Storybook branch.

## Acceptance Criteria

- Storybook includes a dedicated diagnostics-launched Mithril partial-sync overlay story.
- Storybook includes refreshed diagnostics coverage for the partial-sync CTA and confirmation boundary.
- The refreshed diagnostics story uses truthful props for action-shaped interactions, including the live `onRestartNode.trigger()` contract.
- Storybook coverage makes it easy to review at least:
  - confirmation copy,
  - active progress,
  - cancelled state,
  - failed state with safe restart allowed,
  - failed state where wipe-and-full-sync is required.
- All touched user-facing strings are localized through the standard `react-intl` pipeline.
- Any newly added or revised runtime strings land in `defaultMessages.json`, `en-US.json`, and `ja-JP.json` with polished locale values.
- Touched stories are verified in both `en-US` and `ja-JP`, not only under the default English locale.
- Theme coverage exists for the touched diagnostics and overlay states, using existing theme variables unless a real missing-token gap is discovered.
- If new theme variables are required, they are added through `createTheme` and propagated to generated theme outputs instead of being hard-coded in component styles.
- The implementation stays aligned with backend-owned recovery-action truth and does not add renderer-only state inference.

## Verification Plan

- Run `yarn storybook:build` to verify that the new and refreshed stories compile.
- Manually inspect the touched stories in Storybook and switch themes to confirm the diagnostics and overlay states remain theme-driven.
- Verify the touched stories in both `en-US` and `ja-JP` through the Storybook locale switcher so confirmation, progress, and recovery wording remains truthful outside the default locale.
- Verify directly that the Storybook stories cover:
  - ready diagnostics CTA,
  - blocked diagnostics CTA,
  - confirmation view,
  - active partial-sync progress,
  - cancelled terminal view,
  - restart-allowed failure,
  - wipe-only failure,
  - completed success state if included.
- Verify directly that the refreshed diagnostics story does not break on interaction because action-shaped props are wired truthfully, especially the `onRestartNode.trigger()` seam.
- If user-facing strings change, run `yarn i18n:manage` and confirm touched runtime locale values in `en-US.json` and `ja-JP.json` are polished.
- If theme files change, run `yarn themes:check:createTheme`.
- If implementation touches any renderer runtime copy or props in a way that could invalidate existing focused coverage, run the relevant focused Jest specs, most likely:
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

## Risks And Open Questions

- The main scope-creep risk is turning Storybook coverage work into a broader refactor of diagnostics or Mithril loading components.
- A second risk is overreacting to the word “theme” in the task title and adding new theme tokens even though the live repo already has semantic Mithril overlay tokens.
- A third risk is leaving the stale diagnostics story in place and adding only overlay stories, which would still leave the CTA and confirmation review surface inaccurate.
- A fourth risk is creating Storybook states that do not match backend-owned recovery-action combinations and therefore mislead review.
- A fifth risk is using simplified Storybook callbacks for live action-shaped props, which would make the diagnostics story compile but misrepresent how the real component is wired.
- Open implementation choice to keep honest during coding:
  - if the diagnostics story can express confirmation cleanly with direct props and minimal local state triggering, prefer that;
  - keep partial-sync fixture data inline in the story files first;
  - only add a tiny shared fixture helper if repeated interaction wiring would otherwise obscure the states being reviewed.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-304.md`.
- Preserve the expected review-log paths, but do not write the review-log files during planning.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- No `.agent/system/architecture.md` update is expected unless implementation unexpectedly adds a durable new Storybook or theme-management seam beyond the minimal coverage work planned here.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-304-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-304-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Completed with approved implementation review and validated manual Storybook checks.
- Refreshed diagnostics Storybook coverage to the live partial-sync CTA and confirmation seams.
- Added a dedicated `MithrilPartialSyncOverlay` Storybook story with active, cancelled, restart-allowed failure, wipe-only failure, and completed states.
- Kept existing loading-story registration intact while adding the new partial-sync coverage.
- Added minimal Storybook-only compatibility fixes required for truthful validation:
  - browser-safe environment stub instead of importing `source/main/environment`
  - Storybook webpack replacements/stubs for browser-incompatible hardware-wallet transport modules
  - earlier top-level registration of `Nodes` and `Loading` so Storybook does not hide the new review surface behind unrelated import-chain failures
- Fixed one locale validation gap by generating partial-sync active-progress labels from the live `react-intl` context at render time.
- No runtime application code, message catalogs, or theme token files required product changes for this task.

## Final Verification

- Automated:
  - `yarn storybook:build`
  - `yarn test:jest source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- Manual:
  - User validated the refreshed diagnostics stories and partial-sync overlay stories across locale/theme switching.
  - One intermediate Japanese localization issue in `Active Progress` was corrected and revalidated.

## Review Outcome

- Planning review log: approved after `Planner: Iteration 2`
- Implementation review log: approved after `Code Review: Iteration 4`

## Self-Review

- Scope-creep check: the plan keeps this task on coverage and review surfaces, not new renderer behavior, backend work, or speculative theme abstraction.
- Workflow freshness check: Storybook, theme, i18n, frontend, test, and doc-update guidance were used only where they materially change the plan.
- Missing tests/docs check: the plan calls out Storybook build verification, conditional i18n/theme commands, review-log paths, and the canonical task-plan path.
- Consistency check: the dependency typo is corrected, files-expected-to-change now match the smaller inline-fixture-first path, and the plan now explicitly covers truthful action-shaped props plus `en-US` and `ja-JP` Storybook verification.
