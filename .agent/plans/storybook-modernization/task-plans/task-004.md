# Task task-004: Remove the /staking/epochs screen

## Task ID and Title

- ID: `task-004`
- Title: `Remove the /staking/epochs screen`

## Why Chosen Now

`task-004.dependencies` is `[task-001]` and `task-001` is complete. It is the last of the three
phase 1 deletions and the only one that removes a route, a container and a screen rather than only
stories.

It also has to land before `task-021` converts the staking tranche and before `task-010` replaces
the barrel, because every hour it stays is an hour of conversion work spent on a screen that is
about to go.

## Interaction Mode

- Mode: `agent_execution`

Every edit and every check reproduces here, including the i18n regeneration, by the route proved
during `task-003`. The file removals are performed by the operator against the list this task
produces.

## Scope

- Remove the `/staking/epochs` route binding and its path constant.
- Remove the container, the five components, the stylesheet and the helpers under
  `source/renderer/app/components/staking/epochs/`.
- Remove the commented navigation item and the message it was the only reader of.
- Remove the two dummy JSON fixtures the container and the story read.
- Remove the Storybook support module and its registration.
- Regenerate the three tracked i18n artifacts that carry the twelve message ids.

## Non-Goals

- No change to `components/staking/delegation-center/DelegationCenterHeader.tsx` or to
  `DelegationCenter.tsx`, which are what keep epoch information in front of users.
- No change to the `IS_STAKING_INFO_PAGE_AVAILABLE` binding at `Routes.tsx:200-206`, which sits
  immediately below the one being removed and belongs to the staking Info screen.
- No change to `ROUTES.STAKING.PAGE`, `ROUTES.STAKING.ROOT` or any other staking route.
- No change to `isEpochsInfoAvailable`, a `DelegationCenter` prop that shares a word with this
  screen and has nothing to do with it. It appears four times in `Staking.stories.tsx` and stays.
- No translation work. All twelve ids carry real translations in both locales, English and Japanese,
  and all twelve go. This is a correction to what this section said before the regeneration was run;
  the original claim that they were untranslated was carried over from `task-003`, where the six
  `staking.chart.tooltip.*` ids genuinely did carry the `!!!` prefix in both. See the implementation
  review. Nothing is recoverable-only-in-principle: the strings are in git history and come back
  with a revert.
- No hand edit to any i18n artifact.

## Dependencies

- `task-001`, complete. `task-002` and `task-003` are also complete, so the staking story file and
  the sidebar arithmetic are both at their post-deletion state.
- No task depends on `task-004`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 14 at
  `:283-295`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-004`
- `.agent/plans/storybook-modernization/research/05-reachable-screens.md`, the excluded-route list
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact,
  `task-002.md` and `task-003.md` for the current corpus state, and `task-005.md` for the
  `Routes.tsx` overlap it recorded
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
  - `.agent/system/architecture.md` for the container, route and store layering
- Workflows:
  - `.agent/workflows/storybook.md`, read for the barrel model only, per the task-plans readme's
    caution.
- Skills:
  - `.agent/skills/i18n-messaging/SKILL.md` for the id convention and the `!!!` prefix, both of which
    the twelve removed messages follow.
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `d036a9515`, 2026-09-14, against the working tree.

- The route binding is at `source/renderer/app/Routes.tsx:195-199`, not the `:196-200` the task
  entry gives. `task-005` removed six lines from this file after the entry was written. The import
  is still at `:20` as stated.
- `ROUTES.STAKING.EPOCHS` is at `source/renderer/app/routes-config.ts:12`, not `:13`. `task-005`
  removed `REDEEM_ITN_REWARDS` from `:4`, moving every later line up one. `Routes.tsx:197` is its
  only reader.
- `StakingEpochsPage` has exactly one importer, `Routes.tsx:20`.
- `source/renderer/app/components/staking/epochs/` holds 7 tracked files: `StakingEpochs.tsx`,
  `StakingEpochsCurrentEpochData.tsx`, `StakingEpochsDataTable.tsx`, `StakingEpochsNoData.tsx`,
  `StakingEpochsPreviousEpochData.tsx`, `StakingEpochs.scss` and `helpers.ts`. An eighth file on
  disk, `StakingEpochs.scss.d.ts`, is gitignored generator output, as `task-003` established for the
  legacy directory.
- Only `StakingEpochs` is imported from outside that directory, by two files:
  `containers/staking/StakingEpochsPage.tsx:3` and
  `storybook/stories/staking/_support/Epochs.tsx:5`. Both go.
- The screen is unreachable. `components/staking/navigation/StakingNavigation.tsx:64-67` carries the
  navigation item as a comment inside the `navigationItems` array, and `messages.epochs` at `:24-28`
  is read by nothing else. `containers/staking/Staking.tsx:66` builds `ROUTES.STAKING.PAGE` and
  `:72-73` triggers `goToRoute` with it, so that constant is a navigation target rather than a
  `<Switch>` binding and cannot land on `/staking/epochs`.
- The screen is a stub rather than a suspended feature, which is the distinction locked decision 14
  draws from decision 13. `StakingEpochsPage.tsx:5-6` imports
  `config/stakingPreviousEpoch.dummy.json` and `config/stakingCurrentEpoch.dummy.json` and its
  entire `render` passes their fields through. It reads no store, despite carrying
  `@inject('stores', 'actions')`.
- Both dummy JSON fixtures are tracked and have exactly two readers each, `StakingEpochsPage.tsx:5-6`
  and `storybook/stories/staking/_support/Epochs.tsx:7-8`. Nothing else in the repository imports
  either.
- What users keep is verifiable and unaffected.
  `components/staking/delegation-center/DelegationCenter.tsx:3` imports `DelegationCenterHeader` and
  `:61` mounts it. That header reads `networkTip`, `epochLength` and `nextEpoch` from its own props
  at `:129`, renders a countdown to the next epoch at `:154` and `:159-175`, and renders the
  sentence about when a delegation change takes effect from `messages.description` at `:42-46`. None
  of it touches the deleted page.
- What users lose is nothing visible. The epoch progress bar at
  `components/staking/epochs/StakingEpochs.tsx:124-135` renders only inside
  `selectedEpoch === CURRENT_EPOCH` on the deleted page, and no affordance reaches that page.
- Twelve message ids go, as the task entry says: eleven under `staking.epochs.*` defined across
  `StakingEpochs.tsx`, `StakingEpochsCurrentEpochData.tsx`, `StakingEpochsNoData.tsx` and
  `StakingEpochsPreviousEpochData.tsx`, plus `staking.navigation.epochs` in `StakingNavigation.tsx`.
  `defaultMessages.json` carries four `path` entries naming files under the epochs directory, at
  `:5190`, `:5205`, `:5215` and `:5250`.
- The i18n check at `perSystem/checks.nix:64-76` regenerates and requires no diff, so the artifacts
  must be regenerated in this commit. The route was proved during `task-003`: the CI `node_modules`
  from `nix build .#internal.x86_64-linux.node_modules`, then `formatjs extract` and
  `translations/translation-runner.ts`, which together reproduce the committed artifacts byte for
  byte when nothing has changed.
- The story side: `storybook/stories/staking/Staking.stories.tsx:14` imports `StakingEpochsStory`,
  `:36` holds the `epochs: 'Epochs'` entry in `pageNames`, and `:130-132` is the registration. The
  task entry gives `:17` and `:172`, which were correct before `task-002` removed 88 lines from this
  file. `storybook/stories/staking/_support/Epochs.tsx` holds the two knob call sites the entry
  names, `date` at `:13` and `number` at `:28`.
- `pageNames['stake-pools-tooltip']` at `:34` is unread and was before this task. It stays, as it
  did through `task-002`: removing it is an unrelated edit.
- The sidebar stands at 259 registrations across 49 titles in 14 groups. Removing the one `Epochs`
  registration takes it to 258. `Decentralization / Staking` goes from 14 registrations to 13 and
  survives, so no panel, title or group disappears.
- The checks are green at `d036a9515`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n}` all exit 0, and
  `compile` is `yzxicnczzmp24f709i7rl6az258ns21f-daedalus-compile.drv`.

## Files Expected To Change

Removed, 11 files:

- `source/renderer/app/containers/staking/StakingEpochsPage.tsx`
- `source/renderer/app/components/staking/epochs/StakingEpochs.scss`
- `source/renderer/app/components/staking/epochs/StakingEpochs.tsx`
- `source/renderer/app/components/staking/epochs/StakingEpochsCurrentEpochData.tsx`
- `source/renderer/app/components/staking/epochs/StakingEpochsDataTable.tsx`
- `source/renderer/app/components/staking/epochs/StakingEpochsNoData.tsx`
- `source/renderer/app/components/staking/epochs/StakingEpochsPreviousEpochData.tsx`
- `source/renderer/app/components/staking/epochs/helpers.ts`
- `source/renderer/app/config/stakingCurrentEpoch.dummy.json`
- `source/renderer/app/config/stakingPreviousEpoch.dummy.json`
- `storybook/stories/staking/_support/Epochs.tsx`

Edited, four files:

- `source/renderer/app/Routes.tsx`, the import at `:20` and the binding at `:195-199`
- `source/renderer/app/routes-config.ts`, `EPOCHS` at `:12`
- `source/renderer/app/components/staking/navigation/StakingNavigation.tsx`, `messages.epochs` at
  `:24-28` and the commented item at `:64-67`
- `storybook/stories/staking/Staking.stories.tsx`, the import at `:14`, the `pageNames` entry at
  `:36` and the registration at `:130-132`

Regenerated, three files:

- `translations/messages.json`
- `source/renderer/app/i18n/locales/defaultMessages.json`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`

That is four artifacts, not the three the task entry lists. The entry names `en-US.json`,
`ja-JP.json` and `translations/messages.json` and omits `defaultMessages.json`, which is in the same
snapshotted directory and carries both the message ids and the source paths of the deleted
components.

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-004.status` and
  the line-number corrections its prose carries
- `.agent/plans/storybook-modernization/task-plans/task-004.md`
- `.agent/plans/storybook-modernization/task-plans/task-004-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-004-impl-review.md`

No barrel entry changes. `storybook/stories/index.ts` reaches this story only through
`staking/Staking.stories.tsx`, so removing that import and registration is the equivalent edit.

## Implementation Approach

1. Make the four source edits first, so the deletion list is handed off against a tree where nothing
   still imports what is about to go. Removing the `Routes.tsx` import and binding together matters:
   an import with no use is a warning here rather than an error, so leaving one would not fail a
   check.
2. In `StakingNavigation.tsx`, remove the commented navigation item as well as `messages.epochs`.
   Leaving the comment would leave dead commented code pointing at a message that no longer exists,
   which locked decision 14 settles explicitly.
3. In `Staking.stories.tsx`, remove the import, the `pageNames` entry and the registration. The
   `epochs` entry goes because the registration was its only reader; `stake-pools-tooltip` stays
   because it was already unread before this task.
4. Write the 11 paths to the deletion list and hand off.
5. After the removals, regenerate the i18n artifacts by the `task-003` route and confirm the diff is
   confined to the twelve ids and the four `path` entries, with no insertion.
6. Stage everything, then verify the tree: 11 deletions, four source modifications, four regenerated
   artifacts, nothing untracked outside `.agent/`.
7. Regenerate the sidebar and diff. Expect one registration gone and no panel, title or group to
   move.
8. Run `compile`, `lint`, `storybook` and `i18n` as flake checks.
9. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- A grep over `source/`, `storybook/` and `tests/` returns no reference to `StakingEpochs`,
  `ROUTES.STAKING.EPOCHS` or either dummy JSON fixture. Evidence: the post-change greps, run
  per name.
- `yarn storybook:build`, `yarn compile`, `yarn lint` and `yarn i18n:manage` pass, with the
  regenerated locale files and `translations/messages.json` committed. Evidence:
  `nix build .#checks.x86_64-linux.{storybook,compile,lint,i18n}` all succeed on a derivation that
  moved, and `i18n` is precisely the check that regenerates and compares.
- The sidebar diff against `task-001` shows exactly one further registration removed, `Epochs` under
  `Decentralization / Staking`. Evidence: the regenerated tree diffed against the post-`task-003`
  capture.
- `/staking/delegation-center` still renders the current epoch, the next epoch countdown and the
  sentence about when a delegation change takes effect. Evidence: `DelegationCenter.tsx:3` and `:61`
  mount `DelegationCenterHeader`, which renders all three from its own props. No file in this
  task's change set is on that path.

The `StakingEpochs` grep needs one qualification. `isEpochsInfoAvailable` is a `DelegationCenter`
prop that survives and contains neither string, and `nextEpoch`, `epochLength` and
`stakingEpochsPage` style class names are unrelated. The grep is run for `StakingEpochs` as a word
and for the two fixture basenames, so a substring match cannot make it look clean or dirty by
accident.

Added for this plan:

- The fourth i18n artifact, `defaultMessages.json`, is regenerated and committed. The task entry
  lists three.
- The sidebar keeps 49 titles, 14 groups and every panel. Only the registration count moves, 259 to
  258.

## Verification Plan

Already run for planning:

- Every line reference in the task entry checked against the current tree, and three found stale.
- Importer enumeration for `StakingEpochsPage`, for the epochs directory, and for both dummy JSON
  fixtures.
- The twelve message ids enumerated from source, and the four `defaultMessages.json` path entries
  located.
- The unreachability of the screen established from three directions: the commented navigation item,
  the absence of any other affordance, and `ROUTES.STAKING.PAGE` being a `goToRoute` target.
- The `DelegationCenterHeader` path read end to end, so the fourth acceptance criterion rests on
  code rather than on the task entry repeating itself.

To run for the build:

- `git status --short` before any check.
- Greps for `StakingEpochs`, `ROUTES.STAKING.EPOCHS`, `staking/epochs`, `stakingCurrentEpoch.dummy`,
  `stakingPreviousEpoch.dummy` and `staking.epochs.` across `source/`, `storybook/`, `tests/` and
  `translations/`.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, diffed
  against the post-`task-003` capture and against the `task-001` baseline.
- `nix build --no-link .#checks.x86_64-linux.{storybook,compile,lint,i18n}`.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a moved path.

If `i18n` fails, the regeneration and the committed artifacts disagree and the response is to rerun
the regeneration on the final tree, never to hand-edit an artifact. If a grep still returns a hit
outside the regenerated artifacts, a reference exists that planning missed.

## Risks and Open Questions

- This is the only task in the phase that removes a screen a user could in principle have reached,
  had the navigation item not been commented out. The case rests on three independent facts rather
  than one, and the thing a user would lose, the epoch progress bar, renders only on that page.
- The overlap with `task-005` in `Routes.tsx` was predicted in that task's plan and has materialised
  exactly as recorded: two of the task entry's line references are stale because `task-005` landed
  first. They are corrected here rather than followed.
- `defaultMessages.json` being absent from the task entry's artifact list is the same shape of gap
  `task-003` found: an i18n consequence that no listed acceptance criterion would catch, but the
  `i18n` check would.
- One stale `StakingEpochs.scss.d.ts` will remain on disk unless it is cleared with the tracked
  files, as the legacy directory's eight were. It is gitignored and invisible to every check.
- `task-021` converts the staking tranche in phase 3 and its file count assumes these removals have
  happened. Nothing else depends on this task.
- Rollback is `git revert` of a single commit, which restores the screen, the fixtures and the
  artifacts together.

## Required Docs, Research, and Tracking Updates

- Set `task-004.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Correct the line references in `task-004`'s prose that `task-002` and `task-005` invalidated, and
  add `defaultMessages.json` to the artifacts the entry names.
- No PRD change. Locked decision 14 remains accurate; its `DelegationCenterHeader` line references
  have drifted by a few lines and the substance holds.
- `research/05-reachable-screens.md` section 4 loses an entry and the container count goes from 105
  to 104, which the task entry already records. The note is a census at a stated commit and is not
  edited.
- The `task-001` baseline is not edited.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-004-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-004-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The route binding, the path constant, the container, the seven files under
  `components/staking/epochs/`, both dummy fixtures and the Storybook support module are gone.
  `components/staking/epochs/` is absent from disk, its stale `.scss.d.ts` cleared with it.
- `storybook/stories/staking/_support/` keeps its seven other modules; only `Epochs.tsx` went.
- The four source edits total 21 deletions against 1 insertion.
- The four i18n artifacts lost 184 lines between them, every one a deletion and no insertion.

## Final Outcome

- `task-004` completed. No task depends on it, and phase 1's deletions are finished.
- Every grep is clean: `StakingEpochs`, `ROUTES.STAKING.EPOCHS`, `staking/epochs`,
  `stakingCurrentEpoch.dummy` and `stakingPreviousEpoch.dummy` return nothing across `source/`,
  `storybook/`, `tests/` and `translations/`. `isEpochsInfoAvailable` survives untouched in
  `Staking.stories.tsx`, which is the substring the grep was designed not to catch.
- The sidebar reads 258 registrations across 49 titles in 14 groups, against 259, 49 and 14 before
  this task. `Decentralization / Staking` went from 14 registrations to 13 and no panel, title or
  group moved, exactly as planned.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile`, `.lint` and `.i18n` all exit 0,
  on a `compile` derivation that moved to `v24rzf5lhqk54xm4s0z9mmh019ixvzzi-daedalus-compile.drv`.
  The `i18n` check is the one that regenerates and compares, so the second acceptance criterion is
  answered by the check itself rather than by a claim about it.
- `yarn lint` reports 5410 warnings against the 5445 the corpus carried after `task-003`.
- `/staking/delegation-center` is untouched. `DelegationCenter.tsx:3` and `:61` still mount
  `DelegationCenterHeader`, and no file in this change set is on that path.

## Self-Review

- Three line references in the task entry were stale and were corrected rather than followed. Two
  were `task-005`'s doing and one `task-002`'s, and both tasks predicted the overlap in their own
  plans, so this was a collision the phase saw coming rather than one it discovered.
- The entry listed three i18n artifacts and there are four. That is the second time in two tasks
  that an i18n consequence sat outside the stated acceptance criteria, and the `i18n` check is the
  only thing that would have caught either.
- One claim in the approved plan was wrong and is corrected rather than quietly dropped: the twelve
  ids were said to be untranslated in both locales, and all twelve carry real Japanese. The
  assumption was carried over from `task-003`, where it happened to be true. The outcome does not
  change, because the screen is unreachable and the strings return with a revert, but "no
  translation work" and "real translator output is being removed" are different statements and the
  second is the true one.
- `babeledit-project.babel` still names the eleven `staking.epochs.*` ids and was deliberately left.
  It is a 2022 snapshot that `i18n:manage` does not write, already inconsistent with the source in
  both directions, and pruning eleven of its 338 excess ids would be arbitrary.
- The repository is not prettier-clean and nothing in CI checks that it is. `StakingNavigation.tsx`
  was already non-clean, so the edits were hand-applied rather than formatted, to keep an unrelated
  reformat out of a public diff.
