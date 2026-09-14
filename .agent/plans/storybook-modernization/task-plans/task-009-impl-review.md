Implementation: Iteration 1
Timestamp: 2026-09-14T21:28:10Z

Changes made:
- Recorded the census in `task-009.md`: 65 functions and components across 39 story files depend on
  the `StoryWrapper` prop pass-through, 35 naming `currentTheme`, `osName` or `locale` and 30
  forwarding the whole render argument by spread.
- Added one `implementationNotes` entry carrying the relevant figures to `task-016`, `task-018`,
  `task-019`, `task-020`, `task-021`, `task-022` and `task-024`.
- Set `task-009.status` to `completed` and `## Build Status` to `completed`.

Files touched:
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`
- `.agent/plans/storybook-modernization/task-plans/task-009.md`
- `.agent/plans/storybook-modernization/task-plans/task-009-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-009-impl-review.md`

Verification run:
- The census was taken with the TypeScript compiler API over every file under `storybook/stories/`
  plus the four colocated story files, at `ab7060622`. Result: 65 rows across 39 files, 35 naming a
  value, 30 spreading only, split 18 first render argument, 16 second, 1 `this.props`.
- Per tranche, by each task's own `targetPaths`: `task-018` 21 functions in 9 files, 16 named;
  `task-019` 11 in 8, 9 named; `task-020` 13 in 11, 5 named; `task-021` 17 in 8, 5 named;
  `task-022` 3 in 3, none named. First acceptance criterion met.
- Adjustments stated per tranche with the file named: `task-002` removes 2 spread-only forwarders
  from `task-020`, taking it to 11; `task-007` removes 1 from `task-021`, taking it to 16;
  `task-003` and `task-004` remove none. No named-read count moves. Second acceptance criterion met
  on the alternative the task entry offers.
- Registration reconciliation: the census walks 273 registrations, the same total the `task-001`
  extractor reports at this commit. Of those, 188 story functions declare no parameter, 17 declare
  one, 32 declare two and 36 register an imported symbol by reference.
- Cross-check by the greps the task entry names: `props.currentTheme`, `props.osName` and
  `props.locale` occur on 44 lines, and the `(_, props)` and `(_, {` signatures on 27. Both are line
  counts against the census's function counts, and the gap is concentrated in
  `navigation/Sidebar.stories.tsx`, which contributes 14 of the 44 lines from 7 functions.
- The four in-repo comments the task entry names as the known starting set were each read and
  classified. `nodes/status/Diagnostics.stories.tsx:88` and
  `loading/mithril/MithrilPartialSyncDialogue.stories.tsx:30-31` describe the two different
  signatures and are each correct about their own file. `governance/DRepDetail.stories.tsx:67` and
  `governance/DRepDirectory.stories.tsx:248` record the opposite case, a deliberate decision not to
  read locale as a prop, and neither file appears in the 35.
- The tasks JSON was re-parsed after the edit: 61 tasks, valid, and the round-trip through
  `indent=1` reproduces the file, so the diff is 15 insertions against 8 deletions and shows only
  the seven added notes and the status.
- No flake check was run. Every edited file is under `.agent/`, which `nix/internal/common.nix:269`
  excludes from `srcWithoutNix`; `task-001` established that with a control probe.

Two corrections were made to the census before the figures above were published, both worth
recording because a census that silently changed its answer is one nobody can check:

- The first pass swept whole `source/` directories and picked up
  `wallet-token-picker/helpers.ts` and `features/discreet-mode/ui/withDiscreetMode.tsx`, which are
  application code that spreads a parameter and have nothing to do with the workbench. Restricting
  the sweep to `storybook/stories/` plus the four colocated story files took the population from 67
  to 63.
- The second pass matched only property accesses on a bare identifier bound by a function
  parameter. That missed the class component `StakingDelegationSteps` at
  `staking/_support/DelegationSteps.tsx:115`, which reads `this.props.locale` and
  `this.props.currentTheme`, and it missed
  `loading/mithril/MithrilPartialSyncDialogue.stories.tsx:36`, which reads
  `(context as { osName?: string }).osName`. Both were found by the raw grep the task entry
  prescribes, which is the reason to run it as a cross-check. Adding class-component detection and
  unwrapping parenthesised and `as` expressions took the population from 63 to 65.

Deviations from the approved plan:
- None.

User interaction is now required:
- Yes, on one point that is outside this task's scope and cannot be settled by measurement: three
  colocated story files sit in no phase 3 tranche's `targetPaths`.
  `components/profile/analytics/Analytics.stories.tsx`,
  `features/discreet-mode/ui/DiscreetValue.story.tsx` and
  `features/discreet-mode/ui/discreet-toggle/DiscreetModeToggle.story.tsx` carry four registrations
  between them and are `storiesOf()` files phase 3 has to convert. `task-020` names the fourth
  colocated file, `wallet-token-picker/WalletTokenPicker.stories.tsx`, and no tranche names these
  three. None of the four is in this census, so the figures do not move either way. Which tranche
  adopts them is a scoping decision for the owner.

Outcome: Both acceptance criteria met, the census reconciles with the task-001 baseline, and the
figures are attached to all seven phase 3 entries; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-14T21:33:05Z

Summary:
- Approved. The count is measured rather than estimated, split along the lines that decide how each
  site gets edited, reconciled against an independent artifact, and attached where the tasks that
  need it will find it.

Blocking findings:
- None.

Non-blocking observations:
- Splitting named reads from spread-only forwarding is what makes this count usable. The 30
  spread-only sites compile, lint clean and fail only by rendering a component with three props
  missing, and no check in this repository sees that. A single total of 65 would have hidden the
  half of the population that nothing else will catch.
- The two-signature finding is the part `task-016` most needed. Its note now says the replacement
  has to serve both, which is a different design from the one a single-signature count implies.
- Running the prescribed greps as a cross-check rather than as the measurement earned its place
  twice over: the grep is what surfaced the class component and the cast that the AST pass was
  missing. Neither method alone would have produced this figure.
- Recording both census corrections rather than publishing the final number alone is the right
  standard for a task whose only deliverable is a number.
- The unassigned colocated files are correctly raised rather than absorbed. Adopting them into a
  tranche is a scoping call, and a census task quietly extending a tranche's `targetPaths` would be
  exactly the kind of scope drift the plan structure exists to prevent.

Approval bar:
- Met. `task-009` is complete and `task-016` is unblocked on this dependency.

Decision: approved
