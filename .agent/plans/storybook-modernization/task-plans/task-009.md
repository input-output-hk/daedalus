# Task task-009: Count the story functions reading theme, locale and OS from props

## Task ID and Title

- ID: `task-009`
- Title: `Count the story functions reading theme, locale and OS from props`

## Why Chosen Now

`task-009.dependencies` is `[]`. `task-016` depends on it, and `task-016` is what replaces the
`DaedalusMenu` addon with Storybook globals, so the count is what tells that task and the five
hand-finish tranches how much per-file work the change to context carries.

It is taken now because `task-002`, `task-003`, `task-004` and `task-007` are blocked on a
permission this environment does not grant: each removes a file from the working tree and file
removal is refused here. The task entry anticipates this case directly, saying the count is taken
after `task-002` and `task-003` land or is adjusted for the deleted files. It is adjusted, and the
adjustment for `task-007` is given as well because that task also removes a module the count
includes.

## Interaction Mode

- Mode: `agent_execution`

The task produces a measurement and attaches it to five task entries. No build is involved.

## Scope

- Count every function and component in the story corpus whose shape depends on the `StoryWrapper`
  pass-through, broken down by phase 3 tranche.
- Distinguish those that name `currentTheme`, `osName` or `locale` from those that forward the whole
  object by spread without naming any of them, because the two need different edits.
- Distinguish which render argument each named read comes from, because the corpus uses two
  incompatible signatures and the repository's own comments disagree about which is in use.
- Attach the per-tranche count to `task-016`, `task-018`, `task-019`, `task-020`, `task-021`,
  `task-022` and `task-024` in the tasks JSON.
- State the adjustment for the files `task-002`, `task-003` and `task-007` remove.

## Non-Goals

- No change to `StoryWrapper.tsx`. `task-024` deletes the pass-through, and it cannot go until every
  consumer reads globals.
- No change to any story file. This task measures.
- No change to `storybook/preview.tsx` or to `storybook/addons/DaedalusMenu/`. `task-016` owns both.
- No attempt to decide what each site becomes under globals. The count sizes the work; the tranche
  tasks do it.

## Dependencies

- None. `task-009.dependencies` is `[]`.
- `task-016.dependencies` lists `task-009`, so this unblocks it.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-009` and the
  phase 3 entries `task-016`, `task-018` through `task-022`, and `task-024`
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the UI, store and process
  section on the toolbar globals
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`, the file-level census
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact, for the
  registration total the census is reconciled against
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, read for the decorator model only, per the task-plans readme's
    caution.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Measured at `ab7060622`, 2026-09-14, against the working tree, with a TypeScript AST pass over every
file under `storybook/stories/` plus the four colocated story files.

- `storybook/preview.tsx:8` is the only decorator the preview registers:
  `[(story) => <StoryWrapper>{story}</StoryWrapper>]`. `StoryWrapper.tsx:77-81` renders that story
  with `osName`, `locale` and `currentTheme`. Everything below follows from those five lines.
- 65 functions and components across 39 files depend on the pass-through. That is the population
  whose shape changes.
- 35 of the 65 name one of the three values. They do not agree on where to read it from:
  - 18 read it from the first render argument, written `(props)` or `({ locale })`
  - 16 read it from the second, written `(_, props)` or `(_, { locale })`
  - 1 reads `this.props`, in the class component `StakingDelegationSteps` at
    `storybook/stories/staking/_support/DelegationSteps.tsx:115`
- The remaining 30 name none of the three and forward the whole argument by spread, `{...props}`.
  These change shape too: the object stops carrying the three values, so the spread silently
  delivers nothing rather than failing.
- The repository's own comments record both signatures and each is right about its own file.
  `storybook/stories/nodes/status/Diagnostics.stories.tsx:88` says `StoryWrapper` hands
  `currentTheme` to the story as a prop, the first parameter.
  `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx:30-31` says the OS
  selection arrives on the story context, the second render argument, and not on the args object.
  Both are accurate, which is the finding: there is no single shape to convert.
- Two more comments, at `governance/DRepDetail.stories.tsx:67` and
  `governance/DRepDirectory.stories.tsx:248`, record that locale is deliberately not wired in those
  files because the `IntlProvider` inside `StoryWrapper` handles it. They are therefore the opposite
  case: files that had the question and answered it by not depending on the prop. Neither appears in
  the 35.
- The raw greps the task entry names: `props.currentTheme`, `props.osName` and `props.locale` occur
  44 times under `storybook/stories`, and the `(_, props)` and `(_, {` signatures occur 27 times.
  Both are line counts rather than function counts, which is why the AST pass is the measurement of
  record: `navigation/Sidebar.stories.tsx` alone accounts for 14 of the 44 across 7 functions.
- Per tranche, mapped by each task's own `targetPaths`:

  | Tranche | Domains | Files | Functions | Name one of the three | Spread only |
  |---|---|---|---|---|---|
  | `task-018` | common, assets, dapps, notifications, navigation, news | 9 | 21 | 16 | 5 |
  | `task-019` | nodes, loading, settings | 8 | 11 | 9 | 2 |
  | `task-020` | wallets, colocated wallet token picker | 11 | 13 | 5 | 8 |
  | `task-021` | staking | 8 | 17 | 5 | 12 |
  | `task-022` | governance, voting | 3 | 3 | 0 | 3 |

- The concentration is not where the file counts suggest. `task-022` holds the largest story files
  in the corpus and has no named read at all, while `task-018` holds the smallest and has 16.
  `storybook/stories/navigation/Sidebar.stories.tsx` alone carries 7 of them, all reading
  `currentTheme` from the second argument.
- `task-021` has the largest spread-only population, 12, of which 10 are the `(_, props)`
  registrations in `storybook/stories/staking/Staking.stories.tsx` that forward to an imported
  support component. Those are also the 9 dynamic registrations `task-021` already calls out, so the
  two pieces of work land in the same lines.
- Adjustments for the phase 1 deletions that have not landed:
  - `task-002` deletes `storybook/stories/wallets/legacyWallets/TransferFunds.stories.tsx`, which
    holds 2 spread-only forwarders. `task-020` goes from 13 to 11.
  - `task-003` deletes `storybook/stories/staking/Legacy.stories.tsx`, which holds none. No change.
  - `task-007` deletes `storybook/stories/staking/StakingWrapper.tsx`, which holds 1 spread-only
    forwarder. `task-021` goes from 17 to 16.
  - `task-004` deletes `storybook/stories/staking/_support/Epochs.tsx`, which holds none.
  - The named-read counts are unchanged by every one of these.
- Registration-level view, over the 273 registrations the `task-001` extractor finds at this commit:
  188 story functions declare no parameter at all and are unaffected, 17 declare one, 32 declare two,
  and 36 register an imported symbol by reference, whose parameter shape lives in the support module
  rather than at the registration.
- Four registrations sit in no tranche's `targetPaths`. `task-020` names
  `wallet-token-picker/WalletTokenPicker.stories.tsx` among the colocated files and no tranche names
  the other three: `components/profile/analytics/Analytics.stories.tsx`,
  `features/discreet-mode/ui/DiscreetValue.story.tsx` and
  `features/discreet-mode/ui/discreet-toggle/DiscreetModeToggle.story.tsx`. All four take no
  parameter, so none of them is in this census, but all four are `storiesOf()` files that phase 3
  has to convert and no tranche currently claims them.

## Files Expected To Change

Tracking only:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-009.status` plus
  one `implementationNotes` entry each on `task-016`, `task-018`, `task-019`, `task-020`,
  `task-021`, `task-022` and `task-024`
- `.agent/plans/storybook-modernization/task-plans/task-009.md`
- `.agent/plans/storybook-modernization/task-plans/task-009-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-009-impl-review.md`

No file under `source/`, `storybook/`, `nix/`, `perSystem/` or the repository root changes.

## Implementation Approach

1. Measure with the TypeScript compiler API rather than a grep. Three shapes have to be told apart
   and a regular expression tells none of them: the parameter position a value is read from, a
   destructured binding, and a spread that forwards the whole object without naming anything.
2. Count over every file under `storybook/stories/` plus the four colocated story files, and
   nothing else. An earlier pass that swept whole `source/` directories picked up
   `wallet-token-picker/helpers.ts` and `features/discreet-mode/ui/withDiscreetMode.tsx`, which are
   application code that happens to spread a parameter and have nothing to do with the workbench.
3. Include class components. `DelegationSteps.tsx` reads `this.props.locale` and
   `this.props.currentTheme` and is invisible to a function-parameter pass.
4. Unwrap parenthesised and `as` expressions before matching a property access.
   `MithrilPartialSyncDialogue.stories.tsx:36` reads `(context as { osName?: string }).osName` and a
   bare-identifier match misses it.
5. Reconcile the registration total against the `task-001` baseline, which is 273 at this commit.
   A census that disagrees with the baseline about how many registrations exist is measuring a
   different corpus.
6. Attach the per-tranche figures to the seven phase 3 entries as an `implementationNotes` line
   each, phrased so it is usable without this document.
7. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- A per-domain count exists and is attached to the phase 3 tranche tasks. Evidence: the table above,
  and one `implementationNotes` entry on each of `task-016`, `task-018` through `task-022` and
  `task-024`.
- The count is taken after `task-002` and `task-003` land, or is adjusted for the deleted files.
  Evidence: neither has landed, and the adjustment is stated per tranche, per deleting task, with
  the affected file named.

Added for this plan:

- The count separates named reads from spread-only forwarding, and named reads by which render
  argument they come from. A single number would have been useless to `task-016`, which has to
  serve both signatures.
- The registration total reconciles with the `task-001` baseline at 273.

## Verification Plan

Already run:

- The AST census, at `ab7060622`: 65 functions and components across 39 files, 35 naming a value,
  30 spreading only.
- The raw greps the task entry names, 44 property-access lines and 27 second-argument signatures,
  recorded as the cross-check that shows why they are not the measurement.
- Registration reconciliation against `task-001`: 273 both ways.
- The four in-repo comments read in full, and each classified as describing its own file correctly.

To run for the build:

- `git status --short`, expecting only the four files this task edits.

No flake check is run. This task changes nothing that any check reads: every edited file is under
`.agent/`, which `nix/internal/common.nix:269` excludes from `srcWithoutNix`. `task-001` established
that with a control probe rather than by assertion, and nothing has changed since.

## Risks and Open Questions

- The census counts functions, not edits. A function that reads `currentTheme` three times is one
  row, and the tranche tasks will see three lines to change. The 44-line grep is the upper bound and
  the 35-function count is the lower one, and the difference is concentrated in
  `navigation/Sidebar.stories.tsx`.
- The 30 spread-only forwarders are the part most likely to be underestimated when the tranches are
  scheduled. They compile after the pass-through goes, they lint clean, and they fail only by
  rendering a component with three props missing. Nothing in the required check set sees that. They
  are counted separately here so they are not read as free.
- Four colocated registrations belong to no tranche. That is a gap in the phase 3 task graph rather
  than in this count, and it is recorded here because this is the task that swept the corpus. It
  needs a decision about which tranche adopts them, and none of the four is in this census, so the
  figures above do not move either way.
- The count is taken against a corpus that phase 1 has not finished reducing. Every adjustment is
  stated with the deleting task and the file, so the figures can be re-derived rather than
  re-measured when those land.

## Required Docs, Research, and Tracking Updates

- Set `task-009.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Add one `implementationNotes` entry carrying the count to `task-016`, `task-018`, `task-019`,
  `task-020`, `task-021`, `task-022` and `task-024`.
- No PRD change. The count is new information rather than a correction to anything the PRD states.
- No research-note change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-009-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-009-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- 65 functions and components across 39 files depend on the `StoryWrapper` pass-through: 35 name
  `currentTheme`, `osName` or `locale`, and 30 forward the whole argument by spread.
- The 35 split 18 first argument, 16 second argument, 1 `this.props` in a class component.
- Per tranche: `task-018` 21 of which 16 named, `task-019` 11 of which 9, `task-020` 13 of which 5,
  `task-021` 17 of which 5, `task-022` 3 of which none.
- The seven phase 3 entries each carry their figure in `implementationNotes`.

## Final Outcome

- `task-009` completed. `task-016` is unblocked.
- Both acceptance criteria met: the per-domain count exists and is attached, and it is adjusted for
  the files `task-002`, `task-003`, `task-004` and `task-007` will remove, with each adjustment
  named.
- The census reconciles with the `task-001` baseline at 273 registrations.
- One gap in the phase 3 task graph was found and recorded rather than silently absorbed: three
  colocated story files are in no tranche's `targetPaths`.

## Self-Review

- The measurement was taken with a parser rather than a grep, and the plan says what each of the
  three shapes is that a grep cannot tell apart.
- Two corrections were made to the census during the work and both are recorded: an earlier pass
  swept application source that is not part of the corpus, and a function-parameter pass missed a
  class component and a cast expression. The published figures are from the corrected pass.
- The count separates the population that will fail loudly from the population that will fail
  silently, which is the distinction that matters for scheduling the tranches.
- The two contradictory in-repo comments about which render argument carries the values were both
  checked rather than one being assumed wrong. Both are right about their own file.
