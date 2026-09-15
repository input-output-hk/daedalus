# Task task-014: Run the codemod chain as a dry run over a real tranche

## Task ID and Title

- ID: `task-014`
- Title: `Run the codemod chain as a dry run over a real tranche`

## Why Chosen Now

`task-014.dependencies` is `[task-013]`, which is complete and left a working 8.6.18 scratch project
with the codemod CLI available. It gates `task-015`, the hop branch.

Codemod coverage has so far been a prediction from reading the transform source. This turns it into
an observation while the branch is still green, because the alternative is discovering the answer
inside the one landing that cannot be decomposed.

It carries a kill criterion: if the output needs more hand correction than hand conversion would
have taken, the codemod comes out of the plan.

## Interaction Mode

- Mode: `agent_execution`

The codemods ship with the 8.6.18 CLI already installed in the `task-013` scratch project, and the
tranche can be copied there without touching the branch.

## Scope

- Run `storiesof-to-csf`, then `csf-hoist-story-annotations`, then `csf-2-to-3` over a real domain
  in the scratch project.
- Measure what the output actually is: registrations preserved, labels preserved, exports renamed,
  files needing hand repair.
- Establish what happens to the dynamic `.add()` registrations.
- Establish whether the four control-flow strings survive.
- Apply the kill criterion to the measured ratio and record the decision.

## Non-Goals

- Nothing is merged. No story file on the branch is converted.
- No manifest change. `task-015` owns that.
- No conversion decisions about individual tranches beyond the corpus-wide use-or-not question this
  task exists to answer.

## Dependencies

- `task-013`, complete.
- `task-015` depends on this task.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the section on what
  codemods do and do not cover at `:767-828`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-014` and the
  phase 3 header
- `.agent/plans/storybook-modernization/task-plans/task-013.md`, for the scratch project
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md`
- Workflows:
  - `.agent/workflows/storybook.md`, which documents the `storiesOf` model this task converts away
    from and which `task-060` rewrites.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `071215ba1`, 2026-09-15, against the working tree.

- **The corpus is 68 `storiesOf()` calls across 65 files, not the 73 the brief carries.** 73 was the
  pre-phase-1 figure. Phase 1 removed six calls, the `Decentralization / Countdown` panel in
  `Staking.stories.tsx`, `CountdownParty`, `PaperWallets`, `LegacyNotification`, `TransferFunds` and
  the `StakingChart` call in `Legacy.stories.tsx`, and `task-006` added one. 73 − 6 + 1 = 68.
- **The dynamic `.add()` registrations number 5, not 9.** All five are in
  `storybook/stories/staking/Staking.stories.tsx`: four read `pageNames.X` and one is a template
  literal. The other four the entry counted were removed by `task-002` (`countdown`, `info`,
  `info-countdown`) and `task-004` (`epochs`), which took the `pageNames` entries with them.
- Three files hold two `storiesOf()` calls each: `nodes/status/Diagnostics.stories.tsx`,
  `staking/Staking.stories.tsx` and `voting/Voting.stories.tsx`. The remaining 62 hold exactly one.
  That distinction turns out to decide most of the correction cost.
- The dry-run tranche, from the task's `targetPaths`, is `storybook/stories/nodes` and
  `storybook/stories/settings`: 9 files carrying 28 registrations.
- The four control-flow sites are where the brief says: `SettingsWrapper.tsx:29` passes
  `context.kind` to `linkTo` and `:31` reads `context.story`; `WalletsWrapper.tsx:14` branches on
  `context.story !== 'Empty' && context.story !== 'Wallet Add'`; and
  `WalletWithNavigationLayout.tsx:32` reads `context.kind`.
- All three codemods the plan names exist in the 8.6.18 CLI: `storiesof-to-csf`,
  `csf-hoist-story-annotations` and `csf-2-to-3` are all listed by `storybook migrate --list`.
- The checks are green at `071215ba1`.

## Files Expected To Change

On the branch, tracking only:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-014.status`, the
  two count corrections and the measured findings
- the three `task-014` plan documents

Outside the repository, not committed: the codemod dry-run copies in the scratch project.

No source file changes.

## Implementation Approach

1. Copy the tranche into the scratch project and take a before copy, so the output can be diffed
   rather than judged by eye.
2. Run the three codemods in order and record the CLI's own counts.
3. Extract the effective sidebar label of every converted story the way Storybook computes it, an
   explicit `storyName` if present and otherwise `startCase` of the export identifier, and compare
   the label set against the `task-001` baseline for those files. A conversion that keeps every
   label is the property that matters; a conversion that keeps every file is not.
4. Check each converted file for structural validity, specifically that it has exactly one default
   export, because a file with two `storiesOf` calls has nowhere to put the second title.
5. Run the chain separately over `Staking.stories.tsx` to see what happens to the five dynamic
   registrations.
6. Where a defect is found, test the remedy rather than only proposing it.
7. Measure whether `context.kind` and `context.story` still exist at 8.6.18, and whether the strings
   the control-flow sites compare against can still match anything.
8. Apply the kill criterion to the measured ratio and record the decision either way.
9. Land it as one signed commit carrying the findings.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- A recorded per-file correction cost for one real domain. Evidence: 9 files, 0 codemod errors, 28
  of 28 labels preserved, 1 file needing hand repair. The correction cost is zero for 8 of 9 and one
  file split for the ninth.
- A decision on whether the codemod is used for the whole corpus or only for part of it. Evidence:
  the ratio below, and the decision recorded in the entry.

Added for this plan:

- The dynamic registrations are characterised precisely, because "skipped rather than mangled" turns
  out not to describe what happens.
- The remedy for each defect is demonstrated, not proposed.

## Verification Plan

Already run:

- The corpus counts, re-measured, correcting two figures the brief and the entry carry.
- The codemod chain over the 9-file tranche, with the CLI's counts recorded.
- Label extraction and comparison against the `task-001` baseline.
- A structural check for exactly one default export per converted file.
- The chain over `Staking.stories.tsx`, with a label comparison.
- The inlining remedy, run and re-measured.
- `context.kind` and `context.story` at 8.6.18, through `composeStories`.
- Whether `'Empty'` and `'Wallet Add'` match any registration `WalletsWrapper` decorates.

To run for the build:

- `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` on the branch, which must
  stay green because this task changes no source.

## Risks and Open Questions

- The dry run covers 9 of 65 files. The two defects it found are both structural and their
  incidence across the rest of the corpus is countable rather than estimated: three files with two
  `storiesOf` calls, five dynamic registrations in one file. What it cannot rule out is a defect
  whose trigger appears only in a domain not sampled, which is why every tranche task still diffs
  its labels against the baseline.
- The label comparison depends on reproducing Storybook's `storyNameFromExport`. The first attempt
  used a hand-written `startCase` and reported two stories as lost that had converted correctly.
  The measurement of record uses lodash `startCase`, which is what Storybook uses.
- `csf-2-to-3` left most files unmodified, so the story shapes stay function-form. That is not a
  defect; it means the third codemod earns little here and the tranche tasks should not expect it to
  do much.
- Nothing here lands, so there is nothing to roll back.

## Required Docs, Research, and Tracking Updates

- Set `task-014.status` to `completed`.
- Correct the entry's `storiesOf` count from 73 to 68 and its dynamic-registration count from 9 to
  5, with the reason for each.
- Record the measured outcome, the two defects, their remedies, and the corpus-wide decision.
- Record that `context.kind` and `context.story` survive, and that two of the four control-flow
  strings are already dead.
- No PRD change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-014-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-014-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The codemod chain runs cleanly over a real 9-file, 28-registration tranche with zero errors and
  every label preserved.
- Two defects found, both structural, both with a demonstrated remedy.
- The kill criterion was applied to a measured ratio and did not trigger.

## Final Outcome

- `task-014` completed. `task-015` is unblocked.
- **Result over the tranche:** 9 files, 0 errors, 0 skipped. 28 of 28 labels preserved. The codemod
  attached an explicit `storyName` to the 5 exports whose sanitized identifier would not have
  round-tripped, and the other 23 derive correctly from the export name.
- **Defect 1: a file with two `storiesOf` calls produces two `export default` statements**, which is
  not valid TypeScript. It hit 1 of the 9 tranche files, `nodes/status/Diagnostics.stories.tsx`, and
  corpus-wide it hits exactly 3 of 65: that file, `staking/Staking.stories.tsx` and
  `voting/Voting.stories.tsx`. The remedy is to split each into one file per title before running
  the codemod.
- **Defect 2: dynamic labels are silently dropped, not skipped.** Over `Staking.stories.tsx` the
  chain reported `1 ok, 0 errors` and produced a file carrying 14 of the 19 registrations. The five
  lost are exactly the non-string-literal labels: `Delegation Center`, `Pools Index`,
  `Pools Index - Loading`, `Rewards` and `Stake Pools List`. The task entry expected them to be
  skipped rather than mangled; dropped while the run reports success is worse than either, because
  nothing in the output says a story went missing.
- **The remedy for defect 2 is demonstrated, not proposed.** Replacing the five `pageNames` lookups
  and the one template literal with the string literals they evaluate to, then running the chain,
  preserves all 19 registrations.
- `context.kind` and `context.story` are both still populated at 8.6.18, carrying the title and the
  story name, so the four control-flow sites survive the version hop itself.
- Two of those four cannot break at all. `'Wallet Add'` matches no label anywhere in the corpus, and
  neither `'Empty'` nor `'Wallet Add'` matches any of the 40 registrations `WalletsWrapper`
  decorates, so the ternary at `WalletsWrapper.tsx:14` already always takes the same branch.
- **Kill criterion: not triggered. The codemod is used for the whole corpus.** 62 of 65 files
  convert with zero hand correction. Three need a mechanical split first, and one of those three
  also needs five labels inlined. Against hand-converting 65 files carrying 258 registrations, that
  is not close.

## Self-Review

- The decision rests on a ratio that was measured rather than eyeballed, which is what the kill
  criterion asks for. 62 of 65 clean is a different answer from "it mostly worked".
- Both defects are silent in different ways, and that is the finding rather than the defects
  themselves. Two default exports fails `tsc` loudly; five dropped registrations fails nothing at
  all and would have reached the sidebar diff as an unexplained shortfall three tranches later.
- The remedies were run rather than described. The inlining test is what turns "inline the dynamic
  labels first" from advice into a procedure with a known outcome.
- My own instrument produced a false result again, the second time in this phase: a hand-written
  `startCase` reported `Step 1` and `Step 2` as lost when the codemod had converted them correctly.
  Using lodash `startCase`, which is what Storybook uses, removed both. Reporting a codemod defect
  that does not exist would have been worse than missing one, because it would have argued for
  dropping a tool that works.
- Two counts in the entry were stale and are corrected with their arithmetic, so neither is
  re-derived later.
