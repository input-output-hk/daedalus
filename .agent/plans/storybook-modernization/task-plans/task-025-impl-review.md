Implementation: Iteration 1
Timestamp: 2026-09-15T17:42:30Z

Changes made:
- `tests/jest/setup/storybookExportsResolver.js`, new: a Jest resolver that reads the `exports` map
  of `@storybook/core` and `storybook` and hands everything else to Jest's own resolver.
- `jest.config.js`: point `resolver` at it.

Why a resolver rather than a mapping. The first attempt mapped
`storybook/internal/preview-api` to the file the map names. That resolved, and then the file
required `@storybook/core/preview-api`, another subpath behind another map. Mapping that too
resolved, and that file required `@storybook/core/channels`. Storybook 8 publishes almost everything
this way, so a per-path mapping does not converge: each bundle names more of them. Reading the map
is what Node does and it terminates.

The resolver picks the CommonJS condition deliberately. The ESM entry beside it resolves too, and
then fails to parse, because `transformIgnorePatterns` excludes `node_modules` and nothing would
transpile it.

Verification:
- All eight checks defined for `x86_64-linux` pass: `compile`, `lint`, `stylelint`, `i18n`,
  `storybook`, `jest`, `shellcheck`, `cucumber-unit`.
- `jest`: 70 suites passed, 930 tests passed, 3 skipped. Before the fix it was 69 of 70 and 922
  passed, so the suite that could not run contributes 8 tests that had not been running.
- Knobs: a story using `text`, `boolean`, `number` and `select` inside a `withKnobs` decorator
  renders `default label | true | 42 | b`. Each knob returned its default and each value reached the
  component.
- `withState`: a story renders `count=0`; calling `store.set({ count: 7 })` re-renders it as
  `count=7`.
- Sidebar: 258 stories across 49 panels from `index.json`, identical pair for pair to the
  pre-conversion set.
- `story-args-audit.js`: no story reads an argument nothing fills.

Not executable here: the walk through all fifteen sidebar groups in a running workbench. There is no
browser and no display. What covers the ground it can: the preview build compiles every story module
and every component it reaches, the indexer lists all 258 with matching titles and names, and the
audit rules out the one failure mode found in this corpus that a build cannot see. That is less than
looking at fifteen groups and it is said in those terms rather than reported as met.

Two findings that prevent the merge, neither resolved here:
- `master` has moved fourteen commits since the merge base. `git merge-tree` reports one conflict,
  in `storybook/stories/governance/DRepDirectory.stories.tsx`, which two of those commits touch and
  this branch rewrote. Everything else auto-merges.
- `master` defines a ninth check, `docs`, from `tests/docs.nix`. It does not exist on this branch,
  so nothing here has been measured against it.

The task entry says to rebase onto `master`. This branch is public and has been pushed throughout,
so a rebase means force-pushing published history. Reported rather than done.

Deviations from the approved plan:
- None.

Outcome: Every check this branch defines passes, knobs and story state are verified by rendering
rather than by compiling, and the two things standing between the branch and the trunk are named;
ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T17:49:05Z

Summary:
- Approved for the verification. The merge is not this task's to make.

Blocking findings:
- None in the work. Two in the way of landing it, both stated.

Non-blocking observations:
- The jest failure is the most instructive thing here. It had been broken since the version hop and
  four tasks went past without it being seen, because the window was being tracked by three checks
  and jest was not one of them. Nothing about it was hidden; nobody was looking. A check that is not
  run is not green.
- Chasing the resolution one mapped path at a time and watching it fail to converge was worth the
  two attempts. The third one is right because it reads what Node reads, rather than because it
  happened to satisfy the next error.
- Rendering knobs and `withState` rather than reasoning about them closes the last of the "carried
  across untouched" claims in this phase. Both were argued to work; now both are shown to.
- Not merging is the correct outcome. The task says rebase, the branch is public, and a rebase there
  means overwriting published history. Reporting the conflict and the missing check hands over a
  decision rather than a surprise.

Approval bar:
- Met for the verification. `task-025` is complete as far as this branch can take it.

Decision: approved
