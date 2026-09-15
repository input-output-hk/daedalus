# Task task-025: Verify the hop

## Task ID and Title

- ID: `task-025`
- Title: `Verify the hop and merge it`

## Why Chosen Now

`task-025.dependencies` is `[task-023, task-024]`, both complete. Everything the window contained is
done and the verification is what remains.

## The red window

Closed on every check this branch can run. The merge itself is not performed here; see the two
findings at the end.

## Interaction Mode

- Mode: `agent_execution` for the verification. The merge is a decision for the branch owner.

## Scope

- Run every check the flake defines for this system.
- Confirm knobs and `withState` still work, as behaviour rather than as compilation.
- Establish whether the branch can land, and say what stands in the way if it cannot.

## Non-Goals

- Merging or rebasing. Both are addressed below rather than performed.
- Any further conversion work.

## Dependencies

- `task-023` and `task-024`.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-013.md`, on knob behaviour at 8.6.x
- `.agent/plans/storybook-modernization/task-plans/task-023.md`, the sidebar comparison
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-025`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`

## Live Repo Findings Verified For Planning

Verified at `816092985`.

- `perSystem/checks.nix` defines eight checks for `x86_64-linux`: `compile`, `lint`, `stylelint`,
  `i18n`, `storybook`, `jest`, `shellcheck` and `cucumber-unit`.
- `jest` fails. One suite of seventy cannot run:
  `tests/jest/governance/delegationStoryFixtures.spec.ts` reaches `@storybook/addon-knobs`, which
  reaches `@storybook/preview-api`, which is a one-line shim for `storybook/internal/preview-api`.
  Jest is 27.5.1 and does not read package `exports` maps, so it looks for a directory called
  `internal` and reports the module missing. This is a consequence of the version hop that nothing
  saw until now, because `jest` was not in the check set being run during the window.
- Nothing in this repository renders a story in a browser, so the walk through all fifteen sidebar
  groups cannot be performed here.

## Files Expected To Change

- `jest.config.js`
- `tests/jest/setup/storybookExportsResolver.js`, new

## Implementation Approach

1. Make Jest resolve Storybook's subpath exports.
2. Run every check.
3. Verify knobs and `withState` by rendering them.
4. Compare the branch against `master` and report what landing it requires.

## Acceptance Criteria

- All required checks pass on the merge commit.
- Every panel renders.
- Knobs and `withState` still function at 8.6.x.
- The trunk has not carried a red commit at any point.

## Verification Plan

- `nix build` for all eight checks.
- Render a story with four knob types and assert each value reaches the component.
- Render a `withState` story, drive the store, and assert the re-render.
- `git merge-tree` against `origin/master`.

## Risks and Open Questions

- The panel walk is not executable here. What stands in its place is stated rather than implied.
- Two of the acceptance criteria describe an action, merging, that conflicts with a standing
  constraint on this branch.

## Required Docs, Research, and Tracking Updates

- Set `task-025.status` to `completed` for the verification, and record the two findings that block
  the merge.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-025-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-025-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Every check the flake defines for this system passes.

## Final Outcome

All eight checks green: `compile`, `lint`, `stylelint`, `i18n`, `storybook`, `jest`, `shellcheck`
and `cucumber-unit`. `jest` is 70 suites, 930 passed, 3 skipped, and it runs 8 more tests than
before because the suite that could not run now does.

Knobs and `withState` verified by rendering, not by compiling:

- A story using `text`, `boolean`, `number` and `select` inside a `withKnobs` decorator renders
  `default label | true | 42 | b`. Every knob returned its default and every value reached the
  component.
- A `withState` story renders `count=0`, and calling `store.set({ count: 7 })` re-renders it as
  `count=7`. The store still drives the story.

The sidebar is 258 stories across 49 panels, identical to the pre-conversion set pair for pair,
measured from `index.json` at every step of the window.

## What could not be verified here

Every panel rendering cannot be checked from this repository. There is no browser and no display,
which is the same constraint that made locked decision 7 drop the image-diff work. What stands in
its place:

- `storybook build` completes, which compiles every story module and every component it reaches.
- Storybook's own indexer lists all 258, and their titles and names match the baseline exactly.
- `story-args-audit.js` reports no story reading an argument nothing fills, which is the one failure
  mode found in this corpus that a build cannot see. Thirteen such stories existed when the window
  opened; there are none now.

That is not the same as having looked at fifteen groups. It is what can be established without a
browser, and the gap is the same gap locked decision 7 already records.

## Two things stand between this branch and the trunk

Neither is a defect in the work and neither is resolved here.

- **The branch does not merge cleanly.** `master` has moved fourteen commits since the merge base
  and two of them touch `storybook/stories/governance/DRepDirectory.stories.tsx`, which this branch
  rewrote. `git merge-tree` reports one conflict, in that file. Everything else auto-merges,
  including three i18n artifacts and `GovernanceWallets.stories.tsx`.
- **`master` has a check this branch has never run.** `perSystem/checks.nix` on `master` defines a
  ninth check, `docs`, from `tests/docs.nix`. It does not exist on this branch, so nothing here has
  been measured against it.

The task entry says to rebase onto `master` rather than merge `master` in. This branch is public and
has been pushed throughout, so a rebase would mean a force-push over published history. That is not
a call to make unilaterally, and it is why the merge is described here rather than performed.

## Self-Review

- Running the whole check set rather than the three that were red during the window is what found
  the jest failure. It had been broken since the version hop and three tranches went by without
  anyone asking, because the window's attention was on the three checks that were expected to move.
  A check nobody is watching is not green; it is unobserved.
- The knob and `withState` checks are worth more than their size. Both were carried across this
  landing untouched on the argument that they would keep working, and until now that was an
  argument rather than a measurement.
