# Task task-012: Verify the glob against the baseline

## Task ID and Title

- ID: `task-012`
- Title: `Verify the glob against the baseline`

## Why Chosen Now

`task-012.dependencies` is `[task-011, task-001]` and both are complete. It is the last verification
step of phase 2 and the point at which the silent-absence defect is closed: `task-010` made the
indexer read the corpus rather than a list, and this task establishes that it does.

## Interaction Mode

- Mode: `agent_execution`

Both acceptance criteria are answerable here. The second needs a file created and then removed, and
the removal goes through the operator, which is recorded as a step rather than assumed.

## Scope

- Establish that the registration count the indexer sees equals the count the corpus carries.
- Establish that a story file placed in a directory no barrel ever referenced is indexed with no
  configuration change.
- Correct the expected count in the task entry.

## Non-Goals

- No change to `storybook/main.ts`, which is in the task's `targetPaths` and needs none. This task
  verifies the glob rather than adjusting it.
- No change to any story, to `preview.tsx`, or to the sidebar order `task-011` set.
- No permanent addition to the corpus. The probe file is removed in the same commit that adds it,
  so it never reaches the branch.

## Dependencies

- `task-011` and `task-001`, both complete.
- No task depends on `task-012`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-012`
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/task-010.md` and `task-010-impl-review.md`, for
  the matcher resolution and the first probe
- `.agent/plans/storybook-modernization/task-plans/task-011.md`, for what the order change did and
  did not touch
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, which still documents the barrel and is rewritten by
    `task-060`.
- Skills:
  - `.agent/skills/storybook-creation/SKILL.md`, consulted for the shape of a minimal `storiesOf`
    file, which is what the probe is. Its statement that the corpus uses `storiesOf` rather than CSF
    is accurate and is why the probe is written that way.
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `1441b923d`, 2026-09-15, against the working tree.

- **The expected count in the task entry is wrong.** It says 257, derived as the `task-001` baseline
  of 272 minus the 15 registrations phase 1 removed. The subtraction is right and the premise is
  incomplete: `task-006` added one registration, `Wallets / Settings | Undelegate Wallet`, and
  recorded it at the time. 272 − 15 + 1 = 258, which is what the corpus carries and what every
  capture since `task-007` reports.
- The 15 removals, for the record: 12 in `task-002`, 2 in `task-003` and 1 in `task-004`.
- The corpus is 258 registrations across 49 titles in 14 groups, `UNREACHABLE 0`.
- The indexed count follows from two facts already established rather than needing a browser.
  `task-010` resolved the two `stories` entries through `@storybook/core-common@6.4.22` and showed
  the generated `require.context` matchers select exactly the 65 files on disk that follow the
  naming convention. The `task-001` extractor reports those same 65 files carrying 258
  registrations. `require.context` includes every matching file and nothing else, so the indexed
  registration count is 258.
- The first probe, in `task-010`, used
  `storybook/stories/loading/chain-storage/ChainStorageLocationPicker.stories.tsx`, a file that
  already existed and that a barrel had reached. It proved the glob loads the existing corpus. It
  does not answer this task's second criterion, which is about a file no barrel ever referenced.
- A never-referenced location exists in quantity under `source/renderer/app`. That directory holds
  1508 tracked files of which 4 are stories, and the glob covers all of it recursively. Any new
  directory under `storybook/stories` also qualifies, since every existing one was reachable from a
  barrel.
- The checks are green at `1441b923d`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` all exit 0, and `compile` is
  `lkq92rv2p1ac5hwh353sdv7dl3hynkp2-daedalus-compile.drv`.

## Files Expected To Change

Added and then removed within this task, one file:

- `source/renderer/app/components/widgets/GlobCheck.stories.tsx`

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-012.status` and
  the corrected expected count
- `.agent/plans/storybook-modernization/task-plans/task-012.md`
- `.agent/plans/storybook-modernization/task-plans/task-012-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-012-impl-review.md`

The commit contains no source change. The probe file is added, tested and removed before it, so the
branch never carries it.

## Implementation Approach

1. Confirm the registration count: run the `task-001` extractor and require 258, then restate the
   composition that makes it the indexed count rather than merely the on-disk count.
2. Write the probe as a minimal valid `storiesOf` file at
   `source/renderer/app/components/widgets/GlobCheck.stories.tsx`, a directory that has never
   contained a story and that no barrel ever named. Stage it, because an untracked file is invisible
   to the flake source.
3. Confirm the matcher selects it: apply the `require.context` regular expression from `task-010` to
   the new path, and confirm the extractor now reports 259 registrations, one more than before.
4. Prove the indexer loads it, rather than merely that it exists. A valid story file added to a
   working build produces a green build, and so does a file the indexer ignores; the two are
   indistinguishable. So add an unresolvable import to the probe and require
   `nix build .#checks.x86_64-linux.storybook` to fail with `Can't resolve`. That is the same
   instrument `task-010` used and the reason it exists.
5. Remove the unresolvable import, confirm the build returns green with the probe still present, so
   the failure is attributable to the import and not to the file.
6. Hand the probe path off for removal, then confirm the corpus is back to 258 and the checks are
   green.
7. Land it as one signed commit containing the plan documents and the tracking correction only.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- The indexed registration count equals the expected count. Evidence: the extractor reports 258, and
  the `require.context` matchers select exactly the 65 files that carry those 258. The entry's
  expected figure of 257 is corrected to 258, with the reason, so the next reader does not
  re-derive the stale number from the same subtraction.
- A story file added anywhere under the globbed paths appears without a further edit. Evidence: a
  probe file in a directory no barrel ever referenced is matched by the regular expression, raises
  the extractor count to 259, and makes `storybook:build` fail when given an unresolvable import,
  which is only possible if the indexer loaded it. No file under `storybook/` was edited to make
  that happen.

Added for this plan:

- The green build with the probe present but its bad import removed, so the red result is
  attributable.
- The corpus returns to 258 and the checks return to green after the probe is removed.

## Verification Plan

Already run for planning:

- The arithmetic behind 258, traced to the three tasks that moved it.
- The `task-010` matcher resolution and probe, re-read to establish what they did and did not prove.
- The count of tracked files under `source/renderer/app` and how many are stories.

To run for the build:

- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, before,
  during and after the probe: 258, 259, 258.
- The `require.context` regular expression applied to the probe path.
- `nix build --no-link .#checks.x86_64-linux.storybook` with the probe broken, expecting failure
  with `Can't resolve`.
- The same check with the probe valid, expecting exit 0.
- `git status --short` before each check, since an untracked probe would be invisible and would make
  the whole test pass for the wrong reason.
- `nix build --no-link .#checks.x86_64-linux.{storybook,compile,lint}` after removal.

If the probe does not raise the count to 259, the extractor and the matcher disagree about the
naming convention and that is a defect in one of them rather than in the glob. If the broken probe
does not turn the build red, the indexer is not loading it and the glob does not do what
`task-010` claimed.

## Risks and Open Questions

- The count is established by composition rather than by reading it out of a running workbench. The
  two facts composed are each verified, and the browser this repository has no way to run is the
  only thing that could measure it directly. The composition is stated so it can be disputed.
- The probe is added under `source/renderer/app`, which is application source rather than the
  workbench. That is deliberate: it is the half of the glob with 1508 files and 4 stories, so it is
  the half where an indexing failure would be least visible. The file is removed in the same task
  and the commit carries no source change.
- A probe left behind would be a real defect, a fake story shipped in the application tree. The
  removal is a hand-off step with its own confirmation, and the final extractor run at 258 is what
  proves it went.
- Rollback is not applicable: this task lands no source change.

## Required Docs, Research, and Tracking Updates

- Set `task-012.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Correct the expected registration count in the `task-012` entry from 257 to 258, and say why 257
  was wrong.
- No PRD change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-012-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-012-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The corpus is 258 registrations across 49 titles in 14 groups, and the artifact is byte-identical
  to the pre-probe capture.
- The probe is absent from the index and from the working tree, and nothing named `globcheck`
  remains tracked or on disk.
- The commit carries no source change.

## Final Outcome

- `task-012` completed, and with it phase 2.
- The indexed registration count is 258. It is established by composition rather than read out of a
  running workbench, which cannot be opened here: `task-010` resolved the two `stories` entries
  through `@storybook/core-common@6.4.22` and showed the generated `require.context` matchers select
  exactly the 65 convention-following files on disk, and the `task-001` extractor reports those 65
  carrying 258 registrations. The entry's 257 is corrected, with the reason.
- The second criterion is answered in both directions rather than one.
  `source/renderer/app/components/widgets/GlobCheck.stories.tsx` was added in a directory no barrel
  ever referenced, in the half of the glob holding 1508 tracked files and 4 stories. The
  `require.context` matcher selected it, the extractor count rose to 259, the valid probe built
  green, and the same file with `import './GlobCheckProbeMissing';` appended failed the build with
  `Can't resolve './GlobCheckProbeMissing'`. The green run is what makes the red one attributable:
  a file the indexer ignored would have stayed green in both directions, which is precisely how
  `Legacy.stories.tsx` stayed dead for years.
- After removal the corpus returned to 258 and
  `nix build --no-link .#checks.x86_64-linux.{compile,storybook,lint}` all exit 0, on the same
  `compile` derivation as before the probe, `lkq92rv2p1ac5hwh353sdv7dl3hynkp2`, which is itself
  evidence that the tree is back where it started.

## Self-Review

- The instrument matters more than the result here. A story file added to a working corpus produces
  a green build whether or not anything indexes it, so a one-directional test would have proved
  nothing and looked like proof. Running it both ways is what converts "the glob should pick this
  up" into "the glob picked this up".
- The probe's location was chosen rather than convenient. Under `storybook/stories` the test would
  have been easier and weaker; under `source/renderer/app` it exercises the recursive context over
  1508 files where a failure would be silent.
- The count is stated as a composition of two verified facts, with the step that cannot be taken
  here named, rather than implying a number was read off a running instance as the task entry's
  wording suggests.
- The probe had to leave through the hand-off, and the evidence that it did is the corpus returning
  to 258 and the `compile` derivation hashing back to its pre-probe value, not an assurance that it
  was removed.
