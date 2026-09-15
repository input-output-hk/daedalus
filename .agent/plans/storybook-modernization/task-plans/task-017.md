# Task task-017: Run the codemod chain over the whole corpus

## Task ID and Title

- ID: `task-017`
- Title: `Run the codemod chain over the whole corpus`

## Why Chosen Now

`task-017.dependencies` is `[task-015]`, which is complete, and `task-016` has since cleared the
manager build so the `storybook` check reaches the corpus. Every remaining task in the window
depends on this one, directly or through `task-018`.

## The red window

`compile` is at 68, one `TS2305` per `storiesOf` call site. This task is what drives that to zero:
after it no file imports `storiesOf`. It is also the task most able to do silent damage, because
the transform's exit status is not a measure of what it converted.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- Apply `storiesof-to-csf`, then `csf-hoist-story-annotations`, then `csf-2-to-3` to all 68 story
  files, 64 under `storybook/stories` and 4 colocated under `source/renderer/app`.
- Run `nix fmt` over the result.
- Commit the mechanical output with no hand edits.

## Non-Goals

- No hand-finishing. That is `task-018` through `task-022`, one tranche each.
- No move from props to context. `task-018` onward does that per tranche, so a story body is not
  touched twice.
- No knob or `withState` change.

## Dependencies

- `task-015`, complete. `task-018` through `task-022` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-014.md`, the dry run over one tranche and
  the two defects it found
- `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-baseline.txt`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-017`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `d5c2366f7`.

- The corpus is 68 files, every one `.tsx`: 64 matched by `storybook/stories/**/*.stories.tsx` and
  4 by the colocated glob under `source/renderer/app`. `grep -rl storiesOf` returns the same 68, so
  there is no story file without a `storiesOf` call and no non-story file with one.
- No file holds more than one `storiesOf` call, and no `.add()` carries a non-literal label. Both
  were true only after the two preparatory remedies; they are the conditions under which the
  transform is safe on this corpus.
- The pre-conversion sidebar reading is 258 registrations across 49 panels and 14 groups. That is
  272 in the `task-001` baseline less the 15 registrations phase 1 removed, plus the one restaged in
  `task-006`.
- The chain converts all 68 with `0 errors, 0 skipped` and emits no warning. That figure is not the
  measurement; see below.
- Dry-run measurement over a copy of the corpus: 258 registrations after, 258 before, and the two
  label sets are identical. Per file, the title and the ordered sequence of labels are identical for
  all 68. 151 of the 258 come back with an explicit `name`, which is the sanitizer re-attaching a
  display name that the export identifier cannot carry.
- The transform emits no `__namedExportsOrder`, so within-panel order rests on export order in the
  file, which the dry run shows is preserved.
- The 8.6 CLI shells out for a package-manager probe, and `npm` 10.9.7 rejects this repository's
  `devEngines` key, so the CLI cannot be invoked from the repository root. It runs from any other
  directory against an absolute glob. Also, its `--glob` argument reaches a shell, so an extglob
  pattern is a syntax error there and the two file extensions have to be passed as separate runs.

## Files Expected To Change

- All 68 story files, listed in the commit.
- Nothing else. No config, no manifest, no support module.

## Implementation Approach

1. Read the pre-conversion label set and per-file registration order and keep both.
2. Run the three transforms in the documented order. Running `csf-2-to-3` before
   `csf-hoist-story-annotations` produces a shape the later transform cannot read.
3. Run `nix fmt`. The transform emits double quotes and its own line breaks.
4. Read the label set back with two independent instruments and require them to agree with each
   other and with the pre-conversion reading.
5. Run the three checks.

## Acceptance Criteria

- Every file the codemod can take has been taken, with the skips listed. Expected: no skips.
- The commit contains no hand edits.

Added, because neither of the two above can detect the failure this task is exposed to: the label
set after conversion is identical to the label set before, all 258 of them, and the per-file
ordered sequence of labels is identical too.

## Verification Plan

The instrument is the label set, not the exit status. `task-014` established that the chain reports
`1 ok, 0 errors` while discarding a registration, so a clean run says nothing about whether stories
survived.

- Before: the committed extractor over the `storiesOf` corpus.
- After: an AST reading of the converted files that resolves each export's effective label the way
  Storybook does, an explicit `name` winning and `startCase` of the export identifier otherwise.
- After, second instrument: `index.json` from a real Storybook build, which is Storybook's own
  indexer rather than a model of it. The two after-readings must agree with each other before either
  is compared to the before-reading.
- `nix build` for `compile`, `lint` and `storybook`.
- A grep for `storiesOf` across the tree, expecting nothing.

## Risks and Open Questions

- The chain reports success while dropping work. This is measured rather than trusted.
- The sanitizer renames an export and re-attaches a display name that differs by a character. This
  is why the comparison is on the label, not on the export identifier. `task-023` re-checks each of
  these individually against the baseline.
- Within-panel order has no explicit marker after conversion. It is compared as a sequence, not as a
  set.
- `nix fmt` runs after the transform and before the measurement, so the measurement reads what is
  committed rather than what the transform emitted.

## Required Docs, Research, and Tracking Updates

- Set `task-017.status` to `completed`; record the measured label set, the two instruments, and the
  CLI constraints.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-017-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-017-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- All 68 files converted, formatted, and measured.

## Final Outcome

- 68 of 68 converted, `0 errors, 0 skipped`, and no file imports `storiesOf` any more.
- The label set is unchanged: 258 registrations across 49 panels, before and after, and the two
  after-instruments agree with each other exactly. Per file, the title and the ordered sequence of
  labels are identical for all 68.
- `compile` moves from 68 `TS2305` to 19 `TS2339`, in one shape, in 8 files. `csf-2-to-3` cannot
  turn `export const X = ImportedStory` into an object, so it leaves `X.parameters = {}` and
  `X.storyName = ''` attached to a value TypeScript has typed as `() => Element`.
- `lint` moves from 0 errors to 49, all `react/function-component-definition`, across 25 files. The
  rule fires on `render: () => <X />`, which is what CSF 3 asks for and what the old `.add()`
  argument position did not trigger.
- `storybook` reaches the preview build for the first time and fails there, in
  `storybook/main.ts` rather than in any story: it takes `ProvidePlugin` from the root `webpack`,
  5.106.2, while `@storybook/builder-webpack5` runs its own nested 5.111.0. The two `Dependency`
  classes are not the same class, so the plugin's `loc` write throws and 156 modules fail to parse.
  Not conversion work and fixed separately.

## Self-Review

- The first attempt converted 1 of the 64 files under `storybook/stories` and reported
  `0 errors, 1 ok`. The cause is in the tool, and it is worth stating precisely because it is not
  visible from the outside: `storybook migrate` re-launches itself through
  `child_process.spawn(..., { shell: true })`, so `--glob` is expanded by `/bin/sh` before the CLI
  reads it. `/bin/sh` has no `**`, so the pattern collapsed to one directory level, the first match
  became the glob and the other 63 became ignored arguments. Driving the transform through
  `@storybook/codemod`'s own entry point removes the shell.
- That is the same failure the telemetry defect had: a run that reports success for work it did not
  do. It was caught by reading the file count rather than the exit status, and it would have been
  caught again by the label set.
- Checking formatting with `node_modules/.bin/prettier` was checking against a formatter this
  repository does not run. `.eslintrc` extends `eslint-config-prettier`, which only disables rules,
  so nothing runs the pinned 2.1.2; `nix fmt` runs 3.6.2 and is the only formatter with authority.
  One file was committed earlier on the strength of the wrong instrument and is corrected in its own
  commit.
