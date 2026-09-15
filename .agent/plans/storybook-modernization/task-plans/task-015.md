# Task task-015: Open the hop branch: manifest, framework field, binaries and preview object

## Task ID and Title

- ID: `task-015`
- Title: `Open the hop branch: manifest, framework field, binaries and preview object`

## Why Chosen Now

`task-015.dependencies` is `[task-014, task-063]` and both are complete. It is the configuration half
of the hop and the commit that opens the red window.

Everything that could be done green has been. `task-013` found the one configuration defect that
would otherwise have surfaced here disguised as a React incompatibility, `task-014` measured the
codemod, and the prep commit at `40e9eecbf` removed both shapes the codemod cannot convert. This
task changes the manifest and nothing else that can be deferred.

## The red window

From this commit, `yarn compile` is red and stays red until `task-025`.

`tsconfig.json` declares no `include` and excludes only `node_modules`, so every story file is in the
`tsc --noEmit` program. At 8.6.x `@storybook/react` no longer exports `storiesOf`, so all 68
`storiesOf` call sites fail the moment the manifest moves, whether or not the indexer sees them.
There is no ordering of this work that keeps compile green: the manifest and the corpus have to
change together, and they are too large to change in one commit anyone could review.

**What closes it:** `task-017` runs the codemod over the whole corpus, `task-018` through `task-022`
hand-finish the five tranches, `task-023` diffs every label against the baseline, `task-024` removes
the `StoryWrapper` prop pass-through, and `task-025` verifies and merges. `task-017` is what removes
the last `storiesOf` import and therefore what makes `compile` green again; the tranches and the
label diff are what make the result correct rather than merely compiling.

Hydra will report red on #3405 for that span. The pull request is draft and that is the intended
state, not something to be worked around.

## Interaction Mode

- Mode: `agent_execution`

The host carries yarn 1.22.21, which is exactly the version `package.json` pins, so the lockfile this
task regenerates is the one the pinned package manager produces.

## Scope

- Move the Storybook dependency set to 8.6.18 by hand, add `@storybook/react-webpack5` and the
  `storybook` CLI package, and remove the four packages that no longer exist in the 8 line.
- Bump `@storybook/addon-knobs` from 6.4.0 to 8.0.1.
- Rename the CLI binaries in the two scripts, keeping the script names.
- Add the mandatory `framework` field and carry the `config.resolve` merge fix from `task-013`.
- Convert `preview.tsx` to a default-export Preview object.
- Regenerate `yarn.lock`.

## Non-Goals

- No story file changes. `task-017` and the tranches own the corpus.
- No `storybook upgrade`. Locked decision 12 settles it: the CLI drives installs through the detected
  package manager, and Yarn 1 is the least tested path.
- No change to the four legacy-decorator settings, which carry across unchanged.
- No change to the script names `storybook` and `storybook:build`, which `perSystem/checks.nix:78`
  invokes.

## Dependencies

- `task-014` and `task-063`, both complete, plus the prep commit `40e9eecbf`.
- `task-016` and `task-017` depend on this task.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decisions 1 and 12
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-015` and the
  phase 3 header
- `.agent/plans/storybook-modernization/task-plans/task-013.md`, for the `config.resolve` finding,
  the version facts and the signature rule
- `.agent/plans/storybook-modernization/task-plans/task-014.md`, for the codemod decision
- `.agent/plans/storybook-modernization/task-plans/task-007.md`, for the lockfile gating precedent

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`, and the note there that `yarn install` outside the Nix shell builds
  native modules wrong on this machine
- Workflows: `.agent/workflows/storybook.md`, which this task makes wrong and `task-060` rewrites
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `40e9eecbf`, 2026-09-15.

- The host `yarn --version` is 1.22.21 and `package.json` pins
  `yarn@1.22.21+sha1.1959a18351b811cdeedbd484a8f86c3cc3bbaf72`, so the lockfile regenerated here is
  the pinned resolver's own output. The flake's yarn is 1.22.22, a patch apart.
- The Storybook packages to remove, all pinned at 6.4.22: `@storybook/builder-webpack5`,
  `@storybook/manager-webpack5`, `@storybook/core`, `@storybook/addons`. None has an 8.x release
  under those names.
- `@storybook/addon-knobs` is pinned at 6.4.0 and its last release is 8.0.1, which peers
  `@storybook/theming`, `@storybook/components`, `@storybook/core-events` and
  `@storybook/manager-api` at `^8.0.0`. Yarn 1 does not install peers, and the `task-013` pre-flight
  had to declare all four for the addon to resolve, so they are declared here too.
- The `config.resolve` merge fix is mandatory, not cosmetic. `@storybook/react-dom-shim`'s own
  preset sets `resolve.alias['@storybook/react-dom-shim']` to its `react-16` build whenever
  `react-dom` is below 18, and the current `webpackFinal` assigns `config.resolve` wholesale, which
  discards it and makes the preview resolve `react-dom/client`. Measured in `task-013`.
- The four legacy-decorator settings, unchanged by this task: `tsconfig.json:17`
  `experimentalDecorators`; and in the `swc-loader` rule, `parser.decorators`,
  `transform.legacyDecorator` and `transform.useDefineForClassFields`. A dropped setting fails at
  runtime, not at build, so nothing in the check set would catch it.
- `perSystem/checks.nix:78` runs `yarn storybook:build`, so the script name is load-bearing while the
  binary behind it is not.
- The corpus is 68 `storiesOf` calls across 68 files after the prep split, every label a string
  literal, no file with two titles.
- The checks are green at `40e9eecbf`. This is the last commit at which they are.

## Files Expected To Change

- `package.json`, the Storybook dependency set and the two script bodies
- `yarn.lock`, regenerated
- `storybook/main.ts`, the `framework` field, the removal of `core.builder`, and the
  `config.resolve` merge
- `storybook/preview.tsx`, converted to a default-export Preview object

Tracking: `task-015.status` and the three `task-015` plan documents.

## Implementation Approach

1. Edit `package.json` by hand: remove the four dead packages, move the three surviving Storybook
   packages to 8.6.18, add `storybook` and `@storybook/react-webpack5` at 8.6.18, add the four
   packages `@storybook/addon-knobs@8.0.1` peers, and bump the addon.
2. Change the two script bodies from `start-storybook` and `build-storybook` to `storybook dev` and
   `storybook build`, keeping both script names.
3. Add `framework: { name: '@storybook/react-webpack5', options: {} }` to `main.ts` and remove
   `core: { builder: 'webpack5' }`, which the framework package now supplies.
4. Apply the `config.resolve` merge fix, spreading `config.resolve` and merging `extensions` and
   `fallback` rather than assigning over them, with a comment recording why.
5. Convert `preview.tsx` to `export default { decorators, parameters }`, keeping the frozen clock and
   the two side-effect imports.
6. Regenerate `yarn.lock` with the pinned yarn, using `--ignore-scripts` so no native module is
   built on a host where `libudev` and `libusb` are absent. The lockfile is what matters here; the
   `node_modules` the Nix build produces is the one the checks use.
7. Gate on `nix build .#internal.x86_64-linux.node_modules`, which installs with `--frozen-lockfile`
   and is what decides whether the manifest and the lockfile agree.
8. Run `storybook` and `compile`. Both are expected to fail, and the plan states in advance what an
   acceptable failure looks like: `compile` failing only on `storiesOf` imports, and `storybook`
   failing on the corpus rather than on configuration. A configuration failure is not acceptable and
   is fixed before the commit lands.
9. Land it as one signed commit.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- The manifest resolves and `yarn install` completes on Yarn 1 without a partially hoisted Storybook
  tree. Evidence: the pinned yarn resolves it, and `.#internal.x86_64-linux.node_modules` rebuilds
  under `--frozen-lockfile`, which is the stronger of the two because it is what the checks consume.
- `yarn storybook:build` fails only on unconverted `storiesOf` files, not on configuration.
  Evidence: the failure output is read and classified. A `Can't resolve` on a story file's
  `storiesOf` import is expected; anything naming `main.ts`, `preview.tsx`, the framework or the
  builder is not.

Added for this plan:

- No `@storybook/addons` remains anywhere in the manifest or the lockfile, which is what `task-063`
  cleared the way for.
- The four legacy-decorator settings are present after the edit, checked rather than assumed.
- `compile`'s failures are all `storiesOf`-related, counted, so the number that has to go to zero is
  known before the conversion starts.

## Verification Plan

To run:

- `yarn install --ignore-scripts` with the pinned yarn, then `git diff --stat yarn.lock`.
- `nix build --no-link .#internal.x86_64-linux.node_modules`.
- `nix build --no-link .#checks.x86_64-linux.compile`, expected red; the error list is captured and
  classified.
- `nix build --no-link .#checks.x86_64-linux.storybook`, expected red; the failure is classified as
  corpus or configuration.
- A grep for the four decorator settings.
- A grep for `@storybook/addons` across the manifest and the lockfile.

If `storybook` fails on configuration rather than on the corpus, this task is not done and the
failure is fixed before committing. If the `node_modules` build fails, the manifest and lockfile
disagree and the yarn error is read rather than the edit reverted blindly.

## Risks and Open Questions

- This commit makes the trunk red for the length of the conversion. That is inherent to the phase and
  is stated at the top of this plan rather than discovered from a failing check.
- Regenerating the lockfile on the host rather than in the Nix shell is the one step that depends on
  the local toolchain. It is mitigated by the host yarn being the pinned version and by the
  `node_modules` derivation being the arbiter of the result.
- `--ignore-scripts` means no native module is built locally. That is deliberate: this repository's
  own guidance is that `node-hid` and `usb` build wrong here, and the lockfile does not depend on
  them building.
- If `@storybook/addon-knobs@8.0.1` turns out to need something the pre-flight did not exercise, the
  route through 8.6.x is in question. The pre-flight rendered it, so this is unlikely rather than
  untested.
- Rollback is `git revert` of a single commit, which restores the manifest, the lockfile and both
  configuration files together.

## Required Docs, Research, and Tracking Updates

- Set `task-015.status` to `completed`.
- Record in the entry: the `config.resolve` fix and why it is mandatory, the four peer packages the
  knobs addon needs under Yarn 1, the measured `compile` error count that has to reach zero, and
  what closes the red window.
- No PRD change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-015-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-015-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The manifest is at Storybook 8.6.18 with the framework field, the four dead packages gone and the
  knobs addon at 8.0.1. The lockfile is regenerated and agrees with it under `--frozen-lockfile`.
- `main.ts` carries the `config.resolve` merge fix and all four legacy-decorator settings.
- `preview.tsx` is a default-export Preview object with the frozen clock intact.

## Final Outcome

- `task-015` completed. The red window is open. `task-016` is unblocked.
- `compile` is red with exactly 70 errors, all classified: 68 `TS2305` for `storiesOf` and 2
  `TS2307` for `@storybook/addons`. Nothing outside those categories. 70 is the number that reaches
  zero at `task-017` plus the tranches, with the 2 going at `task-016`.
- `storybook` is red on the DaedalusMenu addon, which `task-016` removes.
- `lint`, `i18n` and `stylelint` are all green.
- Three defects found and fixed here: `yarn storybook:build` exiting 0 on a failed build, and two
  dependencies the 6.4.22 tree had been supplying transitively.

## Self-Review

- The most important finding came from distrusting a green check. `storybook` passed on the first
  run after a manifest edit that had just removed a package the local addon imports, which is not a
  result a correct hop produces either. Treating the unexpected pass as a defect rather than as
  success is what surfaced the telemetry prompt swallowing the exit code.
- Classifying all 70 compile errors, rather than noting that compile is red, is what makes the
  window measurable. Anything that appears later outside those two categories is a new defect rather
  than part of the expected state.
- When `os-browserify` turned out to be undeclared, the response was to check every module
  `webpackFinal` resolves in one pass rather than to fix it and rebuild. That found
  `@types/webpack-env` in the same step instead of one build later.
- The second acceptance criterion is not met as worded and the entry says so, with the probe that
  establishes what is behind the addon failure, rather than the criterion being reported as
  satisfied.
