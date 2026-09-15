# Task task-007: Remove the orphaned support modules and the unused dependency

## Task ID and Title

- ID: `task-007`
- Title: `Remove the orphaned support modules and the unused dependency`

## Why Chosen Now

`task-007.dependencies` is `[task-002, task-003]` and both are complete, which is the point of the
dependency: the task exists to re-verify orphan status *after* those deletions rather than to act on
a list written before them. `task-004` has also landed, so the re-verification covers all three of
the phase's removals.

It is the last task in phase 1. When it lands, `task-010` is unblocked and the glob can replace the
barrel.

## Interaction Mode

- Mode: `agent_execution`

The `package.json` and `yarn.lock` edits and every check reproduce here. The four file removals are
performed by the operator against the list this task produces.

## Scope

- Re-verify which modules under `storybook/` are unreachable from the workbench's entry points, now
  that `task-002`, `task-003` and `task-004` have landed.
- Remove the modules that re-verification confirms are orphaned.
- Remove `storybook/preview-head.html`, which is zero bytes.
- Remove `storybook-addon-swc` from `package.json` and its entry from `yarn.lock`.

## Non-Goals

- No change to `storybook/main.ts`. The handwritten `swc-loader` rule in `webpackFinal` at `:68-95`
  is what compiles the corpus and it is unaffected by removing the addon; the settled decision in
  the PRD keeps that rule rather than re-deriving it through
  `@storybook/addon-webpack5-compiler-swc`.
- No change to the `swc-loader` or `@swc/core` entries in `package.json`. Both are direct
  dependencies that `storybook/main.ts` uses.
- No removal of any module under `storybook/stories/wallets/_utils/` that re-verification finds
  reachable. Five of the seven modules in that directory are reachable and stay.
- No CSF conversion, no knob work, no glob.

## Dependencies

- `task-002` and `task-003`, both complete. `task-004` is also complete and is included in the
  re-verification even though the entry does not name it.
- No task depends on `task-007`. `task-010` is gated on `task-062`, which is complete.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the settled-by-evidence
  paragraph listing the two cleanups that need no discussion, at `:360-366`, and the webpack 5
  builder decision above it
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-007`
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`
- `.agent/plans/storybook-modernization/task-plans/task-002.md`, `task-003.md` and `task-004.md`,
  whose removals this task re-verifies against
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, read for the entry-point model only, per the task-plans
    readme's caution.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `e6ba8759b`, 2026-09-14, against the working tree.

- Orphan status was re-verified by walking the module graph from every entry point the workbench
  has, not by grepping for importers. The entry points are `storybook/main.ts`,
  `storybook/preview.tsx`, `storybook/stories/index.ts` and
  `storybook/addons/DaedalusMenu/register.tsx`, the last of which `main.ts:13` names through
  `require.resolve`. The walk follows static imports, re-exports, `require()` and `require.resolve()`.
- The result: 119 TypeScript files under `storybook/`, 116 reachable, 3 orphaned. The three are
  exactly the three the task entry names, and no fourth module was orphaned by `task-002`,
  `task-003` or `task-004`. The re-verification the dependency exists for was worth running and
  changed nothing.
- `storybook/stories/staking/StakingWrapper.tsx` has no importer anywhere in `storybook/`, `source/`
  or `tests/`.
- `storybook/stories/wallets/_utils/HardwareWalletWithNavigationLayout.tsx` has no importer. Note
  that `storybook/stories/wallets/_utils/HardwareWalletsWrapper.tsx` is a different file with a
  similar name, it is reachable, and it stays.
- `storybook/stories/wallets/_utils/defaultWalletProps.tsx` has no importer. The three grep hits for
  that identifier are a local `const defaultWalletProps` in
  `source/renderer/app/components/wallet/tokens/wallet-token/WalletToken.spec.tsx:15`, which is an
  unrelated name collision in a spec file this task does not touch.
- Removing the three cascades to nothing. Between them they import `_support/StoryLayout`,
  `_support/StoryProvider` and `_support/StoryDecorator`, all of which many surviving files import,
  and otherwise only components under `source/`.
- `storybook/stories/wallets/_utils/` holds seven modules. Two go and five stay:
  `CreateWalletScreens.tsx`, `HardwareWalletsWrapper.tsx`, `WalletWithNavigationLayout.tsx`,
  `WalletsTransactionsWrapper.ts` and `WalletsWrapper.tsx` are all reachable. The directory survives.
- `storybook/preview-head.html` is zero bytes. Storybook 6.4 loads
  `<configDir>/preview-head.html` when it exists and injects its contents into the preview `<head>`,
  so an empty file injects nothing and removing it changes nothing. No file references it by name.
- `storybook-addon-swc` is declared at `package.json:175` as `"1.1.7"` and is referenced nowhere in
  the repository. A grep across `storybook/` and `source/` returns nothing.
- Removing it does not change the build. The SWC rule Storybook uses is handwritten in
  `webpackFinal` at `storybook/main.ts:68-95` and carries the four legacy-decorator settings the
  corpus depends on, `decorators: true`, `legacyDecorator: true`, `useDefineForClassFields: false`
  and `target: 'es2019'`. The addon is not in the `addons` array at `main.ts:9-14`.
- The `yarn.lock` entry is a single self-contained block at `:19130-19137`, declaring four
  dependencies: `@babel/runtime@^7.17.2`, `@swc/core@^1.2.152`, `deepmerge@^4.2.2` and
  `swc-loader@^0.1.15`.
- Two of those four ranges have `storybook-addon-swc` as their only requester in the whole lockfile.
  `"@swc/core@^1.2.152"` at `:3837` resolves to 1.2.175, and `swc-loader@^0.1.15` at `:19559`
  resolves to 0.1.16. Both are distinct from the pinned direct dependencies the repository actually
  uses, `"@swc/core@1.10.18"` at `:3818` and `swc-loader@0.1.15` at `:19553`, which are declared at
  `package.json:93` and `:183` and are untouched.
- `@babel/runtime` and `deepmerge` each have a single lockfile entry serving many requesters and are
  unaffected.
- `nix/internal/common.nix:315` writes `'"--frozen-lockfile" true'` into `.yarnrc` for the
  `node_modules` build, and `offlineCache` at `:298` is derived from `yarn.lock` through
  `mkYarnNix`. A manifest and lockfile that disagree are therefore a build-time failure rather than a
  silent drift, which makes the `node_modules` derivation the test of whether the edit is correct.
- The checks are green at `e6ba8759b`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n}` all exit 0, and
  `compile` is `v24rzf5lhqk54xm4s0z9mmh019ixvzzi-daedalus-compile.drv`.
- The sidebar stands at 258 registrations across 49 titles in 14 groups, with `UNREACHABLE 0`. None
  of the four files this task removes registers a story, so every one of those numbers must be
  unchanged afterwards. That is the strongest available statement of correctness for this task: a
  change that removes only unreachable modules must move nothing.

## Files Expected To Change

Removed, four files:

- `storybook/stories/staking/StakingWrapper.tsx`
- `storybook/stories/wallets/_utils/HardwareWalletWithNavigationLayout.tsx`
- `storybook/stories/wallets/_utils/defaultWalletProps.tsx`
- `storybook/preview-head.html`

Edited, two files:

- `package.json`, the `storybook-addon-swc` line
- `yarn.lock`, the `storybook-addon-swc@1.1.7` block

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-007.status`
- `.agent/plans/storybook-modernization/task-plans/task-007.md`
- `.agent/plans/storybook-modernization/task-plans/task-007-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-007-impl-review.md`

## Implementation Approach

1. Remove the `storybook-addon-swc` line from `package.json` and the matching
   `storybook-addon-swc@1.1.7` block from `yarn.lock`, keeping the edit to the one block the
   manifest change invalidates.
2. Leave the two now-unrequested range entries, `"@swc/core@^1.2.152"` and `swc-loader@^0.1.15`, in
   place. They are the transitive tail of the removed package and yarn prunes such entries on its
   next full install rather than being broken by them. Removing them by hand would mean removing
   thirteen `@swc/core-*@1.2.175` platform entries behind one of them, and a hand-pruned lockfile is
   harder to trust than a lockfile with a stale tail. This is a deliberate choice and is recorded as
   one.
3. Build `.#internal.x86_64-linux.node_modules` before running any check. That derivation is what
   every check copies in, it installs with `--frozen-lockfile`, and its `offlineCache` is derived
   from `yarn.lock`, so it is the thing that decides whether the manifest and the lockfile agree. If
   it fails, the lockfile edit is wrong and the failure is read rather than worked around.
4. Write the four paths to the deletion list and hand off.
5. Re-run the orphan walk after the removals. It must report zero orphans: the three that went were
   the only three, and nothing they imported was reachable only through them.
6. Regenerate the sidebar and require every count to be identical to the pre-change capture.
7. Run `compile`, `lint` and `storybook` as flake checks.
8. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- A grep confirms no importer for each removed module at the time of removal. Evidence: the module
  graph walk, which is stronger than a grep because it answers reachability rather than mention, and
  a per-name grep alongside it to catch a dynamic reference the walk would miss.
- `yarn storybook:build`, `yarn compile` and `yarn lint` pass. Evidence:
  `nix build .#checks.x86_64-linux.{storybook,compile,lint}` all succeed on a derivation that moved.

Added for this plan:

- `.#internal.x86_64-linux.node_modules` builds, which is what proves the `package.json` and
  `yarn.lock` edits agree under `--frozen-lockfile`.
- The orphan walk reports zero orphans afterwards.
- Every sidebar count is unchanged: 258 registrations, 49 titles, 14 groups, `UNREACHABLE 0`.

## Verification Plan

Already run for planning:

- The module graph walk from all four entry points: 119 files, 116 reachable, 3 orphaned, matching
  the task entry exactly.
- Per-name grep for each of the three, including the `defaultWalletProps` name collision in an
  unrelated spec file.
- The import list of each of the three, to confirm no cascade.
- The `storybook/stories/wallets/_utils/` inventory, five reachable and two not.
- The `yarn.lock` block and the requester count for each of its four dependencies.
- `storybook/main.ts` read for the SWC rule and the `addons` array.

To run for the build:

- `nix build --no-link .#internal.x86_64-linux.node_modules`, before anything else.
- `git status --short`: four deletions, two modifications, nothing untracked outside `.agent/`.
- The orphan walk, expecting zero.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, expecting
  every count identical to the pre-change capture.
- `nix build --no-link .#checks.x86_64-linux.{storybook,compile,lint}`.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a moved path.

If the `node_modules` build fails, the manifest and lockfile disagree and the response is to read
the yarn error rather than to revert the lockfile edit blindly. If the sidebar moves at all,
something reachable was removed and the change is wrong.

## Risks and Open Questions

- A hand-edited lockfile is the only part of this task that is not mechanically verifiable by
  reading the diff, which is why the `node_modules` build is run first and treated as the gate.
- The stale range entries left behind, `"@swc/core@^1.2.152"` and `swc-loader@^0.1.15`, are a
  deliberate residue. They cost one tarball each in the offline cache and disappear the next time
  anyone runs a full `yarn install`. The alternative was a fourteen-block hand prune.
- `HardwareWalletWithNavigationLayout.tsx` and `HardwareWalletsWrapper.tsx` differ by three
  characters and one goes while the other stays. The deletion list is written from `git ls-files`
  output rather than typed, and the survivor is named in this plan so a mistake in either direction
  is visible.
- The three orphans are Storybook wrappers of the kind phase 6 will want when it builds the
  container harness. They are not a template for it: `task-039` onwards specifies that harness
  against the store map, and these are `storiesOf`-era layout wrappers that the phase 3 conversion
  would have had to rewrite anyway. They remain in history.
- Rollback is `git revert` of a single commit. The lockfile and manifest revert together, so the
  `node_modules` derivation returns to its current hash.

## Required Docs, Research, and Tracking Updates

- Set `task-007.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Record in the entry that the re-verification covered `task-004` as well as the two dependencies it
  names, and that it found no additional orphan.
- No PRD change. The settled-by-evidence paragraph describing both cleanups remains accurate.
- No research-note change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-007-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-007-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The three orphaned modules and the zero-byte `storybook/preview-head.html` are gone.
- `storybook-addon-swc` is out of `package.json` and its block is out of `yarn.lock`.
- `storybook/stories/wallets/_utils/` keeps its five reachable modules, including
  `HardwareWalletsWrapper.tsx`.

## Final Outcome

- `task-007` completed, and with it phase 1. `task-010` is unblocked.
- The orphan walk now reports 116 files under `storybook/`, 116 reachable, 0 orphaned. Before the
  removal it reported 119, 116 and 3. Nothing was orphaned by the removal itself, which is what the
  absence of a cascade predicted.
- The sidebar is byte-identical to the pre-change capture under `cmp`: 258 registrations, 49
  distinct titles, 14 groups, `UNREACHABLE 0`. A change that removes only unreachable modules must
  move nothing, and it moved nothing.
- `.#internal.x86_64-linux.node_modules` rebuilt clean in 3 minutes 24 seconds with the edited
  manifest and lockfile. That build installs with `--frozen-lockfile` and derives its offline cache
  from `yarn.lock`, so it is what establishes that the two edits agree rather than merely look
  consistent.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile`, `.lint` and `.i18n` all exit 0,
  on a `compile` derivation that moved to `dg7zrim49pnfimsngzy20z7cfjngbbjp-daedalus-compile.drv`.
- `yarn lint` reports 5391 warnings against the 5410 the corpus carried after `task-004`.

## Self-Review

- The re-verification this task exists for returned exactly what the entry predicted. That is worth
  recording rather than passing over: the dependency on `task-002` and `task-003` was taken out on
  the chance the answer had changed, and the way to know it had not was to walk the graph rather
  than to trust the list.
- Reachability was measured by a module graph walk from all four entry points, not by grep. A grep
  answers whether a name is mentioned; the question here is whether the workbench loads a file, and
  `storybook/stories/staking/Legacy.stories.tsx` was the standing proof in this repository that
  those are different questions.
- The lockfile edit is the one change in this phase whose correctness cannot be read off the diff,
  so it was gated on the derivation that performs the install rather than on inspection.
- The two unrequested range entries left in `yarn.lock` are a stated trade rather than an oversight,
  and the commit body says so, because a reviewer finding `swc-loader@^0.1.15` still present after
  its only requester has gone would otherwise be right to ask.
- Scope held. `storybook/main.ts` is untouched, both SWC dependencies the build actually uses are
  untouched, and five of the seven modules in the `_utils` directory stay.
