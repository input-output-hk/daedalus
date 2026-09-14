# Task task-010: Replace the barrel with a glob and rehome its side effects

## Task ID and Title

- ID: `task-010`
- Title: `Replace the barrel with a glob and rehome its side effects`

## Why Chosen Now

`task-010.dependencies` is `[task-002, task-003, task-004, task-007, task-062]` and all five are
complete. It is the first task of phase 2 and gates `task-011` and `task-063`.

The reason it matters is narrower and sharper than "the barrel is hand-maintained". `yarn compile`
does not report an unresolvable relative import in this repository, which was measured during
`task-006` with a probe and recorded as a correction in `task-062-impl-review.md`.
`yarn storybook:build` does report one, but only for a module the indexer reaches. Under the barrel
the indexer reaches exactly what someone remembered to list, so a story file that is never listed is
invisible to the only check that would notice. `storybook/stories/staking/Legacy.stories.tsx` sat in
that state for years and `task-003` deleted it one commit ago. Replacing the barrel with a glob
widens what the required check can see from a hand-written list to the corpus on disk, which is an
increase in coverage rather than a tidying of configuration.

## Interaction Mode

- Mode: `agent_execution`

The `storybook/main.ts` edit and every check reproduce here. The nine file removals are performed by
the operator against the list this task produces.

## Scope

- Replace the single barrel entry in `storybook/main.ts` with two glob entries covering both story
  naming conventions and both locations.
- Remove the seven barrel `index.ts` files and the two import-only `_support` modules that only a
  barrel reached.

## Non-Goals

- No change to `storybook/preview.tsx`. Both side effects the task entry asks to rehome are already
  there. See the findings.
- No sidebar ordering work. `task-011` owns it, and this task is expected to reorder the tree.
- No CSF conversion, no default exports, no change to any registration.
- No change to the 13 remaining `_support` modules that register their siblings' exports. They stay
  reachable from the files that register them, which is the condition `task-062` created them for.

## Dependencies

- `task-002`, `task-003`, `task-004`, `task-007` and `task-062`, all complete. The first four had to
  land so the glob indexes a corpus that has already been reduced; `task-062` had to land so the 15
  register-nothing files no longer match the story naming convention.
- `task-011` and `task-063` depend on this task.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the settled-by-evidence
  paragraph on replacing the barrel with a glob at `:355-359`, and locked decision 16 at `:338-366`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-010`, `task-011`,
  `task-012` and `task-063`
- `.agent/plans/storybook-modernization/task-plans/task-062.md`, which recorded that these two
  import-only `_support` modules disappear when this task deletes the barrel
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/task-006-impl-review.md` and
  `task-062-impl-review.md`, for the measured limits of `yarn compile`
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, read for the barrel model it documents, which this task
    invalidates. `task-060` rewrites it at the end of the epic.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `5311ce0d0`, 2026-09-15, against the working tree.

- `storybook/main.ts:8` is `stories: ['../storybook/stories/index.ts']`, a single entry naming the
  barrel.
- **Both side effects are already in `storybook/preview.tsx` and have been throughout.** `:4` imports
  the global theme stylesheet and `:6` imports `./stories/_support/environment`. The task entry asks
  for them to be moved there; what is actually required is to stop loading the barrel's duplicate
  copies of them.
- The two copies of the stylesheet import are not equivalent, and the one that survives is the
  correct one. `storybook/stories/index.ts:1` imports
  `'../../source/renderer/app/themes/index.global.scss'` as a plain specifier, which goes through
  the `.scss` rule in `webpackFinal` at `storybook/main.ts:96-118`, where `css-loader` is configured
  with `modules: { localIdentName: '[name]_[local]' }`. That rewrites every class name in a
  stylesheet whose entire purpose is to be global. `preview.tsx:4` imports it as
  `'!style-loader!css-loader!sass-loader!...'`, an inline loader chain that bypasses the rule and so
  runs `css-loader` without `modules`, emitting the class names unchanged. Deleting the barrel
  removes the scoped duplicate and leaves the unscoped one.
- The corpus is 65 files matching the story naming conventions: 61 under `storybook/stories/` and 4
  colocated under `source/renderer/app/`. Every one of the 65 contains a `storiesOf` call, so the
  register-nothing class `task-062` created is still empty.
- There is no `*.story.ts` file anywhere. The two `.story.tsx` files are both under
  `features/discreet-mode/`. The glob still admits `.story.ts` so the convention is covered rather
  than the current file list.
- The proposed entries are:

      '../storybook/stories/**/*.stories.@(ts|tsx)',
      '../source/renderer/app/**/*.@(stories|story).@(ts|tsx)',

- 6.4.22 does not expand these with a file-system glob at config time. `normalizeStories` splits each
  entry into a `directory` and a `files` pattern, and `toRequireContext` converts the pattern to a
  regular expression for a webpack `require.context`. Resolved through the installed
  `@storybook/core-common@6.4.22`, the two entries give directories `./storybook/stories` and
  `./source/renderer/app`, both recursive, with these matchers:

      /^\.(?:(?:^|\/|(?:(?:(?!(?:^|\/)\.).)*?)\/)(?!\.)(?=.)[^/]*?\.stories\.(ts|tsx))$/
      /^\.(?:(?:^|\/|(?:(?:(?!(?:^|\/)\.).)*?)\/)(?!\.)(?=.)[^/]*?\.(stories|story)\.(ts|tsx))$/

- Applying those two regular expressions to the tracked file list matches exactly 65 files: 61 of
  the 110 tracked under `storybook/stories`, and 4 of the 1508 tracked under `source/renderer/app`.
  That is the authoritative check at this version, because it is the same regular expression webpack
  will use. An independent expansion with `globby` from the same `node_modules` agrees at 65.
- **No `_support` module matches either regular expression.** That is what `task-062` was for, and it
  is verified rather than assumed: at 6.4.22 a matched file with no `storiesOf` call and no CSF meta
  would be auto-titled by `StoryStoreFacade` and its named exports registered, which would move the
  `task-001` baseline silently.
- Seven barrel files exist and every one is a pure list of side-effect imports with no other content:
  `storybook/stories/index.ts` (37 lines after phase 1), `loading/index.ts`,
  `loading/chain-storage/index.ts`, `loading/mithril/index.ts`, `nodes/index.ts`, `settings/index.ts`
  and `wallets/index.ts`. None matches the glob, so all seven stop being loaded the moment the entry
  changes, and all seven become unreferenced.
- Two of the 15 files `task-062` renamed are import-only barrels rather than support modules, and
  each has exactly one importer, which is a barrel this task deletes.
  `storybook/stories/nodes/status/_support/Status.ts` is one line, `import '../Diagnostics.stories';`,
  imported only by `nodes/index.ts:3`. `storybook/stories/wallets/addWallet/_support/AddWallet.ts` is
  five such lines, imported only by `wallets/index.ts:14`. Under the glob the six story files they
  reach are matched directly, so both become orphans. `task-062`'s plan recorded this outcome in
  advance.
- The other 13 `_support` modules keep their importers, which are the story files that register their
  exports. None is affected.
- The sidebar stands at 258 registrations across 49 distinct titles in 14 groups, `UNREACHABLE 0`.
  Membership must not change. Order is expected to change and `task-011` fixes it.
- The checks are green at `5311ce0d0`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n,stylelint}` all exit 0.

## Files Expected To Change

Edited, one file:

- `storybook/main.ts`, the `stories` entry

Removed, nine files:

- `storybook/stories/index.ts`
- `storybook/stories/loading/index.ts`
- `storybook/stories/loading/chain-storage/index.ts`
- `storybook/stories/loading/mithril/index.ts`
- `storybook/stories/nodes/index.ts`
- `storybook/stories/settings/index.ts`
- `storybook/stories/wallets/index.ts`
- `storybook/stories/nodes/status/_support/Status.ts`
- `storybook/stories/wallets/addWallet/_support/AddWallet.ts`

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-010.status` and
  the corrections its prose carries
- `.agent/plans/storybook-modernization/task-plans/task-010.md`
- `.agent/plans/storybook-modernization/task-plans/task-010-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-010-impl-review.md`

`storybook/preview.tsx` is listed in the task's `targetPaths` and is not edited, because the side
effects it is supposed to receive are already in it.

## Implementation Approach

1. Replace the single `stories` entry in `storybook/main.ts` with the two globs. Cover the
   convention rather than the current file list: admit `.story.ts` even though no such file exists,
   so a file added under that spelling is indexed rather than silently absent, which is the whole
   point of the change.
2. Build `storybook` as a flake check with the barrels still present. They no longer match anything
   in `stories`, so this first build tests the glob alone: if the corpus is indexed, every
   registration arrives through `require.context` and nothing arrives through a barrel. A green
   check here and an unchanged registration count is the evidence that the glob is complete before
   anything is deleted.
3. Write the nine paths to the deletion list and hand off. The two `_support` barrels go with the
   seven `index.ts` files rather than in a later task, because they are orphaned by this change and
   by nothing else.
4. After the removals, re-run the orphan walk with the entry points updated: `main.ts`,
   `preview.tsx` and the 65 glob-matched files, since `stories/index.ts` is no longer a root. Expect
   zero orphans.
5. Regenerate the sidebar and require membership to be unchanged: 258 registrations, 49 titles, 14
   groups, `UNREACHABLE 0`. The extractor reads source rather than the indexer, so this proves the
   corpus did not change; step 2 proves the indexer sees it.
6. Run `compile`, `lint` and `storybook` as flake checks, and compare the `storybook` build time
   against its previous run, because `require.context` over `source/renderer/app` makes webpack scan
   a tree of 1508 tracked files.
7. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- `yarn storybook:build`, `yarn compile` and `yarn lint` pass. Evidence:
  `nix build .#checks.x86_64-linux.{storybook,compile,lint}` all succeed on a derivation that moved.
- Every story file on disk is indexed, verified by count against `task-001`. Evidence: the two
  `require.context` regular expressions match exactly the 65 files on disk that follow the naming
  convention, and no other file. The registration count those 65 files carry is 258 and is unchanged
  by this task.
- The frozen clock in `storybook/preview.tsx` still applies. Evidence: `preview.tsx` is not edited,
  and `timemachine.config` at `:9-11` is in the preview entry rather than in the barrel, so nothing
  this task removes could have carried it.

The second criterion says "verified by count against task-001". The `task-001` baseline records 272
registrations and the corpus now carries 258, after 15 removals and one addition across phase 1.
`task-012` states the expected figure as 257, which is the baseline minus the 15 removals and does
not account for the registration `task-006` added. The figure to verify against is 258, and
`task-012`'s entry is corrected.

Added for this plan:

- No `_support` module is matched by either regular expression, checked directly rather than
  inferred from the rename.
- The orphan walk reports zero orphans after the removals.
- The `storybook` build time is recorded either side, so a `require.context` over 1508 files is a
  measured cost rather than an unexamined one.

## Verification Plan

Already run for planning:

- The two entries resolved through the installed `@storybook/core-common@6.4.22` `normalizeStories`
  and `toRequireContext`, giving the exact directories, recursion flags and regular expressions.
- Those regular expressions applied to the tracked file list: 65 matches, 61 and 4, zero `_support`.
- An independent `globby` expansion of the same patterns: 65.
- The seven barrels read in full and confirmed to be pure import lists.
- The two import-only `_support` modules and their single importers.
- Both `preview.tsx` side effects, and the `css-loader` `modules` difference between the two
  stylesheet imports.

To run for the build:

- `nix build --no-link .#checks.x86_64-linux.storybook` with the barrels still present, as step 2.
- `git status --short` after the removals.
- The orphan walk with updated entry points, expecting zero.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, expecting
  membership unchanged.
- `nix build --no-link .#checks.x86_64-linux.{storybook,compile,lint}`.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a moved path.

If the first `storybook` build fails, the glob is wrong and the barrels are still there to fall back
on, which is why that build comes before the deletion rather than after. The task entry's fallback,
folding this task and `task-011` into phase 3 and verifying at 8.6.x, is only reached if 6.4.22
rejects a glob outright; the `normalizeStories` resolution above shows it does not.

## Risks and Open Questions

- `require.context('./source/renderer/app', true, ...)` makes webpack enumerate a directory of 1508
  tracked files to find four. That is how Storybook resolves any glob and the alternative is naming
  the four colocated files individually, which reintroduces the hand-maintained list this task
  exists to remove, for the part of the corpus most likely to grow. The build time is measured
  rather than assumed.
- Sidebar order will change and this task does not fix it. Between this commit and `task-011` the
  tree is in glob order. That is a deliberate intermediate state, it breaks no check, and
  `task-011` is the next task.
- Deleting the two import-only `_support` modules is the one part of this change that goes beyond
  the task's `targetPaths`. It is not a judgment call: they are orphaned by this change alone,
  `task-062` recorded in advance that this task would remove them, and leaving them would leave the
  first orphans in a tree that `task-007` just brought to zero.
- The stylesheet duplicate being removed is the only change here with a possible visual consequence,
  and it removes the CSS-modules-scoped copy rather than the correct one. If global styling were to
  regress, the symptom would be unstyled previews across every story, which `storybook:build` cannot
  see. It is named here so that if anyone reports it, the cause is already written down.
- Rollback is `git revert` of a single commit, which restores the barrels and the entry together.

## Required Docs, Research, and Tracking Updates

- Set `task-010.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Record in the `task-010` entry why the glob is a coverage increase rather than a tidy-up, since
  several later entries still carry the inference that `yarn compile` catches a dangling import.
- Record in the `task-010` entry that the side effects were already in `preview.tsx`, so the
  instruction to move them is satisfied by deleting the barrel's duplicates.
- Correct `task-012`'s expected registration count from 257 to 258.
- No PRD change. The settled-by-evidence paragraph on the glob remains accurate.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-010-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-010-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `storybook/main.ts` declares two globs in place of the barrel entry.
- The seven barrel `index.ts` files and the two import-only `_support` modules are gone. No
  `index.ts` remains anywhere under `storybook/stories`.
- `task-001-sidebar-extract.js` takes its reachability roots from the glob rather than the barrel.
- `storybook/preview.tsx` is unedited.

## Final Outcome

- `task-010` completed. `task-011` and `task-063` are unblocked.
- The glob is verified against the mechanism 6.4.22 actually uses. `normalizeStories` and
  `toRequireContext` from the installed `@storybook/core-common@6.4.22` turn the two entries into
  two recursive `require.context` calls, and their regular expressions match exactly 65 files on
  disk: 61 of the 110 tracked under `storybook/stories` and 4 of the 1508 under
  `source/renderer/app`. No `_support` module matches either, which is what `task-062` was for.
- Positive evidence that the glob loads the corpus, not merely that the build survives it. A green
  build on a glob matching nothing is indistinguishable from a green build on a working one, so an
  import to a nonexistent module was added to
  `storybook/stories/loading/chain-storage/ChainStorageLocationPicker.stories.tsx`, a file that was
  reachable only through two levels of sub-barrel before this change and only through the glob
  after it. The build failed with `Can't resolve './GlobProbeDoesNotExist'`. Reverting it returned
  the check to exit 0.
- Sidebar membership is byte-identical to the pre-change capture: 258 registrations, 49 titles, 14
  groups, `UNREACHABLE 0`. Order is not asserted here and `task-011` owns it.
- The orphan walk, re-rooted on the glob, reports 107 files under `storybook/`, 107 reachable, 0
  orphaned.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile` and `.lint` all exit 0, on a
  `compile` derivation that moved to `6idfm6qdwmhdhrmczr6hd7z3gvvm5xsd-daedalus-compile.drv`. Lint
  holds at 5391 warnings.
- The `storybook` build cost 40 seconds of yarn time against 42 for the barrel build, so the
  `require.context` over 1508 tracked files under `source/renderer/app` is not a measurable penalty.

## Self-Review

- The verification instrument was wrong in the first draft and the critique caught it. A `globby`
  expansion is not what 6.4.22 does; it converts the pattern to a regular expression for
  `require.context`. Checking the pattern with the wrong tool would have passed a pattern the build
  could still reject.
- Running the `storybook` check once with the barrels present but unreferenced separated two
  failure modes that would otherwise have been one red build.
- The probe is the part of this task that earns the claim. Every other piece of evidence here is
  consistent with a glob that matches nothing.
- Two things surfaced that the entry did not have: the side effects were already in `preview.tsx`,
  and the barrel's stylesheet import was being processed as a CSS module. The second is a defect the
  barrel was masking rather than something this change introduces, and it is recorded with its
  symptom because no check in this repository can see it.
- One consequence was not planned for and is recorded rather than quietly absorbed: the committed
  sidebar extractor rooted its walk at the barrel and stopped working the moment the barrel went. It
  was updated in the same task, its output is byte-identical across the change, and the reachability
  column it produces has lost most of its meaning now that the glob guarantees what the barrel used
  to be trusted for.
