# Task task-062: Rename the 15 sibling-registering story files out of the convention

## Task ID and Title

- ID: `task-062`
- Title: `Rename the 15 sibling-registering story files out of the convention`

## Why Chosen Now

`task-062` has no dependencies and gates `task-010`, the phase 2 change that replaces the
hand-maintained barrel at `storybook/main.ts:8` with a glob. Until the 15 files stop matching
`*.stories.*` the glob cannot land. At the pinned 6.4.22 it would silently auto-title them and
register their named exports, moving the phase 1 sidebar baseline that every later phase diffs
against; at 8.6 and 10.6 it is a hard `build-storybook` failure. Doing it first also keeps the
cost flat, because the file is renamed before `task-020`, `task-021`, `task-035` and `task-044`
rewrite its contents.

## Interaction Mode

- Mode: `agent_execution`

The change is a set of `git mv` operations plus the import rewrites they force. Both required
checks, `yarn compile` and `yarn storybook:build`, are reproducible in this environment as the
flake checks `checks.x86_64-linux.compile` and `checks.x86_64-linux.storybook`, which run the same
commands over the same Nix-built `node_modules` that CI uses (`perSystem/checks.nix:16-31`,
`:53-54`, `:79`). Nothing here needs an operator.

## Scope

- Move the 15 files listed in the task's `targetPaths` into a `_support/` directory beside their
  current location, dropping the `.stories` infix from each filename and keeping the basename and
  extension.
- Rewrite the relative specifiers inside each moved file, which all gain one directory level.
- Rewrite the specifier in every importer of those 15 files.
- Correct the two other task entries in the tasks JSON whose `targetPaths` name a path this change
  moves, so the graph keeps pointing at files that exist.
- Record the rename map in this plan, because four later tasks refer to these files by their old
  names in prose.

## Non-Goals

- No change to any `storiesOf()` call, any `.add()` call, or any sidebar title or label.
- No CSF conversion, no knob removal, no default export added to any of the 15. Those are phases 3
  and 4, and `task-020` settles the fixture-versus-story question for exactly these exports.
- No glob in `storybook/main.ts`. That is `task-010`, and this task exists to unblock it.
- No deletion. `task-002`, `task-003`, `task-004`, `task-005` and `task-007` own the deletions, and
  `staking/Epochs.stories.tsx` is moved here and deleted there.
- No edit to the PRD's locked decision 16 or to the research notes. Both describe the corpus as it
  was surveyed and remain accurate as history.

## Dependencies

- None in the task graph. `task-062.dependencies` is `[]`, and `task-010.dependencies` lists
  `task-062`.
- Practical ordering: this lands before `task-004` and `task-006`, whose `targetPaths` name two of
  the 15. This plan corrects those two entries rather than leaving them dangling.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 16 at
  `:338-366`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-062` at
  `:283-312`, `task-010` at the phase 2 entry, `task-004` and `task-006`
- `.agent/plans/storybook-modernization/research/01-current-coverage.md:461-476`, the file-level
  census that first isolated the 15
- `.agent/plans/storybook-modernization/research/04-tooling-alternatives.md:43`, which records 69
  files calling `storiesOf()` directly and 15 exporting story functions consumed by a sibling
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for the repository conventions
  - `.agent/readme.md`
- Workflows:
  - `.agent/workflows/storybook.md`, read for the barrel and registration model only. Per the
    task-plans readme it teaches `storiesOf`, `withKnobs` and `@dump247/storybook-state`, which is
    accurate today and rewritten by `task-060`. Nothing in it was followed as guidance here.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.
  - `.agent/skills/storybook-creation/SKILL.md` was not followed. `:26` states in bold that
    Daedalus uses `storiesOf()` rather than CSF, which is true of the corpus and irrelevant to a
    rename.

## Live Repo Findings Verified For Planning

Verified at `76db6d336`, 2026-09-14, against the working tree, not against the PRD.

- The corpus matching the story naming convention is 84 files: 80 under `storybook/stories/` and 4
  colocated under `source/renderer/app/`. `find storybook/stories source -name '*.stories.ts' -o
  -name '*.stories.tsx' -o -name '*.story.ts' -o -name '*.story.tsx' | wc -l` returns 84.
- Exactly 15 of those 84 register nothing, and they are exactly the 15 in `targetPaths`. Classifying
  every one of the 84 by whether it contains `storiesOf(` or a line beginning `export default`
  gives 69 with a `storiesOf` call, 14 with neither, and 1 with a default export that is not a meta
  object. No sixteenth file has drifted into the shape since the PRD was written.
- The one default export is
  `storybook/stories/wallets/settings/WalletSettingsScreen.stories.tsx:139`,
  `export default function (props: { locale: Locale }) {`. It is an anonymous React component. It
  carries no `title`, so `StoryStoreFacade` would auto-title it; it is not a CSF meta object.
- The 6.4.22 auto-title path the PRD cites is present in the installed tree.
  `node_modules/@storybook/client-api/dist/cjs/StoryStoreFacade.js:206` destructures
  `fileExports.default`, `:211` takes `defaultExport || {}`, and `:215` falls back to
  `autoTitle(fileName, ...)`. `@storybook/client-api` is at `6.4.22`.
- None of the 15 contains `storiesOf`, `require(` or a dynamic `import(`. Every reference to them
  is a static ES import, so the importer set below is complete.
- Every importer of the 15, and there are seven files:
  - `storybook/stories/staking/Staking.stories.tsx:14` `./StakePools.stories`
  - `storybook/stories/staking/Staking.stories.tsx:15` `./Rewards.stories`
  - `storybook/stories/staking/Staking.stories.tsx:16` `./DelegationCenter.stories`
  - `storybook/stories/staking/Staking.stories.tsx:17` `./Epochs.stories`
  - `storybook/stories/staking/Staking.stories.tsx:18` `./DelegationSteps.stories`
  - `storybook/stories/staking/Staking.stories.tsx:26` `./RedeemItnWallets.stories`
  - `storybook/stories/staking/Staking.stories.tsx:30` `./Undelegate.stories`
  - `storybook/stories/staking/Staking.stories.tsx:31` `./StakePoolsTable.stories`
  - `storybook/stories/nodes/index.ts:3` `./status/Status.stories`
  - `storybook/stories/nodes/errors/Errors.stories.tsx:6` `./NoDiskSpaceError.stories`
  - `storybook/stories/nodes/errors/Errors.stories.tsx:7` `./SystemTimeError.stories`
  - `storybook/stories/nodes/syncing/Syncing.stories.tsx:11` `./SyncingConnecting.stories`
  - `storybook/stories/nodes/updates/Updates.stories.tsx:7` `./DataLayerMigration.stories`
  - `storybook/stories/wallets/index.ts:14` `./addWallet/AddWallet.stories`
  - `storybook/stories/wallets/settings/WalletSettings.stories.tsx:6`
    `./WalletSettingsScreen.stories`
- Two of the 15 are import-only barrels and export nothing.
  `storybook/stories/nodes/status/Status.stories.ts` is one line, `import './Diagnostics.stories';`.
  `storybook/stories/wallets/addWallet/AddWallet.stories.ts` is five such lines, for
  `./Add.stories`, `./Create.stories`, `./Import.stories`, `./Restore.stories` and
  `./RestoreOld.stories`. Both are reached only from a barrel, so after the move each must still be
  imported or the five `addWallet` panels and the diagnostics panel disappear from the sidebar.
- The other 13 export named story functions and nothing else. `WalletSettingsScreen.stories.tsx` is
  the sole file exporting a default.
- A per-directory `_support/` is the established shape, not a new one.
  `storybook/stories/loading/_support/` already holds four support modules beside the story files
  it serves, alongside the corpus-wide `storybook/stories/_support/`.
- Five of the moved files import the corpus-wide helper as `../_support/utils`:
  `DelegationCenter`, `DelegationSteps`, `RedeemItnWallets`, `Undelegate` and `StakePools`. From
  `staking/_support/` that specifier would resolve to the new directory itself, so it has to become
  `../../_support/utils`. `WalletSettingsScreen.stories.tsx` imports it as `../../_support/utils`
  and becomes `../../../_support/utils`.
- `tsconfig.json` declares no `include` and excludes only `node_modules`, so every `.ts` and `.tsx`
  file in the tree is in the `tsc --noEmit` program. A dangling importer cannot hide.
- `.eslintrc` contains no rule keyed on a `stories` or `storybook` filename glob, so no lint rule
  changes behavior when a file leaves the convention.
- `.prettierignore` selects by extension under `storybook/`, not by filename, so the moved files
  stay formatted.
- CI runs `yarn compile` and `yarn storybook:build` as `perSystem/checks.nix:53` and `:79`, both
  wrapped in `lib.optionalAttrs (system == "x86_64-linux")` at `:51`. `mkJsCheck` at `:16-31` copies
  a prebuilt `node_modules` into the sandbox, so `nix build .#checks.x86_64-linux.compile`
  reproduces the CI check exactly.
- `nix build --dry-run .#checks.x86_64-linux.compile` at `76db6d336` resolves to a substitutable
  output, which is direct evidence that the pre-change tree is green on that check.
- Running `yarn compile` outside the Nix shell against the host `node_modules` reports four errors,
  three at `source/renderer/app/utils/crypto.ts:107` and `:109` and one at
  `source/renderer/app/utils/dataSerialization.ts:309`. They reproduce identically in the untouched
  `master` clone, and they come from duplicate `@types/node` copies in the locally installed tree,
  `14.18.1` at the top level against `11.11.6` nested under `cardano-crypto.js` and `cardano-js`.
  They are a property of the host tree, not of the source, which is why the flake check and not the
  host run is the verification of record.

## Files Expected To Change

Renames, 15:

| From | To |
|---|---|
| `storybook/stories/staking/DelegationCenter.stories.tsx` | `storybook/stories/staking/_support/DelegationCenter.tsx` |
| `storybook/stories/staking/DelegationSteps.stories.tsx` | `storybook/stories/staking/_support/DelegationSteps.tsx` |
| `storybook/stories/staking/StakePoolsTable.stories.tsx` | `storybook/stories/staking/_support/StakePoolsTable.tsx` |
| `storybook/stories/staking/Rewards.stories.tsx` | `storybook/stories/staking/_support/Rewards.tsx` |
| `storybook/stories/staking/RedeemItnWallets.stories.tsx` | `storybook/stories/staking/_support/RedeemItnWallets.tsx` |
| `storybook/stories/staking/Undelegate.stories.tsx` | `storybook/stories/staking/_support/Undelegate.tsx` |
| `storybook/stories/staking/Epochs.stories.tsx` | `storybook/stories/staking/_support/Epochs.tsx` |
| `storybook/stories/staking/StakePools.stories.tsx` | `storybook/stories/staking/_support/StakePools.tsx` |
| `storybook/stories/wallets/addWallet/AddWallet.stories.ts` | `storybook/stories/wallets/addWallet/_support/AddWallet.ts` |
| `storybook/stories/wallets/settings/WalletSettingsScreen.stories.tsx` | `storybook/stories/wallets/settings/_support/WalletSettingsScreen.tsx` |
| `storybook/stories/nodes/updates/DataLayerMigration.stories.tsx` | `storybook/stories/nodes/updates/_support/DataLayerMigration.tsx` |
| `storybook/stories/nodes/errors/SystemTimeError.stories.tsx` | `storybook/stories/nodes/errors/_support/SystemTimeError.tsx` |
| `storybook/stories/nodes/errors/NoDiskSpaceError.stories.tsx` | `storybook/stories/nodes/errors/_support/NoDiskSpaceError.tsx` |
| `storybook/stories/nodes/syncing/SyncingConnecting.stories.tsx` | `storybook/stories/nodes/syncing/_support/SyncingConnecting.tsx` |
| `storybook/stories/nodes/status/Status.stories.ts` | `storybook/stories/nodes/status/_support/Status.ts` |

Importers edited, 7:

- `storybook/stories/staking/Staking.stories.tsx`
- `storybook/stories/nodes/index.ts`
- `storybook/stories/nodes/errors/Errors.stories.tsx`
- `storybook/stories/nodes/syncing/Syncing.stories.tsx`
- `storybook/stories/nodes/updates/Updates.stories.tsx`
- `storybook/stories/wallets/index.ts`
- `storybook/stories/wallets/settings/WalletSettings.stories.tsx`

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-062.status` and
  the `targetPaths` of `task-004` and `task-006`
- `.agent/plans/storybook-modernization/task-plans/task-062.md`
- `.agent/plans/storybook-modernization/task-plans/task-062-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-062-impl-review.md`

## Implementation Approach

1. Move each file with `git mv` into a `_support/` directory beside its current location, dropping
   the `.stories` infix. `git mv` keeps the rename detectable in the diff, which matters because
   four later tasks rewrite these same files and a rename recorded as delete-plus-add loses that
   history.

2. Fix the specifiers inside each moved file. Every relative specifier gains exactly one `../`,
   because every file moves down exactly one directory. Two cases are not a mechanical prefix and
   are edited by hand:
   - `../_support/utils` in the five staking files becomes `../../_support/utils`. Prefixing it
     blindly would give `../../_support/utils` by luck in the staking case, but the intent is the
     corpus-wide helper, so it is stated rather than derived.
   - The sibling imports in `AddWallet.ts` and `Status.ts` go from `./X.stories` to `../X.stories`.

3. Fix the seven importers. Each `./Name.stories` becomes `./_support/Name`, and the two barrel
   entries become `./status/_support/Status` and `./addWallet/_support/AddWallet`.

4. Prove the sidebar did not move. The sidebar is the set of `storiesOf('...')` panel titles and
   `.add('...')` labels across the corpus. Extract both sets before and after, sorted and without
   file paths so a rename cannot perturb them, and require the two extractions to be identical.
   None of the 15 contains either call, so the extraction is unchanged by construction; the check
   exists to catch an accidental edit to a registering file.

5. Stage the renames before running any flake check. `nix build .#checks...` evaluates the flake
   from the git repository, and on a git source an untracked file is invisible to the build. A
   `git mv` stages both sides, but a file created by any other means would not be seen, and the
   check would then build the pre-change tree and pass for the wrong reason. `git status --short`
   showing 15 `R` entries and no `??` under `storybook/` is the precondition for trusting either
   check.

6. Run the two required checks as flake checks, which use the CI `node_modules` rather than the
   host tree.

7. Correct `task-004.targetPaths` and `task-006.targetPaths` in the tasks JSON, and the one
   `implementationNotes` string in `task-004` that names `storybook/stories/staking/Epochs.stories.tsx`.
   Leave the PRD, the research notes and the prose of every other task alone.

8. Land it as one signed commit on `docs/storybook-modernization-plan`, the branch the epic's
   single draft pull request tracks. One commit, because a rename and the import it forces are not
   separable states: any split leaves `yarn compile` red in the middle, and the trunk is never
   allowed to carry that.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- No file under `storybook/stories` matches `*.stories.*` without either a `storiesOf` call or a
  CSF default export whose value is a meta object. Evidence: the classification over all 84
  convention-matching files reports zero in the register-nothing class.
- `yarn compile` passes, proving no importer was left dangling. Evidence:
  `nix build .#checks.x86_64-linux.compile` succeeds on the post-change tree.
- `yarn storybook:build` passes and the sidebar tree is unchanged. Evidence:
  `nix build .#checks.x86_64-linux.storybook` succeeds, and the panel-and-label extraction is
  identical before and after.

The task entry words that last criterion as byte-identical to the `task-001` baseline. `task-001`
is still `pending` and no baseline artifact exists, so there is nothing to diff against. The
substitute is a before-and-after extraction taken across this change alone, which answers the same
question for this change and answers nothing about the rest of phase 1. When `task-001` lands, its
baseline supersedes this capture.

Added for this plan:

- Every one of the 15 files is present at its new path and absent at its old one, and `git status`
  records 15 renames rather than 15 deletions and 15 additions.
- `yarn lint` and `yarn prettier:check` pass, because the import rewrites are the kind of change
  that trips `import/no-unresolved` and line-width reflow.

## Verification Plan

Already run for planning:

- Classification of all 84 convention-matching files into register and register-nothing.
- Importer enumeration by grep for each of the 15 basenames across the tree, excluding the plan
  workspace.
- Relative-specifier inventory per moved file.
- `nix build --dry-run .#checks.x86_64-linux.compile` at `76db6d336`, substitutable.

To run for the build:

- `nix build --no-link .#checks.x86_64-linux.compile`
- `nix build --no-link .#checks.x86_64-linux.storybook`
- `nix build --no-link .#checks.x86_64-linux.lint`
- The panel-and-label extraction, diffed against the pre-change capture.
- The register-nothing classification, rerun, expecting an empty result.
- `git status --short` and `git diff --stat -M`, expecting 15 `R` entries and no untracked file
  under `storybook/`, checked before either flake check is run.

If the extraction differs, the cause is an edit to a registering file rather than to the 15, and
the response is to revert that edit rather than to accept the new sidebar. If a flake check fails,
the diff is small enough to read in full, and the failure is a specifier that gained the wrong
number of levels.

## Risks and Open Questions

- The two barrels, `AddWallet` and `Status`, are import-only modules rather than support modules,
  so `_support/` names them slightly wrong. The task lists both explicitly and the criterion is the
  naming convention rather than the taxonomy, so they move with the rest. `task-010` deletes the
  barrel they belong to, at which point both files disappear.
- There are now two directories named `_support` on some import paths, the corpus-wide
  `storybook/stories/_support/` and the new per-directory ones. This ambiguity already exists for
  `storybook/stories/loading/`, so the change adds instances of a shape the corpus has rather than a
  new shape.
- Four later tasks name these files by their old paths in prose rather than in `targetPaths`:
  `task-020`, `task-021`, `task-035` and `task-044`. Prose is not resolved mechanically, so the
  rename map in this plan is the reference. `task-010` also notes that the 15 must stay reachable
  from the file that registers their exports, which this change preserves.
- `staking/_support/Epochs.tsx` is deleted outright by `task-004` together with the
  `/staking/epochs` screen. Moving a file that is about to be deleted is wasted motion in isolation,
  but leaving it behind would leave the trap in place for however long `task-004` takes, and
  `task-010` depends on both.
- Rollback is `git revert` of a single commit. Nothing here is stateful, nothing is generated, and
  no other branch depends on the new paths yet, because `task-010` has not started.
- The host `yarn compile` is red for four pre-existing reasons unrelated to this change. The
  verification of record is the flake check, which uses the CI `node_modules`. If the flake check
  could not be built here, the honest outcome would be to say so rather than to report the host run
  as a pass.

## Required Docs, Research, and Tracking Updates

- Set `task-062.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Correct `task-004.targetPaths`, `task-004.implementationNotes` and `task-006.targetPaths` in the
  same file, which are the only two entries whose `targetPaths` name a moved path.
- No PRD change. Locked decision 16 at `:338-366` describes the decision and the corpus as surveyed,
  and both remain accurate.
- No research-note change. `research/01-current-coverage.md` and `research/04-tooling-alternatives.md`
  are censuses taken at a stated commit.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-062-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-062-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- All 15 files moved into a `_support/` directory beside their previous location with the
  `.stories` infix dropped, recorded as 15 renames in the index rather than 15 deletions and 15
  additions.
- Fifteen specifiers rewritten across the seven importers, and every relative specifier inside the
  moved files given the one extra directory level the move costs.
- The diff is 85 insertions against 85 deletions, and every changed line is an import. No
  `storiesOf` call, `.add` call, title or label was touched.
- Line numbering inside the moved files is unchanged, so the `path:line` references later tasks
  carry still resolve: `_support/Epochs.tsx:13` and `:28` are still the two knob call sites, and
  `_support/WalletSettingsScreen.tsx:326` is still the undelegation dialog.

## Final Outcome

- `task-062` completed. The corpus now holds 69 files matching `*.stories.*`, and all 69 register
  stories through `storiesOf()`. The register-nothing class is empty, which is the first acceptance
  criterion.
- `nix build .#checks.x86_64-linux.compile` succeeded, which is `yarn compile` over the Nix-built
  `node_modules` CI uses. No importer was left dangling.
- `nix build .#checks.x86_64-linux.storybook` succeeded in 42 seconds, which is
  `yarn storybook:build`.
- `nix build .#checks.x86_64-linux.lint` succeeded, with the 5483 warnings the corpus already
  carried and no error.
- The panel-and-label extraction is byte-identical across the change: 73 `storiesOf()` panel titles
  and 234 literal `.add()` labels before and after. The sidebar did not move, which is what a set of
  files that register nothing should do when renamed.
- A module-graph walk from `storybook/stories/index.ts` reaches 68 of the 69 convention-matching
  files, against 83 of 84 before. The one unreached file on both sides is
  `storybook/stories/staking/Legacy.stories.tsx`, the dead file the barrel never loaded. No module
  left the graph, which the registration extraction on its own does not establish.
- The host `yarn compile` remains red for the four pre-existing errors recorded in the planning
  findings. The flake check run on the same post-change tree is green, which confirms those four
  are a property of the locally installed `node_modules` and not of the source.
- `task-004` and `task-006` had their `targetPaths` corrected to the new paths, along with the path
  references in their own prose, so neither now names a file that does not exist.

## Self-Review

- The classification over all 84 convention-matching files confirms the set is exactly 15 and no
  sixteenth file has appeared since the PRD was written.
- The plan states where the host toolchain disagrees with CI and names the flake check as the
  verification of record rather than reporting the host run.
- Scope held to renames, the import rewrites they force, and the two tracking entries this change
  invalidates. No CSF conversion, no default exports, no glob.
- One deviation from the approved plan, recorded in the implementation review: the plan named
  `task-004.implementationNotes` as the only prose to correct, and `task-006` carried two path
  references of its own that the rename invalidated. Both were corrected, because a task whose
  `targetPaths` and whose description disagree about where a file lives is worse than either state
  alone.
