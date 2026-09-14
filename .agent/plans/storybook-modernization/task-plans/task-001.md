# Task task-001: Capture the sidebar baseline

## Task ID and Title

- ID: `task-001`
- Title: `Capture the sidebar baseline`

## Why Chosen Now

`task-001` has no dependencies and gates `task-002`, `task-003`, `task-004` and `task-006`, each of
which states its acceptance as a sidebar diff against this baseline. It also gates every later
phase: the `storiesof-to-csf` codemod renames roughly 80 exports through its sanitizer, and a story
whose sidebar label changes silently passes `yarn compile`, `yarn lint` and `yarn storybook:build`.
Nothing in the required check set looks at a label. The baseline is the only thing that can.

It goes first because a baseline captured after a deletion cannot prove the deletion removed what it
claimed to. `task-062` has already landed, so the capture is taken against a corpus that has been
renamed but not yet reduced.

## Interaction Mode

- Mode: `agent_execution`

The task asks for a record, not a change to the application. Both acceptable methods are named in
the task entry, and the static one is reproducible here without a browser. `.agent/` is excluded
from `srcWithoutNix` (`nix/internal/common.nix:269`), from `yarn lint`, which reads `source`,
`storybook` and `utils` only (`package.json` `lint`), from `.prettierignore`, which admits nothing
outside `source/`, `features/`, `storybook/`, `hardware-wallet-tests/` and `tests/`, and from
treefmt (`perSystem/formatter.nix:48`). A baseline artifact committed there cannot move any check.

## Scope

- Record every story registration in the corpus with its group, its panel and its label, together
  with the file and line it is registered at and whether the barrel reaches it.
- Commit the baseline and the extractor that produced it under the plan's `task-plans/` directory,
  so later phases diff against a fixed artifact rather than against a re-derivation.
- Record the counts alongside the tree, so a lost registration reads as an arithmetic difference.
- Correct the `task-001` entry in the tasks JSON where it names paths `task-062` moved and a raw
  grep figure that does not reproduce.

## Non-Goals

- No deletion, no restaging, no story edit of any kind. `task-002` through `task-007` own those.
- No `storybook/main.ts` change and no glob. That is `task-010`.
- No attempt to make the baseline survive the phase 3 hop. This extractor reads `storiesOf()` and
  `.add()`, which stop existing when CSF lands. The artifact it produces outlives it; the script
  does not.
- No PRD edit. The PRD's own figures reproduce; the task entry's do not, and that is where the
  correction belongs.

## Dependencies

- None. `task-001.dependencies` is `[]`.
- Gates `task-002`, `task-003`, `task-004` and `task-006`, which name a sidebar diff against this
  baseline in their acceptance criteria. `task-005`, `task-008` and `task-009` do not depend on it.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the Status Log entry
  `2026-09-14 — Counts reconciled across the plan and the research notes` at `:1171-1199`, which is
  the source of the 272 and 267 figures, and locked decision 16 at `:338-366`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-001` and the
  acceptance criteria of `task-002`, `task-003`, `task-004` and `task-006`
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`, the file-level census
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/storybook-modernization/task-plans/task-062.md`, the immediately preceding task,
  whose renames this capture is taken after
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
  - `.agent/plans/readme.md` for the plan-folder conventions and the append-only log rule
- Workflows:
  - `.agent/workflows/storybook.md`, read for the barrel and registration model only. Per the
    task-plans readme it teaches `storiesOf`, `withKnobs` and `@dump247/storybook-state`, accurate
    for the corpus today and rewritten by `task-060`. Nothing in it was followed as guidance.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.
  - `.agent/skills/storybook-creation/SKILL.md` not followed, for the reason the task-plans readme
    gives.

## Live Repo Findings Verified For Planning

Verified at `bfc6ec3d2`, 2026-09-14, against the working tree.

- The corpus matching the story naming convention is 69 files, 65 under `storybook/stories/` and 4
  colocated under `source/renderer/app/`. `find storybook/stories source -name '*.stories.ts' -o
  -name '*.stories.tsx' -o -name '*.story.ts' -o -name '*.story.tsx' | wc -l` returns 69. The PRD
  and the task entry both say 84, which was true before `task-062` moved 15 register-nothing files
  into `_support/`. Every one of the 69 registers at least one story, so the registration total is
  unaffected by that move.
- The registration total is 272: 267 under `storybook/stories/` and 5 in the four colocated files.
  This reproduces the PRD figure exactly.
- The five colocated registrations are `Analytics.stories.tsx` (1),
  `wallet-token-picker/WalletTokenPicker.stories.tsx` (1), `DiscreetValue.story.tsx` (2) and
  `discreet-toggle/DiscreetModeToggle.story.tsx` (1).
- `grep -rno '\.add(' storybook/stories | wc -l` returns 272, not the 270 the task entry states.
  Five of those 272 are not registrations: `moment().add()` at
  `storybook/stories/staking/_support/Epochs.tsx:10`,
  `storybook/stories/staking/_support/Undelegate.tsx:108` and
  `storybook/stories/staking/_support/DelegationSteps.tsx:211`, and two `Set.add()` calls at
  `storybook/stories/governance/_utils/drepPopulation.ts:134` and `:140`. That leaves 267, which is
  the figure the same task entry and the PRD both give. The entry's 270 cannot be reconciled with
  its own arithmetic under any reading, and the three `moment()` paths it names were moved by
  `task-062`.
- The sidebar has 15 top-level groups: `Analytics`, `Assets`, `Common`, `Decentralization`,
  `Discreet Mode`, `Governance`, `Loading`, `Navigation`, `News`, `Nodes`, `Settings`,
  `StakingChart`, `Voting`, `Wallets`, `dApps`.
- There are 53 distinct `storiesOf()` titles across 73 `storiesOf()` calls, so 20 calls share a
  title with another call. `Wallets / Settings` is reached by six separate files.
- The `storiesOf()` title is the full sidebar path, not a group-and-panel pair. Seven titles carry
  three levels, six under `Loading / Mithril / …` and one at
  `Nodes / Diagnostic / Mithril Partial Sync Confirmation`. Two carry one level and sit at the root,
  `Analytics` and `StakingChart`. The baseline stores the full title so no structure is lost, and
  prints it split at the first separator so the tree is readable.
- Two of the 272 registrations are unreachable from the barrel:
  `storybook/stories/staking/Legacy.stories.tsx:31` `Tooltip only` and `:49`
  `Chart with Tooltips`, both under the root-level title `StakingChart`. A module-graph walk from
  `storybook/stories/index.ts` across relative imports reaches 68 of the 69 convention-matching
  files; `Legacy.stories.tsx` is the one it does not. `task-003` deletes it.
- 263 of the 272 labels are string literals. Eight resolve through a `const` object of string values,
  all in `storybook/stories/staking/Staking.stories.tsx` through its `pageNames` map at `:41-51`.
  One is a template literal, `Staking.stories.tsx:111`, which resolves to `Pools Index - Loading`.
  No label is unresolvable, so the baseline carries no placeholder.
- `storybook/main.ts:8` is `stories: ['../storybook/stories/index.ts']`, a single barrel entry, which
  is why reachability and registration are two different questions today and why the baseline records
  both.
- Storybook is pinned at 6.4.22 (`@storybook/react`, `@storybook/core`, `@storybook/addon-actions`,
  `@storybook/addon-links`, `@storybook/builder-webpack5`, `@storybook/manager-webpack5`), with
  `@storybook/addon-knobs` at 6.4.0. At 6.4.22 there is no static index to read: `storiesOf()`
  registrations are built in the browser at preview boot, so `dist/storybook` contains no
  `stories.json` and walking the sidebar means driving a browser. The task entry offers a static
  extraction as an equal alternative, and that is the one taken.
- `typescript` is 4.9.5 and ships its own parser, so the extraction runs off the repository's own
  compiler rather than a regex over source text. A regex cannot do this job: 35 `.add(` calls in the
  corpus end the line at the open parenthesis and put the label on the next line, and nine of the
  272 labels are not string literals.
- The worktree carries no `node_modules`, because the flake checks build their own
  (`perSystem/checks.nix:23`). The extractor therefore has to resolve `typescript` from wherever the
  dependencies are installed, which is the repository root in a normal dev tree and inside the Nix
  dev shell. It resolves `typescript` by `require` and fails with a stated message rather than a
  stack trace when it cannot.
- `nix/internal/common.nix:269` excludes any directory named `.agent` from `srcWithoutNix`, which is
  the `src` of every check in `perSystem/checks.nix:16-31`. A file committed under `.agent/` cannot
  change a check's derivation, which is verifiable by comparing derivation paths before and after.
- The four checks this phase relies on are green at `bfc6ec3d2`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n}` all exit 0. Their
  derivations at that commit are `wjgi3x5v1jzibl9krs8dr69g1hk8gglp-daedalus-compile.drv`,
  `r9gk4vqv5ykvmar18hz7njnn5zsl8cw2-daedalus-lint.drv`,
  `2v7wvacpfl3vd1gpi0n2lw9gcldpbfid-daedalus-storybook-build.drv` and
  `izlvafbn8w3k5vff3z9d5riqab4mwyyv-daedalus-i18n.drv`.
- Running `yarn compile` outside the Nix shell against a host `node_modules` reports four errors, at
  `source/renderer/app/utils/crypto.ts:107` and `:109` and
  `source/renderer/app/utils/dataSerialization.ts:309`. They reproduce in an untouched `master`
  clone and come from duplicate `@types/node` copies in the locally installed tree. The flake check
  is the verification of record throughout this phase; the host run is not.

## Files Expected To Change

Added:

- `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-baseline.txt`, the baseline
- `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js`, the extractor

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-001.status` and
  the two corrections to `task-001.description`
- `.agent/plans/storybook-modernization/task-plans/task-001.md`
- `.agent/plans/storybook-modernization/task-plans/task-001-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-001-impl-review.md`

No file under `source/`, `storybook/`, `nix/`, `perSystem/` or the repository root changes.

The task entry says the baseline may live outside the repository or as a scratch artifact attached
to the branch, and calls it a working reference rather than a deliverable. It is committed here
because the phases that consume it run in later sessions and a reference that does not survive the
session cannot be diffed against. The task-plans readme lists three files per task; a fourth and a
fifth are added deliberately rather than by drift, and they are data and the tool that produced it,
not a fourth kind of prose.

## Implementation Approach

1. Extract every registration with the TypeScript compiler API. For each file containing
   `storiesOf(`, parse it, then:
   - record the title argument of every `storiesOf()` call, and the variable it is assigned to when
     the chain is broken across statements;
   - walk every `.add()` call back through its receiver chain to the `storiesOf()` call or the
     variable that owns it, so a `.add()` on an unrelated object is never counted;
   - resolve the label from a string literal, a property or element access into a file-local `const`
     object of string values, a file-local string `const`, or a template literal whose spans all
     resolve, and report anything else as unresolved rather than guessing.
2. Walk the module graph from `storybook/stories/index.ts` across relative imports, so each
   registration is marked as loaded or not loaded by the barrel. This is what distinguishes the two
   `Legacy.stories.tsx` registrations, which is the task's second acceptance criterion.
3. Emit two sections into one artifact. The tree, group then panel then label, is what later tasks
   diff: it carries no file path and no line number, so a rename or an edit above a registration
   cannot perturb it. The index that follows carries title, label, `path:line` and load state, so a
   changed line in the tree can be traced to a file. The index is provenance and is expected to move
   whenever a file is edited above a registration, which is why it is not the diff surface.

   The diff a later task runs is the whole artifact regenerated against the tree of the day,
   compared with the committed baseline. Every line of difference in the tree section has to be a
   registration that task intended to remove, rename or add, and any other difference is a defect in
   that task rather than a correction to the baseline. The baseline file itself is never updated:
   it records the corpus at `bfc6ec3d2`, and a baseline that is edited to match each change stops
   being one.
4. Sort deterministically on title, then label, then path, then zero-padded line, so two captures of
   the same corpus are byte-identical regardless of filesystem order.
5. Cross-check the total by a second, independent method before trusting it: a raw `.add(` grep,
   less the five known non-registrations, plus the colocated five.
6. Correct the two defects in the `task-001` entry: the raw-grep figure, and the three `moment()`
   paths `task-062` moved.
7. Stage every added file before running a flake check. `nix build` on a git source sees tracked
   content only, so an untracked artifact is invisible to it. Here that cuts the opposite way from
   `task-062`: the evidence this task offers is that the four derivation paths are *unchanged*, and
   unchanged derivations over an unstaged tree would prove nothing at all. `git status --short`
   showing the five files staged and nothing untracked under `.agent/` is the precondition for the
   comparison to mean anything.
8. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- Every one of the 272 registrations appears in the baseline with its group, panel and label.
  Evidence: the artifact reports `REGISTRATIONS 272`, and the independent grep cross-check arrives
  at the same figure from a different direction.
- The baseline distinguishes the two registrations in `staking/Legacy.stories.tsx` that the barrel
  never loads. Evidence: both carry `(UNREACHABLE)` in the tree and `NOT LOADED` in the index, and
  they are the only two that do.

The task entry words the first criterion as 272 registrations across 84 story files. The file count
was correct when the entry was written and is 69 after `task-062`, which moved 15 files that
registered nothing. The registration count is unchanged, which is the part the criterion turns on.

Added for this plan:

- No label is recorded as unresolved. A placeholder in the baseline would make every later diff
  against that row meaningless.
- The four flake checks are still green and their derivation paths are unchanged, which is the
  direct evidence that a commit touching only `.agent/` cannot move a check.

## Verification Plan

Already run for planning:

- The extraction itself, at `bfc6ec3d2`: 15 groups, 53 titles, 272 registrations, 270 reachable, 2
  not reachable, 0 unresolved labels.
- The independent cross-check: 272 raw `.add(` under `storybook/stories`, less 5 non-registrations,
  plus 5 colocated, is 272.
- Per-file attribution: 69 files carry registrations, 267 of them under `storybook/stories/`.
- `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n}`, all exit 0.

To run for the build:

- Re-run the extractor from the committed copy, not the scratch copy, and require its output to be
  byte-identical to the committed baseline. A baseline that its own committed tool cannot reproduce
  is worthless. The run needs a `node_modules` carrying `typescript`, which this worktree does not
  have, so it is run with `NODE_PATH` pointed at an installed tree and that is stated in the result
  rather than left implicit.
- `git status --short` before either comparison, expecting the five files staged and nothing
  untracked under `.agent/`.
- `nix path-info --derivation` for the four checks, compared against the four recorded above.
- `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}`.
- `git status --short`, expecting only the five files this task adds or edits.

If the extractor's output differs from the committed baseline, the artifact is regenerated rather
than the difference accepted, because the tool is the definition. If a flake check goes red on a
commit that touches only `.agent/`, the exclusion in `nix/internal/common.nix:269` is not doing what
this plan claims and the finding is reported before anything else lands.

## Risks and Open Questions

- A static extraction reads what the source registers, not what the browser renders. It cannot catch
  a registration lost to a module that throws at preview boot, and `storybook:build` cannot either,
  because it bundles without evaluating preview modules. This is the residual risk the PRD records
  under locked decision 7, and this task does not close it. What the baseline does close is the
  label question, which is the one no check answers at all.
- The extractor resolves labels through file-local constants only. A label imported from another
  module would be reported unresolved rather than silently wrong. None exists today, and the
  unresolved count is checked rather than assumed.
- The `storiesOf` and `.add` shapes the extractor reads stop existing at the end of phase 3. The
  script is a phase 1 and phase 2 tool. The comparison across the hop itself needs a CSF-side
  extractor, which belongs to the phase 3 tasks that perform the hop, not here.
- Sidebar ordering is not captured. Order at 6.4.22 is implicit in barrel import order, and the
  settled decision in the PRD is that it becomes explicit when the barrel goes, so a captured order
  would encode something the plan intends to change. The baseline sorts deterministically instead,
  which answers what exists rather than in which sequence.
- Rollback is `git revert` of a single commit that touches only `.agent/`.

## Required Docs, Research, and Tracking Updates

- Set `task-001.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Correct `task-001.description`: the raw `.add(` grep figure reads 272, and the three `moment()`
  call sites are named at the paths `task-062` moved them to.
- No PRD change. Its Status Log entry of 2026-09-14 gives 272 and 267 and both reproduce. The 84
  file count in that entry was measured before `task-062` and is history, which the log's
  append-only rule says is corrected by a later entry rather than an edit. No later entry is due
  from this task, which changes no count.
- No research-note change. The notes are censuses taken at a stated commit.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-001-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-001-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `task-001-sidebar-baseline.txt` records 272 registrations under 53 titles in 15 groups, each with
  its label, and an index giving `path:line` and barrel load state for every one.
- `task-001-sidebar-extract.js` reproduces that file byte for byte from the repository, so the
  baseline is a derived artifact rather than a transcription.
- Two registrations are marked not loaded, both in `storybook/stories/staking/Legacy.stories.tsx`,
  and no other registration is.
- No label is unresolved.

## Final Outcome

- `task-001` completed. The baseline exists as a committed artifact and a committed tool, and the
  four tasks that diff against it are unblocked.
- The extraction agrees with an independent grep cross-check at 272, and with the PRD's Status Log
  figures of 272 and 267.
- The commit touches only `.agent/`. The derivation paths of `compile`, `lint`, `storybook` and
  `i18n` are unchanged across it. A control probe establishes that this means something: appending
  one comment line to `storybook/stories/index.ts` moved the `compile` derivation, and reverting it
  moved it back. The unchanged derivations are therefore a measurement rather than an appeal to the
  exclusion at `nix/internal/common.nix:269`.
- `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` all exit 0 on the post-change
  tree.
- Two defects were corrected in the `task-001` entry: a raw-grep figure of 270 that its own sentence
  contradicted, and three `moment()` paths that `task-062` had moved.

## Self-Review

- The baseline was cross-checked by a second method before being trusted, and the two methods agree.
- The plan states where the task entry disagrees with the repository and corrects the entry rather
  than quietly matching it.
- The plan says what the baseline does not capture: render behaviour and sidebar order, each with
  the reason.
- Scope held to the capture and the two tracking corrections. No story file, no configuration file
  and no source file was touched.
- One deviation from the task entry's implementation notes, taken deliberately and recorded above:
  the baseline is committed rather than left outside the repository, because the tasks that consume
  it run in later sessions.
- The staging precondition the plan carried over from `task-062` turned out to be narrower than
  stated. The control probe shows a modification to a tracked file reaches the flake source unstaged;
  it is an untracked file that is invisible. The precondition still holds for a task that adds
  files, which this one does, and the correction is recorded in the implementation review rather
  than silently applied.
