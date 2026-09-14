# Task task-002: Delete the four flag-disabled story sets

## Task ID and Title

- ID: `task-002`
- Title: `Delete the four flag-disabled story sets`

## Why Chosen Now

`task-002.dependencies` is `[task-001]`, and `task-001` is complete, so the sidebar baseline the
removal is measured against exists. `task-007` lists `task-002` and `task-003` as dependencies,
because it re-verifies orphan status after both have removed their files, so this is the first of
the three deletions that has to land.

Doing it before phase 3 removes 12 registrations and 19 knob call sites from the conversion surface
rather than converting them and deleting them afterwards.

## Interaction Mode

- Mode: `agent_execution`

Every edit and every check reproduces here. The file removals themselves are performed by the
operator against the list this task produces, which is recorded under Implementation Approach as a
step rather than left implicit.

## Scope

- Remove the four story files that document screens no affordance reaches: `PaperWallets` (5
  registrations), `TransferFunds` (2), `LegacyNotification` (1) and `CountdownParty` (1).
- Remove three registrations from `storybook/stories/staking/Staking.stories.tsx`: the whole
  `Decentralization / Countdown` panel, and the `Info` and `Info Countdown` registrations.
- Remove the imports, constants and decorator branches in `Staking.stories.tsx` that those three
  registrations were the only readers of.
- Remove the four barrel entries that reach the deleted files.

## Non-Goals

- No change to any file under `source/renderer/app/components` or `source/renderer/app/config`. The
  components and the flags stay, which is the acceptance criterion and locked decision 4.
- No change to the voting stories. Locked decision 13 keeps `storybook/stories/voting/Voting.stories.tsx`
  and everything rendering a component under `source/renderer/app/components/voting/`, on the
  judgment that Catalyst is suspended rather than retired. Same observable state as these four,
  different product judgment.
- No change to `storybook/stories/staking/_support/Epochs.tsx` or the `Epochs` registration.
  `task-004` owns the `/staking/epochs` removal.
- No knob conversion and no CSF conversion.
- No removal of `storybook/stories/wallets/_utils/` modules. `task-007` re-verifies those after this
  task and `task-003` land, and re-verification is the point.

## Dependencies

- `task-001`, complete. Its baseline at
  `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-baseline.txt` is what the
  removal is measured against.
- `task-007` depends on this task and on `task-003`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 4 at
  `:207-215` and locked decision 13 at `:296-306`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-002`, `task-003`
  and `task-007`
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`, the story census
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, read for the barrel and registration model only, per the
    task-plans readme's caution.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `553dde379`, 2026-09-14, against the working tree.

- The four files exist at the paths the task entry names, all four are tracked by git, and the
  registration counts are as stated: `PaperWallets.stories.tsx` 5 (69 lines),
  `TransferFunds.stories.tsx` 2 (102 lines), `LegacyNotification.stories.tsx` 1 (22 lines),
  `CountdownParty.stories.tsx` 1 (70 lines).
- The three registrations in `Staking.stories.tsx` are at the lines the task entry gives:
  `storiesOf('Decentralization / Countdown', module)` at `:92`, `pageNames.info` at `:176` and
  `pageNames['info-countdown']` at `:193`.
- The four barrel entries: `storybook/stories/wallets/index.ts:17` `./legacyWallets/TransferFunds.stories`,
  `:18` `./legacyWallets/LegacyNotification.stories`, `:19` `./paperWallets/PaperWallets.stories`,
  and `storybook/stories/index.ts:11` `./staking/CountdownParty.stories`.
- Nothing else imports the four files. A grep for each basename across `source/`, `storybook/` and
  `tests/` returns only those four barrel lines. Every other hit is application code that shares a
  name, such as `components/notifications/LegacyNotification.tsx` and the transfer-funds containers,
  none of which this task touches.
- Every flag the task entry names is where it says and holds the value it says:
  `source/renderer/app/config/stakingConfig.ts:104` `IS_STAKING_INFO_PAGE_AVAILABLE = false`,
  `source/renderer/app/config/walletsConfig.ts:44` `IS_BYRON_WALLET_MIGRATION_ENABLED = false`, and
  `source/renderer/app/stores/SidebarStore.ts:123` and `:124`, which map
  `PAPER_WALLET_CREATE_CERTIFICATE` and `STAKING_DELEGATION_COUNTDOWN` to `false` in the sidebar
  category filter.
- The flags are load-bearing, not decorative. `IS_STAKING_INFO_PAGE_AVAILABLE` gates the route at
  `Routes.tsx:201` and the navigation tab at `containers/staking/Staking.tsx:115`.
  `IS_BYRON_WALLET_MIGRATION_ENABLED` gates `components/layout/TopBar.tsx:95`, which is the only
  thing that renders `LegacyNotification`, and its action is what opens the transfer-funds flow.
- The knob figure in the task entry is short. The four deleted files hold 13 knob call sites, 6 in
  `TransferFunds`, 5 in `CountdownParty`, 2 in `LegacyNotification` and none in `PaperWallets`. The
  three registrations removed from `Staking.stories.tsx` hold 6 more: the `date` call inside
  `startDateTimeKnob`, the `number` call in the `Info` story, and four `boolean` calls in
  `Info Countdown`. The total this task removes from the phase 4 surface is 19, not 13.
- The three removed registrations are the only readers of five things in `Staking.stories.tsx`, all
  of which go with them: the `StakingCountdown`, `StakingInfo` and `StakingInfoCountdown` imports;
  `defaultPercentage`, `defaultStartDateTime` and the `startDateTimeKnob` helper; the `date` binding
  in the `@storybook/addon-knobs` import; and the `countdown`, `info` and `info-countdown` entries in
  the `pageNames` map. `number` and `boolean` stay imported, because surviving registrations use
  both.
- The decorator at `Staking.stories.tsx:54` carries two branches that only the countdown story
  reached: the `if (context.parameters.id === 'countdown')` test that selects
  `CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN.route`, and the `context.parameters.id === 'countdown' ||`
  arm of the test that decides whether to wrap the story in `StakingWithNavigation`. After the
  removal no registration in the file carries `id: 'countdown'`.
- `pageNames['stake-pools-tooltip']` is already unread and was before this task. It is left alone:
  it is not one of the three registrations this task removes, and removing it would be an unrelated
  edit in this commit.
- `.eslintrc:90` sets `no-unused-vars` to `warn`, so none of the imports or constants above would
  fail a check if left behind. They are removed deliberately.
- The four deleted files hold 21 registrations' worth of nothing else: none of them exports a symbol
  another module imports, and none contains `require(` or a dynamic `import(`.
- The `task-001` baseline records 272 registrations. The corpus is at 273 after `task-006` added
  `Wallets / Settings | Undelegate Wallet`. Removing 12 takes it to 261.
- Three panels disappear entirely, because this task removes every registration they hold:
  `Decentralization / Countdown` (2), `Wallets / Legacy Wallets` (3) and `Wallets / Paper Wallets`
  (5). `Decentralization / Staking` loses 2 of its 16 and survives. The distinct-title count goes
  from 53 to 50.
- The checks are green at `553dde379`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n,stylelint}` all exit 0,
  and `compile` is `09fldjchx059x42zin99x8v1dvdq0hwk-daedalus-compile.drv`.
- `yarn compile` does not catch a dangling relative import in this repository; that was measured
  during `task-006` and recorded as a correction in `task-062-impl-review.md`. The check that
  catches a barrel entry pointing at a removed file is `storybook:build`, and it catches it only for
  modules the barrel reaches, which these are. Both are run.

## Files Expected To Change

Removed, four files:

- `storybook/stories/wallets/paperWallets/PaperWallets.stories.tsx`
- `storybook/stories/wallets/legacyWallets/LegacyNotification.stories.tsx`
- `storybook/stories/wallets/legacyWallets/TransferFunds.stories.tsx`
- `storybook/stories/staking/CountdownParty.stories.tsx`

Edited, three files:

- `storybook/stories/staking/Staking.stories.tsx`
- `storybook/stories/wallets/index.ts`
- `storybook/stories/index.ts`

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-002.status`
- `.agent/plans/storybook-modernization/task-plans/task-002.md`
- `.agent/plans/storybook-modernization/task-plans/task-002-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-002-impl-review.md`

No file under `source/` changes. Removing `paperWallets/` and `legacyWallets/` empties both
directories, and git does not track directories, so no directory entry is removed.

## Implementation Approach

1. Remove the three registrations from `Staking.stories.tsx` together with the imports, constants,
   knob binding and `pageNames` entries they were the only readers of, and simplify the two
   decorator branches that only the countdown story reached. The countdown branch is removed rather
   than left because a test against `id: 'countdown'` in a file where no registration carries that
   id is a statement that is no longer true, and nothing in the check set would ever flag it.
2. Remove the four barrel entries in the same change. A barrel line pointing at a removed file is
   the one failure `storybook:build` does catch, so the ordering does not matter for correctness,
   but landing them apart would leave an intermediate state that cannot build.
3. Write the four paths to the deletion list, one per line, repo-relative, and hand off. Every path
   is a tracked file and none is a directory, so the list needs no expansion.
4. After the removals are made, confirm the working tree state before running anything: four
   deletions staged, no untracked file under `storybook/`.
5. Regenerate the sidebar with the `task-001` extractor and diff against the baseline. The expected
   difference is 12 registrations gone, three panels gone, and the one `task-006` addition.
6. Verify with the flake checks.
7. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- `yarn storybook:build`, `yarn compile` and `yarn lint` pass. Evidence:
  `nix build .#checks.x86_64-linux.{storybook,compile,lint}` all succeed on a derivation that moved.
- The sidebar diff against `task-001` shows exactly 12 registrations removed and nothing else.
  Evidence: the regenerated tree, diffed line by line against the baseline.
- No file under `source/renderer/app/components` or `source/renderer/app/config` is modified.
  Evidence: `git diff --name-only` lists seven paths and none is under `source/`.

The second criterion needs one qualification. The baseline was taken before `task-006`, which added
one registration deliberately and recorded it. The diff against the baseline therefore shows 12
removals and 1 addition, and the addition is `task-006`'s. Measured against the tree as it stood
before this task, this change removes 12 registrations and adds none.

Added for this plan:

- Three panels disappear and are named, so a panel vanishing is not read as a defect.
- The corpus total moves from 273 to 261 and the distinct-title count from 53 to 50.

## Verification Plan

Already run for planning:

- Existence, tracked status, line count and registration count for each of the four files.
- Importer enumeration for all four basenames across `source/`, `storybook/` and `tests/`.
- The three registration line numbers in `Staking.stories.tsx` against the task entry.
- Reader enumeration inside `Staking.stories.tsx` for every import, constant and `pageNames` entry
  the removal touches.
- Every flag named in the task entry, read at its stated line, plus its readers.
- Knob call site counts per file.

To run for the build:

- `git status --short`, before any check: four deletions, three modifications, nothing untracked
  under `storybook/`.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, diffed
  against the baseline.
- `nix build --no-link .#checks.x86_64-linux.storybook`
- `nix build --no-link .#checks.x86_64-linux.compile`
- `nix build --no-link .#checks.x86_64-linux.lint`, with the warning count compared against 5483.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a moved path.
- `git diff --name-only`, expecting no path under `source/`.

If the sidebar diff shows a removal outside the 12, a surviving registration was disturbed and the
response is to restore it. If `storybook:build` fails on an unresolved module, a barrel entry was
missed.

## Risks and Open Questions

- Deleting a story is not reversible in the sidebar's history, only in git's. Locked decision 4
  accepts that: every flag is a one-line reversion, and writing the story back in CSF against a live
  feature is cheaper than carrying it through the conversion. The components stay, so nothing about
  the features themselves is decided here.
- Three panels disappearing is the most visible change this task makes and the one most likely to be
  reported as a regression by someone who has the old sidebar in mind. They are named in the commit
  body for that reason.
- `TransferFunds.stories.tsx` imports `WalletsWrapper` from `wallets/_utils/`, which three surviving
  files also import, so it is not orphaned. `task-007` re-verifies the whole `_utils` directory after
  this task and `task-003`, which is why that task depends on both.
- The decorator simplification is the one edit in this task that is not a straight deletion of named
  material. It is confined to `Staking.stories.tsx`, it affects no surviving registration's
  rendering, because every survivor already took the else branch, and it is recorded here so it is
  reviewed rather than discovered.
- Rollback is `git revert` of a single commit.

## Required Docs, Research, and Tracking Updates

- Set `task-002.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- No PRD change. Locked decisions 4 and 13 describe the decision and remain accurate. The knob
  figure of 13 in the task entry is short by the six in `Staking.stories.tsx`; the correction is
  recorded here rather than by editing the entry, because the entry's sentence is about the four
  deleted files and is true of them.
- No research-note change. `research/01-current-coverage.md` is a census at a stated commit.
- The `task-001` baseline is not edited.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-002-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-002-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The four story files are gone, with `storybook/stories/wallets/paperWallets/` and
  `storybook/stories/wallets/legacyWallets/` emptied and therefore absent from the tree.
- `Staking.stories.tsx` lost the `Decentralization / Countdown` panel, the `Info` and
  `Info Countdown` registrations, three component imports, three constants, the `date` knob binding,
  three `pageNames` entries and two decorator branches, at 88 deletions against 3 insertions.
- The four barrel entries are gone from `storybook/stories/wallets/index.ts` and
  `storybook/stories/index.ts`.
- `git diff --name-only` lists seven paths and none is under `source/`.

## Final Outcome

- `task-002` completed. `task-003` and `task-007` are unblocked on this dependency.
- The sidebar reads 261 registrations across 50 distinct titles, against 273 and 53 before this
  task. The removal is exactly the 12 registrations the task names, and the sidebar diff against the
  `task-001` baseline shows those 12 removals plus the single addition `task-006` recorded.
- Three panels disappeared entirely, because this task removed every registration each held:
  `Decentralization / Countdown`, `Wallets / Legacy Wallets` and `Wallets / Paper Wallets`.
  `Decentralization / Staking` went from 16 registrations to 14.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile` and `.lint` all exit 0, on a
  `compile` derivation that moved to `p9d3hpssnxqpdspkrm63k8vh1nisl6cw-daedalus-compile.drv`.
- `yarn lint` reports 5468 warnings against the 5483 the corpus carried, the difference being
  warnings that lived in the deleted files.
- The knob measurement reconciles against the PRD exactly. The corpus held 396 call sites before
  this task and holds 377 after, and 396 is the whole-corpus figure the PRD's Status Log records.
  The 19 that left are the 13 in the four deleted files plus 6 in the three removed registrations.

## Self-Review

- The acceptance criterion about the sidebar was reported both ways rather than picked to suit:
  against the current tree the change is 12 removals and no addition, and against the `task-001`
  baseline it is 12 removals and `task-006`'s 1 addition.
- The knob figure was measured rather than copied, found short by six, corrected in the task entry
  because the phase 4 total is derived from it, and then cross-checked against the PRD's own
  corpus-wide 396, which it reconciles with exactly.
- The decorator simplification is the one edit that is not the removal of named material. It was
  argued in the plan before being made, it changes no surviving registration's rendering, and every
  survivor already took the branch that remains.
- Scope held. No file under `source/` changed, no flag moved, the voting stories were left alone,
  and the `_utils` re-verification was left to `task-007` where it belongs.
