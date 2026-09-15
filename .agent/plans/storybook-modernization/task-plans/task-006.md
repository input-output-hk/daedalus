# Task task-006: Restage the wallet settings undelegation story

## Task ID and Title

- ID: `task-006`
- Title: `Restage the wallet settings undelegation story`

## Why Chosen Now

`task-006.dependencies` is `[task-001]`, and `task-001` is complete, so the sidebar baseline this
change is measured against exists. Nothing in the graph depends on `task-006`.

It is taken ahead of its position in the phase order because `task-002`, `task-003` and `task-004`
are still open and `task-006` shares no file with any of them. Its only dependency is satisfied.

Doing it before phase 3 matters more than its position suggests. The dialog and its fixture are
currently expressed as a prop to another component's story. The `storiesof-to-csf` codemod has no
opinion about a prop, so a misframed story stays misframed through the conversion and arrives in CSF
with the same defect and a rewritten shape around it.

## Interaction Mode

- Mode: `agent_execution`

The change is a story extraction verified by `yarn storybook:build`, `yarn compile` and `yarn lint`,
all three of which reproduce here as flake checks over the CI `node_modules`.

## Scope

- Extract the `UndelegateWalletConfirmationDialog` currently passed as the
  `undelegateWalletDialogContainer` prop of `WalletSettings` into a story of its own, under the same
  `Wallets / Settings` panel it sits in today, following the shape the sibling dialog stories in
  that directory already use.
- Move the wallet fixture that exists only to feed that dialog out of the settings screen module
  with it.
- Leave `WalletSettings` receiving an explicit `null` for the prop, which is what the application
  renders for that box.

## Non-Goals

- No change to `source/renderer/app/components/wallet/settings/WalletSettings.tsx`, to
  `UndelegateWalletConfirmationDialog`, or to any other file under `source/`.
- No change to `walletsConfig.ts:45` or to any feature flag. Locked decision 6 keeps the flag.
- No change to the existing `Undelegate Confirmation` and `Undelegate Confirmation Result` stories
  under `Decentralization / Staking`, which present the same dialog from the delegation flow with a
  different fixture. Those are a different presentation of the component, not a duplicate of this
  one.
- No knob conversion. The four knob call sites that move keep their `@storybook/addon-knobs` form;
  phase 4 owns them.
- No CSF conversion and no default export.

## Dependencies

- `task-001`, complete. Its baseline at
  `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-baseline.txt` is what the
  sidebar change is measured against.
- No task depends on `task-006`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 6 at
  `:225-231`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-006`
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`, the story census for the
  wallet settings screen
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
  - `.agent/skills/i18n-messaging/SKILL.md`, consulted and found not to apply: the extracted story
    defines no message and adds no id
- Workflows:
  - `.agent/workflows/storybook.md`, read for the registration model only, per the task-plans
    readme's caution that it teaches `storiesOf`, `withKnobs` and `@dump247/storybook-state`.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.
  - `.agent/skills/storybook-creation/SKILL.md` not followed as guidance, for the reason the
    task-plans readme gives. Its statement that the corpus uses `storiesOf()` is true and is why the
    new file uses `storiesOf()` rather than CSF.

## Live Repo Findings Verified For Planning

Verified at `1612cc70f`, 2026-09-14, against the working tree.

- The dialog is at `storybook/stories/wallets/settings/_support/WalletSettingsScreen.tsx:325-352`,
  passed as `undelegateWalletDialogContainer`. `task-062` moved this file; the task entry's
  `:326` line reference still resolves, because that move preserved line numbering.
- `source/renderer/app/components/wallet/settings/WalletSettings.tsx:196-211` is
  `renderUndelegateWalletBox`, which returns `null` at `:210` when
  `!IS_WALLET_UNDELEGATION_ENABLED || isLegacy`. `source/renderer/app/config/walletsConfig.ts:45`
  sets `IS_WALLET_UNDELEGATION_ENABLED = false`. `WalletSettings.tsx:252` is the only place
  `undelegateWalletDialogContainer` is rendered, and it is inside that box.
- The story is doubly dead, which the task entry does not say. Even with the flag on, the story's own
  `isDialogOpen` callback at `WalletSettingsScreen.tsx:196-226` has no branch for
  `UndelegateWalletConfirmationDialog` and falls through to `return false` at `:225`. The dialog
  would not open from this story under any knob setting.
- The component is live. `source/renderer/app/containers/staking/DelegationCenterPage.tsx:118-119`
  renders `UndelegateWalletDialogContainer` whenever `uiDialogs.isOpen(UndelegateWalletConfirmationDialog)`.
  This is what separates locked decision 6 from decision 4: the component is reachable, only the
  story's framing is wrong.
- The fixture in `WalletSettingsScreen.tsx` exists solely for this dialog. `selectedWallet` at
  `:112-119` has one reader, `:327`. `assets` at `:78-111` has one reader, `:115`. The imports that
  serve only those two are `bignumber.js` at `:4`, `generateHash`, `generatePolicyIdHash` and
  `generateWallet` at `:7-11`, and `stakingStakePools.dummy.json` at `:12`. `BigNumber` also appears
  at `:348` inside the dialog itself, so every one of its five uses leaves with the extraction.
- `undelegateWalletId` at `:40` does not leave. `:167` uses it as the knob group for the delegation
  status `select`, which belongs to the settings screen.
- Four knob call sites move with the dialog: `text` at `:328` and `:332`, `boolean` at `:341` and
  `:350`. `text` and `boolean` remain imported, because the settings screen uses both elsewhere.
- `WalletSettings.tsx:129` declares `undelegateWalletDialogContainer: Node` without a `?`, so
  omitting it is a TypeScript error at the JSX opening tag rather than a warning. `tsconfig.json:79`
  sets `strict: false` and `:81` leaves `strictNullChecks` commented out, so `null` is assignable to
  it and is the honest value: it is what the box holds in the application.
- The sibling dialog stories in the same directory establish the shape.
  `WalletUnpair.stories.tsx:30-31` and `WalletDelete.stories.tsx:42-43` both call
  `storiesOf('Wallets / Settings', module)` and take a single
  `(story) => <StoryDecorator>{story()}</StoryDecorator>` decorator. Neither uses knobs, so neither
  adds `withKnobs`; this story does use knobs and adds it.
- `WalletSettings.stories.tsx:7-11` imports the five sibling story files that register into the
  panel. A sixth import is how the new file is reached, because `storybook/main.ts:8` names one
  barrel entry and nothing globs.
- The `Wallets / Settings` panel currently holds 10 registrations, listed at
  `task-001-sidebar-baseline.txt:615-624`. The dialog has no label of its own in the baseline,
  because it is a prop rather than a registration.
- The checks are green at `1612cc70f`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` all exit 0, and the `compile`
  derivation is `y8k73visabqv6wyh6kdng3qbky2a3wry-daedalus-compile.drv`.

## Files Expected To Change

Added:

- `storybook/stories/wallets/settings/UndelegateWallet.stories.tsx`

Edited:

- `storybook/stories/wallets/settings/_support/WalletSettingsScreen.tsx`, losing the dialog, the
  fixture that fed it and the five imports that served only those
- `storybook/stories/wallets/settings/WalletSettings.stories.tsx`, gaining the import that reaches
  the new file

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-006.status`
- `.agent/plans/storybook-modernization/task-plans/task-006.md`
- `.agent/plans/storybook-modernization/task-plans/task-006-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-006-impl-review.md`

`storybook/stories/staking/_support/Undelegate.tsx` is named in the task's `targetPaths` and is not
edited. It is the model the new file follows, not a file the change touches. Its two stories present
the same component from the delegation flow and are unaffected.

## Implementation Approach

1. Create `storybook/stories/wallets/settings/UndelegateWallet.stories.tsx` on the shape
   `WalletUnpair.stories.tsx` uses: one `storiesOf('Wallets / Settings', module)`, a `StoryDecorator`
   decorator, and one `.add()`. Add `withKnobs` ahead of it, because the four knobs move with the
   dialog and a knob call outside a `withKnobs` decorator renders its default silently.
2. Move `assets`, `selectedWallet` and the five imports that serve only them into that file
   unchanged. Moving rather than rewriting keeps the fixture identical, so any difference in what
   the dialog renders is attributable to the framing and nothing else.
3. Give the story the label `Undelegate Wallet`, which is the section name the settings screen
   already uses for this dialog at `WalletSettingsScreen.tsx:40` and the knob group the moved knobs
   are already filed under. It does not collide with `Undelegate Confirmation` under
   `Decentralization / Staking`, which is a different fixture of the same component.
4. Replace the prop in `WalletSettingsScreen.tsx` with `null` and state the condition in a one-line
   comment, so the next reader does not have to find `WalletSettings.tsx:209` to understand why.
5. Add the import to `WalletSettings.stories.tsx` beside the other five.
6. Regenerate the sidebar from the `task-001` extractor and diff against the baseline. The expected
   difference is exactly one added line in the tree and one in the index, and a `Wallets / Settings`
   panel count of 11.
7. Verify with the flake checks.
8. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- The dialog renders standalone rather than inside a settings box the application returns null for.
  Evidence: it is registered by its own `.add()` under `Wallets / Settings`, and
  `WalletSettingsScreen.tsx` no longer imports it.
- `yarn storybook:build`, `yarn compile` and `yarn lint` pass. Evidence:
  `nix build .#checks.x86_64-linux.{storybook,compile,lint}` all succeed, on a derivation that
  moved.

The task entry also says to keep the story label recognizable against the `task-001` baseline or to
record the rename deliberately. Neither applies as worded: the dialog has no label in the baseline,
because a prop is not a registration. What happens instead is an addition, recorded here. The
sidebar gains one registration, `Wallets / Settings` goes from 10 to 11, and the corpus total goes
from 272 to 273. Every other line of the baseline is unchanged.

Added for this plan:

- The sidebar diff against the `task-001` baseline shows exactly one added registration and nothing
  else.
- The moved fixture is byte-identical to the fixture it replaces, so the dialog renders what it
  rendered before.

## Verification Plan

Already run for planning:

- The null path, read end to end: `WalletSettings.tsx:196-211`, `:252` and `walletsConfig.ts:45`.
- The second, independent reason the dialog never renders: `isDialogOpen` at
  `WalletSettingsScreen.tsx:196-226` has no branch for it.
- Reader enumeration for `selectedWallet`, `assets`, `BigNumber`, `STAKE_POOLS`, the three fixture
  helpers and `undelegateWalletId` across the settings screen module.
- The shape of the two sibling dialog stories in the same directory.

To run for the build:

- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, diffed
  against `task-001-sidebar-baseline.txt`, expecting one added tree line and one added index line.
- `nix build --no-link .#checks.x86_64-linux.storybook`
- `nix build --no-link .#checks.x86_64-linux.compile`
- `nix build --no-link .#checks.x86_64-linux.lint`, with the warning count compared against 5483.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a path other than
  `y8k73visabqv6wyh6kdng3qbky2a3wry-daedalus-compile.drv`.
- `git diff -M --stat`, expecting one added file and two edited.

If the sidebar diff shows more than the one addition, a label was disturbed and the response is to
restore it rather than accept the new tree. If `compile` goes red, the likely cause is the `null`
prop, and the fallback is to type it explicitly rather than to put the dialog back.

## Risks and Open Questions

- Passing `null` means that if `IS_WALLET_UNDELEGATION_ENABLED` is ever flipped to `true`, the
  settings screen story renders the undelegate box with no dialog behind its button. That is a
  latent gap and it is worth naming rather than leaving to be discovered. It is accepted because the
  story is already in that state for a second reason that a flag flip would not fix: its
  `isDialogOpen` never returns true for this dialog, so the button would not open anything either
  way. Whoever turns the flag on has to rewire the story regardless, and a `null` with a comment is
  a clearer signal to them than a dialog that was never reachable.
- The `Wallets / Settings` panel gains a story whose component already appears twice under
  `Decentralization / Staking`. That is duplication of a component, not of a story: the staking
  fixtures exercise a delegating wallet with a fee breakdown and a hardware variant, and this one
  exercises the stake pool name and ticker as text knobs. Locked decision 6 chose restaging over
  deletion precisely so that second surface survives.
- The new file is a sixth `storiesOf('Wallets / Settings')` call in one directory. The panel is
  already assembled from five files, so this adds an instance of an existing shape rather than a new
  one. `task-010` replaces the barrel with a glob and the file is reached the same way either side
  of that change.
- Rollback is `git revert` of a single commit.

## Required Docs, Research, and Tracking Updates

- Set `task-006.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- No PRD change. Locked decision 6 describes the defect and the remedy and both remain accurate.
- No research-note change. `research/01-current-coverage.md` is a census at a stated commit.
- The `task-001` baseline is not edited. A baseline that is updated to match each change stops being
  one; the addition is recorded here and in the implementation review.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-006-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-006-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `storybook/stories/wallets/settings/UndelegateWallet.stories.tsx` registers the dialog as
  `Undelegate Wallet` under `Wallets / Settings`, carrying the fixture and the four knobs unchanged.
- `_support/WalletSettingsScreen.tsx` lost the dialog, `assets`, `selectedWallet` and five imports,
  and passes `null` for `undelegateWalletDialogContainer` with the condition stated in a comment.
- `WalletSettings.stories.tsx` reaches the new file through a sixth sibling import.

## Final Outcome

- `task-006` completed. The dialog has a registration of its own and is no longer expressed as a
  prop to a box the application renders as `null`.
- The sidebar diff against the `task-001` baseline is one added registration and nothing else.
  `Wallets / Settings` reads 11, the corpus total reads 273.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile` and `.lint` all exit 0, with
  5483 lint warnings, the count the corpus already carried.
- The `compile` derivation moved from `y8k73visabqv6wyh6kdng3qbky2a3wry` to
  `s5mhd4b761b2lbl154ypx95x41348b6w`, so the green results are about the changed tree.
- One finding that outlives this task, recorded in full in the implementation review: the `compile`
  check does not catch a dangling relative import. A probe adding `import './NoSuchModuleProbe.stories';`
  to a tracked story file left `nix build .#checks.x86_64-linux.compile` at exit 0. Only
  `storybook:build` catches it, and only for a module the barrel reaches.

## Self-Review

- The plan found a second reason the story was dead that the task entry does not record, and that
  finding changed the risk assessment rather than being noted and set aside.
- The plan says plainly that the task entry's label criterion does not apply as worded, and states
  what it substitutes and what the sidebar does instead.
- The fixture was moved rather than rewritten, so nothing about what the dialog renders is left to
  judgment.
- Scope held to three story-side files. No component, no flag and no `source/` file was touched, and
  the staking undelegate stories were left alone.
