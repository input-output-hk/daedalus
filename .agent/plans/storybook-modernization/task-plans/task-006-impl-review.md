Implementation: Iteration 1
Timestamp: 2026-09-14T20:24:15Z

Changes made:
- Added `storybook/stories/wallets/settings/UndelegateWallet.stories.tsx`, registering
  `UndelegateWalletConfirmationDialog` as `Undelegate Wallet` under `Wallets / Settings`, on the
  shape `WalletUnpair.stories.tsx` uses plus a `withKnobs` decorator, because four knobs move with
  the dialog.
- Moved `assets`, `selectedWallet` and the five imports serving only them out of
  `_support/WalletSettingsScreen.tsx` and into the new file unchanged.
- Replaced `undelegateWalletDialogContainer` in `_support/WalletSettingsScreen.tsx` with `null`, and
  stated the condition under which `WalletSettings` renders it in a three-line comment.
- Added `import './UndelegateWallet.stories';` to `WalletSettings.stories.tsx` beside the five
  siblings.
- Set `task-006.status` to `completed` and `## Build Status` to `completed`.

Files touched:
- `storybook/stories/wallets/settings/UndelegateWallet.stories.tsx` (added)
- `storybook/stories/wallets/settings/_support/WalletSettingsScreen.tsx`
- `storybook/stories/wallets/settings/WalletSettings.stories.tsx`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`
- `.agent/plans/storybook-modernization/task-plans/task-006.md`
- `.agent/plans/storybook-modernization/task-plans/task-006-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-006-impl-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-062-impl-review.md`, one appended
  correction entry, for the reason under Deviations

Verification run:
- Sidebar regenerated with the `task-001` extractor and diffed against
  `task-001-sidebar-baseline.txt`. The tree gains exactly one line, `STORY  Undelegate Wallet` under
  `Wallets / Settings`, the panel count reads `[11]` against `[10]`, and the totals read
  `REGISTRATIONS 273` and `REACHABLE 271`. No other tree line moved. The index gains the
  corresponding row and shifts `Wallets / Settings | Wallet Settings` from
  `WalletSettings.stories.tsx:17` to `:18`, which is the added import line and is exactly the kind
  of movement the plan says the index is expected to show and the tree is not.
- `nix build --no-link .#checks.x86_64-linux.storybook` exit 0, after the failure described below.
- `nix build --no-link .#checks.x86_64-linux.compile` exit 0, on derivation
  `s5mhd4b761b2lbl154ypx95x41348b6w-daedalus-compile.drv`, which is not the
  `y8k73visabqv6wyh6kdng3qbky2a3wry` of the previous commit.
- `nix build --no-link -L .#checks.x86_64-linux.lint` exit 0, 5483 warnings. That is the count the
  corpus carried before this change, so the suppression comments that moved with the dialog are
  reported against their new file and none was added or lost.
- `prettier --check` over all three story files: clean.
- The dialog renders standalone: `_support/WalletSettingsScreen.tsx` no longer imports
  `UndelegateWalletConfirmationDialog`, and the new file registers it directly with no settings
  screen around it. First acceptance criterion met.

Two findings from the run, both worth keeping:

- The first `storybook` run failed with
  `Module not found: Error: Can't resolve './UndelegateWallet.stories'`. The new file was untracked,
  and `nix build` on a git source sees tracked content only. Staging it and rerunning gave exit 0 in
  72 seconds. This is the precondition `task-062` recorded, hitting for the first time in the form
  it actually takes: the danger is not a modified tracked file, which the flake source does see
  unstaged, but a newly created one, which it does not.
- The same tree passed `compile` while the `storybook` build could not resolve the import, which
  means `yarn compile` does not catch a dangling relative import in this repository. That was
  confirmed directly rather than inferred: appending
  `import './NoSuchModuleProbe.stories';` to a tracked story file and running
  `nix build .#checks.x86_64-linux.compile` returned exit 0. The probe line was then removed. This
  matters beyond this task, because `task-062` recorded "yarn compile passes, proving no importer
  was left dangling" as its evidence for exactly that property. Its conclusion still holds, because
  it also ran `storybook:build` green and walked the module graph, but the inference from `compile`
  does not. A correction entry was appended to `task-062-impl-review.md` rather than editing what it
  recorded.

Deviations from the approved plan:
- One file outside the plan's list was touched: `task-062-impl-review.md`, which gained an appended
  correction entry. The task-plans readme says a correction is a new entry saying what was wrong and
  never an edit to the original, and that is what it is. Leaving a verification claim standing that
  this task has just measured to be false would mislead every later task that copies it.

User interaction is now required:
- No.

Outcome: Both acceptance criteria met, the sidebar diff is the single intended addition, and the
three checks are green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-14T20:29:40Z

Summary:
- Approved. The dialog now has a registration of its own, the fixture moved with it unchanged, and
  the sidebar diff against the `task-001` baseline is one added line and nothing else. The `null`
  and the comment that explains it are the right outcome for a prop the application does not render.

Blocking findings:
- None.

Non-blocking observations:
- Moving the fixture rather than rewriting it is what makes the sidebar diff readable. Had the
  fixture been rebuilt in the new file, a difference in what the dialog renders would have been
  impossible to attribute between the framing change and a fixture change, and neither `compile` nor
  `storybook:build` would have said a word about it.
- The plan's finding that the story was dead twice over, once through the flag and once through
  `isDialogOpen`, changed the answer on the `null`. Without it, the flag-flip gap would read as an
  oversight; with it, it is a gap that already existed and that a flag flip alone never closed.
- The `compile` probe is the most valuable thing in this log. A check that everyone in this plan has
  been treating as the guard against a broken import does not guard against it. The distinction that
  matters for the phases ahead is that `storybook:build` catches an unresolvable import only for
  modules the barrel reaches, which is why `task-010` replacing the barrel with a glob widens what
  that check can see rather than merely tidying the configuration.
- Correcting `task-062`'s log rather than quietly knowing better is right. The claim is copied
  almost verbatim into several later task entries' acceptance criteria, and they will be read.
- The `Wallets / Settings` panel now holds two presentations of the undelegate flow across the
  corpus, three counting the staking result dialog. The plan argues they are different fixtures of
  one component rather than duplicates, which is correct as written and is the reason locked
  decision 6 chose restaging over deletion.

Approval bar:
- Met. `task-006` is complete. No task depends on it.

Decision: approved
