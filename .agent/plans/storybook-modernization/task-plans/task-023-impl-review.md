Implementation: Iteration 1
Timestamp: 2026-09-15T16:08:25Z

Changes made:
- None to `storybook/` or `source/`. The comparison found nothing to correct.

Verification:
- Baseline 272 `title | label` pairs from the committed baseline file; today 258 from `index.json`.
  Set difference in both directions: 15 absent, 1 present that the baseline lacks, 257 identical.
- The 15 absent are the phase 1 deletions in full: `Decentralization / Countdown` two,
  `Decentralization / Staking` three, `StakingChart` two, `Wallets / Legacy Wallets` three,
  `Wallets / Paper Wallets` five.
- The one addition is `Wallets / Settings | Undelegate Wallet`, the `task-006` restaging.
- Panels 53 to 49, four gone and none added, each of the four having held only deleted
  registrations. Groups 15 to 14, `StakingChart` gone and none added.
- Of the 152 baseline labels an export identifier cannot round-trip, 147 are present and identical
  and the 5 absent are all phase 1 deletions. Listed individually rather than counted.
- 151 stories carry an explicit `name`, and for all 151 the derived name would differ. No explicit
  name is redundant, so none is masking a mismatch.

The four control-flow strings, checked as behaviour:
- `context.kind` and `context.story` still carry the panel title and the story display name at
  8.6.18, measured by composing a synthetic story through `@storybook/react` and printing both.
- `WalletsWrapper`'s `context.story !== 'Empty' && context.story !== 'Wallet Add'` has always been
  true for every wallets story. The baseline's three `Empty` stories are under `Governance`,
  `Navigation` and `News`, and `Wallet Add` appears nowhere in it. The wrapping path is taken now as
  it was then.
- `SettingsWrapper`'s `linkTo(context.kind, ...)` resolves as before, except for
  `/settings/terms-of-service`, which `pageNames` spells `Terms of service` against a story called
  `Terms of Service`. `linkTo` is case sensitive and that item has never navigated.
- `SettingsWrapper`'s active-item derivation maps `Themes` to `/settings/display` correctly and
  `General` to `/settings/general`, which is not one of the five menu routes, so the General panel
  has never shown an active item.
- `WalletWithNavigationLayout`'s `context.kind.replace('Wallets|', '')` never matches, because
  titles use ` / `. `activeItem` gets `wallets / summary` instead of `summary`, and the
  `walletStories` map beside it targets `Wallets|Send` and the rest. Both are Storybook 5 separator
  syntax and both were wrong before this work.

None of the four changed. Three of the four do less than they appear to, and that was true at the
baseline as well, so the criterion "still wraps or does not wrap as it did before" is met in the
literal sense while being worth writing down in the honest one.

Deviations from the approved plan:
- None.

Outcome: The sidebar matches the baseline with sixteen enumerated differences, all of them phase 1
work, and no label was silently renamed; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T16:14:40Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- Checking the four strings as behaviour rather than as text is what makes this task worth running,
  and it is the difference between "the strings are unchanged" and "here is what each of them
  does". Three of the four do less than they look like they do.
- The three pre-existing defects are reported and not fixed. That is the right boundary for a
  verification task: fixing a wallet navigation link here would have made the sidebar comparison
  test a tree that no longer matched what the tranches produced.
- Re-deriving 80 as 152 cost one command and closed 72 labels that would otherwise have gone
  unexamined on the strength of a figure taken before the conversion.
- The strongest single line is that 257 of 258 pairs are identical to a baseline taken before any of
  this started, and the one that is not was deliberately restaged.

Approval bar:
- Met. `task-023` is complete.

Decision: approved
