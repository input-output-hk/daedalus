Implementation: Iteration 1
Timestamp: 2026-09-15T13:58:30Z

Changes made:
- Six empty-render sites moved onto `(_args, context)` and `_support/globals.ts`:
  `addWallet/Create`, `addWallet/Restore`, `receive/WalletReceive` twice,
  `settings/PublicKeyQRCode` and `summary/WalletSummary`. `Restore`'s now unused local `Props` type
  went with it.
- `settings/WalletSettings.stories.tsx`: `<WalletSettingsScreen locale={localeOf(context)} />`
  instead of spreading the whole story context, and the six barrel-era `import './X.stories'` lines
  removed.
- `settings/PublicKeyQRCode.stories.tsx`: the `{...props}` spread dropped. It spread
  `context.args`, which nothing declares, so it contributed nothing.
- `settings/WalletRecoveryPhraseVerification.stories.tsx`: locale from the context.
- `tokens/WalletTokens` and `tokens/WalletTokensList`: display names in the object form, clearing
  two `TS2339`.
- `transactions/TransactionsList.stories.tsx`: `WalletsTransactionsWrapper` is given
  `locale={localeOf(props)}` rather than picking it out of the spread.
- All six `StoryLayout` call sites get an explicit `currentTheme={currentThemeOf(context)}`:
  `wallets/_utils/WalletsWrapper`, `wallets/_utils/HardwareWalletsWrapper`, and outside the tranche
  `common/ItemsDropdown`, `common/Widgets`, `staking/_support/decorator` and
  `settings/utils/SettingsWrapper`.
- `_support/globals.ts`: `localeOf` returns `Locale` rather than `string`.

Verification:
- Empty-render burn-down 7 to 1, the six predicted. The remaining one is
  `staking/Staking.stories.tsx`, in `task-021`.
- `compile` 16 to 14, the two cleared being the `wallets/tokens` display names.
- `lint` exit 0, `storybook` exit 0.
- Label set from `index.json`: 258 entries across 49 titles, identical to the pre-conversion set. In
  particular the six stories whose side-effect imports were removed are all still there, which is
  what that removal needed to prove.
- `@ts-ignore` in the tranche unchanged at 57.
- The audit's unfollowable count falls 18 to 16, because the two `wallets/tokens` exports are now
  objects with a `render` it can read.

One correction during the work:

- Passing `localeOf(context)` to `WalletSettingsScreen` produced a new error,
  `TS2322: Type 'string' is not assignable to type 'Locale'`. The helper was typed to return
  `string` while the two locales are a closed set. Narrowing `localeOf` to `Locale` fixed it at the
  source rather than at the call site, and every other consumer gets the narrower type. Worth noting
  that this appeared only because a prop was passed explicitly: the spread it replaced was passing
  the same value with no type checking at all.

Deviations from the approved plan:
- None.

Outcome: Six empty renders cleared, the last one left for `task-021`, and no layout in the corpus
depends on the prop pass-through for its theme; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T14:05:15Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- Taking all six `StoryLayout` sites rather than the two in this directory is the right call and the
  plan review is right about why. The half-done state is not merely untidy: it passes `task-024`'s
  own check while leaving four layouts themed from nothing.
- The `WalletSettings` spread is worth keeping in mind as the mirror of `IncidentOverlay`. One read
  an argument nothing fills; the other read one filled with the wrong thing and handed a component a
  story id, a parameters object and a globals object as props. Neither is visible to any check.
- The `TS2322` is a small piece of evidence for explicit props over spreads. The same value was
  flowing before and nothing typed it.
- Removing the six side-effect imports was safe and the label set proves it rather than asserting
  it. Those lines were how the barrel reached the siblings, and the glob has reached them directly
  since `task-010`.

Approval bar:
- Met. `task-020` is complete.

Decision: approved
