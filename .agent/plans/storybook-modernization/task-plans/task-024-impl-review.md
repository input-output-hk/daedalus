Implementation: Iteration 1
Timestamp: 2026-09-15T16:52:10Z

Changes made:
- `_support/StoryWrapper.tsx`: `<Story osName locale currentTheme />` becomes `<Story />`. `themeId`
  and the `themesIds` import go with it, both having existed only to be passed down. The component
  comment now describes the frame it builds rather than the mechanism it replaced.
- `wallets/receive/WalletReceive.stories.tsx` and
  `wallets/settings/WalletRecoveryPhraseVerification.stories.tsx`: two `@ts-ignore` describing a
  `({ locale })` story signature the tranches replaced. Removed, and `compile` stays at zero, so
  they were suppressing nothing.

Verification:
- The required grep returns nothing in any story file. The `props.currentTheme` and `props.locale`
  matches that remain are inside components that declare those props and are handed them explicitly.
- Seven spreads of a story argument remain: six into `StoryLayout` and one into
  `WalletsTransactionsWrapper`. Each is followed by explicit props for what the pass-through used to
  supply, and what the spreads still carry is `story` and `kind`, which Storybook provides.
- `compile` exit 0, `lint` exit 0, `storybook` exit 0.
- Label set from `index.json`: 258 entries across 49 titles, identical to the pre-conversion set.
- `story-args-audit.js`: no story reads an argument nothing fills.

The probe, because a green check that cannot go red is not evidence: removing
`currentTheme={currentThemeOf(context)}` from the `StakePoolsTableStory` call in
`staking/Staking.stories.tsx` makes `compile` report
`storybook/stories/staking/Staking.stories.tsx:114:6 - error TS2741: Property 'currentTheme' is
missing in type '{}' but required in type 'Props'`. Restoring it returns the check to exit 0. So the
compiler is watching these call sites, and its silence about the rest is a result rather than an
absence.

Deviations from the approved plan:
- None.

Outcome: The pass-through is gone, the toolbar selections reach a story only through its context,
and the check that says so has been shown to be capable of failing; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T16:57:30Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- The probe is what makes this task's green mean something. The `task-009` census predicted that
  this exact change would fail silently for thirty spread-forwarders, and the reason it does not is
  that those thirty no longer spread. Demonstrating that the compiler catches a missing prop turns
  that from a claim into a measurement, and it took one edit and one build.
- Removing `themeId` matters more than its two lines. It was the last thing in the wrapper feeding a
  route that no longer exists, and leaving it would have left the file describing two mechanisms
  where there is one.
- The two suppressions came off with `compile` still at zero, which is the small version of the same
  discipline: remove and check, rather than remove and assume.

Approval bar:
- Met. `task-024` is complete and `task-025` has its last precondition.

Decision: approved
