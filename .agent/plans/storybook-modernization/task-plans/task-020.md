# Task task-020: Hand-finish the wallets tranche

## Task ID and Title

- ID: `task-020`
- Title: `Hand-finish the wallets tranche`

## Why Chosen Now

`task-020.dependencies` is `[task-018]`, complete. It is the largest tranche and carries six of the
seven remaining empty-render sites.

## The red window

`compile` 16 before, 14 after. `lint` and `storybook` green throughout.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- The 27 story files under `wallets`, plus the colocated `WalletTokenPicker.stories.tsx`.
- The wrappers in `wallets/_utils/`.
- Every `StoryLayout` call site in the corpus, for the reason set out below.

## Non-Goals

- Knobs and `withState` are untouched.
- No label changes; the restaged undelegation story keeps its `task-006` framing.
- No `@ts-ignore` added or removed.

## Dependencies

- `task-018`. `task-023` and `task-024` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-018.md`, the settled patterns
- `.agent/plans/storybook-modernization/task-plans/task-009.md`, the census
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-020`, `task-024`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `77787c399`.

- 27 files, of which 11 change.
- Six of the seven remaining empty-render sites are here: `addWallet/Create`, `addWallet/Restore`,
  `receive/WalletReceive` twice, `settings/PublicKeyQRCode` and `summary/WalletSummary`.
- `settings/WalletSettings.stories.tsx` spreads the entire story context into `WalletSettingsScreen`,
  which takes `{ locale: Locale }`. Everything else in that spread, `id`, `name`, `parameters`,
  `globals` and the rest, arrives as a prop the screen never declared.
- The same file imports six sibling `.stories` modules for their side effects. That is barrel-era
  registration; under the glob each sibling is indexed on its own.
- `settings/PublicKeyQRCode.stories.tsx` spreads its first argument into the dialog. That argument is
  `context.args` and nothing declares any, so the spread contributes nothing.
- `StoryLayout` takes `currentTheme` and has no default for it. Six call sites give it the whole
  context with a spread, and the context has `currentTheme` only because `StoryWrapper` still renders
  `<Story currentTheme=... />`. The six are in four different tranches: `common/ItemsDropdown`,
  `common/Widgets`, `staking/_support/decorator`, `wallets/_utils/WalletsWrapper`,
  `wallets/_utils/HardwareWalletsWrapper` and `settings/utils/SettingsWrapper`.
- `wallets/_utils/WalletsTransactionsWrapper` reads `locale` from the same spread.
- Two of the 16 `TS2339` are here, both `storyName` on a `withState` result under `wallets/tokens`.
- The tranche holds 57 `@ts-ignore` directives.

## Files Expected To Change

Wallets: `addWallet/Create`, `addWallet/Restore`, `receive/WalletReceive`,
`settings/PublicKeyQRCode`, `settings/WalletRecoveryPhraseVerification`, `settings/WalletSettings`,
`summary/WalletSummary`, `tokens/WalletTokens`, `tokens/WalletTokensList`,
`transactions/TransactionsList`, `_utils/WalletsWrapper`, `_utils/HardwareWalletsWrapper`.

Outside the tranche, for the `StoryLayout` sweep: `common/ItemsDropdown`, `common/Widgets`,
`staking/_support/decorator`, `settings/utils/SettingsWrapper`. Also
`storybook/stories/_support/globals.ts`, to return `Locale` rather than `string`.

## Implementation Approach

1. Move the six empty-render sites onto `(_args, context)` and `_support/globals.ts`.
2. Give `WalletSettingsScreen` the one prop it declares instead of the whole context, and drop the
   six barrel-era side-effect imports.
3. Drop the `PublicKeyQRCode` spread, which contributes nothing.
4. Give every `StoryLayout` call site an explicit `currentTheme` from the globals, and give
   `WalletsTransactionsWrapper` its `locale` the same way.
5. Convert the two `wallets/tokens` display names to the object form.
6. Type `localeOf` as `Locale`.
7. `nix fmt`, then measure.

## Why the sweep reaches outside the tranche

Six call sites, one construct, one remedy. Splitting them across three more tranches would leave the
tree in a state where `task-024`'s check passes, because no story file reads a global from props,
while two or three layouts quietly lose their theme, because they read it from a spread instead.
That is the failure this whole tranche is about, so doing five sixths of it would be the wrong shape
of care.

## Acceptance Criteria

- `storybook:build`, `compile` and `lint` pass, allowing for the `TS2339` in tranches that have not
  run.
- Panel titles and story labels match the baseline.
- The `@ts-ignore` count in this tranche is unchanged.

Added: the empty-render burn-down falls from seven to one, and no `StoryLayout` anywhere depends on
the prop pass-through for its theme.

## Verification Plan

- `story-args-audit.js`, expecting seven then one.
- A grep for `<StoryLayout` with no explicit `currentTheme`.
- Label set from `index.json` of a real build.
- `@ts-ignore` count for the tranche.
- `nix build` for `compile`, `lint` and `storybook`.

## Risks and Open Questions

- Removing the six side-effect imports could drop stories from the sidebar if the glob did not
  already reach those files. The label set is the check.
- `WalletSettingsScreen` may have been relying on a prop the context spread happened to supply. Its
  declared type is `{ locale: Locale }` and `tsc` is the check.

## Required Docs, Research, and Tracking Updates

- Set `task-020.status` to `completed`; record the burn-down, the sweep, and why it crossed tranches.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-020-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-020-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Sixteen files changed, eleven in the tranche and five outside it.

## Final Outcome

- Empty-render burn-down: 7 before, 1 after. This tranche cleared 6. The last one is in `task-021`.
- `compile` 16 to 14. `lint` exit 0. `storybook` exit 0.
- Label set unchanged: 258 entries across 49 titles.
- `@ts-ignore` in the tranche unchanged at 57.
- No `StoryLayout` in the corpus takes its theme from a context spread.

## Self-Review

- Typing `localeOf` as `Locale` rather than `string` was forced by the compiler and is the better
  answer anyway: the two locales are a closed set and every consumer of the helper now gets that.
- The `WalletSettings` spread is the same defect as `IncidentOverlay` seen from the other side. There
  the story read an argument nothing filled; here it read one that is filled with the wrong thing,
  and passed a story id, a parameters object and a globals object to a React component as props.
