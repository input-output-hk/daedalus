# Task task-021: Hand-finish the staking tranche

## Task ID and Title

- ID: `task-021`
- Title: `Hand-finish the staking tranche`

## Why Chosen Now

`task-021.dependencies` is `[task-018]`, complete. It holds the last of the empty-render sites and
eleven of the fourteen remaining `TS2339`.

## The red window

`compile` 14 before, 3 after, all three in `task-022`'s files. `lint` and `storybook` green.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- `staking/Staking.stories.tsx` and `staking/RedeemItnRewards.stories.tsx`.
- Clear the last three empty-render sites and eleven `TS2339`.

## Non-Goals

- The support components under `staking/_support/` keep their signatures. What changes is how the
  stories supply them.
- No label changes. `Epochs`, the countdown and the info panels were removed in phase 1 and are not
  reinstated.

## Dependencies

- `task-018`. `task-023` and `task-024` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-018.md`, the settled patterns
- `.agent/plans/storybook-modernization/task-plans/task-020.md`, the `StoryLayout` sweep
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-021`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `8e62f14be`.

- The tranche is two story files, not the eight the entry records. The other six were the sibling
  modules `task-011` renamed into `staking/_support/`, plus `Epochs.stories.tsx` and the two files
  phase 1 removed. `RedeemItnRewards.stories.tsx` is new, split out in the preparatory commit
  because the codemod cannot convert a file with two `storiesOf` calls.
- The nine dynamic registrations the entry describes as hand work no longer exist as such. Their
  labels were inlined as string literals in the same preparatory commit, so the codemod took them,
  and what is left here is the argument question rather than the label question.
- Three empty-render sites, all here, and all three only visible since the scan learned to follow a
  binding: `PoolsIndex` and `StakePoolsList` bind support functions that read `currentTheme`,
  `locale` and `isLoading` from their first argument, and `PoolsIndexLoading` reads it directly.
- Eight `TS2339` in `RedeemItnRewards.stories.tsx` and three in `Staking.stories.tsx`, every one a
  `parameters` or `storyName` assignment onto a const typed `() => Element`.
- Nine of the stories in `Staking.stories.tsx` spread the story context into a support component
  that declares exactly which props it wants. `StakingDelegationCenterStory` declares
  `{ locale, isLoading, isEpochsInfoAvailable, currentTheme }`, `StakePoolsStory` declares
  `{ currentTheme, locale, isLoading }`, `StakePoolsTableStory` declares `{ currentTheme }`,
  `StakingDelegationSteps` declares `{ currentTheme, locale, isDisabled?, oversaturationPercentage }`,
  `StakingUndelegateConfirmationStory` declares `{ unknownStakePool?, isHardwareWallet? }` and
  `StakingUndelegateConfirmationResultStory` declares `{ locale }`.
- The seven `@ts-ignore` in `Staking.stories.tsx` are `ts-migrate(2739)` four times,
  `ts-migrate(2769)` twice and `ts-migrate(2345)` once. Each sits on one of those spreads and
  suppresses the mismatch between an untyped context and a declared prop type.
- `RedeemItnRewards.stories.tsx` imports seven names it does not use. All seven were already unused
  when the file was created, copied across by the split.

## Files Expected To Change

- `storybook/stories/staking/Staking.stories.tsx`
- `storybook/stories/staking/RedeemItnRewards.stories.tsx`

## Implementation Approach

1. Replace each context spread with the props the support component declares, taken from
   `_support/globals.ts`.
2. Convert every `X.parameters = { ... }` and `X.storyName = '...'` to the CSF object form.
3. Remove the seven unused imports.
4. `nix fmt`, then measure.

## Acceptance Criteria

- All nine dynamic registrations have explicit exports and explicit display names matching the
  baseline. Already true, settled in the preparatory commit, and the label set is the check.
- `storybook:build`, `compile` and `lint` pass, allowing for the three `TS2339` in `task-022`.
- The `@ts-ignore` count in this tranche is unchanged.

The third criterion is not met and cannot be, for a reason worth stating rather than working
around: see below.

## Verification Plan

- `story-args-audit.js`, expecting three then zero.
- Label set from `index.json` of a real build.
- `@ts-ignore` count for the tranche, with each removal accounted for by the error it suppressed.
- `nix build` for `compile`, `lint` and `storybook`.

## Risks and Open Questions

- Replacing a spread with declared props changes what reaches a component. Where the spread carried
  more than the component declared, the extra is gone; where it carried less, the missing value now
  arrives. Both are the point, and `tsc` sees the difference for the first time.
- `isLoading` is not optional on `StakePoolsStory`, so the non-loading stories now pass it
  explicitly as `false`. Before, it was `undefined`, which is falsy and looked the same.

## Required Docs, Research, and Tracking Updates

- Set `task-021.status` to `completed`; record the burn-down reaching zero and the seven `@ts-ignore`
  removals with their causes.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-021-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-021-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Both files rewritten.

## Final Outcome

- Empty-render burn-down: 3 before, **0 after**. The scan reports no story reading an argument
  nothing fills.
- `compile` 14 to 3, all three in `task-022`.
- `lint` exit 0. `storybook` exit 0.
- Label set unchanged: 258 entries across 49 titles.
- `@ts-ignore` in the tranche 7 to 0, each accounted for.

## Self-Review

- The seven suppressions did not become unnecessary by accident. Every one sat on a spread of the
  untyped story context into a component with a declared prop type, and that is exactly the
  construct this tranche removes. Typing the call sites is what made them redundant, which is a
  fact worth having about locked decision 8: some of the 229 are load-bearing and some are
  scaffolding around a defect.
- Passing `isLoading={false}` where nothing was passed before looks like a change and is not one:
  `undefined` was already falsy. Making it explicit is what lets `tsc` check the rest of the call.
