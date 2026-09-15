# Task task-024: Delete the StoryWrapper prop pass-through

## Task ID and Title

- ID: `task-024`
- Title: `Delete the StoryWrapper prop pass-through`

## Why Chosen Now

`task-024.dependencies` is the five tranches, all complete. Every consumer now reads the toolbar
selections from its own story context, so the pass-through has nothing left to serve.

## The red window

All three required checks are green before and after. `task-025` closes the window.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- Remove `<Story osName locale currentTheme />` from `StoryWrapper` and everything that existed only
  to feed it.

## Non-Goals

- The globals, the toolbar and the three helpers stay exactly as they are. This removes the second
  route, not the first.
- No story changes; the tranches did that work.

## Dependencies

- `task-018` through `task-022`. `task-025` depends on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-009.md`, the 65-consumer census
- `.agent/plans/storybook-modernization/task-plans/task-018.md`, the settled patterns
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-024`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`

## Live Repo Findings Verified For Planning

Verified at `fc48ad1ea`.

- The required grep returns nothing in any story file. Every remaining `props.currentTheme`,
  `props.locale` and `this.props.locale` is inside a component that declares those props and is
  given them explicitly by its caller, which is the opposite of the pass-through.
- Seven spreads of a story argument remain, six into `StoryLayout` and one into
  `WalletsTransactionsWrapper`. Each is followed by explicit props for what the pass-through used to
  supply. What the spreads still carry and the components still use is `story` and `kind`, which
  Storybook provides itself.
- `themeId` in `StoryWrapper` exists only to be passed down, and `themesIds` is imported only for it.
- Two `@ts-ignore` in `wallets/receive/WalletReceive.stories.tsx` and
  `wallets/settings/WalletRecoveryPhraseVerification.stories.tsx` describe a `({ locale })` story
  signature that the tranches replaced.

## Files Expected To Change

- `storybook/stories/_support/StoryWrapper.tsx`
- `storybook/stories/wallets/receive/WalletReceive.stories.tsx`
- `storybook/stories/wallets/settings/WalletRecoveryPhraseVerification.stories.tsx`

## Implementation Approach

1. Replace `<Story osName locale currentTheme />` with `<Story />`.
2. Remove `themeId` and the `themesIds` import.
3. Remove the two suppressions whose subject no longer exists.
4. Rewrite the component comment so it describes what the wrapper does rather than what it used to.
5. Prove the compiler would notice a missing theme, then measure.

## Acceptance Criteria

- No story file reads theme, locale or OS from props.
- `storybook:build`, `compile` and `lint` pass.

The census warns that neither criterion can see the real risk: a story that quietly renders a
component with three props missing compiles and lints. Added: a deliberate probe showing that the
compiler does report a missing theme on these call sites, so a green `compile` here is evidence
rather than silence.

## Verification Plan

- The required grep across every story file.
- A sweep for remaining spreads of a story argument, each checked against what the receiver declares.
- Remove one explicit `currentTheme` and require `compile` to go red, then restore it.
- Label set from `index.json`.
- `nix build` for all three checks.

## Risks and Open Questions

- The census counted 30 consumers that forward their whole argument by spread and would fail
  silently. All 30 were rewritten by the tranches to pass declared props, so what protects this
  change is that the receivers declare their props and `tsc` checks them. That is only worth
  something if `tsc` actually reports it, which is what the probe establishes.

## Required Docs, Research, and Tracking Updates

- Set `task-024.status` to `completed`; record the probe and its result.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-024-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-024-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The pass-through is gone and three files changed.

## Final Outcome

- `StoryWrapper` renders `<Story />`. The theme, locale and OS reach a story only through its own
  story context.
- `compile` exit 0, `lint` exit 0, `storybook` exit 0.
- Label set unchanged: 258 entries across 49 titles.
- The probe: removing `currentTheme` from one `StakePoolsTableStory` call makes `compile` report
  `TS2741: Property 'currentTheme' is missing in type '{}' but required in type 'Props'`, and
  restoring it returns the check to green.

## Self-Review

- The census's warning was the right one and the answer to it is not "the build is green". It is
  that the receivers declare their props, so a missing one is a type error, plus a demonstration
  that the type error actually appears. A check that cannot fail proves nothing, and this one can.
- Removing `themeId` and its import was not tidying. It was the last thing in the wrapper that
  existed only to be passed down, and leaving it would have left a reader wondering which route was
  live.
