# Task task-022: Hand-finish the governance and voting tranche

## Task ID and Title

- ID: `task-022`
- Title: `Hand-finish the governance and voting tranche`

## Why Chosen Now

`task-022.dependencies` is `[task-018]`, complete. It is the last of the five tranches and holds
the last three `TS2339`.

## The red window

`compile` 3 before, **0 after**. `lint` and `storybook` green. All three required checks pass for
the first time since `task-015` opened the window.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- The six files under `governance` and `voting`.

## Non-Goals

- The fixtures stay exported and stay where a screen story can import them, for phase 7.
- Knobs and `withState` untouched.

## Dependencies

- `task-018`. `task-023` and `task-024` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-018.md`, the settled patterns
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-022`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `bae6d4910`.

- Six files, of which two change.
- No story in this tranche reads a global from any argument. The census recorded three
  spread-forwarders here and none naming `currentTheme`, `osName` or `locale`, and the greps agree:
  there is nothing to move.
- The comments at `governance/DRepDetail.stories.tsx:66` and
  `governance/DRepDirectory.stories.tsx:246` say the locale is deliberately left to the
  `IntlProvider` inside `StoryWrapper` rather than read as a prop. That remains true and needs no
  change: the provider is in the preview decorator and is driven by the toolbar global.
- Three `TS2339`, all `storyName` attached to a `withState` result: `ConnectedFlow` and
  `FavoriteToggle` in `DRepDirectory`, `ConnectedFlow` in `Delegation`.
- `voting/Voting.stories.tsx` and `voting/VotingInfo.stories.tsx` each hold one default export and
  one title, `Voting / Voting Registration Wizard` and `Voting / Voting Info`. The two `storiesOf`
  calls the entry warned about were separated in the preparatory commit, so the codemod had one
  each to convert and produced one meta each.
- One `@ts-ignore` in the tranche.
- `governance/_utils/GovernanceShell.tsx` mentions `StoryLayout` only in a comment; it builds its
  own frame and spreads no context.

## Files Expected To Change

- `storybook/stories/governance/DRepDirectory.stories.tsx`
- `storybook/stories/governance/Delegation.stories.tsx`

## Implementation Approach

1. Convert the three display names to the CSF object form.
2. `nix fmt`, then measure.

## Acceptance Criteria

- `storybook:build`, `compile` and `lint` pass. With this tranche there is nothing left to allow
  for: the count goes to zero.
- Panel titles and story labels match the baseline.
- The fixture data remains importable from outside its story file.

## Verification Plan

- Label set from `index.json` of a real build.
- `story-args-audit.js`, expecting zero.
- A grep confirming the fixture exports are untouched.
- `nix build` for `compile`, `lint` and `storybook`.

## Risks and Open Questions

- The smallest tranche by change and the largest by file size. The risk is assuming the absence of
  global reads rather than checking it, which is why the finding above is a grep result and not an
  inference from the census.

## Required Docs, Research, and Tracking Updates

- Set `task-022.status` to `completed`; record that the window's error count reaches zero.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-022-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-022-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Two of the six files changed.

## Final Outcome

- `compile` exit 0, `lint` exit 0, `storybook` exit 0. The window's error count is 70 to 0.
- Label set unchanged: 258 entries across 49 titles.
- Empty-render burn-down stays at 0.
- `@ts-ignore` in the tranche unchanged at 1.

## Self-Review

- The useful work here was confirming there was nothing to do rather than doing it. The census said
  three spread-forwarders and no named global reads; the greps agree, and saying so with the command
  behind it is worth more than a tranche that touches files to look busy.
- The two comments about the locale being left to the `IntlProvider` were checked rather than
  updated. They describe a mechanism that still holds, and rewriting a correct comment to mention
  globals would have been churn.
