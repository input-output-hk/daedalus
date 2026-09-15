# Task task-023: Diff every sidebar label against the baseline

## Task ID and Title

- ID: `task-023`
- Title: `Diff every sidebar label against the baseline`

## Why Chosen Now

`task-023.dependencies` is `[task-018, task-019, task-020, task-021, task-022, task-001]`, all
complete. Nothing else can move the labels now, so this is the moment the comparison means
something.

## The red window

All three required checks are green. This task changes nothing; it establishes what the conversion
did to the sidebar.

## Interaction Mode

- Mode: `agent_execution`. No production change is expected.

## Scope

- Compare every group, panel and story label against the `task-001` baseline and enumerate the
  differences.
- Check each label an export identifier cannot spell, individually.
- Check the four control-flow strings as behaviour rather than as text.

## Non-Goals

- No conversion work. If a label were wrong it would be fixed here, but the check comes first.
- Story order within a panel is not part of the baseline, which records a sorted tree.

## Dependencies

- All five tranches and `task-001`. `task-025` depends on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-baseline.txt`
- `.agent/plans/storybook-modernization/task-plans/task-001.md`, how the baseline was taken
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-023`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`

## Live Repo Findings Verified For Planning

Verified at `d9cca4b1b`.

- The baseline is 272 registrations across 53 panels and 15 groups, taken at `bfc6ec3d2` from the
  `storiesOf` corpus. Today's tree is 258 across 49 panels and 14 groups, read from `index.json`
  emitted by a real Storybook build.
- 152 of the 272 baseline labels cannot be spelled by an export identifier, measured as
  `startCase(label) !== label`. The entry says 80; the difference is that 80 counted labels with a
  character outside letters, digits and spaces, and 152 also counts every label whose capitalisation
  or spacing `startCase` would change. The larger set is the one that matters, because the sanitizer
  is what re-attaches all of them.
- `context.kind` and `context.story` still carry the panel title and the story's display name at
  8.6.18. Measured by composing a synthetic story through `@storybook/react` and printing both.

## Files Expected To Change

- None in `storybook/` or `source/`. This task records a comparison.

## Implementation Approach

1. Build the baseline label set as `title | label` pairs from the committed baseline file.
2. Build today's from `index.json`, which is Storybook's own indexer.
3. Diff the two sets, and separately diff groups and panels.
4. Take the 152 lossy labels and check each one individually against today's set.
5. Read the four control-flow sites and establish what each does now and what it did before.

## Acceptance Criteria

- Every one of the sanitizer-renamed exports is checked against the baseline label, and each
  difference is either reverted with an explicit `storyName` or recorded as deliberate.
- The four control-flow strings are verified by behaviour.
- The sidebar tree matches the `task-001` baseline, group for group and panel for panel, with
  differences enumerated.

## Verification Plan

- Set difference in both directions, listing every member.
- Group and panel difference in both directions.
- Per-label check of the 152.
- A rendered probe for `context.kind` and `context.story`.

## Risks and Open Questions

- The two readings come from different instruments, an AST walk over `storiesOf` calls for the
  baseline and Storybook's indexer for today. They agreed exactly at the conversion, when both were
  run over the same tree, which is what licenses comparing them here.

## Required Docs, Research, and Tracking Updates

- Set `task-023.status` to `completed`; record the enumerated differences and the control-flow
  findings.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-023-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-023-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The comparison is complete and no label needed correcting.

## Final Outcome

- **Sixteen differences, all accounted for.** Fifteen registrations in the baseline are absent, and
  all fifteen are the phase 1 deletions: two under `Decentralization / Countdown`, three under
  `Decentralization / Staking`, two under `StakingChart`, three under `Wallets / Legacy Wallets` and
  five under `Wallets / Paper Wallets`. One registration is present that the baseline does not have,
  `Wallets / Settings | Undelegate Wallet`, which is the `task-006` restaging. The other 257 are
  identical, pair for pair.
- Four panels gone, no panel added: `Decentralization / Countdown`, `StakingChart`,
  `Wallets / Legacy Wallets`, `Wallets / Paper Wallets`. One group gone, none added: `StakingChart`.
  Each corresponds to a phase 1 deletion that removed every registration it held.
- Of the 152 labels an export identifier cannot spell, 147 are present and identical. The five
  absent are all phase 1 deletions. None was silently renamed.
- 151 stories carry an explicit `name` today, and for every one of the 151 the derived name would
  differ, so none is redundant.

## The four control-flow strings

Verified as behaviour, and the finding is that two of the four were already not doing what they
appear to do, before any of this work.

- `WalletsWrapper.tsx:19` branches on `context.story !== 'Empty' && context.story !== 'Wallet Add'`.
  No panel under `Wallets /` has ever held a story with either name: the three `Empty` stories in the
  baseline are under `Governance / DRep Directory`, `Navigation / Wallets Menu` and
  `News / NewsFeed`, and `Wallet Add` appears nowhere in the baseline at all. The branch has always
  taken the wrapping path, and still does. Unchanged, and dead in both directions.
- `SettingsWrapper.tsx:30` passes `context.kind` to `linkTo`, and `context.kind` still holds the
  panel title. The links resolve as they did, with one exception that also predates this work:
  `pageNames` maps `/settings/terms-of-service` to `Terms of service`, and the story is
  `Terms of Service`. `linkTo` is case sensitive, so that one menu item has never navigated.
- `SettingsWrapper.tsx:32` derives the active menu item from `context.story`, which still holds the
  display name. The derivation maps the `Themes` story to `/settings/display` correctly, and the
  `General` story to `/settings/general`, which is not one of the five menu routes, so the General
  panel has never highlighted an active item. Also unchanged.
- `WalletWithNavigationLayout.tsx:32` computes `context.kind.replace('Wallets|', '')`. Titles have
  used ` / ` as their separator throughout, so the replacement never matches and `activeItem`
  receives `wallets / summary` rather than `summary`. The `walletStories` map beside it has the same
  problem, targeting `Wallets|Send` and the rest. Both are Storybook 5 separator syntax left behind.
  Unchanged by this work and wrong before it.

## Self-Review

- The acceptance asked for the four strings to be verified as behaviour rather than as labels, and
  doing that turned up two constructs that do not work and never did. Comparing them as strings
  would have passed them, because the strings are the same as they were.
- The entry's figure of 80 was worth re-deriving rather than reusing. The measure that matters is
  whether an export identifier can spell the label, which is 152, and checking 152 rather than 80
  costs the same command.
