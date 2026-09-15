# Task task-016: Replace DaedalusMenu with globalTypes and a context-reading StoryWrapper

## Task ID and Title

- ID: `task-016`
- Title: `Replace DaedalusMenu with globalTypes and a context-reading StoryWrapper`

## Why Chosen Now

`task-016.dependencies` is `[task-015, task-009]` and both are complete. It has to happen either
way, because `@storybook/addons` has no release in the 8 line, and it happens first in the window
because until it does the manager build fails and nothing behind it can be observed.

## The red window

`compile` is at 68 after this task, down from 70. The two `TS2307` errors for `@storybook/addons`
are what this task clears. The remaining 68 are one `TS2305` per `storiesOf` call site, cleared by
`task-017` and the five tranches. `task-025` closes the window.

## Interaction Mode

- Mode: `agent_execution`, with the addon directory removed through the operator.

## Scope

- Declare `themeName`, `localeName` and `osName` as Storybook globals with toolbar controls.
- Make `StoryWrapper` read them from story context instead of owning them.
- Move the one write-back site from the addon channel to `updateGlobals`.
- Remove the addon from the `addons` array and delete `storybook/addons/`.

## Non-Goals

- The `StoryWrapper` prop pass-through stays. `task-024` removes it, once every consumer reads the
  globals from context.
- No story conversion. `task-017` owns that.
- No change to what the nine themes, two locales or three OS profiles are.

## Dependencies

- `task-015` and `task-009`, both complete. `task-024` depends on this.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-016`, `task-024`
- `.agent/plans/storybook-modernization/task-plans/task-009.md`, for the two story signatures
- `.agent/plans/storybook-modernization/task-plans/task-013.md`, for the measured signature rule
- `.agent/plans/storybook-modernization/task-plans/task-015.md`, for the state of the window

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`, which documents the addon and is rewritten by
  `task-060`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `c044f2521`.

- The addon is five tracked files under `storybook/addons/DaedalusMenu/`. Its public surface is
  three functions in `index.ts`: `setInitialState`, `updateParam` and `onReceiveParam`, all built on
  `addons.getChannel()` and two channel events.
- It has exactly two consumers: `StoryWrapper.tsx:6` imports `onReceiveParam` and `setInitialState`,
  and `General.stories.tsx:7` imports `updateParam` for the Themes story's write-back.
- `_support/config.ts` already exports `themeNames`, `localeNames` and `osNames`, so the toolbar
  option lists come from the same place the values do rather than being restated.
- `getInitialState()` at `config.ts:60` reads a URL query parameter, then `sessionStorage`, then a
  default. Storybook does persistence and URL encoding for globals itself, so that function's
  reason to exist goes with the addon.
- `@storybook/preview-api` at 8.6.18 exports `useGlobals`, which is how a story writes a global.
- The corpus is 68 `storiesOf` calls; none is touched here.

## Files Expected To Change

- `storybook/preview.tsx`, `globalTypes` and the decorator
- `storybook/stories/_support/StoryWrapper.tsx`, reads props instead of state
- `storybook/stories/settings/general/General.stories.tsx`, `updateGlobals`
- `storybook/main.ts`, the addon entry
- `package.json` and `yarn.lock`, declaring `@storybook/preview-api`
- two comments describing the removed mechanism
- removed: the five addon files

## Implementation Approach

1. Declare the three globals in `preview.tsx`, taking option lists from `config.ts`.
2. Change the preview decorator to `(story, context)` and pass the three globals to `StoryWrapper`.
3. Rewrite `StoryWrapper` to read them as props with defaults, keeping the pass-through.
4. Replace `updateParam` with `useGlobals` in the Themes story.
5. Remove the addon entry from `main.ts`, then hand off the directory.
6. Declare `@storybook/preview-api`, rather than leaving a third instance of the transitive weakness.
7. Confirm the error count moves to exactly 68 and that the `storybook` failure has moved from the
   manager to the indexer.

## Acceptance Criteria

- All nine themes, both locales and all three OS profiles switch from the toolbar. Not executable
  here; see the limit below.
- A selection survives a reload and a story URL carries it. Not executable here; this is Storybook's
  own behaviour for globals and is why the addon's `sessionStorage` and location-hash code could go.
- The Themes story still moves the toolbar selection. Not executable here; the write-back is now
  `updateGlobals`.
- `storybook/addons/` is gone. Structural, and met.

Added: the error count reaches exactly 68, and the `storybook` failure is the indexer rejecting
`storiesOf` rather than anything in configuration.

## Verification Plan

- `nix build .#checks.x86_64-linux.compile`, expecting 68 and only `TS2305`.
- `nix build .#checks.x86_64-linux.storybook`, expecting the manager to build and the indexer to
  reject the corpus.
- `nix build .#checks.x86_64-linux.lint`, expecting zero errors.
- `nix build .#internal.x86_64-linux.node_modules` after the manifest change.
- A grep for `DaedalusMenu` across `storybook`, `source` and `tests`.

## Risks and Open Questions

- Three of the four acceptance criteria are about a running preview and cannot be exercised here.
  What replaces them is stated rather than implied.
- Persistence and URL encoding are now Storybook's rather than this repository's. That is the point
  of the change and it is also the part that cannot be checked from here.
- The pass-through staying means `StoryWrapper` has two ways of telling a story about the theme for
  the length of the conversion. That is deliberate and `task-024` ends it.

## Required Docs, Research, and Tracking Updates

- Set `task-016.status` to `completed`; record the result, the count movement, the `preview-api`
  declaration, the corrected comments and the limit.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-016-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-016-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Three globals with toolbar controls, a context-reading `StoryWrapper`, a `updateGlobals`
  write-back, and no `storybook/addons/`.

## Final Outcome

- `task-016` completed. `compile` is at 68, all `TS2305`. `lint` is clean at 5414 warnings.
- The `storybook` check now fails at the indexer with `Unable to index files: Unexpected storiesOf
  usage`, having built the manager in 228ms. That is the failure `task-015` predicted and could not
  reach.
- `@storybook/preview-api` declared rather than left transitive.

## Self-Review

- The count prediction was made before the run and met exactly, which is what makes 68 usable as the
  number `task-017` drives to zero.
- Three of four acceptance criteria are not executable here and the entry says so in those terms
  rather than reporting them met on the strength of a green build.
- Declaring `preview-api` rather than relying on it was the consistent thing to do while writing up
  a finding about exactly that pattern.
