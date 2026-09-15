# Task task-018: Hand-finish the shared widgets tranche

## Task ID and Title

- ID: `task-018`
- Title: `Hand-finish the shared widgets tranche`

## Why Chosen Now

`task-018.dependencies` is `[task-016, task-017]`, both complete. It comes before the other four
tranches because they depend on it: it is where the patterns are settled.

## The red window

`compile` is at 19, `lint` at 0 errors, `storybook` green. This tranche clears no `TS2339` of its
own; the 19 are in other tranches' files.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- The 13 files under `common`, `assets`, `dapps`, `notifications`, `navigation` and `news`.
- Move every theme, locale and OS read off the `StoryWrapper` prop pass-through and onto the story
  context.
- Add the one `_support` module that reading a global goes through.
- Fix the fixture injection in `news/IncidentOverlay.stories.tsx`, which the conversion left
  reading an argument that no longer carries it.

## Non-Goals

- Knobs and `withState` are untouched. `common/ItemsDropdown`, `dapps/TransactionRequest` and
  `notifications/Notifications` carry `withState` and are phase 4's work.
- No `@ts-ignore` is added or removed.
- No story label changes.

## Dependencies

- `task-016` and `task-017`, both complete. `task-019` through `task-024` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-009.md`, the prop-pass-through census
- `.agent/plans/storybook-modernization/task-plans/task-017.md`, what the conversion left behind
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-018`, `task-024`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `430968c69`.

- The tranche is 13 files, matching the task entry.
- Measured, rather than reasoned about: a Storybook 8 render function is called as
  `render(context.args, context)`. A probe composed a synthetic CSF module through
  `@storybook/react`'s own `composeStories` and printed both arguments. The first is
  `context.args`, `{}` when the story declares none. `context.globals` carries the toolbar
  selections. A decorator that calls `story({ x })` merges `x` onto the context at top level, not
  into `args`.
- That makes the two reading shapes in this corpus unequal, and one of them wrong. The nine sites
  written `(_, props)` read the context and work, because `StoryWrapper` still renders
  `<Story osName locale currentTheme />` and React's call merges those onto the context. The six
  written `(props)` or `({ locale })` read `context.args` and get `{}`.
- `news/IncidentOverlay.stories.tsx` is the only file in the whole corpus whose decorator injects a
  fixture by calling `story({ ... })`. Its three stories then read that fixture from the first
  argument, so all three currently render an incident with no title, content, date or action.
- Nothing in the required checks sees any of this. `storybook` is green, `compile` reports nothing
  here, and the story still renders a component: it renders it empty.
- 15 sites across 8 of the 13 files read a global: 3 in `common/Widgets`, 1 in
  `dapps/TransactionRequest`, 7 in `navigation/Sidebar`, 2 in `navigation/SidebarWalletsMenu`, 1 in
  `news/AlertsOverlay`, 1 in `news/AppUpdateOverlay`, 1 in `news/NewsFeed`.
- The tranche holds 49 `@ts-ignore` directives: 14, 9, 7, 6, 3, 3, 2, 2, 1, 1, 1 and two files with
  none. `navigation/Sidebar.stories.tsx` holds 9 rather than the 15 in the task entry, the
  difference being `ts-migrate(2345)` directives that sat on `.add(` calls and are audited against
  `task-017`.
- `_support/config.ts` still exports `getInitialState`, which nothing calls now that the addon is
  gone.

## Files Expected To Change

- new: `storybook/stories/_support/globals.ts`
- `storybook/stories/common/Widgets.stories.tsx`
- `storybook/stories/dapps/TransactionRequest.stories.tsx`
- `storybook/stories/navigation/Sidebar.stories.tsx`
- `storybook/stories/navigation/SidebarWalletsMenu.stories.tsx`
- `storybook/stories/news/AlertsOverlay.stories.tsx`
- `storybook/stories/news/AppUpdateOverlay.stories.tsx`
- `storybook/stories/news/IncidentOverlay.stories.tsx`
- `storybook/stories/news/NewsFeed.stories.tsx`
- `storybook/stories/_support/config.ts`, removing the dead `getInitialState`

The other five tranche files read no global and need no edit.

## Implementation Approach

1. Add `_support/globals.ts` with `currentThemeOf`, `localeOf` and `osNameOf`, each taking the story
   context and each mapping through `config.ts` so a component gets the theme id and the locale
   code rather than the toolbar labels.
2. Rewrite all 15 reading sites to `(_args, context)` and a helper call.
3. In `IncidentOverlay.stories.tsx`, move the injected fixture from the decorator's `story({ ... })`
   call to `args` on the meta, which is the argument the render function is actually given.
4. Remove `getInitialState`.
5. `nix fmt`, then measure.

## Acceptance Criteria

- `storybook:build`, `compile` and `lint` pass, allowing for the 19 `TS2339` in other tranches.
- Panel titles and story labels match the baseline.
- The `@ts-ignore` count in this tranche is unchanged, measured from the post-conversion count of
  49 rather than from the pre-conversion note.
- No story in this tranche reads `currentTheme`, `osName` or `locale` from props.

Added: no story in this tranche reads anything from the first render argument that the first
argument does not carry. This is what the fourth criterion above misses, and it is the defect
actually present in the tranche.

## Verification Plan

- A grep for `props.currentTheme`, `props.osName`, `props.locale` and destructured equivalents
  across the 13 files, expecting nothing.
- A grep for every remaining first-argument read in the tranche, checked against whether the meta
  declares `args`.
- Label set from `index.json` of a real build, diffed against the pre-conversion set.
- `@ts-ignore` count per file, before and after this change.
- `nix build` for `compile`, `lint` and `storybook`.

## Risks and Open Questions

- The helpers read `context.globals` while `StoryWrapper` still passes the same values as props.
  Both paths are live until `task-024` removes the pass-through, and they agree by construction
  because both resolve through `config.ts`.
- Moving the `IncidentOverlay` fixture to `args` changes what the knobs panel shows, because args
  are surfaced as controls where knobs are not. That is a visible difference and it is the correct
  CSF shape.
- The five files with no global read are unchanged, so this tranche is smaller than its file count
  suggests.

## Required Docs, Research, and Tracking Updates

- Set `task-018.status` to `completed`; record the measured render contract, the patterns settled,
  and the `IncidentOverlay` defect.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-018-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-018-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Eight of the thirteen files changed; the other five read no global.

## Final Outcome

- All 15 global reads in the tranche go through `_support/globals.ts` and take the story context.
  No file in the tranche reads `currentTheme`, `osName` or `locale` from props.
- `news/IncidentOverlay.stories.tsx`'s three stories now receive their fixture. They did not before:
  the decorator handed it to `story({ ... })`, which merges onto the context, while the stories read
  the first argument, which is `context.args`. The fixture is now `args` on the meta.
- `compile` 19, unchanged and all in other tranches. `lint` exit 0. `storybook` exit 0.
- The label set is still the same 258 pairs across 49 panels as before the conversion, read from
  `index.json` of a real build.
- `@ts-ignore` in the tranche goes 49 to 48. The one removed sat on the `story({ ... })` call that no
  longer exists, which is the same category as the 28 audited against `task-017`.

## Patterns this tranche settles, for the four that follow

- **Reading a global.** `render: (_args, context)` and a helper from `_support/globals.ts`. Never
  the first argument, which is `context.args`.
- **Mapping.** The helpers map: a component gets the theme id `dark-blue` and the locale code
  `en-US`, not the toolbar labels `DarkBlue` and `English`. Mapping lives in one module beside the
  list the toolbar is built from.
- **Fixture data.** Anything a story reads from its first argument has to be declared as `args`, on
  the meta or on the story. A decorator cannot supply it by calling `story({ ... })`.
- **Decorator placement.** The meta's `decorators` array, outermost first, in the order the
  `.addDecorator` chain had them.
- **A display name with punctuation.** `name: '...'` on the story object, which is what the
  conversion emitted; it is not re-derived from the export identifier.

## Self-Review

- The plan's own acceptance criteria would have passed a tranche in which three stories render an
  empty component. The criterion that catches it had to be added, and the defect was found by
  measuring what Storybook hands a render function rather than by reading the diff.
- Measuring that was worth more than the tranche. It is the same question every remaining tranche
  faces, and the answer is now a printed result rather than an inference from a bundle.
- The `nix` checks read the git tree, so a new file that is not staged is invisible to them. The
  first run reported seven `TS2307` for a module sitting on disk. Worth knowing before reading a red
  check as a code defect.
