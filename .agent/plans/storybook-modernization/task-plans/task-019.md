# Task task-019: Hand-finish the nodes, loading and settings tranche

## Task ID and Title

- ID: `task-019`
- Title: `Hand-finish the nodes, loading and settings tranche`

## Why Chosen Now

`task-019.dependencies` is `[task-018]`, which is complete and has settled the patterns this tranche
applies.

## The red window

`compile` at 19 before, 16 after. `lint` and `storybook` green throughout. `task-025` closes the
window.

## Interaction Mode

- Mode: `agent_execution`.

## Scope

- The 17 story files under `nodes`, `loading` and `settings`.
- Move every global read onto the story context, through `_support/globals.ts`.
- Clear this tranche's share of the empty-render burn-down and of the `TS2339` count.

## Non-Goals

- Knobs and `withState` are untouched.
- `nodes/about/About.stories.tsx` is converted in place; phase 6 replaces it with a harness-based
  story.
- No label changes.

## Dependencies

- `task-018`. `task-023` and `task-024` depend on this.

## Research Consulted

- `.agent/plans/storybook-modernization/task-plans/task-018.md`, the settled patterns
- `.agent/plans/storybook-modernization/task-plans/task-010-impl-review.md`, on the removed barrels
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-019`

## Docs, Workflows, and Skills Consulted

- Docs: `AGENTS.md`, `CLAUDE.md`
- Workflows: `.agent/workflows/storybook.md`
- Skills: `.agent/skills/git-commit-formatter/SKILL.md`

## Live Repo Findings Verified For Planning

Verified at `c76607dad`.

- The tranche is 17 files, not the 20 the entry records. The difference is the phase 2 reshuffle,
  not a missing file: see the note below.
- Five of the files the entry names as needing a CSF split decided are not story files any more.
  `task-011` renamed `nodes/errors/NoDiskSpaceError`, `nodes/errors/SystemTimeError`,
  `nodes/syncing/SyncingConnecting` and `nodes/updates/DataLayerMigration` into their sibling
  `_support/` directories so the glob would not index them, and `nodes/status/Status.ts` went with
  the barrels in `task-010`, its whole content having been `import '../Diagnostics.stories';`. The
  entry is stale rather than wrong about the repository.
- Six global reads: 1 in `nodes/about/About`, 3 in `nodes/environment/TopBarEnvironment`, 1 in
  `nodes/errors/Errors`, 1 in `settings/general/General`. Five more sites in
  `loading/mithril/MithrilPartialSyncDialogue` go through one helper.
- Four of the eleven empty-render sites are here: `About` once and `TopBarEnvironment` three times,
  all reading `props` where `props` is `context.args` and nothing declares any.
- `nodes/status/MithrilPartialSyncConfirmation.stories.tsx` reads `props.currentTheme` to key the
  modal. The scan does not flag it, because its stories come back from a call rather than being
  written as functions, and following that would mean analysing the callee. It is the same defect
  in a shape the instrument cannot see, and it loses the remount rather than the whole render.
- Two of the 19 `TS2339` are here, both `storyName` attached to a `withState` result in
  `settings/language/Language.stories.tsx`.
- The tranche holds 9 `@ts-ignore` directives.

## Files Expected To Change

- `storybook/stories/nodes/about/About.stories.tsx`
- `storybook/stories/nodes/environment/TopBarEnvironment.stories.tsx`
- `storybook/stories/nodes/errors/Errors.stories.tsx`
- `storybook/stories/nodes/status/MithrilPartialSyncConfirmation.stories.tsx`
- `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx`
- `storybook/stories/settings/general/General.stories.tsx`
- `storybook/stories/settings/language/Language.stories.tsx`
- `.agent/plans/storybook-modernization/task-plans/story-args-audit.js`, to report what it skips

The other ten files read no global and carry no `TS2339`.

## Implementation Approach

1. Move the six reads and the one helper onto `(_args, context)` and `_support/globals.ts`.
2. Rewrite `MithrilPartialSyncConfirmation`'s factory so the returned render takes
   `(_args, context)` and keys on `currentThemeOf(context)`.
3. Convert the two `Language` stories to the object form with `name`, the shape `task-018` settled.
4. Make the audit list the renders it cannot follow, so the shape it just missed is visible in its
   own output rather than only in this entry.
5. `nix fmt`, then measure.

## Acceptance Criteria

- `storybook:build`, `compile` and `lint` pass, allowing for the `TS2339` in tranches that have not
  run.
- Panel titles and story labels match the baseline.
- Every sibling-registering file has an explicit, documented CSF shape. Satisfied by the phase 2
  reshuffle rather than by work here, and recorded as such.

Added: the empty-render burn-down falls by four, from eleven to seven, and the audit reports which
shapes it cannot follow rather than passing over them.

## Verification Plan

- `story-args-audit.js` before and after, expecting eleven then seven.
- A grep for `props.currentTheme`, `props.osName` and `props.locale` in the 17 files.
- Label set from `index.json` of a real build.
- `@ts-ignore` count for the tranche.
- `nix build` for `compile`, `lint` and `storybook`.

## Risks and Open Questions

- The audit cannot follow a render that comes back from a call, which is 18 exports in the corpus.
  Fourteen are `withState(...)`, whose returned function declares no parameters and so cannot read a
  story argument at all. The other four were `MithrilPartialSyncConfirmation`'s and are fixed here.
- `nodes/about/About.stories.tsx` is a container story converted in place. Phase 6 replaces it, so
  the conversion is deliberately minimal.

## Required Docs, Research, and Tracking Updates

- Set `task-019.status` to `completed`; record the burn-down, the stale file names, and the audit's
  blind spot.

## Review-Log Paths

- `.agent/plans/storybook-modernization/task-plans/task-019-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-019-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Seven of the seventeen files changed.

## Final Outcome

- Empty-render burn-down: 11 before, 7 after. This tranche cleared 4.
- `compile` 19 to 16. `lint` exit 0. `storybook` exit 0.
- Label set unchanged: 258 entries across 49 titles from `index.json`.
- `@ts-ignore` in the tranche unchanged at 9.

## Self-Review

- The instrument found four of this tranche's five defective sites. The fifth,
  `MithrilPartialSyncConfirmation`, it could not see, and the only reason it was found is that the
  file was read as well as scanned. Making the scan print what it skips is the difference between a
  known limit and a silent one.
- The five stale file names in the entry are the phase 2 reshuffle showing through, and checking
  where each went cost one command each. A task entry naming files an earlier task legitimately
  moved is stale, not a mismatch.
