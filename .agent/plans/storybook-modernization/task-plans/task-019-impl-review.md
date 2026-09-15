Implementation: Iteration 1
Timestamp: 2026-09-15T13:02:15Z

Changes made:
- `nodes/about/About.stories.tsx`: `render: (_args, context)` and `osNameOf(context)`.
- `nodes/environment/TopBarEnvironment.stories.tsx`: three renders onto `(_args, context)` and
  `currentThemeOf(context)`.
- `nodes/errors/Errors.stories.tsx`: `localeOf(context)`.
- `nodes/status/MithrilPartialSyncConfirmation.stories.tsx`: the factory's returned render takes
  `(_args, context)` and keys the modal on `currentThemeOf(context)`.
- `loading/mithril/MithrilPartialSyncDialogue.stories.tsx`: `applyStoryOs` goes through `osNameOf`
  rather than casting the context and reading a top-level `osName`, which only existed because of
  the prop pass-through.
- `settings/general/General.stories.tsx`: the terms-of-use path is built from `localeOf(context)`.
- `settings/language/Language.stories.tsx`: both stories become objects with `render` and `name`,
  which clears two `TS2339`.
- `story-args-audit.js` now lists the renders it cannot follow.

Verification:
- Empty-render burn-down 11 to 7, exactly the four predicted. Remaining: 6 in `task-020`, 1 in
  `task-021`, none in `task-022`.
- `compile` 19 to 16. `lint` exit 0. `storybook` exit 0.
- Label set from `index.json`: 258 entries across 49 titles, identical to the pre-conversion set.
- `@ts-ignore` in the tranche unchanged at 9, so nothing was dropped here.
- No `props.currentTheme`, `props.osName` or `props.locale` left in the 17 files.

On the audit's blind spot: it cannot follow a render that comes back from a call, and there are 18
such exports. Fourteen are `withState(...)`, and `withState` returns `function WithLocalState()`
with no parameters, so none of them can read a story argument. The other four were
`MithrilPartialSyncConfirmation`'s and are fixed. The scan now prints all 18 under a heading saying
why they were skipped, so the limit is in its output rather than only in this log.

On the five stale file names in the task entry: `nodes/errors/NoDiskSpaceError`,
`nodes/errors/SystemTimeError`, `nodes/syncing/SyncingConnecting` and
`nodes/updates/DataLayerMigration` were renamed into their sibling `_support/` directories by
`task-011` so the glob would not index them. `nodes/status/Status.ts` went with the barrels in
`task-010`; its entire content was `import '../Diagnostics.stories';`. The acceptance criterion
about sibling-registering files is therefore satisfied by the phase 2 reshuffle rather than by work
here.

Deviations from the approved plan:
- None.

Outcome: Four empty renders cleared, two type errors cleared, and the audit now declares its own
blind spot; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T13:08:40Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- The `MithrilPartialSyncConfirmation` site is the useful one. It was found by reading the file, not
  by the scan, and the honest response was to fix the site and then make the scan say what it cannot
  see. An instrument that quietly skips a shape is the thing this epic keeps catching in other
  people's tools.
- Its symptom is also milder and worth distinguishing: the component still rendered, only the `key`
  was `undefined`, so the modal stopped remounting on a theme switch. Same cause, smaller blast
  radius, and correspondingly harder to notice.
- Classifying the eighteen unfollowable exports rather than reporting the number turns an open
  unknown into a closed one: fourteen take no parameters and cannot have the defect.
- Removing the cast in `MithrilPartialSyncDialogue` matters beyond tidiness. It read a top-level
  `osName` off the context, which is there only because the pass-through puts it there, so it would
  have broken silently at `task-024`.

Approval bar:
- Met. `task-019` is complete.

Decision: approved
