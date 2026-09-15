Implementation: Iteration 1
Timestamp: 2026-09-15T15:26:40Z

Changes made:
- `governance/DRepDirectory.stories.tsx`: `ConnectedFlow` and `FavoriteToggle` become objects with
  `render` and `name`.
- `governance/Delegation.stories.tsx`: `ConnectedFlow` the same.
- Nothing else. The other four files in the tranche needed no change.

Verification:
- `compile` exit 0. `lint` exit 0. `storybook` exit 0. All three required checks pass together for
  the first time since the window opened.
- Label set from `index.json`: 258 entries across 49 titles, identical to the pre-conversion set.
- `story-args-audit.js`: no story reads an argument nothing fills.
- `@ts-ignore` in the tranche unchanged at 1.
- The fixtures in all four large files remain exported, so the phase 7 screen stories can still
  import them.

What the scan still cannot resolve is 13 renders that come back from a call, down from 16 because
the three converted here are now objects with a `render` it can read. Nine are `withState(...)`,
which returns a function declaring no parameters, and four are `renderConfirmationStory(...)`, whose
returned function names its first parameter `_args`.

Deviations from the approved plan:
- None.

Outcome: The last three type errors are gone and all three required checks pass; ready for code
review

Code Review: Iteration 1
Timestamp: 2026-09-15T15:31:05Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- Re-deriving the no-global-reads claim rather than quoting the census was the right instinct. The
  census was measured on a tree that four tranches have since rewritten, and a stale measurement
  reused as a current fact is the same error in a different costume.
- Two files changed out of six, and the entry says why the other four did not. A tranche that
  reports "nothing to do here, and here is the command" is more useful than one that finds work.
- The count reaching zero is the whole window: 70 errors at `task-015`, 2 cleared by `task-016`, 68
  by the conversion and the five tranches, and every step of it accounted for in an entry.

Approval bar:
- Met. `task-022` is complete and all five tranches are done.

Decision: approved
