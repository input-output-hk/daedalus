Planner: Iteration 1
Timestamp: 2026-09-15T08:32:10Z

Plan Summary:
- Created `task-016.md` with the 21 sections. Scope held to the globals, the wrapper, the write-back
  and the addon removal.

Repo-Verified Findings Used To Shape The Plan:
- The addon has exactly two consumers, `StoryWrapper.tsx:6` and `General.stories.tsx:7`.
- `_support/config.ts` already exports the three option lists.
- `getInitialState()` exists only to do what Storybook does for globals natively.
- `@storybook/preview-api@8.6.18` exports `useGlobals`.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T08:36:45Z

Blocking gaps:
- Three of the four acceptance criteria describe a running preview, and the plan does not say it
  cannot execute them. Reporting them met on a green build would be the same error this phase has
  already made three times, in the form the coordinator named: an instrument reporting on something
  other than what it appears to measure.
- The plan does not commit to an expected error count. The window is being tracked by a number and
  a task that changes it should say what it will become before it runs, otherwise whatever appears
  gets rationalised.

Non-blocking observations:
- `@storybook/preview-api` will be imported directly and is only present transitively. Leaving it
  that way while writing up a finding about that exact pattern would be inconsistent.
- Two comments elsewhere describe the channel mechanism and will be wrong afterwards.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T08:40:20Z

Plan Updates In Response To Critique:
- Marked the three behavioural criteria as not executable here, said what stands in their place, and
  noted that persistence and URL encoding are Storybook's own behaviour rather than this
  repository's.
- Committed to 68 as the expected count, and to the `storybook` failure moving from the manager to
  the indexer.
- Added declaring `@storybook/preview-api` to the approach.
- Added the two stale comments to the file list.

Outcome: Canonical task plan revised after critique and approved for build execution
