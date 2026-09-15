Planner: Iteration 1
Timestamp: 2026-09-15T11:05:40Z

Plan Summary:
- Created `task-018.md` with the 21 sections. Thirteen files, of which eight change.

Repo-Verified Findings Used To Shape The Plan:
- A Storybook 8 render function is called as `render(context.args, context)`, and a decorator's
  `story({ x })` merges `x` onto the context rather than into `args`. Measured through
  `composeStories`, not read off the bundle.
- Nine sites read the context and work. Six read `context.args` and get `{}`.
- `news/IncidentOverlay.stories.tsx` is the only file in the corpus injecting a fixture through
  `story({ ... })`, and its three stories read it from the wrong argument.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T11:10:15Z

Blocking gaps:
- The first draft treated the props-to-context move as a rename. It is not: half the sites read an
  argument that does not carry what they ask for, so the move is a fix rather than a relocation, and
  the plan has to say which sites are which or the diff reads as churn.
- The stated acceptance criteria cannot see the `IncidentOverlay` defect. "No story reads
  currentTheme, osName or locale from props" is satisfied by a story that reads a fixture from the
  wrong argument and renders an empty component. A criterion covering that had to be added.

Non-blocking observations:
- Reading globals through one `_support` module rather than at 15 sites gives `task-024` a single
  grep target and keeps the label-to-value mapping next to the list the toolbar is built from.
- `getInitialState` in `config.ts` has had no caller since the addon went. Removing it here costs
  nothing and stops it being carried through five more tranches.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T11:14:50Z

Plan Updates In Response To Critique:
- Split the 15 sites into the nine that work and the six that do not, and said so in the findings.
- Added the acceptance criterion about first-argument reads.
- Added `IncidentOverlay`'s fixture move to the approach and the file list.
- Added the `getInitialState` removal.

Outcome: Canonical task plan revised after critique and approved for build execution
