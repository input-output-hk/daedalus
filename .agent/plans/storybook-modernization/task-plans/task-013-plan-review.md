Planner: Iteration 1
Timestamp: 2026-09-15T05:02:40Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-013.md` with the 21 sections the
  task-plans readme requires.
- Scope held to a scratch pre-flight. Nothing lands on the branch but the findings.
- Classified the task `agent_execution`, on the strength of the registry being reachable.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decisions 1 and 12.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-013` through
  `task-015` and the phase 3 header.
- `.agent/plans/storybook-modernization/research/02-storybook-upgrade-path.md` and
  `03-react-upgrade-gate.md`.
- `.agent/plans/storybook-modernization/task-plans/task-009.md` for the two signatures and counts.
- `.agent/workflows/storybook.md` for the current runtime shape.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed `registry.npmjs.org` answers, which is what makes this executable rather than manual.
- Confirmed `@storybook/react` and `@storybook/react-webpack5` 8.6.18 peer React `^16.8.0`, so
  16.14.0 is in range, and that `typescript` and `@storybook/test` are optional peers.
- Confirmed `@storybook/addon-knobs` has no 8.6.x and its last release is 8.0.1, and that all four
  packages it peers at `^8.0.0` publish 8.6.18.
- Recorded that the repository pins the addon at 6.4.0, not the 8.0.1 the analysis assumes.
- Listed the four legacy-decorator settings at their lines and noted that a dropped one fails at
  runtime, not at build.
- Inventoried the `webpackFinal` body that has to carry across.
- Recorded that `StoryDecorator` is itself an `@observer` and uses `Children.map` and
  `cloneElement`, so both MobX and the decorator-argument shape are on the critical path.

Planned Approach:
- Stand up a scratch project pinned to this repository's non-Storybook versions, copy the real
  support modules in, carry `webpackFinal` across, build, and render through `composeStories` under
  jsdom.

Scope Guard / Self-Review:
- Nothing merged, no codemod, no manifest change, no real story converted.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T05:07:15Z

Blocking gaps:
- The first acceptance criterion says stories render in the preview, and there is no preview here.
  The plan cannot quietly substitute a successful `storybook build` for it: a build that emits a
  bundle says nothing about whether `react-polymorph` renders under the new runtime, and that is the
  kill criterion this whole task exists to test. The plan has to name an instrument that actually
  renders the story through the 8.6 pipeline, and say what it does not cover.
- The plan proposes pinning React and Storybook and says nothing about the rest. A pre-flight that
  installs whatever `react-polymorph` or `mobx` version npm resolves today is testing a tree this
  repository does not have. Every version under test has to be pinned to the repository's, and the
  support modules have to be the repository's own files rather than stand-ins.

Non-blocking observations:
- `git worktree add` would need `git worktree remove` later, which this environment cannot do. A
  plain scratch directory meets the requirement the task entry is actually expressing.
- The addon being pinned at 6.4.0 rather than 8.0.1 is worth stating in the entry, since the
  compatibility analysis silently assumes the target version.
- `withKnobs` failing at 8.0.1 is not listed as a kill criterion but would change `task-016` and all
  five tranche tasks. Worth saying what happens if it does.
- The signature question should produce a written rule, not just an observation, because five later
  tasks apply it and one of the two shapes currently reads `undefined` in production.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T05:12:30Z

Plan Updates In Response To Critique:
- Named `composeStories` from `@storybook/react` 8.6.x as the instrument: it applies project
  annotations, decorators and parameters and returns a component, so rendering it under jsdom
  exercises the 8.6 preview pipeline rather than an approximation. Added assertions on the produced
  DOM for theming, `react-polymorph` markup and the MobX re-render, and added to Risks what it does
  not cover: the manager, the addon panels and the iframe bootstrap.
- Added step 2 pinning every non-Storybook version to the repository's, listed individually, with
  the reason, and step 3 copying the real `StoryDecorator`, `StoryProvider` and theme modules rather
  than writing stand-ins.
- Recorded the scratch-directory choice over `git worktree` and why.
- Added the 6.4.0 versus 8.0.1 discrepancy to the findings and to the tracking updates.
- Added the `withKnobs` failure response: not a kill criterion, but reported before `task-014`
  starts because it changes `task-016` and the tranches.
- Made the signature outcome a written rule in the entry rather than an observation in the log.

Resulting Approved Plan Shape:
- A scratch project pinned to this tree's versions, carrying its real support modules and
  `webpackFinal` body, verified by a build plus `composeStories` under jsdom.
- Five findings recorded in the task entry; nothing merged.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing that lands.
- Scope is unchanged: the pre-flight and its findings.

Outcome: Canonical task plan revised after critique and approved for build execution
