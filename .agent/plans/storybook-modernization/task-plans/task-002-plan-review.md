Planner: Iteration 1
Timestamp: 2026-09-14T22:04:15Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-002.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the four story files, the three registrations in `Staking.stories.tsx`, the material
  only those three read, and the four barrel entries.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 4 at
  `:207-215` and locked decision 13 at `:296-306`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-002`, `task-003`
  and `task-007`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact,
  `task-plans/readme.md` for the cycle, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `.agent/workflows/storybook.md` read for the barrel model only.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed all four files exist, are tracked, and hold the registration counts the task entry
  states.
- Confirmed the three `Staking.stories.tsx` line references, `:92`, `:176` and `:193`, all resolve.
- Confirmed the four barrel entries and that nothing else imports the four files.
- Confirmed all four flags at their stated lines, and traced each to the code that reads it, so the
  claim that these screens are unreachable rests on a path rather than on the flag alone.
- Enumerated every import, constant, knob binding and `pageNames` entry in `Staking.stories.tsx`
  whose only readers are the three removed registrations.
- Found two decorator branches that only the countdown story reached.
- Measured the knob call sites: 13 in the four files, 6 more in the three removed registrations, so
  19 leave the phase 4 surface rather than the 13 the task entry states.
- Worked out the sidebar arithmetic: 273 to 261, three panels gone, 53 distinct titles to 50.

Planned Approach:
- Edit `Staking.stories.tsx` and the two barrels, hand off the four paths for removal, then verify
  the tree, the sidebar and the checks.

Scope Guard / Self-Review:
- No file under `source/` is touched, no flag is changed, the voting stories are left alone, and the
  `_utils` re-verification is left to `task-007`.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T22:08:40Z

Blocking gaps:
- The second acceptance criterion cannot be met as worded and the plan does not say so. It asks for
  a diff against the `task-001` baseline showing exactly 12 registrations removed and nothing else.
  `task-006` has since added one registration deliberately, so the diff will show 12 removals and 1
  addition. Reporting that as a pass without explaining it is how a baseline stops being believed;
  reporting it as a failure would be wrong. The plan has to state the arithmetic both ways.
- The plan removes two decorator branches and describes them as consequences of the deletion. They
  are, but they are the only edit here that is not the removal of something the task entry names,
  and the task's own acceptance is that the sidebar shows twelve removals "and nothing else". That
  needs to be argued in the plan, not noticed in review.

Non-blocking observations:
- Three panels disappear entirely. The plan should name them, because a missing panel is the first
  thing a reader notices and the last thing a count explains.
- The knob figure in the task entry is short by six. Worth recording where the six are, since phase
  4's estimate is built from that surface.
- `pageNames['stake-pools-tooltip']` is already dead and stays dead. Saying so stops a reviewer
  filing it as something this task missed.
- `yarn compile` does not catch a dangling relative import here, which was measured during
  `task-006`. Since this task removes files that a barrel imports, the plan should name
  `storybook:build` as the check that actually tests the barrel edits.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T22:13:05Z

Plan Updates In Response To Critique:
- Added a qualification under the acceptance criteria giving both readings: against the baseline the
  diff is 12 removals and 1 addition, the addition being `task-006`'s; against the tree as it stood
  before this task it is 12 removals and no addition.
- Promoted the decorator simplification from a step into an argued decision, in the implementation
  approach and again in Risks: a test against a story id that no registration carries is a false
  statement that no check will ever flag, it affects no surviving registration because every
  survivor already took the else branch, and it is confined to one file.
- Named the three panels that disappear, in the findings and in the acceptance criteria.
- Added the knob measurement, 13 in the four files plus 6 in the removed registrations, and recorded
  that the task entry's figure is short.
- Added `pageNames['stake-pools-tooltip']` as already-dead and deliberately untouched.
- Added the `compile` limitation and named `storybook:build` as the check that tests the barrel
  edits.

Resulting Approved Plan Shape:
- Four files removed, three files edited, four barrel entries gone.
- Verification by tree state, sidebar diff against the baseline with the arithmetic stated, three
  flake checks, and a derivation-path comparison.
- One tracking update.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the change itself.
- Scope is unchanged: the four files, the three registrations, what only they read, the barrels.

Outcome: Canonical task plan revised after critique and approved for build execution
