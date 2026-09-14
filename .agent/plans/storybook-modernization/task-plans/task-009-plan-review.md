Planner: Iteration 1
Timestamp: 2026-09-14T21:12:40Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-009.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the measurement and to attaching it to the phase 3 entries.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-009` and the
  phase 3 entries `task-016`, `task-018` through `task-022`, and `task-024`.
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md` on the toolbar globals.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact.
- `.agent/plans/storybook-modernization/task-plans/readme.md` for the cycle and section list, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed `storybook/preview.tsx:8` registers `StoryWrapper` as the only preview decorator and
  that `StoryWrapper.tsx:77-81` is where the three values are handed to the story.
- Measured 65 functions and components across 39 files depending on the pass-through, 35 naming one
  of the three values and 30 forwarding by spread.
- Established that the corpus uses two incompatible signatures, 18 reading the first render argument
  and 16 the second, and that both in-repo comments describing them are right about their own file.
- Found a class component reading `this.props`, and a read through an `as` cast, neither of which a
  plain function-parameter pass sees.
- Reconciled the registration total against the `task-001` baseline at 273.
- Recorded the raw grep figures, 44 property-access lines and 27 second-argument signatures, as a
  cross-check rather than as the measurement.

Planned Approach:
- Measure with the TypeScript AST over `storybook/stories/` plus the four colocated story files.
- Break the result down by phase 3 tranche using each task's own `targetPaths`.
- State the adjustment for the files phase 1 removes.
- Attach one `implementationNotes` line per phase 3 entry.

Scope Guard / Self-Review:
- No story file, no `StoryWrapper`, no preview configuration is edited.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T21:16:55Z

Blocking gaps:
- The plan reports a single population and calls it the count. The 30 that forward by spread and the
  35 that name a value need different edits and fail differently, and a tranche task handed one
  number cannot tell how much of its work is which. Worse, the spread-only population is the one
  that fails silently, so collapsing them into a total understates the risk exactly where it is
  highest.
- The plan does not say which render argument each named read comes from. The corpus contains two
  incompatible signatures and `task-016`, which depends on this count, is the task that has to serve
  both. A count that does not distinguish them tells `task-016` the size of its job and nothing
  about its shape.
- The task entry says the count is taken after `task-002` and `task-003` land or is adjusted. The
  plan has to state the adjustment per tranche with the file named, not assert that one was made,
  because the figures will be read months after the deleting tasks land and nobody will re-derive
  them from an assertion.

Non-blocking observations:
- The measurement corrected itself twice, once for sweeping application source outside the corpus
  and once for missing a class component and a cast. Both belong in the record, because a census
  that quietly changed its answer is a census nobody can check.
- Four colocated registrations belong to no tranche. That is a gap in the phase 3 graph rather than
  in this count, and it should be recorded rather than fixed here.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T21:21:30Z

Plan Updates In Response To Critique:
- Split every figure into named reads and spread-only forwarding, in the findings, in the per-tranche
  table and in each `implementationNotes` line, and added a Risks entry saying why the spread-only
  population is the one most likely to be underestimated.
- Added the first-argument, second-argument and `this.props` split to the findings and to the
  `task-016` note, together with the two in-repo comments that each describe one of the two
  signatures correctly.
- Added the per-tranche adjustment with the deleting task and the file named for each, and stated
  that the named-read counts are unchanged by all four.
- Added the two census corrections to the implementation approach and the self-review.
- Added the four unassigned colocated registrations to the findings and to Risks, as a gap in the
  task graph rather than in the count.

Resulting Approved Plan Shape:
- An AST census over the story corpus, split three ways and reported per tranche.
- Seven `implementationNotes` attachments plus the task status.
- No flake check, with the reason stated.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the change.
- Scope is unchanged: measure and attach.

Outcome: Canonical task plan revised after critique and approved for build execution
