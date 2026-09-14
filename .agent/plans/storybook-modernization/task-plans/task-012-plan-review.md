Planner: Iteration 1
Timestamp: 2026-09-15T03:10:40Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-012.md` with the 21 sections the
  task-plans readme requires.
- Scope held to verification plus one tracking correction. No source change lands.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-012`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline, `task-010.md`,
  `task-010-impl-review.md`, `task-011.md`, `task-plans/readme.md`, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.
- `.agent/skills/storybook-creation/SKILL.md` for the shape of a minimal `storiesOf` file.

Repo-Verified Findings Used To Shape The Plan:
- Established that the entry's expected count of 257 is wrong: it subtracts the 15 phase 1 removals
  from the baseline but omits the registration `task-006` added, so the figure is 258.
- Traced the 15 removals to `task-002` (12), `task-003` (2) and `task-004` (1).
- Established that the indexed count follows by composition from two verified facts, the matcher
  selecting exactly 65 files and those 65 carrying 258 registrations.
- Established that `task-010`'s probe used a file a barrel had reached, so it does not answer this
  task's second criterion.
- Identified `source/renderer/app` as the half of the glob where an indexing failure would be least
  visible: 1508 tracked files, 4 of them stories.

Planned Approach:
- Confirm 258, add a probe in a never-referenced directory, confirm the matcher selects it and the
  count rises to 259, break it and require a red build, unbreak it and require green, hand off the
  removal, confirm 258 and green again.

Scope Guard / Self-Review:
- No `main.ts` change, no story change, no permanent addition.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T03:14:55Z

Blocking gaps:
- The plan proposes adding a valid probe story and treating a green build as evidence it was
  indexed. That is the same error the phase has already made once: a file the indexer ignores also
  produces a green build. The plan has to make the probe's presence observable, and the only
  instrument available is the one `task-010` used, an unresolvable import that turns the build red.
- The plan does not say the probe must be removed through the hand-off protocol, nor what proves it
  went. A fake story file left in the application source tree would be a worse outcome than not
  running the test at all, and "remove it afterwards" is not a verification step.

Non-blocking observations:
- The count is established by composition rather than measured in a workbench. That is legitimate
  here and the plan should say so plainly rather than implying it read the number off a running
  instance, since the task entry's wording says "in the running workbench".
- Breaking the probe proves the indexer loads it, but a red build with the probe present is also
  consistent with the file being malformed in some other way. One green build with the probe valid
  and present makes the red result attributable.
- Putting the probe under `source/renderer/app` rather than `storybook/stories` is the better test
  and the plan should say why rather than leaving the location looking arbitrary.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T03:19:10Z

Plan Updates In Response To Critique:
- Rewrote steps 4 and 5: the probe is broken with an unresolvable import and the build is required
  to fail, then unbroken and required to pass with the file still present, so the red result is
  attributable to the import rather than to the file.
- Added the removal as an explicit step with its own evidence, a final extractor run at 258, and
  added the left-behind probe to Risks as the failure that would matter.
- Stated plainly that the indexed count is established by composition, named the two facts composed,
  and noted that the entry's "running workbench" wording is not reachable in this environment.
- Added the reason for the probe's location: `source/renderer/app` is the half of the glob with 1508
  files and 4 stories, so it is where an indexing failure would be least visible.

Resulting Approved Plan Shape:
- Count confirmed by composition, probe added, matched, broken, unbroken, removed, and the corpus
  confirmed back at 258.
- One tracking correction, 257 to 258, with the reason recorded in the entry.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing that lands on the branch.
- Scope is unchanged: verification and one corrected number.

Outcome: Canonical task plan revised after critique and approved for build execution
