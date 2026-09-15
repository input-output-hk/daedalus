Planner: Iteration 1
Timestamp: 2026-09-15T06:44:10Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-014.md` with the 21 sections the
  task-plans readme requires.
- Scope held to running the chain in the scratch project and measuring the result. Nothing merged.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the codemod coverage
  section at `:767-828`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-014`.
- `.agent/plans/storybook-modernization/task-plans/task-013.md` for the scratch project, and
  `task-001.md` for the baseline.

Repo-Verified Findings Used To Shape The Plan:
- Re-measured the corpus: 68 `storiesOf()` calls across 65 files, not 73, and 5 dynamic
  registrations, not 9. Both figures predate phase 1.
- Found that exactly 3 files hold two `storiesOf()` calls and 62 hold one.
- Confirmed all three codemods exist in the 8.6.18 CLI.
- Located the four control-flow sites at their lines.

Planned Approach:
- Copy the tranche, run the chain, extract effective labels the way Storybook computes them, compare
  against the baseline, check structural validity, then handle the dynamic file separately.

Scope Guard / Self-Review:
- No source change, no manifest change, no tranche decisions beyond the use-or-not question.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T06:48:35Z

Blocking gaps:
- The plan proposes to count files that convert without error and call that the correction cost. It
  is not. A file can convert with zero errors and lose stories, which is precisely the failure this
  corpus is exposed to, because five registrations carry non-literal labels. The measurement has to
  be the label set before and after, against the `task-001` baseline, not the exit status.
- The plan says it will check what happens to the dynamic registrations and stops there. The task
  entry predicts they are "skipped rather than mangled". If that prediction is wrong the plan needs
  to say what to do instead, and proposing a remedy without running it is how a phase 3 tranche
  discovers the remedy does not work.

Non-blocking observations:
- Two counts in the entry are stale and should be corrected in the entry, not just worked around.
- `csf-2-to-3` may do very little on this corpus. Worth recording either way so the tranche tasks do
  not budget for it.
- The four control-flow strings are worth testing for whether they can match anything at all, not
  just whether the fields still exist. A comparison against a string no story is named is already
  dead and cannot regress.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T06:53:20Z

Plan Updates In Response To Critique:
- Made the label set the measurement of record: extract each converted story's effective label the
  way Storybook computes it and diff against the `task-001` baseline, with the reasoning that a
  conversion keeping every file is not the property that matters.
- Added step 6, that any defect found has its remedy tested rather than proposed, and added the
  inlining test to the verification plan.
- Added both count corrections to the tracking updates.
- Added the `csf-2-to-3` observation to Risks.
- Extended the control-flow check from "do the fields exist" to "can the strings still match".

Resulting Approved Plan Shape:
- Chain run over a real tranche and over the dynamic file, measured by label set and structural
  validity, remedies demonstrated, kill criterion applied to the ratio.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing that lands.

Outcome: Canonical task plan revised after critique and approved for build execution
