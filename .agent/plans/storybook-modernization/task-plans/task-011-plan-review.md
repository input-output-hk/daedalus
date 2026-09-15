Planner: Iteration 1
Timestamp: 2026-09-15T02:38:15Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-011.md` with the 21 sections the
  task-plans readme requires.
- Scope held to one added `parameters` export in `storybook/preview.tsx`.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the ordering note at
  `:355-359`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-011` and
  `task-012`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md`, `task-010.md`,
  `task-plans/readme.md` and `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.

Repo-Verified Findings Used To Shape The Plan:
- Established that the `task-001` baseline records no order, deliberately, so the first acceptance
  criterion has to be met by its second limb.
- Reconstructed the barrel-era order from `5311ce0d0` by depth-first evaluation of the import graph:
  14 groups, 49 titles, matching the current corpus count exactly.
- Read `storySort.js` and confirmed the `/\s*\/\s*/` separator, the nested-array descent, and the
  same-title shortcut that returns 0.
- Read `StoryStoreFacade.js:128-148` and confirmed the parameter path and that the fallback with no
  parameter is `fileNameOrder`, which under the glob is `require.context` order.
- Enumerated the seven multi-file panels holding 45 of the 258 registrations, whose internal story
  order this task cannot pin.

Planned Approach:
- Generate the nested order array from the reconstruction, add it to `preview.tsx`, verify by
  running the installed comparator, then run three flake checks.

Scope Guard / Self-Review:
- No title renamed, no `main.ts` edit, no `includeNames`.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T02:42:40Z

Blocking gaps:
- The plan says it will verify the order and does not say how, in a phase where nothing can open a
  browser. `storybook:build` passing says the parameter parses, not that the tree comes out in the
  intended sequence. Either the plan names an instrument that actually decides the question or the
  acceptance is unverifiable and should say so.
- The plan states that story order inside multi-file panels is out of scope without saying how much
  of the corpus that is. "Some panels are assembled from several files" is not a finding; seven
  panels holding 45 of 258 registrations is. Without the number a reader cannot tell whether the
  limitation is marginal or half the sidebar.

Non-blocking observations:
- The entry's "15 top-level groups and 53 panel titles" is stale in both figures and should be
  corrected in the entry rather than only worked around here.
- Hand-typing a 49-entry nested literal is a transcription risk for no benefit when the
  reconstruction is already machine-readable.
- Worth stating what happens to a group added later, since an explicit order silently sends it to
  the end.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T02:46:55Z

Plan Updates In Response To Critique:
- Named the instrument: `storySort` is a pure function exported from the installed
  `@storybook/store`, so feeding it the extracted corpus and the new `order` array performs the same
  comparison the preview will. Added it to the implementation approach and as the first item of the
  verification plan, with the failure response.
- Added the measurement: seven panels, named with their file counts, holding 45 of 258
  registrations, and stated that the other 42 come from a single file each and are unaffected.
- Added the correction of both stale figures to Required Docs, Research, and Tracking Updates and to
  the acceptance criteria.
- Added that the order array is generated from the reconstruction rather than transcribed.
- Added the new-group-goes-last consequence and the `'*'` wildcard escape to Risks.

Resulting Approved Plan Shape:
- One added export in `preview.tsx`, verified by the real comparator, then three flake checks.
- Two corrections to the task entry beyond its status.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the change.
- Scope is unchanged: the order array.

Outcome: Canonical task plan revised after critique and approved for build execution
