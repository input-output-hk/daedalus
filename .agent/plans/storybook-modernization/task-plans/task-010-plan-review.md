Planner: Iteration 1
Timestamp: 2026-09-15T01:42:20Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-010.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the `stories` entry in `storybook/main.ts` and the barrel files the change orphans.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the glob decision at
  `:355-359` and locked decision 16 at `:338-366`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-010` through
  `task-012` and `task-063`.
- `.agent/plans/storybook-modernization/task-plans/task-062.md`, `task-001.md`,
  `task-006-impl-review.md` and `task-062-impl-review.md`.
- `.agent/plans/storybook-modernization/task-plans/readme.md` and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.

Repo-Verified Findings Used To Shape The Plan:
- Resolved the proposed glob entries through the installed `@storybook/core-common@6.4.22` rather
  than assuming globby semantics, and recorded the two `require.context` regular expressions the
  version actually builds.
- Applied those regular expressions to the tracked file list: exactly 65 matches, 61 under
  `storybook/stories` and 4 under `source/renderer/app`, and zero `_support` modules.
- Found that both side effects the task asks to rehome are already in `preview.tsx`, and that the
  two copies of the stylesheet import differ: the barrel's goes through the `.scss` rule with
  `css-loader` `modules` enabled, the preview's uses an inline loader chain without it.
- Confirmed all seven barrels are pure import lists and that the glob matches none of them.
- Confirmed the two import-only `_support` modules have exactly one importer each, a barrel this
  task deletes, which is the outcome `task-062` recorded in advance.
- Confirmed the other 13 `_support` modules keep their importers.

Planned Approach:
- Swap the entry, build `storybook` before deleting anything, hand off nine paths, re-run the orphan
  walk with updated roots, require unchanged sidebar membership, then run three flake checks.

Scope Guard / Self-Review:
- No ordering work, no CSF conversion, no change to any registration or to the 13 support modules.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T01:47:05Z

Blocking gaps:
- The plan verifies the glob with `globby` and calls it authoritative. It is not. 6.4.22 does not
  glob the file system at config time: it converts the pattern to a regular expression and hands it
  to a webpack `require.context`. A pattern that globby expands correctly and `globToRegexp`
  converts differently would pass this plan's check and fail the build. The verification has to run
  through the version's own `normalizeStories` and `toRequireContext`.
- The plan proposes deleting the barrels and running the build afterwards. That throws away the only
  cheap way to isolate a failure. With the barrels still on disk but no longer referenced, a
  `storybook` build tests the glob alone and leaves a one-line revert if it is wrong. Deleting first
  means a red build could be the glob or the deletion and the plan cannot say which.
- The second acceptance criterion is "verified by count against task-001", and the plan repeats it
  without noticing that `task-012` states the expected count as 257 while the corpus carries 258.
  257 is the baseline minus the 15 phase 1 removals and predates the registration `task-006` added.
  Carrying a wrong expected count into the task whose whole job is counting would guarantee a false
  failure two tasks from now.

Non-blocking observations:
- That `_support` modules are excluded is the single thing `task-062` existed to achieve, and the
  plan should check it against the real matcher rather than reason from the rename.
- `require.context` over `source/renderer/app` scans 1508 tracked files to find four. Worth
  measuring the build time rather than waving at it.
- The two stylesheet imports are not equivalent and the plan should say which one survives and why
  that is the right one, because it is the only change here that could alter what a preview looks
  like.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T01:52:40Z

Plan Updates In Response To Critique:
- Replaced the globby verification with a resolution through the installed
  `@storybook/core-common@6.4.22`, recording the directories, the recursion flags and both regular
  expressions verbatim, then applying those expressions to the tracked file list. The globby
  expansion is kept only as an independent second opinion that agrees at 65.
- Restructured the implementation approach so the `storybook` check runs with the barrels still
  present, as step 2, before the deletion list is handed off, and said what that isolates.
- Added a paragraph under the acceptance criteria giving the real expected count, 258, explaining
  where `task-012`'s 257 comes from, and committing to correct that entry.
- Added the `_support` exclusion as an explicit acceptance criterion checked against the matcher.
- Added the build-time measurement to the approach and to Risks.
- Added the `css-loader` `modules` difference to the findings and the visual-regression risk to
  Risks, naming the symptom so a later report can be traced back.

Resulting Approved Plan Shape:
- One edited line in `main.ts`, nine files removed, one tracking correction to another task.
- Verification staged: matcher resolution, a build before deletion, then orphan walk, sidebar
  membership and three flake checks.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the change itself.
- Scope is unchanged: the entry and the files it orphans.

Outcome: Canonical task plan revised after critique and approved for build execution
