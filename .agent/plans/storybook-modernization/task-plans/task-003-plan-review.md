Planner: Iteration 1
Timestamp: 2026-09-14T23:02:20Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-003.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the story, the 17 tracked files under `components/staking/legacy/`, and the i18n
  artifacts the removal invalidates.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 5 at
  `:216-224`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-003` and
  `task-007`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline, `task-002.md`,
  `task-plans/readme.md`, and `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.
- `.agent/skills/i18n-messaging/SKILL.md` for the message id convention.

Repo-Verified Findings Used To Shape The Plan:
- Enumerated the 17 tracked files with `git ls-files` and established that the eight `.scss.d.ts`
  files on disk are gitignored generator output, not tracked content.
- Confirmed nothing imports `Legacy.stories.tsx` and that only `StakingChart` and
  `StakingChartTooltip` are referenced from outside the directory, both by that story.
- Mapped the internal reference graph and found a dead subtree rooted at `legacy/Staking.tsx`,
  which refines locked decision 5's "referenced by nothing at all" for four of the five files it
  names.
- Distinguished the two `_stakingConfig.scss` partials and listed the importers of each, confirming
  nothing outside the directory imports the copy that goes.
- Found that `StakingChartTooltip.tsx` defines six i18n messages and traced them into four tracked
  artifacts, which the task entry does not mention.
- Proved the i18n regeneration mechanism on the unchanged tree: `formatjs extract` reproduces
  `translations/messages.json` byte for byte under `cmp`, and the translation runner leaves the
  locales byte-identical under `diff -r`.
- Worked out the sidebar arithmetic: 261 to 259, 50 titles to 49, `UNREACHABLE` 2 to 0.

Planned Approach:
- Hand off the 18 paths, regenerate the i18n artifacts with the CI `node_modules`, verify the tree,
  the sidebar and four flake checks.

Scope Guard / Self-Review:
- No change to the parent `_stakingConfig.scss` or to any surviving staking component.
- No hand edit to a generated artifact.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T23:07:45Z

Blocking gaps:
- The plan states the i18n consequence and then leaves it in the plan only. The task entry is what
  the next person reads, and as written it would walk them into a red required check with no warning.
  A finding that changes what a task has to do belongs in the entry, not just in the plan that found
  it.
- The plan proposes regenerating i18n but does not say why its output can be trusted. Running a
  generator on a changed tree and committing whatever falls out is how a tracked artifact silently
  acquires an unrelated change. The mechanism has to be shown to be a no-op on the unchanged tree
  first, or the diff afterwards means nothing.
- The task entry says the directory carries `.scss.d.ts` files and to delete them together. The plan
  should not quietly do something different from what the entry says. It has to state that they are
  gitignored generator output, that none is tracked, and that eight will therefore be left on disk.

Non-blocking observations:
- Locked decision 5 says five components are referenced by nothing at all. Four of them are
  referenced, by each other, under a root that is referenced by nothing. Same conclusion, and the
  measured shape is worth recording because it is what a reviewer checking the greps will see.
- Two files named `_stakingConfig.scss` in the same subtree, one deleted and one kept, is the kind
  of thing that reads as a mistake six months later. Naming both with their importers costs three
  lines.
- `UNREACHABLE` going from 2 to 0 is worth stating: after this task the extractor's reachability
  column is uniformly clean, so any later non-zero is a new defect rather than a known one.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T23:12:30Z

Plan Updates In Response To Critique:
- Added an entry to Required Docs, Research, and Tracking Updates committing to record the i18n
  consequence in the `task-003` entry itself, with the four artifacts named, and to correct the
  entry's `.scss.d.ts` note.
- Added the proof of the regeneration mechanism to the findings, both halves, stating that the round
  trip is a verified no-op before the change and that this is what makes its output after the change
  trustworthy. Added the exact commands to the implementation approach.
- Added the `.scss.d.ts` correction to the findings, to Non-Goals and to Risks, with `.gitignore:141`
  and the `precompile` script cited, and stated that eight stale files remain on disk.
- Added the dead-subtree measurement as a refinement of locked decision 5 rather than a
  contradiction of it.
- Added both `_stakingConfig.scss` partials with their importers.
- Added `UNREACHABLE` 2 to 0 to the acceptance criteria.

Resulting Approved Plan Shape:
- 18 files removed, four i18n artifacts regenerated, no hand-edited artifact.
- Verification by tree state, per-component grep, sidebar diff, and four flake checks including
  `i18n`.
- Two tracking updates to the task entry beyond its status.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the removal set.
- Scope is unchanged: the story, the directory, the artifacts the removal invalidates.

Outcome: Canonical task plan revised after critique and approved for build execution
