Planner: Iteration 1
Timestamp: 2026-09-14T23:52:10Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-004.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the route, the container, the epochs component directory, the commented navigation
  item and its message, the two dummy fixtures, the Storybook support module and its registration,
  and the i18n artifacts the removal invalidates.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 14 at
  `:283-295`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-004`.
- `.agent/plans/storybook-modernization/research/05-reachable-screens.md`.
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline, `task-002.md`,
  `task-003.md` and `task-005.md`, `task-plans/readme.md`, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.
- `.agent/skills/i18n-messaging/SKILL.md` for the id convention.

Repo-Verified Findings Used To Shape The Plan:
- Checked every line reference the task entry carries and found three stale: the route binding is at
  `Routes.tsx:195-199` rather than `:196-200` and `EPOCHS` is at `routes-config.ts:12` rather than
  `:13`, both because `task-005` landed first; the story import and registration are at
  `Staking.stories.tsx:14` and `:130-132` rather than `:17` and `:172`, because `task-002` removed 88
  lines from that file.
- Enumerated the 7 tracked files under `components/staking/epochs/` and confirmed only
  `StakingEpochs` is imported from outside it, by two files that both go.
- Confirmed both dummy fixtures are tracked and have exactly two readers each.
- Established unreachability three ways: the commented navigation item, no other affordance, and
  `ROUTES.STAKING.PAGE` being a `goToRoute` target rather than a `<Switch>` binding.
- Read the `DelegationCenterHeader` path end to end so the fourth acceptance criterion rests on code.
- Located the twelve message ids and the four `defaultMessages.json` path entries.
- Worked out the sidebar arithmetic: 259 to 258, with no panel, title or group moving.

Planned Approach:
- Four source edits, then hand off 11 paths, then regenerate i18n by the `task-003` route, then
  verify the tree, the sidebar and four flake checks.

Scope Guard / Self-Review:
- No change to `DelegationCenterHeader`, `DelegationCenter`, the neighbouring
  `IS_STAKING_INFO_PAGE_AVAILABLE` binding, or any other staking route.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T23:57:35Z

Blocking gaps:
- The task entry lists three i18n artifacts to regenerate and the plan repeats the number without
  checking it. There are four. `defaultMessages.json` lives in the same snapshotted directory, and
  it carries both the removed ids and four `path` entries naming the deleted component files. This
  is the identical gap `task-003` found one task ago, and repeating it after finding it once would
  be worse than missing it the first time.
- The first acceptance criterion is a grep for `StakingEpochs`, and the plan does not say how it
  avoids being satisfied or defeated by a substring. `isEpochsInfoAvailable` survives in four places
  in the same story file, and a careless grep either flags it or is loosened until it flags nothing.
  The plan has to state what is matched and how.

Non-blocking observations:
- Three stale line references in one task entry is worth stating as a pattern rather than as three
  separate corrections. Two are `task-005`'s doing and one is `task-002`'s, and both were predicted
  in those tasks' plans.
- The fourth acceptance criterion is the only one in this phase that is about what a user sees. It
  should be answered by naming the mounting path and showing that nothing in the change set is on
  it, not by asserting the header is independent.
- `pageNames['stake-pools-tooltip']` survives another task untouched. Saying so again is cheap and
  stops it being read as an omission.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T00:02:20Z

Plan Updates In Response To Critique:
- Added `defaultMessages.json` to the regenerated artifacts with the reason, and recorded in
  Required Docs, Research, and Tracking Updates that the task entry's list of three is to be
  corrected. Added the gap to Risks as the same shape `task-003` found.
- Added a qualification under the acceptance criteria stating that the grep is run for
  `StakingEpochs` as a word and for the two fixture basenames, and naming `isEpochsInfoAvailable`,
  `nextEpoch` and `epochLength` as the survivors a substring match would otherwise catch.
- Stated the three stale line references together, with which task invalidated each.
- Rewrote the fourth acceptance criterion's evidence as the mounting path, `DelegationCenter.tsx:3`
  and `:61` into `DelegationCenterHeader`, plus the observation that no file in the change set is on
  it.
- Recorded `pageNames['stake-pools-tooltip']` as already unread and deliberately untouched.

Resulting Approved Plan Shape:
- 11 files removed, four source files edited, four i18n artifacts regenerated.
- Verification by tree state, five greps, two sidebar diffs, and four flake checks.
- Two tracking updates to the task entry beyond its status.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the removal set.
- Scope is unchanged: the screen, what only it reads, and the artifacts its removal invalidates.

Outcome: Canonical task plan revised after critique and approved for build execution
