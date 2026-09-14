# Task Plans

Execution records for this plan, one set per task in the companion `*-tasks.json`.

This directory follows the `mithril-partial-sync` convention rather than the
`01-workstream.md` pattern described in `.agent/plans/readme.md`. The workstream
pattern groups notes by theme, which loses the per-task plan, critique and review
cycle that the task graph is built around.

## Files per task

Three files, created when the task is picked up, not in advance.

| File | What it is | Write mode |
|---|---|---|
| `task-NNN.md` | The canonical plan for that task | Revised in place while planning, then stable |
| `task-NNN-plan-review.md` | Planner iterations and the critique of each | Append-only |
| `task-NNN-impl-review.md` | Implementation iterations and code review of each | Append-only |

`NNN` matches the task `id` in the tasks JSON exactly, so `task-007` in the graph
is `task-007.md` here.

## The cycle

1. Write `task-NNN.md` from the task's entry in the tasks JSON, expanded against
   the repository as it stands right now rather than as the PRD described it.
2. Critique it. Append the iteration and the critique to `task-NNN-plan-review.md`
   as `Planner: Iteration N`, with a timestamp, what changed in response to the
   critique, a scope guard, and an `Outcome:` line.
3. Repeat until `## Planning Status` in `task-NNN.md` reads `approved`.
4. Implement. Append each attempt to `task-NNN-impl-review.md` as
   `Implementation: Iteration N` with a timestamp and an `Outcome:` line.
5. Review each implementation iteration in the same file, with a `Summary:` and a
   `Decision:` of `approved` or `requires_changes`.
6. When the decision is `approved`, set `## Build Status` to `completed` and
   update the task's `status` in the tasks JSON. Per `.agent/plans/readme.md`,
   update statuses as work proceeds, not in a batch at the end.

A correction to an earlier iteration is a new entry saying what was wrong, never
an edit to the original.

## Sections in `task-NNN.md`

Reproduce these, in this order, from the precedent at
`.agent/plans/mithril-partial-sync/task-plans/task-001.md`:

```
## Task ID and Title
## Why Chosen Now
## Interaction Mode
## Scope
## Non-Goals
## Dependencies
## Research Consulted
## Docs, Workflows, and Skills Consulted
## Live Repo Findings Verified For Planning
## Files Expected To Change
## Implementation Approach
## Acceptance Criteria
## Verification Plan
## Risks and Open Questions
## Required Docs, Research, and Tracking Updates
## Review-Log Paths
## Planning Status
## Build Status
## Current Outcome
## Final Outcome
## Self-Review
```

`Interaction Mode` is `agent_execution` or `manual_execution`. Choose
`manual_execution` when truthful completion needs something this environment
cannot do, and then the plan must still produce the exact procedure and expected
evidence for an operator to finish it.

Every claim in `Live Repo Findings Verified For Planning` carries a `path:line`,
a command, or a count, verified at planning time. A task plan that restates the
PRD without re-checking it against the repository has skipped the only step that
catches a PRD gone stale.

## Two cautions specific to this plan

**Do not follow the scaffold's own Storybook documentation while converting.**
`.agent/skills/storybook-creation/SKILL.md:26` says in bold that Daedalus uses
`storiesOf()` and not CSF, and `.agent/workflows/storybook.md` teaches
`storiesOf`, `withKnobs` and `@dump247/storybook-state`. Both are accurate for
the corpus as it stands today and become wrong the moment phase 3 lands.
`task-060` rewrites them at the end of the epic. Until then, the PRD and the
task graph are the authority, not those two files.

**Phase 3 is one landing that cannot be decomposed.** A task plan for any task in
that phase has to say what happens if the hop stalls midway. There is no partial
state of the branch that keeps `yarn compile` green: a single file importing
`storiesOf` fails `tsc` the moment the package moves, whether or not the indexer
sees that file.
