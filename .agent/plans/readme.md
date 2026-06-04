# Plans & Implementation History

> **Store active and historical PRDs, task graphs, and research-backed planning workspaces for reference.**

This folder contains Product Requirement Documents (PRDs), implementation plans, task trackers, prompts, and research notes for Daedalus work. Use these planning workspaces as templates and references when implementing similar functionality so new work stays aligned with the project's architecture, workflows, and prior decisions.

---

## Purpose

Before implementing any significant feature:

1. **Check for similar past work** - Look for an existing planning workspace in the same domain.
2. **Use as templates** - Follow the same planning structure, depth, and execution patterns.
3. **Ensure consistency** - Keep architectural decisions, testing strategy, and rollout planning consistent across Daedalus features.

For architectural and workflow context, also read:
- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`

---

## How to Use

### Finding Relevant Plans

Plans are stored in per-feature or per-domain subfolders. A plan folder is intended to be a complete planning workspace, not just a single markdown file.

Preferred structure for new work:

```text
.agent/plans/
└── {feature-name}/
    ├── prompt.md
    ├── {feature-name}-prd.md
    ├── {feature-name}-tasks.json
    ├── research/
    │   ├── 01-topic.md
    │   └── 02-topic.md
    └── task-plans/
        ├── 01-workstream.md
        └── 02-workstream.md
```

Folder roles:
- `prompt.md` - initial request context, planning brief, or problem framing.
- `*-prd.md` - the main design and decision document.
- `*-tasks.json` - the serialized execution plan and dependency graph.
- `research/` - supporting investigation, source analysis, and design notes.
- `task-plans/` - optional deeper workstream breakdowns for large efforts.

Existing Daedalus plan folders may not yet follow this preferred structure exactly. Use the conventions in this document for new plans and when normalizing older ones.

When looking for a precedent:

1. Check whether the feature touches a known Daedalus domain such as wallets, staking, transactions, Mithril, hardware wallets, IPC, onboarding, themes, or installers.
2. Prefer plans that touch the same architecture layer:
   - `source/main/` for Electron main-process work
   - `source/renderer/app/` for React and MobX work
   - `source/common/` for shared types and IPC contracts
   - `tests/` for Jest or Cucumber changes
   - `storybook/` for component demos and visual states
   - `nix/` or `installers/` for packaging and build changes
3. Reuse prior requirements, decision patterns, and task graph structure where they still fit the current problem.

### Creating New Plans

When starting a new feature:

1. Ask the agent to run in planning mode.
2. Gather the relevant repo-local source material first.
3. Create or confirm the feature plan folder.
4. Write the PRD before or alongside the task graph.
5. Add research notes and task-plan notes only when they improve clarity.

**Naming convention:**
```text
{feature-name}/prompt.md
{feature-name}/{feature-name}-prd.md
{feature-name}/{feature-name}-tasks.json
{feature-name}/research/01-topic.md
{feature-name}/task-plans/01-workstream.md
```

**Required files for new/current plans:**
- New plans should include a primary `*-prd.md` file.
- New implementation plans should include a matching `*-tasks.json` file next to the PRD.
- Use `prompt.md` when preserving the original planning request or constraints is useful.
- Use `research/` and `task-plans/` when the work is large enough that design analysis or execution detail would make the PRD hard to maintain.
- Older reference plans may only have a subset of these files if they predate the current convention.

### Research Notes

Research notes live in `research/` folders inside each plan directory. Reference them from the PRD when capturing deeper investigation, alternative designs, external constraints, or implementation findings that should not bloat the main plan.

Research note filenames must use a zero-padded numeric prefix so they remain in intended reading order as the folder grows. Use the pattern `01-topic.md`, `02-topic.md`, `03-topic.md`, and so on.

### Task Plans

Use `task-plans/` for large or complex efforts that need additional execution detail beyond the main tasks JSON. These files are useful for workstream-specific notes, dependency callouts, or implementation sequencing that would be too verbose for the top-level PRD.

Task-plan filenames must also use a zero-padded numeric prefix to preserve reading order.

### Plan Execution Updates

- When executing a plan, update both the PRD markdown and the tasks JSON to reflect real changes.
- Keep the `Status` section in the PRD in sync with task progress.
- Update task statuses in `*-tasks.json` after modifications, not just at the end of the effort.
- If implementation diverges from the plan, record the deviation and why.

### PRD Log Rules

- If a PRD contains a historical log section such as `## Status Log`, `## Progress Log`, or any similar append-only execution or history log, treat it as append-only history.
- Every new log entry must be appended at the end of that existing log section only.
- Never insert a new log entry above older entries, even if it appears semantically related to nearby history.
- Never reorder, rewrite, or delete prior log entries to improve grouping.
- Before adding a new log entry, read the final existing entry and preserve chronological order.
- If an earlier log entry is incomplete, incorrect, or was inserted out of order, append a new corrective entry rather than rewriting history.
- Use explicit date prefixes or date columns in the log's existing format, and preserve the real append order when multiple entries share the same date.

---

## Plan Template

When creating a new implementation plan, use this structure:

```markdown
# [Feature Name] PRD

## Overview
Brief description of the feature and its purpose in Daedalus.

## Problem Statement
What problem is being solved, why the current system is insufficient, and why this work matters.

## Goals
- Goal 1
- Goal 2

## Non-Goals
- Non-goal 1
- Non-goal 2

## Inputs And Source Material
- `.agent/system/architecture.md`
- Relevant workflows, skills, code paths, research notes, or prior plans

## Locked Planning Decisions
- Decision 1
- Decision 2

## Requirements

### Functional Requirements
- [ ] Requirement 1
- [ ] Requirement 2

### Non-Functional Requirements
- Performance, reliability, security, UX, compatibility, or rollout constraints

## Technical Design

### Components Affected
- `source/main/...`: Main-process changes
- `source/renderer/app/...`: Renderer changes
- `source/common/...`: Shared types or IPC contracts
- `tests/...`: Test changes
- `nix/...` or `installers/...`: Build or packaging changes

### Data / IPC / API Changes
Describe any new or modified IPC channels, cardano-wallet integrations, data contracts, or external process boundaries.

### UI / Store / Process Changes
Describe component, container, MobX store, theme, i18n, node-management, or startup-flow changes.

## Implementation Strategy

1. Step 1
2. Step 2
3. Step 3

## Testing Strategy
Describe how the feature will be validated:
- Jest
- Cucumber unit or E2E
- Storybook
- Manual QA
- Platform-specific verification where needed

## Rollout / Migration / Rollback
Describe feature flags, migration expectations, fallback behavior, and rollback options.

## Open Questions
Any unresolved decisions.

---

**Status:** Draft | In Progress | Completed | Abandoned
**Date:** YYYY-MM-DD
**Author:** [Name]
```

## Tasks JSON Template

Every active plan should include a `*-tasks.json` file next to its PRD. This file is the serialized execution plan for the feature: phases, dependencies, acceptance criteria, and implementation notes. It should represent how the work will actually be executed, not just a flat checklist.

```json
{
  "metadata": {
    "title": "[Feature Name] Tasks",
    "created": "YYYY-MM-DD",
    "updated": "YYYY-MM-DD",
    "version": "1.0.0",
    "totalTasks": 0,
    "prdReference": ".agent/plans/{feature-name}/{feature-name}-prd.md",
    "description": "One-sentence summary of the feature",

    "sourceFiles": ["Optional: key files or documents relevant to the plan"],
    "importantNote": "Optional: critical constraint or warning"
  },
  "phases": [
    {
      "id": "phase-1",
      "name": "Phase Name",
      "description": "What this phase accomplishes",
      "riskLevel": "low | medium | high",
      "tasks": [
        {
          "id": "task-001",
          "title": "Short task title",
          "description": "What to implement and why",
          "status": "pending | in_progress | completed | cancelled",
          "priority": "low | medium | high | critical",
          "estimatedHours": 1.0,
          "dependencies": [],

          "targetPath": "path/to/file.ext",
          "targetPaths": ["Optional: multiple related files"],

          "completedAt": "YYYY-MM-DD",
          "actualHours": 1.2,
          "completedNotes": "Optional: what was actually done",
          "implementationSummary": "Optional: brief summary of implementation",

          "implementationNotes": ["Optional: step-by-step guidance"],
          "implementation": {
            "method": "methodName",
            "parameters": ["param1: Type"],
            "returnType": "ReturnType",
            "fields": ["field1: Type"],
            "logic": ["Step 1", "Step 2"],
            "change": "Description of the change"
          },

          "testCases": ["Optional: test case descriptions"],
          "acceptance": ["Optional: acceptance criteria"],
          "subtasks": [
            {
              "id": "task-001-a",
              "title": "Subtask title",
              "description": "Subtask description",
              "status": "pending"
            }
          ],

          "notes": "Optional: additional context",
          "parameters": ["Optional: CLI or method parameters"],
          "securityNotes": ["Optional: security considerations"]
        }
      ]
    }
  ],
  "summary": {
    "totalPhases": 0,
    "totalTasks": 0,
    "estimatedTotalHours": 0,
    "criticalPath": ["task-001", "task-002"],
    "keyFiles": ["path/to/most/important/file1", "path/to/most/important/file2"],

    "riskAreas": ["Optional: key risks and unknowns"],
    "testingNotes": "Optional: testing strategy summary",
    "rollbackPlan": "Optional: how to roll back",
    "securityNotes": ["Optional: security considerations"]
  }
}
```

### Field Reference

| Field | Required | Description |
|-------|----------|-------------|
| `metadata.title` | Yes | Human-readable plan title |
| `metadata.created` | Yes | Creation date (YYYY-MM-DD) |
| `metadata.updated` | No | Last modification date |
| `metadata.version` | Yes | Semantic version, bump on structural changes |
| `metadata.totalTasks` | Yes | Count of all tasks across all phases |
| `metadata.prdReference` | Yes | Relative path to the companion PRD |
| `metadata.description` | Yes | One-sentence feature summary |
| `metadata.sourceFiles` | No | Key source files, docs, or prior plans relevant to the work |
| `metadata.importantNote` | No | Critical constraint or warning |
| `phase.id` | Yes | Sequential phase identifier (`phase-1`, `phase-2`, ...) |
| `phase.riskLevel` | Yes | `low`, `medium`, or `high` |
| `task.id` | Yes | Sequential task identifier (`task-001`, `task-002`, ...); use suffixes when splitting tasks |
| `task.status` | Yes | `pending`, `in_progress`, `completed`, or `cancelled` |
| `task.priority` | Yes | `low`, `medium`, `high`, or `critical` |
| `task.estimatedHours` | Yes | Estimated effort in decimal hours |
| `task.dependencies` | Yes | List of task IDs this task depends on; use `[]` if none |
| `task.targetPath` | Conditionally | Primary file path this task modifies when one path is the obvious target |
| `task.targetPaths` | Conditionally | Use when the task intentionally spans multiple important files or directories |
| `task.completedAt` | No | Date the task was completed |
| `task.actualHours` | No | Actual hours spent |
| `task.implementationNotes` | No | Array of step-by-step implementation guidance |
| `task.implementation` | No | Structured implementation details (method, parameters, fields, logic, change) |
| `task.testCases` | No | Array of test case descriptions |
| `task.acceptance` | No | Array of acceptance criteria |
| `task.subtasks` | No | Nested subtask objects with `id`, `title`, `description`, `status` |
| `task.notes` | No | Freeform additional context |
| `task.securityNotes` | No | Array of security considerations |
| `summary.criticalPath` | Yes | Ordered list of task IDs on the critical path |
| `summary.keyFiles` | Yes | Most important files for the feature |
| `summary.riskAreas` | No | Key risks and unknowns |
| `summary.rollbackPlan` | No | How to roll back the feature |

---

## Index of Plans

> Update this section as new plans are added. Existing entries may not yet match the preferred folder structure above.

| Domain | Plan | Status | Date |
|--------|------|--------|------|
| Mithril | [Mithril Snapshot Bootstrapping](mithril/bootstrap-cardano-node.md) | In Progress | 2026-02-16 |
| Mithril | [Mithril Snapshot UX Refinement](mithril/mithril-snapshot-ux.md) | In Progress | 2026-03-24 |
| Mithril | [Chain Storage PR Review Fixes](mithril/chain-storage-pr-review-fixes.md) | Reference | Unknown |
| governance | governance-drep-discovery-plan | 🚧 In Progress | 2026-05-27 |
| Mithril | [Mithril Partial Sync From Diagnostics](mithril-partial-sync/mithril-partial-sync-prd.md) | Draft | 2026-05-18 |

---

## Best Practices

1. **Start with source material** - Gather the relevant code paths, architecture docs, workflows, and prior plans before writing the PRD.
2. **Write the design before the task graph** - Capture problem framing, goals, constraints, and locked decisions before breaking work into tasks.
3. **Keep plans structured** - Prefer one clear planning workspace with `prompt.md`, PRD, tasks JSON, research, and optional task-plan notes.
4. **Track real execution** - Update both markdown and tasks JSON as implementation evolves.
5. **Document deviations** - Note where implementation differs from the original plan and why.
6. **Be explicit about verification** - Call out required Jest, Cucumber, Storybook, manual, and platform-specific checks.
7. **Preserve history** - Treat completed plans and append-only logs as historical records, not disposable drafts.
