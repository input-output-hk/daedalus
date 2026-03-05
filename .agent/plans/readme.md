# 📋 Task & Implementation History

> **Store successful PRDs and implementation plans for reference.**

This folder contains Product Requirement Documents (PRDs) and implementation plans from completed features. Use these as templates and reference when implementing similar functionality.

---

## Purpose

Before implementing any feature:

1. **Check for similar past work** — Look for existing plans that match your task
2. **Use as templates** — Follow the same structure and patterns
3. **Ensure consistency** — Maintain architectural decisions across features

---

## Related Documentation

- [Agent documentation index](../readme.md)
- [Update documentation workflow](../workflows/update-doc.md)

---

## How to Use

### Finding Relevant Plans

Plans are organized by feature domain. Add new domains as needed.

```
.agent/plans/
└── readme.md (this file)
```

### Creating New Plans

When starting a new feature:

1. Ask the agent to run **"plan mode"**
2. Review and finalize the plan
3. Save the final plan in this folder under the appropriate domain
4. Create a matching tasks JSON file to track dependencies and completion timestamps

**Naming convention:**
```
{domain}/{feature-name}.md
{domain}/{feature-name}-tasks.json
```

### Tasks JSON (Required)

Each PRD must have a dedicated tasks JSON file alongside the markdown plan. Use it to track task dependencies, completion timestamps, and status updates in a machine-readable format.

**When updating status:** update both the markdown checklist and the JSON entry for the same task.

**Schema guidance (aligns to existing task files):**

```json
{
  "metadata": {
    "title": "Feature Tasks",
    "created": "YYYY-MM-DD",
    "updated": "YYYY-MM-DD",
    "version": "1.0.0",
    "totalTasks": 0,
    "prdReference": ".agent/plans/{domain}/{feature-name}.md",
    "description": "Optional short description"
  },
  "phases": [
    {
      "id": "phase-1",
      "name": "Phase Name",
      "description": "Phase goal",
      "riskLevel": "low",
      "tasks": [
        {
          "id": "task-001",
          "title": "Task title",
          "description": "Task detail",
          "status": "pending",
          "priority": "medium",
          "estimatedHours": 1,
          "dependencies": ["task-000"],
          "completedAt": "YYYY-MM-DD",
          "targetPath": "path/to/file"
        }
      ]
    }
  ],
  "summary": {
    "totalPhases": 1,
    "totalTasks": 1,
    "criticalPath": ["task-001"]
  }
}
```

**Timestamp convention:** use `completedAt` with an ISO-8601 date (or date-time when available). Existing files may use `completedDate`, but new entries should prefer `completedAt` for consistency.

---

## Plan Template

When creating a new implementation plan, use this structure:

```markdown
# [Feature Name]

## Overview
Brief description of the feature and its purpose.

## Requirements
- [ ] Requirement 1
- [ ] Requirement 2

## Technical Design

### Components Affected
- Module 1: Changes needed
- Module 2: Changes needed

### Store Changes
Describe any MobX store changes.

### IPC Changes  
Describe any new or modified IPC channels.

## Implementation Steps

1. Step 1
2. Step 2
3. Step 3

## Testing Strategy
How this feature will be tested.

## Rollout Plan
How the feature will be deployed.

## Open Questions
Any unresolved decisions.

---

**Status:** ✅ Completed | 🚧 In Progress | ❌ Abandoned  
**Date:** YYYY-MM-DD  
**Author:** [Name]
```

---

## Index of Plans

> Update this section as new plans are added.

| Domain | Plan | Status | Date |
|--------|------|--------|------|
| mithril | bootstrap-cardano-node | 🚧 In Progress | 2026-02-10 |
| mithril | mithril-snapshot-ux | 🚧 In Progress | 2026-03-03 |

---

## Best Practices

1. **Keep plans updated** — Mark status changes as implementation progresses
2. **Mirror status changes** — Update both the markdown checklist and tasks JSON when a task changes
3. **Link to PRs** — Reference the pull requests that implemented the plan
4. **Document deviations** — Note any changes from the original plan
5. **Include learnings** — Add retrospective notes for future reference
