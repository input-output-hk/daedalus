---
description: Update agent documentation based on codebase changes
---

# Documentation Update Workflow

This workflow guides updating the `.agent/` documentation to reflect Daedalus codebase changes.

## When to Update

Run this workflow when:

- ✅ Adding new components or significant features
- ✅ Changing IPC channels or API endpoints
- ✅ Modifying MobX store architecture
- ✅ Adding new workflows or processes
- ✅ Resolving issues that should become SOPs
- ✅ Completing a feature that should be documented in `/plans`

---

## Quick Update Procedure

### Step 1: Identify What Changed

Review recent changes:

```bash
# Recent commits
git log --oneline -20

# Changed files
git diff --name-only HEAD~10

# Files changed in specific area
git diff --name-only HEAD~10 -- source/main/
git diff --name-only HEAD~10 -- source/renderer/
```

### Step 2: Update Relevant Documentation

Based on changes, update the appropriate files:

| Change Type                | Files to Update                           |
|----------------------------|-------------------------------------------|
| Architecture changes       | `.agent/system/architecture.md`           |
| IPC channel changes        | `.agent/system/api-endpoints.md`          |
| cardano-wallet API changes | `.agent/system/api-endpoints.md`          |
| MobX store changes         | `.agent/system/architecture.md`           |
| New workflow               | `.agent/workflows/{name}.md`              |
| Resolved issue             | `.agent/SOPs/{category}/{name}.md`        |
| Completed feature          | `.agent/plans/{domain}/{feature}.md`      |

### Step 3: Update Index

If adding new files, update `.agent/readme.md` to include them.

### Step 4: Commit Documentation

```bash
git add .agent/
git commit -m "docs: Update agent documentation"
```

---

## Documentation Templates

### New Workflow

Create `.agent/workflows/{name}.md`:

```markdown
---
description: Brief description for index
---

# {Workflow Name} Workflow

Description of the workflow.

## Quick Commands

\`\`\`bash
# Primary command
yarn command-here
\`\`\`

## Detailed Steps

### Step 1: {Title}
\`\`\`bash
command
\`\`\`

...
```

### New SOP

Create `.agent/SOPs/{category}/{name}.md`:

```markdown
# SOP: {Name}

## Overview
Brief description.

## Prerequisites
- Nix shell active
- cardano-node running

## Procedure

### Step 1: {Title}
Description and commands.

## Common Pitfalls

### ⚠️ {Issue}
**Problem:** Description  
**Solution:** How to fix

## Verification
How to confirm success.

---
**Created:** {Date}
```

### New Task Plan

Create `.agent/plans/{domain}/{feature}.md`:

```markdown
# {Feature Name}

## Overview
Brief description.

## Requirements
- [ ] Requirement 1
- [ ] Requirement 2

## Technical Design
...

## Implementation Steps
1. Step 1
2. Step 2

---
**Status:** ✅ Completed  
**Date:** {Date}
```

---

## Bulk Updates

### After Major Refactoring

1. Review all files in `.agent/system/`
2. Update architecture diagrams
3. Verify IPC channel documentation accuracy
4. Check MobX store documentation

### After IPC Channel Changes

```bash
# Find IPC channel changes
git diff HEAD~20 -- source/common/ipc/api.ts
```

Update `.agent/system/api-endpoints.md` with new/changed channels.

### After MobX Store Changes

```bash
# Find store changes
git diff --name-only HEAD~20 -- source/renderer/app/stores/
```

Update `.agent/system/architecture.md` with store changes.

### After Component Library Changes

Update `.agent/workflows/frontend.md` with component patterns.

---

## Validation

After updating, verify documentation:

1. **Links work** — Check all internal links
2. **Commands work** — Test command examples
3. **Accuracy** — Verify information matches code
4. **Completeness** — Ensure all new features documented

### Quick Validation

```bash
# Check for broken internal links (manual)
grep -r "\.md)" .agent/ | grep -v node_modules

# Verify workflow files exist
ls .agent/workflows/

# Verify system files exist
ls .agent/system/
```

---

## Documentation Categories

### System Documentation (`.agent/system/`)

| File               | Purpose                                 |
|--------------------|-----------------------------------------|
| `architecture.md`  | Electron architecture, IPC, MobX stores |
| `api-endpoints.md` | cardano-wallet API, IPC channels        |

### Workflows (`.agent/workflows/`)

| File              | Purpose                        |
|-------------------|--------------------------------|
| `build.md`        | Nix shells, Yarn builds        |
| `test.md`         | Jest, Cucumber testing         |
| `electron.md`     | Main process development       |
| `frontend.md`     | React/MobX development         |
| `update-doc.md`   | Documentation updates          |

### Plans (`.agent/plans/`)

Implementation plans and PRDs for completed features.

### SOPs (`.agent/SOPs/`)

Standard operating procedures for common issues.

### Skills (`.agent/skills/`)

Cardano CLI guidance (do not modify - known-good reference).

---

## Commit Message Format

```
docs: Update {area} documentation

- Added {new thing}
- Updated {changed thing}
- Fixed {corrected thing}
```

Examples:
```
docs: Update architecture documentation

- Added hardware wallet IPC channel documentation
- Updated MobX store list
- Fixed incorrect directory paths
```

---

## Daedalus-Specific Guidance

### When Adding New IPC Channels

1. Document in `.agent/system/api-endpoints.md` under appropriate category
2. Include channel name, direction, and purpose

### When Adding New MobX Stores

1. Add to store table in `.agent/system/architecture.md`
2. Document in `.agent/workflows/frontend.md` if new patterns

### When Adding New Components

1. Add to component categories in `.agent/system/architecture.md`
2. Create Storybook story if visual component

### When Modifying Cardano Integration

1. Update `.agent/system/architecture.md` process diagram
2. Document any new cardano-wallet API usage
