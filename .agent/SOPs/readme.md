# 📝 Standard Operating Procedures (SOPs)

> **Learn from experience. Document solutions. Prevent repeated mistakes.**

This folder contains Standard Operating Procedures — step-by-step guides for tasks that have been successfully completed or issues that have been resolved.

---

## Purpose

When the agent (or you) solves a complex problem or successfully integrates a service:

1. **Document the solution** — Create an SOP immediately
2. **Include pitfalls** — Note what went wrong and how it was fixed
3. **Make it reusable** — Future work on similar tasks should reference this

---

## Related Documentation

- [Agent documentation index](../readme.md)
- [Update documentation workflow](../workflows/update-doc.md)

---

## How to Create a New SOP

Ask the agent:

> "Generate SOP for [task/integration name]"

Or manually create using the template below.

---

## SOP Template

```markdown
# SOP: [Task/Integration Name]

## Overview
Brief description of what this SOP covers.

## Prerequisites
- List of required tools, access, or knowledge
- Environment setup needed

## Step-by-Step Procedure

### Step 1: [Step Title]
Description of what to do.

```bash
# Commands if applicable
example command
```

### Step 2: [Step Title]
Continue with next steps...

## Common Pitfalls

### ⚠️ [Pitfall Name]
**Problem:** What goes wrong  
**Solution:** How to fix it

### ⚠️ [Another Pitfall]
**Problem:** What goes wrong  
**Solution:** How to fix it

## Verification
How to confirm the task was successful.

## References
- Link to relevant documentation
- Related code files

---

**Created:** YYYY-MM-DD  
**Last Updated:** YYYY-MM-DD  
**Author:** [Name/Agent]
```

---

## SOP Categories

Organize SOPs by category:

```
.agent/SOPs/
├── readme.md (this file)
├── cardano/
│   ├── node-sync-issues.md
│   └── wallet-recovery.md
├── electron/
│   ├── ipc-debugging.md
│   └── window-management.md
├── hardware-wallets/
│   ├── ledger-connection.md
│   └── trezor-troubleshooting.md
├── testing/
│   └── e2e-test-failures.md
└── nix/
    └── cache-issues.md
```

---

## Index of SOPs

> Update this section as new SOPs are added.

| Category         | SOP Name         | Description | Date |
|------------------|------------------|-------------|------|
| *(No SOPs yet)*  | -                | -           | -    |

---

## When to Create an SOP

Create an SOP when:

- ✅ A complex integration is successfully completed
- ✅ A tricky bug is finally resolved
- ✅ A deployment issue is troubleshot
- ✅ A new service or tool is set up for the first time
- ✅ The same question/issue comes up multiple times

---

## Best Practices

1. **Be specific** — Include exact commands, file paths, and configuration
2. **Include context** — Explain *why* steps are necessary, not just *what*
3. **Test the SOP** — Verify steps work by following them yourself
4. **Keep updated** — Revise when processes change
5. **Link related SOPs** — Cross-reference when tasks are related
