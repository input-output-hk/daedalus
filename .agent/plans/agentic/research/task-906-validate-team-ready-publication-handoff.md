# Task 906 Team-Ready Publication and Handoff Validation Research

- Date: 2026-03-31
- Task: `task-906`
- Evidence: `.agent/plans/agentic/task-plans/task-906.md`, `.agent/plans/agentic/task-plans/task-906-impl-review.md`

## Durable Findings

### All Helper Command Contracts Validated ✅

| Contract | Status | Finding |
|----------|--------|---------|
| `yarn agentic:kb:publish` | ✅ | Runs `sync all` → exports `.dump`+`.manifest.json` → copies to `AGENTIC_KB_SHARED_DIR` |
| `yarn agentic:kb:fetch` | ✅ | Requires explicit basename → validates both files exist → copies to `agentic/snapshots/` |
| `snapshot import` | ✅ | Manifest schema + dump size/hash + embedding compatibility + disposable-target enforcement |
| `sync changed` | ✅ | Delta from stored baseline commits, bounded by `repo_commit_hash`, not unbounded fan-out |
| `sync project --full` | ✅ | Documented in sync.py:128-132 and workflow doc:94,163-164 |

### One Documentation Gap Identified ⚠️

`agentic/README.md` does not explicitly call out `sync project --full` as the manual re-convergence procedure for Project items after edits to already-seen items. The `--full` flag mechanism exists in the shipped code and is documented in the workflow doc, but README lacks an explicit reference.

**Recommended disposition**: Documentation fix — add explicit `sync project --full` reference to `agentic/README.md` in the team-sharing or sync sections.

### Critiquer Notes Addressed

1. ✅ Path error corrected: helpers are in root `package.json` (lines 74-75), scripts in `scripts/agentic-kb-*.sh` (NOT `agentic/package.json`)
2. ✅ Phase 5 format: manual test steps provided with step-by-step pass/fail criteria (7 steps)
3. ✅ Project refresh doc cross-reference: confirmed in sync.py:128-132 and workflow doc:94,163-164

## No New Research Beyond Task Scope

- Task-906 did not execute the full two-developer publish→handoff on separate machines
- task-906 did not fix the documentation gap in `agentic/README.md` (recommended disposition: documentation fix)
- task-905 (Validate two-developer publish-download-import handoff) remains pending and is blocked on task-906 completion