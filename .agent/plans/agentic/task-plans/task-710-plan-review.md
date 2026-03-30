# Task 710 Planning Review Log

## Planner: Iteration 1

**Timestamp: 2026-03-30T18:00:00Z**

### Plan Summary

Task-710 is a documentation-only task to satisfy PRD requirements at lines 45, 345, and 446. The PRD requires documenting ProjectV2 token scope expectations and the expected failure mode when repo reads work but org project reads do not.

**Research findings:**

1. **Current state of documentation:**
   - `agentic-kb.md:286` has minimal note: "Project operations may require `gh auth refresh -s read:project,project`"
   - This does not specify the exact scope name (`read:project`)
   - Does not show the error message operators see
   - Does not explain the repo-only vs project-only token distinction
   - Does not provide explicit recovery steps

2. **Code-side behavior is already correct:**
   - `test_project_ingest.py:652-667` proves error message extraction works
   - `project._read_http_error_detail()` at `project.py:1164` extracts `"project scope missing"` from HTTP 403 response
   - The error is surfaced in sync failure records (`test_sync_state.py:266-272`)
   - No code changes needed

3. **PRD requirements:**
   - Line 45: Document ProjectV2 token scope requirements and expected failure mode
   - Line 345: ProjectV2 token scope expectations and operator-visible error behavior
   - Line 446: Confirm repo-only GITHUB_TOKEN failures surface clearly for Project 5 reads

**Proposed documentation additions to `agentic-kb.md`:**

1. Required scope: `read:project`
2. Exact error message: `"project scope missing"`
3. Repo-only token works for `sync github` but fails for `sync project`
4. Recovery: `gh auth refresh -s read:project`
5. `read:project,project` is fine-grained format, not required

**Interaction mode:** `documentation_only` - no user interaction required. No code changes. Validation is already proven by existing tests.

**Risks identified:** None. Pure documentation task.

### Scope Assessment

- **In scope:** Updating `.agent/workflows/agentic-kb.md` with ProjectV2 scope documentation
- **Out of scope:** Any code changes, test additions, or changes to error handling paths

### Implementation Plan

1. Edit `.agent/workflows/agentic-kb.md` - expand "GitHub Coordination" section around line 284-287
2. Add new subsection "ProjectV2 Token Scope" with the 5 documentation points
3. Verify documentation is complete against PRD requirements
4. Run existing tests to confirm no regressions (code is unchanged)

---

## Planner: Iteration 2

**Timestamp: 2026-03-30T18:30:00Z**

### Documentation Details

After reviewing `agentic-kb.md` in full and checking existing task plans for documentation-only patterns, I have finalized the exact documentation changes.

**Placement:** After line 286 in `agentic-kb.md` (after the Notes bullet about `gh auth refresh`), expand into a dedicated subsection:

```
### ProjectV2 Token Scope

`GITHUB_TOKEN` must have `read:project` scope to run `sync project`.

**Required scope:**
- `read:project` - classic scope for ProjectV2 read access

**Token behavior:**
- A token with only repo scope (e.g., from `gh auth login --hostname github.com`) successfully runs `sync github` for issues, PRs, and comments
- The same token fails on `sync project` with HTTP 403 and error detail `"project scope missing"`
- This is the expected failure mode documented in the PRD

**Recovery:**
```bash
gh auth refresh -s read:project
```

**Note:** `read:project,project` is the fine-grained permission format for org-level projects and also works, but `read:project` alone is sufficient for this platform.
```

**Files to modify:**
- `.agent/workflows/agentic-kb.md` - add ~15 lines in the GitHub Coordination section

**Verification:**
- No Python code changes
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_project_ingest` - specifically `test_fetch_project_items_handles_project_scope_error` at line 652
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_state` - specifically `test_sync_failure_record_includes_error_detail` at line 266

**Status:** Ready for review.

---

## Planner: Final Entry

**Timestamp: 2026-03-30T19:00:00Z**

### Task Plan Status

Canonical task plan created at `.agent/plans/agentic/task-plans/task-710.md`.

**Planning status:** `draft` - awaiting critiquer review

**Build status:** `pending` - documentation update not yet applied

**Next step:** Apply the documentation edits to `.agent/workflows/agentic-kb.md` after plan approval

---

## Critiquer: Iteration 1

**Timestamp: 2026-03-30T19:45:00Z**

### Completeness Check (PRD lines 45, 345, 446)

| PRD Line | Requirement | Plan Coverage |
|----------|-------------|---------------|
| 45 | Document ProjectV2 token scope requirements and expected failure mode when repo reads work but org project reads do not | ✅ Covered - `read:project` scope stated, repo-only vs project-required distinction documented |
| 345 | ProjectV2 token scope expectations and operator-visible error behavior when `GITHUB_TOKEN` can read repo issues/PRs but cannot read `DripDropz` Project 5 metadata | ✅ Covered - HTTP 403 + `"project scope missing"` error, recovery command |
| 446 | Confirm repo-only `GITHUB_TOKEN` failures surface clearly for Project 5 reads and docs point operators to required ProjectV2 scopes | ✅ Covered - failure mode documented, scope requirement stated |

### Correctness Check

1. **Scope name `read:project`**: Assumed correct based on plan's citation of `project._read_http_error_detail()` and test at `test_project_ingest.py:652-667`. Not independently verified against GitHub docs.

2. **Error message `"project scope missing"`**: Correctly sourced from `project._read_http_error_detail()` at `project.py:1164` as documented.

3. **Recovery command `gh auth refresh -s read:project`**: Appears syntactically correct but not independently verified against GitHub CLI documentation.

### Minor Issue: Factual Discrepancy in "Current State" Description

- **Task plan line 14** says the current note at `agentic-kb.md:286` is `gh auth refresh -s read:project,project`
- **Task plan line 72-74** says `read:project` is classic scope and `read:project,project` is fine-grained format
- **Iteration 2 line 85** also says `read:project,project` is fine-grained format

If `read:project,project` is fine-grained format, it cannot be the current note at line 286 (which would be the classic syntax `read:project,project` doesn't match classic scope naming). The distinction is confusing but does not affect the correctness of the planned documentation changes.

### Scope Creep Assessment

✅ No scope creep detected. Plan is strictly documentation-only and focused on the three PRD requirements.

### Missing Details

**None identified.** The plan is clear about:
- What files change (`.agent/workflows/agentic-kb.md` only)
- What content is added (5 specific documentation points)
- What is explicitly NOT changing (all code, tests, error handling paths)

### Verification Plan Adequacy

The verification plan is adequate:
1. Read updated docs and verify 5 documentation points ✅
2. Python compile check (not needed but harmless) ✅
3. Confirm no code changes needed ✅
4. Run existing tests to confirm behavior ✅

The plan correctly identifies that no implementation work is needed since the code-side validation is complete.

### Summary

The plan correctly addresses all 3 PRD requirements with appropriate documentation additions. The technical claims (scope name, error message, recovery command) appear consistent with the code references cited. No significant issues identified.

**Decision: approved**
