# Task Plan: task-710 Document and validate ProjectV2 token scope failure handling

- Task ID: `task-710`
- Title: `Document and validate ProjectV2 token scope failure handling`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

The PRD specifies three locations (lines 45, 345, 446) requiring documentation of ProjectV2 token scope expectations and the operator-visible error behavior when `GITHUB_TOKEN` can read repo issues/PRs but cannot read `DripDropz` Project 5 metadata. The workflow doc at `.agent/workflows/agentic-kb.md:286` mentions `gh auth refresh -s read:project,project` but:

1. Does not state the exact required scope name (`read:project`)
2. Does not show the exact error message operators see when scope is missing
3. Does not explain the difference: repo-only token works for issues/PRs but fails for Project 5 with "project scope missing"
4. Does not give explicit recovery steps

The code-side validation is already complete: `test_project_ingest.py:652-667` proves the error message `"project scope missing"` is extracted correctly via `project._read_http_error_detail()` and stored in sync failure records.

## Scope

- Update `.agent/workflows/agentic-kb.md` to add explicit ProjectV2 token scope documentation
- Add concrete error message example for operator visibility
- Clarify the difference between repo-only token (works for `sync github`) vs project-required token (required for `sync project`)
- Document the correct recovery command: `gh auth refresh -s read:project`
- Note that `read:project,project` (fine-grained vs classic) is an orthogonal concern for org-level permissions

## Non-Goals

- No code changes to `agentic/src/agentic_kb/` - the error handling is already correct and tested
- No changes to `test_project_ingest.py` or other test files - validation is already complete
- No changes to the sync failure storage or error propagation paths
- No new test additions required - existing tests prove the behavior

## Relevant Dependencies

- `task-404` - established Project 5 ingestion with `_read_http_error_detail()` at `project.py:1164`
- `test_project_ingest.py:652-667` - proves error message extraction works correctly
- `agentic-kb.md:286` - current minimal note that needs expansion
- PRD lines 45, 345, 446 - require this documentation

## Files Expected To Change

- `.agent/workflows/agentic-kb.md` - add ProjectV2 token scope documentation section

## Implementation Approach

### Step 1: Documentation Update

Update `.agent/workflows/agentic-kb.md` by expanding the "GitHub Coordination" section (around line 284-287) to include:

**Required scope:**
```
`read:project` - required for ProjectV2 read access
```

**Error behavior when scope is missing:**
When a token without `read:project` scope runs `sync project`:
- HTTP 403 Forbidden response from GitHub GraphQL API
- Error detail: `"project scope missing"`
- This is surfaced via `project._read_http_error_detail()` and stored in sync failure records

**Repo-only token behavior:**
- A `GITHUB_TOKEN` with only repo scope (e.g., from `gh auth login --hostname github.com`) can successfully run `sync github` for issues, PRs, and comments
- The same token will fail on `sync project` with HTTP 403 and `"project scope missing"`
- This is the expected failure mode documented in the PRD

**Recovery command:**
```bash
gh auth refresh -s read:project
```

**Scope clarification:**
- `read:project` - classic scope for ProjectV2 read access (sufficient for this platform)
- `read:project,project` - fine-grained permission format for org-level projects (also works but is not required)

## Acceptance Criteria

1. `.agent/workflows/agentic-kb.md` states the exact required scope: `read:project`
2. The exact error message `"project scope missing"` is documented as the operator-visible failure
3. The difference between repo-only token (works for `sync github`) vs project-required token (required for `sync project`) is explicitly documented
4. The recovery command `gh auth refresh -s read:project` is shown
5. The `read:project,project` fine-grained alternative is noted but distinguished from the basic requirement
6. Existing tests in `test_project_ingest.py:652-667` continue to pass (proving the code-side behavior is correct)

## Verification Plan

1. Read the updated `.agent/workflows/agentic-kb.md` and verify all 5 documentation points above are present
2. Run `python3 -m py_compile` on any modified files (none expected for Python code)
3. Confirm no changes needed to `agentic/src/agentic_kb/` - the code is already correct
4. Confirm existing tests pass: `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_project_ingest` (specifically the scope-missing test at line 652)

## Risks / Open Questions

- **None identified.** This is a pure documentation task. The code behavior is proven by existing tests. No implementation risk.

## Required Docs / Tracking / Research Updates

- This canonical task plan records the approved scope for `task-710`
- Planning review history is preserved in `.agent/plans/agentic/task-plans/task-710-plan-review.md`
- Implementation review history is preserved in `.agent/plans/agentic/task-plans/task-710-impl-review.md` (not needed - documentation-only)
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` updates task-710 status when documentation lands

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-710-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-710-impl-review.md` (not applicable - documentation-only task)
