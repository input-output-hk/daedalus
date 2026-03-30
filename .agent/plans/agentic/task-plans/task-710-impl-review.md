# Implementation Review: task-710

## Task

Document ProjectV2 token scope failure handling

## Status

`implemented`

## Changes Made

### Files Modified

1. **`.agent/workflows/agentic-kb.md`** — Added ProjectV2 token scope documentation after line 286

### Documentation Added (after line 286)

Inserted a new `### ProjectV2 Token Scope` section containing:

- `read:project` as the required classic scope for ProjectV2 read access
- Token behavior: repo-only token works for `sync github` (issues/PRs/comments) but fails on `sync project` with HTTP 403 and `"project scope missing"` error detail
- Recovery command: `gh auth refresh -s read:project`
- Note that `read:project,project` (fine-grained) also works but is not required

## Verification

1. Read `.agent/workflows/agentic-kb.md` lines 288–306 to confirm all 5 acceptance criteria are met:
   - [x] Exact required scope `read:project` stated
   - [x] Error message `"project scope missing"` documented as operator-visible failure
   - [x] Difference between repo-only token (works for `sync github`) vs project-required token (required for `sync project`) explicitly documented
   - [x] Recovery command `gh auth refresh -s read:project` shown
   - [x] `read:project,project` fine-grained alternative noted but distinguished from the basic requirement

2. No Python code changes — error handling already correct and tested per `test_project_ingest.py:652-667`

3. Implementation review log created at `.agent/plans/agentic/task-plans/task-710-impl-review.md`

---

## Code Review: Iteration 1 (2026-03-30T20:33:16Z)

### Documentation Location
`.agent/workflows/agentic-kb.md` lines 288–306

### Acceptance Criteria Verification

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1 | Exact required scope `read:project` stated | PASS | Line 293: `read:project` listed as required classic scope |
| 2 | Error message `"project scope missing"` documented | PASS | Line 297: `"project scope missing"` error detail confirmed |
| 3 | repo-only vs project-required token distinction documented | PASS | Lines 296–298 explicitly describe repo-only token working for `sync github` and failing on `sync project` |
| 4 | Recovery command `gh auth refresh -s read:project` shown | PASS | Lines 301–302 provide the recovery command with correct syntax |
| 5 | `read:project,project` fine-grained alternative noted | PASS | Line 305 notes the fine-grained format as alternative but distinguishes it from the basic requirement |

### Technical Accuracy Check
- Scope name `read:project`: correct (classic token scope for Projects API v2)
- Error message `"project scope missing"`: correct (exact error detail returned by GitHub API)
- Recovery command: correct (`gh auth refresh -s read:project` re-authenticates with the specified scope)
- repo-only token behavior: correct (repo scope does not imply project scope; `sync github` (issues/PRs) works without project scope)

### Completeness
All 5 acceptance criteria satisfied. No code changes required. Documentation is clear and actionable.

Decision: approved
