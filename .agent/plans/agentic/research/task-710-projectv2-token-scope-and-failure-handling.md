# Task 710 ProjectV2 Token Scope Failure Handling Research

- Date: 2026-03-30
- Task: `task-710`
- Evidence: `.agent/workflows/agentic-kb.md`, `agentic/src/agentic_kb/ingest/project.py`, `agentic/tests/test_project_ingest.py`

## Durable Findings

- ProjectV2 read access requires the `read:project` scope on `GITHUB_TOKEN`
- The `read:project,project` notation refers to fine-grained permissions format for org-level projects, which also works but is not required
- When a token without `read:project` scope runs `sync project`, GitHub GraphQL API returns HTTP 403 Forbidden with error detail `"project scope missing"`
- The error is extracted via `project._read_http_error_detail()` at `project.py:1164` and surfaced in sync failure records
- A repo-only token (sufficient for `sync github` issues/PRs/comments) fails on `sync project` with the same HTTP 403 error
- Recovery command: `gh auth refresh -s read:project`

## Documentation Changes

- Updated `.agent/workflows/agentic-kb.md` with new `### ProjectV2 Token Scope` section (lines 288-306)
- Documents: required scope, token behavior distinction, error message, recovery command, fine-grained alternative note

## Verification Notes

- Code-side validation already complete via `test_project_ingest.py:652-667` (`test_fetch_project_items_handles_project_scope_error`)
- No code changes required - error handling was already correct
- Documentation satisfies all 3 PRD requirements (lines 45, 345, 446)
