# Task 702 Stale-Index Detection Research

- Date: 2026-03-29
- Task: `task-702`
- Evidence: `agentic/src/agentic_kb/sync/staleness.py`, `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/ingest/github.py`, `agentic/src/agentic_kb/ingest/project.py`, `agentic/src/agentic_kb/mcp/search_server.py`, `agentic/tests/test_status_command.py`, `agentic/tests/test_github_ingest.py`, `agentic/tests/test_project_ingest.py`, `agentic/tests/test_mcp_search_server.py`, `.agent/plans/agentic/task-plans/task-702-plan-review.md`, `.agent/plans/agentic/task-plans/task-702-impl-review.md`

## Durable Findings

- The packaged stale-detection surface now lives at `agentic/src/agentic_kb/sync/staleness.py` and is consumed by normal `agentic-kb status` rather than embedding freshness rules directly into CLI formatting code.
- Freshness is separate from readiness in the shipped status contract: top-level `StatusReport.ok` remains the runtime/dependency/database aggregate, while a separate `freshness` payload reports per-source `fresh`, `stale`, `missing_baseline`, `skipped`, or `unavailable` states.
- Docs and code freshness reuse the existing repo-scoped `kb_sync_state` rows keyed by `repo_scope_key(DEFAULT_SYNC_REPO)`. A successful stored `repo_commit_hash` that matches local `HEAD` is `fresh`; a differing commit is `stale`; a missing or incomplete row is `missing_baseline`.
- GitHub freshness reuses the existing four per-stream sync-state rows and compares each stored `watermark_timestamp` against a metadata-only latest-watermark probe in `agentic/src/agentic_kb/ingest/github.py`. The helper returns plain timestamps only, and the `issues` probe pages the mixed GitHub `/issues` stream until it finds the first non-PR issue or exhausts the stream so PR stubs do not produce a false-fresh issues result.
- Project freshness reuses the existing single project sync-state row and compares its stored `watermark_timestamp` against a metadata-only GraphQL probe in `agentic/src/agentic_kb/ingest/project.py` that returns only the latest observed project-item `updatedAt` value.
- Remote freshness remains optional for normal status usage. When `GITHUB_TOKEN` is absent, GitHub and Project freshness report `skipped`; they do not report `fresh`, and this does not change readiness success.
- `status --healthcheck` remains unchanged and skips freshness entirely, preserving the task-901 clean-bootstrap boundary and existing exit-code behavior.
- MCP `kb_status` continues to return a successful tool payload even when freshness is stale or skipped because it serializes the same extended status payload shape used by the CLI.
- Matching Project watermarks remain only a narrow freshness signal. Current cursor-based project sync still cannot guarantee replay of edits to already-seen items, so the workflow doc keeps that limitation explicit.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/sync/staleness.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/ingest/github.py" "agentic/src/agentic_kb/ingest/project.py" "agentic/src/agentic_kb/sync/__init__.py" "agentic/tests/test_status_command.py" "agentic/tests/test_github_ingest.py" "agentic/tests/test_project_ingest.py" "agentic/tests/test_mcp_search_server.py" "agentic/tests/test_mcp_search_server_db.py"` was run and passed for syntax validation.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_status_command agentic.tests.test_github_ingest agentic.tests.test_project_ingest agentic.tests.test_mcp_search_server` was run and passed in the main implementation pass.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_github_ingest agentic.tests.test_status_command` was re-run and passed in the follow-up fix pass that corrected the mixed `/issues` latest-watermark edge case.
- DB-backed MCP status coverage remains environment-gated behind `AGENTIC_TEST_DATABASE_URL` and `psycopg`; `agentic.tests.test_mcp_search_server_db` is ready to confirm the real payload shape includes `freshness`, but it was skipped in this environment.

## No New Research Beyond Task Scope

- Task-702 adds detection and operator visibility only. It does not auto-run sync commands, reseed missing baselines, alter `sync changed`, or broaden healthcheck semantics.
