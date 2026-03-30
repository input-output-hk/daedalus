# Task Plan: task-805 Add MCP smoke tests against seeded KB

- Task ID: `task-805`
- Title: `Add MCP smoke tests against seeded KB`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-805` is on the critical path: it adds CI smoke coverage for the MCP server against a properly seeded KB, which is a prerequisite for confident MCP integration work.
- `task-801` completed the packaged MCP server implementation and confirmed the stdio contract; `task-803` finalized the Compose bootstrap and KB seeding workflow.
- The existing `test_mcp_search_server_db.py` tests MCP against a minimally-seeded DB with in-process spawning but does not exercise the real CLI entrypoint `agentic-kb mcp-search` against a realistically seeded KB.
- Adding a realistic smoke test now closes the gap between unit-level MCP coverage and full integration validation.

## Scope

- Create `agentic/tests/test_mcp_smoke.py` as a standalone smoke test that exercises the real CLI entrypoint `agentic-kb mcp-search` over stdio against a properly seeded KB.
- The smoke test must cover all 7 MCP tools: `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, `kb_status`.
- The KB seeding must include a realistic mix of document, code, and GitHub entities that allow meaningful smoke validation of all tools. Minimum diversity floor: at least 3 documents, 3 code chunks, 2 GitHub issues, and 2 GitHub pull requests, plus supporting project entities as needed for foreign-key relationships.
- The test must use the stdio Content-Length + JSON-RPC 2.0 contract that the MCP server actually uses, spawning the process the same way a real MCP client would.
- The test must run as a standard unittest and must not require a running Compose stack (it bootstraps its own DB state).

## Non-Goals

- Do not add full regression or acceptance test suites; this is a smoke test only.
- Do not modify the MCP server implementation, CLI wiring, or any runtime contracts.
- Do not test the Compose stack itself; `test_compose_bootstrap.py` already covers that.
- Do not add vector/hybrid search validation (that is covered by `test_mcp_search_server_db.py`).
- Do not add OpenCode, Claude Code, or `.mcp.json` setup documentation.
- Do not change task-801 or task-803 deliverables.

## Relevant Dependencies

- Completed prerequisites:
  - `task-801` - implemented the packaged read-only MCP server at `agentic/src/agentic_kb/mcp/search_server.py` with 7 tools
  - `task-803` - finalized the Compose bootstrap workflow and KB seeding conventions
- Relevant existing tests:
  - `agentic/tests/test_mcp_search_server_db.py` - tests MCP against minimally-seeded DB with in-process spawning
  - `agentic/tests/test_compose_bootstrap.py` - tests Compose stack boot with healthcheck probes
- Files to reference:
  - `agentic/src/agentic_kb/cli.py` - CLI entrypoint and `mcp-search` subcommand
  - `agentic/src/agentic_kb/mcp/search_server.py` - MCP server implementation
  - `agentic/tests/test_mcp_search_server_db.py` - existing stdio MCP test patterns
  - `agentic/schema/init.sql` - DB schema for seeding
  - `agentic/schema/create_indexes.sql` - index creation for seeding

## Current Repo State To Cover

- The MCP server at `agentic/src/agentic_kb/mcp/search_server.py` exposes 7 read-only tools via stdio JSON-RPC 2.0.
- The CLI entrypoint `agentic-kb mcp-search` is wired through `agentic/src/agentic_kb/cli.py`.
- `test_mcp_search_server_db.py` covers the MCP contract but uses minimal seeding and in-process spawning, not the real CLI entrypoint.
- `test_compose_bootstrap.py` covers Compose stack health but does not exercise MCP tools directly.
- No existing test exercises all 7 MCP tools against a realistically seeded KB via the real CLI entrypoint.

## Files Expected To Change

- `agentic/tests/test_mcp_smoke.py` - new smoke test exercising all 7 MCP tools via real CLI entrypoint
- `.agent/plans/agentic/task-plans/task-805.md` - canonical task plan and final task record
- `.agent/plans/agentic/task-plans/task-805-plan-review.md` - append-only planning review log
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - only if task metadata is updated when implementation completes

## Implementation Approach

- **Standalone stdio smoke test**: spawn `agentic-kb mcp-search` as a subprocess using `sys.executable -m agentic_kb.cli mcp-search` (not a PATH-based `agentic-kb` shim) to avoid PATH dependency, same as any real MCP client would.
- **Realistic KB seeding**: seed a mix of document, code, GitHub issue, and project entities that allow meaningful validation of `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`.
- **Content-Length + JSON-RPC 2.0 stdio contract**: reuse the same `_send_message` / `_read_message` helpers from `test_mcp_search_server_db.py` for the stdio framing.
- **DB bootstrapping**: use the same `AGENTIC_TEST_DATABASE_URL` pattern and psycopg for DB setup, reusing the existing schema bootstrap from `test_mcp_search_server_db.py`.
- **Single test class**: one `McpSmokeTests` class with test methods covering each tool and the full initialization flow. Use a unique class name to avoid parallel-test interference with `test_mcp_search_server_db.py`'s shared `_bootstrapped` class-level flag; instance-level `self._bootstrapped` is preferred.
- **No Compose dependency**: the test bootstraps its own DB state and spawns the MCP process directly; it does not require a running Compose stack.

## Acceptance Criteria

- `agentic/tests/test_mcp_smoke.py` runs as a standard unittest and passes when the MCP server and seeded KB are functioning correctly.
- The test exercises all 7 MCP tools and validates that each tool returns a structurally valid JSON-RPC response (not an error) with the expected result schema.
- The test spawns the real CLI entrypoint `agentic-kb mcp-search` as a subprocess, not an in-process server.
- The test seeds a realistic KB mix covering docs, code, and GitHub entities.
- The test validates stdio communication using Content-Length headers and JSON-RPC 2.0.
- The test does not require a running Docker Compose stack; it manages its own DB state.
- The test does not modify any existing MCP implementation, CLI wiring, or runtime contracts.

## Verification Plan

- Run `python3 -m py_compile` on the new test file.
- Run lint/type-check tools if configured in the project (e.g., `ruff`, `mypy`); verify zero errors.
- Run `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_mcp_smoke` to verify the test executes and passes against a correctly seeded KB.
- Verify that the test exercises all 7 MCP tools by checking test method coverage.
- Re-run `agentic/tests/test_mcp_search_server_db.py` to confirm the existing MCP coverage still passes.
- Verify no regressions in related test suites: `test_search_command_db.py`, `test_entity_command_db.py`, `test_status_command_db.py`.
- Verify the new test file follows the same patterns and conventions as `test_mcp_search_server_db.py`.

## Risks / Open Questions

- **DB seeding complexity**: the smoke test needs a realistic mix of entities across all supported entity types. The seeding must not depend on external services (GitHub API) but should include enough diversity to meaningfully exercise `search_docs`, `search_code`, `search_github`, and `find_related`.
- **Test isolation**: the test must explicitly use `TRUNCATE TABLE ... CASCADE` to clean up DB state between test runs, preventing state pollution in parallel or sequential test execution.
- **Environment requirements**: the test requires `AGENTIC_TEST_DATABASE_URL` and `psycopg`, same as `test_mcp_search_server_db.py`. It should skip gracefully when these are unavailable.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for the approved `task-805` plan, current build state, verification summary, and final outcome.
- Planning review history lives in `.agent/plans/agentic/task-plans/task-805-plan-review.md`.
- Implementation review history lives in `.agent/plans/agentic/task-plans/task-805-impl-review.md`.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when implementation completes so `task-805` status metadata matches the approved outcome.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-805-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-805-impl-review.md`

## Final Outcome

- **Status:** completed
- **Verification:** `python3 -m py_compile agentic/tests/test_mcp_smoke.py` passed
- **Lint/type-check:** Not configured in this project (no ruff/mypy in environment)
- **Files created:** `agentic/tests/test_mcp_smoke.py`
- **Review iterations:** 2 planning iterations, 2 build iterations
- **Decision:** approved by code review after fixing instance-level `_bootstrapped` flag

## Implementation Notes

- The smoke test exercises all 7 MCP tools via the real CLI entrypoint (`sys.executable -m agentic_kb.cli mcp-search`)
- Realistic seeding: 3 docs, 3 code chunks, 2 GitHub issues, 2 GitHub PRs, 2 project items
- Instance-level `_bootstrapped` flag avoids parallel test interference
- `TRUNCATE TABLE ... CASCADE` ensures test isolation
- Test skips gracefully when `AGENTIC_TEST_DATABASE_URL` is not set or `psycopg` is unavailable
