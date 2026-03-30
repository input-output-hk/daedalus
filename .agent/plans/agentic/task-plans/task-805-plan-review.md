Planner: Iteration 1
Timestamp: 2026-03-30T12:00:00Z
Outcome: initial_plan_documented

- Drafted the canonical `task-805` plan for adding MCP smoke tests against a seeded KB.
- Scope covers creating `agentic/tests/test_mcp_smoke.py` that exercises all 7 MCP tools (`search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, `kb_status`) via the real CLI entrypoint `agentic-kb mcp-search` against a realistically seeded KB.
- Implementation approach pins the test to the stdio Content-Length + JSON-RPC 2.0 contract, reuses existing DB bootstrap patterns from `test_mcp_search_server_db.py`, and keeps the test standalone without Compose dependencies.
- Distinction from existing tests: `test_mcp_search_server_db.py` tests MCP in-process with minimal seeding; `test_compose_bootstrap.py` tests Compose health without exercising MCP tools directly; this smoke test exercises the real CLI entrypoint against a properly seeded KB.
- Acceptance criteria require the test to run as standard unittest, pass against a correctly seeded KB, exercise all 7 tools, spawn the real CLI subprocess, and not require a running Compose stack.
- Planning status: `draft`, build status: `in_progress`.

Critiquer: Iteration 1
Timestamp: 2026-03-30T12:30:00Z

**Scope & Boundedness**
The scope is tightly bounded: one new smoke test file, 7 named tools, one test class. Non-goals are explicitly enumerated and discipline is shown by excluding Compose testing, regression suites, implementation changes, and documentation updates. No scope creep detected.

**Distinction from Existing Tests**
The plan clearly differentiates from `test_mcp_search_server_db.py` (minimal seeding, in-process server import) and `test_compose_bootstrap.py` (Compose health, no MCP tool invocation). However, note that `test_mcp_search_server_db.py:_spawn_server` already spawns the CLI via `subprocess.Popen` with `[sys.executable, "-m", "agentic_kb.cli", "mcp-search"]`, so the "real CLI entrypoint" distinction applies to that test too. The true differentiators are: (a) seeding depth/diversity and (b) full tool coverage (all 7 vs. a subset). The plan could make this nuance explicit to avoid reader confusion.

**Acceptance Criteria Specificity**
Most ACs are concrete and testable. However, "seeds a realistic KB mix covering docs, code, and GitHub entities" is underspecified. There is no definition of what count, diversity, or relationships constitute "realistic" vs. "minimal." This ambiguity could lead to an implementation that barely exceeds `test_mcp_search_server_db.py`'s 2-document seeding. Suggest defining a minimum entity count or diversity floor (e.g., at least N rows per entity type) to anchor the "realistic" qualifier.

Additionally, the AC "exercises all 7 MCP tools" is met by calling each tool, but the plan does not require validating that `search_code` and `search_github` return non-empty, well-structured results from seeded data. A smoke test that calls a tool but ignores its output is thin coverage. Suggest adding an AC that output is structurally valid and not an error.

**Implementation Feasibility**
High confidence. The plan correctly identifies reuse of `_send_message`/`_read_message` helpers, `_bootstrap_database`, psycopg patterns, and subprocess spawning from the existing test. The `AGENTIC_TEST_DATABASE_URL` skip condition and DB connection cleanup via `addCleanup` are correctly carried over. No feasibility concerns.

**Gaps in Verification and Risks**
1. Seeding strategy: The risk "realistic KB mix without external services" is acknowledged but not resolved in the plan. The plan should define the seeding SQL/approach to avoid implementation divergence.

2. Test isolation: The plan references reusing existing DB bootstrap patterns but does not explicitly require `TRUNCATE TABLE ... CASCADE` between test runs. The existing `test_mcp_search_server_db.py:_seed_rows` uses this pattern; it should be explicitly mandated to prevent state pollution.

3. Verification plan omits linting/type-checking commands. The plan lists `py_compile` and `unittest` runs but no `ruff`, `mypy`, or equivalent checks. Given the existing codebase has lint/type infrastructure, omitting this creates a verification gap.

4. The plan does not verify that the new test file itself passes a compile check or import check before the full unittest run.

**Risks Not Addressed**
- The plan does not address what happens if the CLI entrypoint `agentic-kb` is not installed on the PATH during test execution. The existing test uses `sys.executable -m agentic_kb.cli` which does not require a shim. This should be the subprocess command used to avoid PATH dependency.
- Parallel test execution: if this smoke test runs concurrently with `test_mcp_search_server_db.py`, both share the class-level `_bootstrapped` flag and may interfere. The plan does not address whether the new test should use a different bootstrapping strategy.

**Human Checkpoints**
No hidden autonomous checkpoints detected. The plan describes a straightforward single-step implementation followed by verification steps.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-30T14:00:00Z

Addressed all 6 critique points from Iteration 1:

1. **Diversity floor**: Added explicit minimum counts to KB seeding: "at least 3 documents, 3 code chunks, 2 GitHub issues, and 2 GitHub pull requests, plus supporting project entities." This anchors the "realistic" qualifier with concrete numbers.

2. **Structural validity AC**: Added new AC requiring each of the 7 tools to return a structurally valid JSON-RPC response (not an error) with the expected result schema. This closes the thin smoke coverage concern.

3. **PATH dependency**: Changed subprocess spawning from `agentic-kb` (PATH shim) to `sys.executable -m agentic_kb.cli mcp-search` to avoid PATH dependency.

4. **Parallel test interference**: Added guidance to use a unique class name `McpSmokeTests` distinct from `test_mcp_search_server_db.py`'s class, and preferred instance-level `self._bootstrapped` over class-level flag to prevent shared-state interference.

5. **Lint/type-check gap**: Added lint/type-check step to verification plan (e.g., `ruff`, `mypy`) alongside `py_compile`.

6. **Test isolation**: Changed cleanup guidance from informal "clean up DB state" to explicit mandate: "TRUNCATE TABLE ... CASCADE" to prevent state pollution.

Also updated `planning_status` from `draft` to `in_review` per iteration protocol.

Outcome: revision_submitted

Critiquer: Iteration 2
Timestamp: 2026-03-30T15:00:00Z

Verified all 6 prior critique points are adequately addressed:

1. **Diversity floor** (line 19): Explicit minimums stated — 3 documents, 3 code chunks, 2 GitHub issues, 2 GitHub PRs, plus supporting project entities. The "realistic" qualifier is now anchored with concrete numbers.

2. **Structural validity** (line 74): New AC requires each tool returns a structurally valid JSON-RPC response with the expected result schema. Thin smoke coverage concern is closed.

3. **PATH dependency** (line 64): Subprocess spawning now uses `sys.executable -m agentic_kb.cli mcp-search` explicitly. No reliance on a `agentic-kb` shim.

4. **Parallel interference** (line 68): Unique class name `McpSmokeTests` and instance-level `self._bootstrapped` explicitly recommended to avoid sharing `_bootstrapped` flag with `test_mcp_search_server_db.py`.

5. **Lint/type-check gap** (line 84): Verification plan now includes lint/type-check step with examples (`ruff`, `mypy`).

6. **Test isolation** (line 94): Cleanup mandate changed from informal "clean up DB state" to explicit `TRUNCATE TABLE ... CASCADE`.

No new issues detected. The plan is thorough, unambiguous, and implementation-ready.

Decision: approved
