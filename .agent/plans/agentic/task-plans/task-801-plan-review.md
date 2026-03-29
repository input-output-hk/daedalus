Planner: Iteration 1
Timestamp: 2026-03-29T15:23:29Z
Outcome: initial_plan_documented

- Drafted the canonical `task-801` plan around the fixed v1 decision that MCP stays read-only and must wrap the existing packaged KB search/entity/status contracts instead of introducing a second retrieval stack.
- Anchored the implementation on the current repo reality that `docker-compose.agentic.yml` still runs a placeholder `mcp-search` shell container and that the tracker path for `task-801` still points at stale un-packaged location `agentic/src/mcp/search_server.py`.
- Proposed the smallest packaged shape: keep MCP implementation under `agentic/src/agentic_kb/mcp/`, replace the placeholder Compose service with a build-backed packaged server process, and add at most one MCP runtime dependency in `agentic/pyproject.toml`.
- Kept downstream boundaries explicit by deferring OpenCode/Claude Code setup docs to `task-802` and the broader workflow/doc reconciliation to `task-803`, while still allowing only minimal truth-maintenance updates if runtime packaging changes require them.
- Pinned the initial `find_related` direction as a minimal search-based wrapper over existing entity lookup plus the current search store, with seed-result exclusion in the MCP layer and no new schema or indexing work.

Critiquer: Iteration 1
Timestamp: 2026-03-29T15:31:00Z
Outcome: requires_changes

- The plan is directionally correct on scope and task boundaries, but it is not implementation-ready yet because the container/runtime contract is still too ambiguous. It says the canonical transport is stdio MCP while also requiring an always-on Compose `mcp-search` service, yet it does not decide what long-lived process the container actually runs, how Compose health should prove server readiness without inventing a second protocol, or whether the container is only a parity/smoke harness versus a user-facing server. Lock one concrete contract for `docker-compose.agentic.yml` so later implementation review can tell whether the service shape matches the approved plan.
- The tool surface is named, but the MCP request/response contracts are still too loose for later implementation and review. The plan should pin, per tool, the accepted inputs and the stdout JSON-safe payload shape that MCP returns, especially for `search_docs`/`search_code`/`search_github` filter behavior, `get_entity` argument names and not-found/error semantics, `kb_status` payload granularity relative to `status --json`, and whether `find_related` takes `entity_type` plus `id` only or any optional limit/mode overrides. Right now multiple materially different MCP schemas could satisfy the plan.
- `find_related` is still underspecified in a way that will cause implementation drift. The current plan says to derive query context from stored text fields and/or stored embedding, but that leaves open whether the tool can trigger live Ollama embedding generation, whether it is allowed to fall back when an entity row lacks enough text or embedding data, what fields count as the seed text source, and what bounded over-fetch/limit behavior is required before self-filtering. Those choices materially affect performance, dependencies, and testability, so they need to be locked in the canonical plan rather than left to implementation.
- The verification section needs one more concrete contract at the MCP boundary itself. The plan already requires focused tests plus one DB-backed smoke path, but it should explicitly require tool-level tests against the actual MCP server registration/invocation layer, not only direct Python helper calls, and it should state how error cases are asserted through the chosen MCP runtime. That matters because downstream `task-802` setup docs and `task-901` clean-machine validation depend on the real MCP surface, not just shared helper functions.
- The plan should also tighten the dependency/runtime boundary around embeddings. Current search reality in `agentic/src/agentic_kb/search/query.py` requires embeddings for vector or hybrid search, and `AgenticConfig` defaults `OLLAMA_BASE_URL` plus `OLLAMA_EMBED_MODEL`. If MCP `search` and `find_related` are meant to preserve those modes, the plan should say explicitly whether the MCP server requires Ollama reachability for those tools in normal operation, what happens when Ollama is unavailable, and whether any tool is intentionally constrained to BM25-only fallback. Without that, the read-only server contract is still incomplete.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T15:28:31Z
Outcome: plan_revised

- Revised the canonical `task-801` plan to pin the packaged/runtime contract on one entrypoint, `agentic-kb mcp-search`, under `agentic/src/agentic_kb/mcp/search_server.py`, and to require the Compose `mcp-search` service to build from the local `agentic/` project and run that exact stdio MCP process rather than any placeholder or alternate daemon wrapper.
- Locked the Compose role and readiness contract so implementation review can verify one concrete shape: Compose is a parity/smoke harness for the same stdio server local MCP clients will launch, it does not expose a network port, and it reuses `agentic-kb status --healthcheck` for readiness instead of inventing a second probe protocol.
- Added a dedicated `Pinned MCP Tool Contracts` section that specifies exact v1 request and success-payload schemas for `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`, including domain-wrapper filter limits, GitHub narrowing rules, and MCP-layer error semantics for validation and not-found cases.
- Tightened `find_related` from an open-ended concept into a minimal implementation-ready contract: required inputs are only `entity_type`, `id`, and optional `limit`; seed text comes only from configured `text_columns`; the tool is BM25-only, uses one bounded over-fetch search, excludes the seed entity in the MCP layer, and never performs live embedding generation.
- Made MCP verification expectations explicit by requiring tests against the actual server registration/invocation layer and a DB-backed stdio smoke path that exercises `tools/list` and `tools/call`, while also pinning Ollama dependency behavior so BM25 works without Ollama and vector/hybrid modes fail explicitly instead of silently falling back.

Critiquer: Iteration 2
Timestamp: 2026-03-29T15:32:01Z
Outcome: approved

- Re-read the revised canonical plan against the current repo state in `docker-compose.agentic.yml`, `agentic/pyproject.toml`, `agentic/src/agentic_kb/cli.py`, `agentic/src/agentic_kb/commands/{search,entity,status,output}.py`, and `agentic/src/agentic_kb/search/query.py`. The previously ambiguous areas are now pinned tightly enough for implementation review: the stdio entrypoint, Compose parity role, healthcheck contract, tool-level schemas, MCP error semantics, and the BM25-only `find_related` behavior all map cleanly onto the existing packaged CLI/search foundations.
- The plan now also handles the main repo-truth mismatches explicitly instead of papering over them. It calls out the stale `task-801.targetPath` in `.agent/plans/agentic/knowledge-base-platform-tasks.json`, the current placeholder `mcp-search` service in Compose, and the absence of any current MCP entrypoint in `agentic_kb.cli`, which makes the expected implementation delta auditable without broadening scope into `task-802` or `task-803`.
- The verification plan is sufficiently concrete for this task boundary. It requires MCP-runtime-level tests, a seeded stdio smoke path, explicit vector/hybrid failure behavior when Ollama is unavailable, and reuse of existing JSON-safe serializer boundaries instead of inventing a second response format. That is enough to judge the eventual implementation without reopening planning.

Decision: approved
