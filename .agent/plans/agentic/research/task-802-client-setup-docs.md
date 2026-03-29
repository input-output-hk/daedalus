# Task 802 Client Setup Docs Research

- Date: 2026-03-29
- Task: `task-802`
- Evidence: `agentic/README.md`, `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, `.agent/plans/agentic/task-plans/task-802-impl-review.md`, `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`

## Durable Findings

- The accepted client launcher contract for the Daedalus KB MCP server is the exact no-TTY stdio command `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`. All client examples should preserve that same argv even if their config schemas split it differently.
- `agentic-kb mcp-search` remains a stdio-only MCP server. The Compose `mcp-search` service is only a parity/smoke harness for that same packaged process and must not be documented as a background network endpoint or as `docker compose ... up -d mcp-search` user setup.
- OpenCode does not use Claude/Desktop-style `mcpServers` JSON for local process config. The durable OpenCode shape for this task is a top-level `mcp` object, one named server entry with `type: "local"`, and a single `command` array that must include the leading `"docker"` token.
- Claude Code's clearest copy-pasteable setup path is the CLI flow `claude mcp add --transport stdio --scope project <name> -- docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` rather than presenting Claude as just another generic JSON block.
- The project-scoped local `.mcp.json` artifact remains useful as the generic Claude-compatible config representation. Its durable shape here is `mcpServers` with `type: "stdio"`, `command: "docker"`, and the remaining launcher tokens in `args`.
- Setup docs should distinguish direct local runtime requirements from Compose-backed launches: `DATABASE_URL` and `OLLAMA_BASE_URL` are required for direct local `agentic-kb mcp-search`, Compose wires them automatically, `OLLAMA_EMBED_MODEL` is an optional override that must stay aligned with the indexed KB, and `GITHUB_TOKEN` is optional for read-only MCP but still required for `sync github` and `sync project` with the existing ProjectV2-read caveat.

## Verification Notes

- Implementation review approved the docs only after fixing three client-facing accuracy issues: the initial OpenCode snippet used the wrong config schema, the Claude Code section needed the documented `claude mcp add` flow instead of a duplicate generic JSON example, and the OpenCode `command` array had to restore the leading `docker` token.
- Final verification for task-802 was documentation-to-runtime consistency review, not a new runtime smoke test: the approved README was re-read against `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, and the task-801 MCP runtime research.

## No New Research Beyond Task Scope

- Task-802 did not change the MCP runtime, Compose service contract, workflow docs, or agent index docs; it only finalized the durable client setup/documentation contract for the already shipped read-only search MCP surface.
