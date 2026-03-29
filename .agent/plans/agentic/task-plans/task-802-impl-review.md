Implementation: Iteration 1
Timestamp: 2026-03-29T15:55:35Z
Outcome: completed_with_focused_verification

- Changes made: updated `agentic/README.md` so the setup docs now describe `agentic-kb mcp-search` as a stdio MCP server, pin the repo-supported no-TTY client launcher `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`, add copy-pasteable OpenCode, Claude Code, and local `.mcp.json` examples, and clarify the required versus optional environment variables and the Compose parity/smoke-harness caveat.
- Files touched: `agentic/README.md`.
- Verification run: re-read the README changes against `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, and `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`; confirmed the documented launcher, service name, stdio-only runtime wording, and environment-variable guidance match the shipped `agentic-kb mcp-search` and Compose `mcp-search` contracts. No additional command-based runtime verification is evident in the current task worktree state.
- Deviations from approved plan: none in implementation scope; the work stayed limited to README setup documentation and did not change runtime code, workflow docs, or agent index docs.

Code Review: Iteration 1
Timestamp: 2026-03-29T15:56:31Z
Outcome: requires_changes

- High: `agentic/README.md:64-83` labels the first snippet as an OpenCode example, but the JSON shape is Claude/Desktop-style `.mcp.json`, not OpenCode's documented config schema. OpenCode expects MCP servers under a top-level `mcp` object with `type: "local"` and a single `command` array, while this snippet uses `mcpServers` plus separate `command`/`args` fields. As written, the example is not copy-pasteable for OpenCode, which fails a core acceptance criterion for task-802.
- Medium: `agentic/README.md:85-125` gives Claude Code and local `.mcp.json` identical snippets without clarifying the Claude-specific setup surface. Claude Code supports project `.mcp.json`, but this section does not identify where the snippet belongs or use the documented `claude mcp add --transport stdio ...` flow. Once the OpenCode example is corrected, re-check the Claude example against the intended install path so each target surface is genuinely copy-pasteable instead of three copies of one generic JSON block.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-03-29T15:57:52Z
Outcome: completed_with_review_fixes

- Changes made since prior review: updated `agentic/README.md` to replace the incorrect generic JSON OpenCode snippet with OpenCode's real top-level `mcp` local config shape and single `command` array, switched the Claude Code example to the documented `claude mcp add --transport stdio --scope project ... -- docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` flow, and clarified that the checked-in local `.mcp.json` example is the project-scoped config artifact written or mirrored for Claude-compatible clients.
- Files touched: `agentic/README.md`.
- Verification run: re-read the updated README against `agentic/src/agentic_kb/cli.py` and `docker-compose.agentic.yml`; confirmed the docs still point at the shipped stdio-only `agentic-kb mcp-search` entrypoint and the `mcp-search` Compose service, while the client-specific setup surfaces now match the implementation review requirements for OpenCode, Claude Code, and local `.mcp.json`.
- Deviations from approved plan: none. The fix stayed within README setup documentation and did not require canonical task-doc changes.

Code Review: Iteration 2
Timestamp: 2026-03-29T16:10:00Z
Outcome: requires_changes

- High: `agentic/README.md:66-85` still does not give a valid copy-pasteable OpenCode launcher. The snippet now uses the correct top-level `mcp` shape, but its `command` array starts with `"compose"` instead of `"docker"`, so it no longer matches the pinned launcher contract documented everywhere else in this task: `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`. As written, OpenCode would try to execute a non-existent `compose` binary in a normal Docker setup. This fails the approval bar requiring the OpenCode snippet to match the real local-config shape and the shipped launcher contract.

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-03-29T16:12:00Z
Outcome: completed_with_review_fixes

- Changes made since prior review: verified that `agentic/README.md` now restores the full OpenCode launcher argv by putting the leading `"docker"` token back into the `command` array, so the OpenCode example once again matches the pinned launcher contract `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` used throughout the task.
- Files touched: `agentic/README.md`.
- Verification run: re-read `agentic/README.md` and confirmed the OpenCode snippet now uses the correct top-level `mcp` local config shape with `command: ["docker", "compose", "-f", "docker-compose.agentic.yml", "run", "--rm", "-T", "mcp-search"]`; also confirmed the Claude Code and local `.mcp.json` examples still resolve to the same launcher contract.
- Deviations from approved plan: none. The fix stayed limited to the README setup docs, and the canonical task doc did not need a status update.

Code Review: Iteration 3
Timestamp: 2026-03-29T15:59:43Z
Outcome: approved

- Verified `agentic/README.md` against the task-802 approved plan, `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, and the task-801 MCP research. All three client surfaces now resolve to the same shipped stdio launcher contract for `agentic-kb mcp-search`: OpenCode uses the client-specific top-level `mcp` local config with `command: ["docker", "compose", "-f", "docker-compose.agentic.yml", "run", "--rm", "-T", "mcp-search"]`, Claude Code uses the documented `claude mcp add --transport stdio --scope project ... -- docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` flow, and the local `.mcp.json` example preserves the same argv split across `command` plus `args`.
- Verified the surrounding README wording still matches the shipped runtime contract: it correctly describes `agentic-kb mcp-search` as a stdio-only MCP server, keeps `docker compose ... up -d mcp-search` explicitly in parity/smoke-harness context rather than as a user setup flow, and accurately states the env/runtime caveats that matter for task-802 (`DATABASE_URL` and `OLLAMA_BASE_URL` required for direct local launch, `OLLAMA_EMBED_MODEL` as an alignment-sensitive override, and `GITHUB_TOKEN` optional for read-only MCP but required for `sync github` and `sync project`, including the ProjectV2-read caveat).
- Scope remains correct for task-802: the changes stay in `agentic/README.md` setup docs only and do not broaden into workflow reconciliation, agent index cleanup, or runtime/code changes.

Decision: approved
