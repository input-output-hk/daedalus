# Task Plan: task-802 Write OpenCode and Claude Code setup docs

- Task ID: `task-802`
- Title: `Write OpenCode and Claude Code setup docs`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-802` is the next unblocked critical-path follow-up after `task-801` shipped the real packaged stdio MCP server at `agentic-kb mcp-search`.
- The current `agentic/README.md` only states that the MCP server exists and that the Compose `mcp-search` service is a parity harness; it does not yet give agent operators copy-pasteable OpenCode, Claude Code, or local `.mcp.json` setup examples.
- Downstream `task-803` and `task-901` both depend on accurate setup docs that describe the shipped runtime instead of the older aspirational MCP wording already called out in `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`.

## Scope

- Update the agent-facing setup documentation so it matches the shipped stdio MCP runtime and current Compose contract.
- Add copy-pasteable setup examples for OpenCode, Claude Code, and a local `.mcp.json` configuration.
- Document the environment variables and runtime caveats that matter for agent setup, including where the Compose-backed launch path already supplies values versus where direct local launches must provide them explicitly.
- Keep the work focused on setup docs in `agentic/README.md`, with at most one additional small example/config artifact only if README-only examples would be materially harder to keep accurate.
- Treat inline environment-variable documentation in `agentic/README.md` as the expected path for this task because `agentic/.env.example` does not exist today.

## Non-Goals

- Do not do the broader workflow reconciliation in `.agent/workflows/agentic-kb.md`; that remains `task-803`.
- Do not update `AGENTS.md`, `CLAUDE.md`, or `.agent/readme.md`; that remains `task-804`.
- Do not change the Compose stack, packaged CLI, MCP tool surface, sync behavior, or snapshot behavior.
- Do not add wrapper scripts, background daemons, HTTP MCP endpoints, or any new setup flow that is not already supported by the shipped `agentic-kb mcp-search` stdio process.

## Relevant Dependencies

- Completed implementation dependencies:
  - `task-801` - shipped the packaged stdio MCP server and fixed the Compose `mcp-search` parity harness around `agentic-kb mcp-search`.
  - `task-701` - shipped the packaged sync surface that setup docs may need to reference when explaining why `GITHUB_TOKEN` exists in the shared env contract.
  - `task-601` and `task-602` - shipped the snapshot contract and commands already described in the surrounding KB docs, which setup text must not contradict.
- Required planning and research context reviewed for this task:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `agentic/README.md`
  - `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
- Downstream boundaries to preserve:
  - `task-803` owns the finalized workflow-document update.
  - `task-804` owns the agent index cleanup after workflow reconciliation.

## Files Expected To Change

- `agentic/README.md` - primary setup-doc target for the shipped MCP runtime, agent config examples, and env-var reference.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - only to update `task-802` status metadata when implementation is complete.
- `.agent/plans/agentic/task-plans/task-802.md` - keep this canonical plan current through review and implementation.
- `.agent/plans/agentic/task-plans/task-802-plan-review.md` - append-only planning review log.
- No broader doc changes are expected for this task. `.agent/workflows/agentic-kb.md` should stay with `task-803` unless implementation finds a narrow factual contradiction that cannot be left for the workflow reconciliation task.

## Implementation Approach

- **Anchor the docs on the shipped stdio contract**: all examples must describe `agentic-kb mcp-search` as a stdio MCP server, not an HTTP service and not a long-running daemon endpoint.
- **Prefer the repo-supported launcher in examples**: document the Docker Compose-backed launch path as the primary setup path because it matches the shipped stack and avoids introducing separate package-install instructions as required setup. Pin the client-spawned no-TTY stdio launcher as `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` for the OpenCode, Claude Code, and `.mcp.json` examples unless a target client requires the same argv split across config fields.
- **Document the Compose caveat clearly**: explain that `mcp-search` in `docker-compose.agentic.yml` is a parity/smoke harness for the same stdio process, but MCP clients should spawn their own stdio session rather than trying to talk to a background container as if it were a network service.
- **Call out non-interactive launch requirements**: the documented client command should preserve a no-TTY stdio contract for MCP. The setup examples should therefore reflect the exact `docker compose ... run --rm -T mcp-search` invocation shape needed for tool-managed stdio instead of a human-oriented interactive shell flow.
- **Make env-var requirements precise instead of aspirational**:
  - `DATABASE_URL` and `OLLAMA_BASE_URL` are runtime requirements for direct local launches of `agentic-kb mcp-search`.
  - `OLLAMA_EMBED_MODEL` should be documented as an optional override that must stay aligned with the indexed KB if changed from the Compose default.
  - `GITHUB_TOKEN` should be documented as optional for the read-only MCP server itself, but required for `sync github` and `sync project` workflows and for any setup that wants full shared-env parity with the Compose services.
  - The docs should mention the ProjectV2-read caveat already captured in `.agent/workflows/agentic-kb.md` when describing `GITHUB_TOKEN`.
- **Keep the examples copy-pasteable and minimal**: include one working example per target surface: OpenCode, Claude Code, and local `.mcp.json`. Avoid duplicating long explanatory prose around each snippet.
- **Remove parity-harness ambiguity from README setup docs**: the final README text must stop implying `docker compose -f docker-compose.agentic.yml up -d mcp-search` is the user MCP setup path; that command may remain discussed only as Compose parity/smoke context, not as the recommended agent-client configuration flow.
- **Avoid scope creep into workflow cleanup**: setup docs may link to `.agent/workflows/agentic-kb.md` for broader operational steps, but they should not take over the remaining task-803 workflow rewrite.

## Acceptance Criteria

- `agentic/README.md` contains setup guidance that accurately describes the shipped MCP runtime as stdio-only and Compose-backed, with no HTTP or placeholder wording.
- The README includes copy-pasteable examples for OpenCode, Claude Code, and local `.mcp.json` setup.
- The documented launcher command matches the current shipped runtime contract around `agentic-kb mcp-search` and the current `docker-compose.agentic.yml` service shape, using the exact client-spawned no-TTY form `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search` unless a client-specific schema requires the same argv split into separate command/args fields.
- The docs explicitly explain that Compose `mcp-search` is a parity/smoke harness and not a user-facing network endpoint.
- The docs no longer imply `docker compose -f docker-compose.agentic.yml up -d mcp-search` is the user MCP setup path.
- The docs distinguish required versus optional environment variables for agent setup and do not incorrectly imply that `GITHUB_TOKEN` is required for read-only MCP search/status.
- The docs mention the current important caveats from the shipped surface: no-TTY stdio launch expectations, Compose-backed env wiring, and vector/hybrid dependence on reachable Ollama.
- The task stays limited to setup docs and does not broaden into workflow reconciliation, agent-index cleanup, or runtime code changes.

## Verification Plan

- Re-read the final README changes against `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, `agentic/README.md`, `.agent/workflows/agentic-kb.md`, and `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md` to ensure the setup wording matches the actual packaged `agentic-kb mcp-search` CLI contract and the shipped Compose parity-harness contract.
- Verify that each documented example uses only currently shipped commands and service names, especially the packaged `agentic-kb mcp-search` subcommand from `agentic/src/agentic_kb/cli.py`, the `mcp-search` service name from `docker-compose.agentic.yml`, and the exact no-TTY client-spawned launcher form `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- Sanity-check the documented env-var guidance against the current service environment contract in `docker-compose.agentic.yml` so `DATABASE_URL`, `OLLAMA_BASE_URL`, `OLLAMA_EMBED_MODEL`, and `GITHUB_TOKEN` are described accurately.
- Confirm the README edits replace any setup implication that `docker compose -f docker-compose.agentic.yml up -d mcp-search` is the MCP client path, leaving that command only in parity/smoke-harness context if it is still mentioned at all.
- If implementation adds a standalone example JSON file, validate that the file content and README snippet stay byte-for-byte aligned.
- If feasible during implementation, run one lightweight packaged stdio smoke probe using the exact documented launcher shape to confirm the setup command can still initialize the MCP server without relying on undocumented wrappers.

## Risks / Open Questions

- **Client config schema drift**: OpenCode and Claude Code may expect slightly different MCP config shapes than a generic `.mcp.json` example. The implementation still needs to verify the exact field names, but those snippets should all resolve to the same pinned launcher contract rather than introducing alternate runtime flows.
- **`GITHUB_TOKEN` wording**: the current high-level plan lists `GITHUB_TOKEN` in required setup documentation, but repo reality suggests it is optional for read-only MCP itself and required for sync/project ingestion. The docs need to state that distinction carefully so they stay accurate without under-documenting the shared env contract.

## Required Docs / Tracking / Research Updates

- This canonical task plan now records the final approved implementation state, verification notes, and outcome for `task-802`.
- Planning review history is preserved in `.agent/plans/agentic/task-plans/task-802-plan-review.md`.
- Implementation review history is preserved in `.agent/plans/agentic/task-plans/task-802-impl-review.md`.
- `agentic/README.md` now contains the finalized OpenCode, Claude Code, and local `.mcp.json` setup docs for the shipped stdio MCP runtime.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated only for `task-802` metadata and now marks the task `completed`.
- `.agent/plans/agentic/research/task-802-client-setup-docs.md` captures the durable client-config and setup-doc findings from the implementation review loop.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-802-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-802-impl-review.md`

## Implementation Notes

- Updated `agentic/README.md` so the setup docs now describe `agentic-kb mcp-search` as the shipped stdio-only MCP server and pin the repo-supported launcher `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- Added copy-pasteable setup examples for all three task-owned client surfaces:
  - OpenCode config using the real top-level `mcp` local-server shape with a single `command` array.
  - Claude Code setup using `claude mcp add --transport stdio --scope project ... -- docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
  - Local `.mcp.json` using `mcpServers` with `command: "docker"` plus split `args` for the same launcher argv.
- Clarified the environment-variable contract in the README: `DATABASE_URL` and `OLLAMA_BASE_URL` are required only for direct local `agentic-kb mcp-search` launches, `OLLAMA_EMBED_MODEL` is an alignment-sensitive optional override, and `GITHUB_TOKEN` is optional for read-only MCP search/status but required for `sync github` and `sync project` with the existing ProjectV2-read caveat.
- Removed the prior setup ambiguity by keeping `docker compose -f docker-compose.agentic.yml up -d mcp-search` explicitly in parity/smoke-harness context rather than as a user MCP setup flow.

## Files Changed

- `agentic/README.md`
- `.agent/plans/agentic/task-plans/task-802.md`
- `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- `.agent/plans/agentic/research/task-802-client-setup-docs.md`

## Implementation Outcome

- `task-802` is implemented and approved.
- The shipped setup documentation now matches the real `task-801` MCP runtime: stdio-only, client-spawned, Compose-backed, and not a background network service.
- All required task-owned documentation/tracking/research follow-ups are complete without broadening into `.agent/workflows/agentic-kb.md`, `AGENTS.md`, or `CLAUDE.md`.

## Review Outcome

- Planning review outcome: `approved`.
- Implementation review outcome: `approved` after two focused client-config corrections.
- The implementation review corrections were:
  - replacing the incorrect OpenCode snippet shape with OpenCode's actual top-level `mcp` local config contract
  - switching the Claude Code example to the documented `claude mcp add --transport stdio --scope project ...` flow
  - restoring the leading `docker` token in the OpenCode `command` array so the snippet matches the pinned launcher contract

## Final Verification Summary

- Re-read the final `agentic/README.md` changes against `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, and `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`.
- Confirmed the README now documents the shipped stdio-only `agentic-kb mcp-search` entrypoint and the correct Compose parity-harness service name `mcp-search`.
- Confirmed all three documented client surfaces resolve to the same launcher contract: `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- Confirmed the docs no longer imply `docker compose -f docker-compose.agentic.yml up -d mcp-search` is the MCP client setup path.
- Confirmed the environment-variable guidance matches the shipped contract: `DATABASE_URL` and `OLLAMA_BASE_URL` required for direct local launch, `OLLAMA_EMBED_MODEL` optional but index-alignment-sensitive, and `GITHUB_TOKEN` optional for read-only MCP but required for GitHub/project sync workflows.
- No additional command-based runtime verification is recorded for `task-802`; verification stayed focused on source-of-truth review of the delivered documentation against the shipped runtime and Compose contracts.

## Final Required Docs / Tracking / Research Updates

- Updated `agentic/README.md` with the approved OpenCode, Claude Code, and local `.mcp.json` setup examples plus the final env/runtime caveats.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-802` is marked `completed` with `completedAt: 2026-03-29`.
- Added `.agent/plans/agentic/research/task-802-client-setup-docs.md` with durable client-config/setup-doc findings from the review loop.
- Kept the append-only review logs unchanged:
  - `.agent/plans/agentic/task-plans/task-802-plan-review.md`
  - `.agent/plans/agentic/task-plans/task-802-impl-review.md`
- No other project metadata updates were needed beyond the required `task-802` tracker status update.

## Outcome

- The canonical `task-802` task doc now serves as the single source of truth for the approved plan, completed build state, final implementation outcome, review result, verification performed, changed files, and supporting research/tracking references.
