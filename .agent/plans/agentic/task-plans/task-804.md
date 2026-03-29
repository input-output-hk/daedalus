# Task Plan: task-804 Check core agent indexes

- Task ID: `task-804`
- Title: `Check core agent indexes`
- Planning Status: `approved`
- Build Status: `completed`
- Review Status: `approved`

## Why This Task Was Chosen Now

- `task-804` is the next unblocked documentation reconciliation task after `task-803` finalized `.agent/workflows/agentic-kb.md` around current shipped KB behavior and after `task-802` finalized the MCP client setup contract in `agentic/README.md`.
- The repo already has the three core agent index docs this task owns, but the task tracker and the task-803/task-804 tracking-adjustment research both treat this as a maintenance pass: verify that `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` point agents at the shipped workflow and MCP setup instead of leaving older generic wording as the only guidance.
- `task-901` validated the narrow clean-bootstrap contract and clarified the remaining boundaries around shared snapshots and `sync changed`, so the index docs can now safely summarize the KB entry points without repeating stale or broader rollout claims.

## Scope

- Review `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` against the finalized KB source-of-truth docs: `.agent/workflows/agentic-kb.md` for operator workflow and `agentic/README.md` for copy-paste MCP client setup.
- Update the three agent index docs only where needed so they accurately route agents to the finalized KB workflow and setup entry points.
- Keep the KB guidance in these index docs short and navigational: identify the KB workflow trigger, point to the current workflow doc, and point to `agentic/README.md` for concrete MCP client config examples.
- Preserve each document's current navigation role while updating the KB route: `CLAUDE.md` remains a Claude-specific quick reference, while `AGENTS.md` and `.agent/readme.md` remain broader cross-agent indexes.
- Reconcile any stale or misleading KB wording in the index docs, especially if it implies generic future setup work instead of the now-shipped read-only MCP and clean-bootstrap documentation.
- Update task tracking only if needed when implementation completes.

## Non-Goals

- Do not change KB runtime behavior, Docker Compose services, MCP server behavior, or CLI commands.
- Do not rework `.agent/workflows/agentic-kb.md` beyond narrow contradiction fixes that block the index updates; `task-803` already owns the workflow reconciliation.
- Do not expand `agentic/README.md` beyond narrow contradiction fixes; `task-802` already owns the detailed client setup contract there.
- Do not broaden into general documentation cleanup outside `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md`.
- Do not implement new research unless planning or implementation uncovers a durable finding that is materially new beyond the existing task-802, task-803/task-804, and task-901 research notes.

## Relevant Dependencies

- Completed implementation and documentation dependencies already in repo reality:
  - `task-802` - finalized MCP client setup contract and launcher examples in `agentic/README.md`.
  - `task-803` - finalized `.agent/workflows/agentic-kb.md` as the current-state KB workflow document.
  - `task-901` - validated the narrow clean-bootstrap contract and documented remaining workflow boundaries around snapshots and `sync changed`.
- Required planning and source-of-truth inputs reviewed for this task:
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `.agent/plans/agentic/research/task-802-client-setup-docs.md`
  - `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`
  - `.agent/readme.md`
  - `AGENTS.md`
  - `CLAUDE.md`
  - `.agent/workflows/agentic-kb.md`
  - `agentic/README.md`
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`

## Current Repo State To Reconcile

- `.agent/workflows/agentic-kb.md` now documents the shipped KB stack, current sync/snapshot contracts, the narrow validated clean-bootstrap flow, GitHub Project 5 coordination, and the stdio-only MCP launcher contract.
- `agentic/README.md` now holds the canonical copy-paste MCP setup examples for OpenCode, Claude Code, and local `.mcp.json`, all pinned to `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` currently list `/agentic-kb` among available workflows, but they still describe it at a generic level such as "knowledge base and MCP workflow" or "Agentic knowledge base, sync, and MCP setup" without explicitly routing agents to the finalized workflow doc and README setup examples.
- None of the three index docs currently mention the clean boundary that the workflow doc is the operator source of truth while `agentic/README.md` is the source of truth for client config snippets.

## Files Expected To Change

- `AGENTS.md` - update root agent instructions so KB guidance points to the finalized workflow and setup docs.
- `CLAUDE.md` - update Claude-specific quick reference so KB guidance points to the finalized workflow and setup docs.
- `.agent/readme.md` - update the centralized agent doc index so KB guidance points to the finalized workflow and setup docs.
- `.agent/plans/agentic/task-plans/task-804.md` - canonical task plan and final task record.
- `.agent/plans/agentic/task-plans/task-804-plan-review.md` - append-only planning review log.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - only if task metadata is updated when implementation completes.
- `.agent/plans/agentic/research/` - only if implementation uncovers a materially new durable finding not already captured by existing research.

## Implementation Approach

- **Treat the three target docs as indexes, not full KB manuals**: keep edits short and navigational. Each doc should tell agents where to go for KB operations instead of duplicating the long workflow or the client config snippets.
- **Pin the KB source-of-truth split explicitly**:
  - `.agent/workflows/agentic-kb.md` is the workflow source of truth for boot, status, sync, snapshot, clean-bootstrap, and GitHub coordination.
  - `agentic/README.md` is the setup source of truth for OpenCode, Claude Code, and local `.mcp.json` MCP client examples.
- **Update index wording from generic to shipped-current**: replace vague KB labels like "knowledge base and MCP workflow" where needed with wording that tells agents the workflow covers Compose boot, sync, snapshots, and read-only MCP usage, and that `agentic/README.md` holds copy-paste setup.
- **Keep the MCP contract aligned with task-802 reality**: when the index docs mention agent setup, they should summarize the stdio-only read-only Search MCP and route readers to `agentic/README.md`; they should not imply a background daemon endpoint or restate alternate launcher shapes.
- **Keep the workflow boundary aligned with task-901 reality**: when the index docs mention the KB workflow, they should not imply a broader bootstrap promise than the finalized workflow currently documents. Summaries must stay compatible with the narrow validated clean-machine path and the current snapshot-sharing boundaries.
- **Protect the audience split across the three docs**: keep `CLAUDE.md` as a concise Claude Code quick reference and avoid expanding it into a second general-purpose index; keep `AGENTS.md` and `.agent/readme.md` as broader agent-entry indexes without forcing them into identical wording or layout.
- **Account for per-file relative-link paths explicitly**: any new KB or MCP cross-reference must be written so it resolves from that document's own directory, especially because `.agent/readme.md` needs different relative paths than the repo-root `AGENTS.md` and `CLAUDE.md`.
- **Prefer minimal placement changes over broad rewrites**: the likely fix is a small wording update in the existing workflow tables and quick-start sections, plus one explicit note in each index doc about where MCP setup examples live.
- **Only touch tracking or research if the implementation discovers something durable and new**: if the work is only index cleanup, the existing research notes should remain sufficient.

## Acceptance Criteria

- `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` all accurately point agents to `.agent/workflows/agentic-kb.md` as the KB workflow source of truth.
- The index docs accurately point agents to `agentic/README.md` for concrete OpenCode, Claude Code, and `.mcp.json` MCP setup examples.
- The updated index docs describe the KB/MCP surface in shipped-current terms and do not reintroduce aspirational or ambiguous setup language.
- The updated index docs do not imply `mcp-search` is a background network endpoint or otherwise contradict the stdio-only launcher contract finalized in `task-802`.
- The updated index docs do not overstate the clean-bootstrap or shared-snapshot story beyond what `.agent/workflows/agentic-kb.md` and `agentic/README.md` now document.
- Any new or revised KB/MCP links resolve correctly from each file's own location, including `.agent/readme.md`'s different relative-path requirements.
- `CLAUDE.md` remains a Claude-oriented quick reference, while `AGENTS.md` and `.agent/readme.md` remain broader agent indexes rather than collapsing into near-duplicates.
- The task remains limited to the three owned index docs plus required planning artifacts and any minimal task metadata update.

## Verification Plan

- Re-read the final `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` changes against `.agent/workflows/agentic-kb.md` to confirm they point to the correct KB workflow and summarize it without contradiction.
- Re-read the final index-doc wording against `agentic/README.md` to confirm all MCP setup references route readers to the canonical client examples rather than duplicating or drifting from them.
- Verify every final KB/MCP cross-reference resolves from the edited file's own directory context: repo-root relative links in `AGENTS.md` and `CLAUDE.md`, and `.agent/`-local relative links in `.agent/readme.md`.
- Verify the final KB summaries in the index docs remain compatible with the durable decisions and current repo reality recorded in `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`, `.agent/plans/agentic/research/task-802-client-setup-docs.md`, and `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`.
- Re-read the final wording to ensure the docs still present `/agentic-kb` as the correct workflow trigger, preserve each document's existing audience-specific role, and do not accidentally remove existing useful navigation to other `.agent/` resources.
- If task metadata is updated, re-read the `task-804` entry in `.agent/plans/agentic/knowledge-base-platform-tasks.json` for status/path accuracy.

## Risks / Open Questions

- **Doc duplication drift**: the main risk is accidentally copying too much KB detail from `.agent/workflows/agentic-kb.md` or `agentic/README.md` into the indexes, which would recreate future drift. The edits should stay index-level.
- **Over-summary risk**: the current indexes are generic enough that a too-small edit could still leave agents unclear about where MCP setup examples actually live. The task should add an explicit route to `agentic/README.md`, not just leave `/agentic-kb` in a table.
- **Cross-doc consistency**: `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` are similar but not identical. The task should keep them consistent on KB entry points without flattening their existing audience-specific tone and structure.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for the approved `task-804` plan, current build state, verification summary, and final outcome.
- Planning review history lives in `.agent/plans/agentic/task-plans/task-804-plan-review.md`.
- Implementation review history lives in `.agent/plans/agentic/task-plans/task-804-impl-review.md`.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when implementation completes so `task-804` status metadata matches the approved outcome.
- Add or update research only if implementation uncovers a durable KB-index finding not already covered by existing research notes; otherwise record a task-local no-new-research note.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-804-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-804-impl-review.md`

## Final Build / Review Outcome

- Final implementation outcome: completed with no deviations from the approved plan.
- Final implementation review outcome: approved in `.agent/plans/agentic/task-plans/task-804-impl-review.md`.

## Final Verification Run

- Re-read the final KB-routing wording in `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` against `.agent/workflows/agentic-kb.md` and `agentic/README.md` to confirm source-of-truth routing and shipped-current wording remained aligned.
- Verified the referenced KB/MCP targets exist in repo reality and that `.agent/readme.md` uses its own relative-path context for `./workflows/agentic-kb.md` and `../agentic/README.md`.
- Re-read `.agent/plans/agentic/knowledge-base-platform-tasks.json` after closeout so `task-804` machine-readable status matches the approved implementation outcome.

## Final Outcome

- Outcome: completed.
- Updated `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` with minimal index-level wording changes so they route KB workflow/operator guidance to `.agent/workflows/agentic-kb.md` and MCP client setup examples to `agentic/README.md`.
- Preserved the audience split across the three docs: `CLAUDE.md` remains a Claude-specific quick reference, while `AGENTS.md` and `.agent/readme.md` remain broader agent indexes.
- Verified the final KB/MCP links resolve from each file's own path context without duplicating large workflow or setup content into the index docs.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-804` is recorded as completed.
- Added `.agent/plans/agentic/research/task-804-core-agent-indexes.md` to record that task closeout produced no new durable research beyond the existing task-802, task-803/task-804, and task-901 notes.
