# Task Plan: task-803 Update agentic knowledge base workflow doc

- Task ID: `task-803`
- Title: `Update agentic knowledge base workflow doc`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-803` is the next critical-path docs reconciliation task after `task-503`, `task-602`, `task-701`, and `task-802` shipped the local CLI, snapshot workflow, incremental sync contract, and MCP client setup docs that the workflow now needs to describe accurately.
- `.agent/workflows/agentic-kb.md` already exists, but the tracker and the task-803/task-804 adjustment research explicitly treat it as a maintenance task: update the existing workflow so it matches final shipped behavior instead of the earlier platform-plan wording.
- Downstream `task-804` depends on this workflow being correct before agent indexes such as `.agent/readme.md`, `AGENTS.md`, and `CLAUDE.md` are checked and reconciled.

## Scope

- Revise `.agent/workflows/agentic-kb.md` so it matches the currently shipped `agentic-kb` command surface and Compose contract.
- Reconcile the workflow doc's boot, status, search, sync, snapshot, and MCP sections with the implemented behavior in `agentic/src/agentic_kb/cli.py` and the task-owned command modules.
- Keep the workflow focused on operator-facing usage and caveats that belong in the workflow, especially around `sync changed`, destructive snapshot import, and the stdio-only MCP launcher contract.
- Explicitly document any current shipped container-path limitations that materially affect the documented workflow, including the current `sync changed` dependency on `git` even though the shipped `kb-tools` image does not install `git` today.
- Update task tracking only if needed to reflect task completion metadata once implementation lands.

## Non-Goals

- Do not change shipped runtime behavior in `agentic/src/agentic_kb/cli.py`, `commands/status.py`, `commands/search.py`, `commands/sync.py`, `commands/snapshot.py`, or `docker-compose.agentic.yml` as part of this task.
- Do not broaden into agent index cleanup in `AGENTS.md`, `CLAUDE.md`, or `.agent/readme.md`; that remains `task-804`.
- Do not create new setup docs in `agentic/README.md` beyond narrow contradiction fixes if one is discovered; `task-802` already owns the finalized client setup contract there.
- Do not add wrapper scripts, new Compose services, new CLI commands, stale-index detection, or scheduled automation.

## Relevant Dependencies

- Completed implementation dependencies already in repo reality:
  - `task-503` - packaged `status`, `search`, and `entity get` CLI behavior in `agentic/src/agentic_kb/cli.py`, `commands/status.py`, and `commands/search.py`.
  - `task-602` - snapshot export/import contract and manifest behavior in `agentic/src/agentic_kb/commands/snapshot.py`.
  - `task-701` - full sync command family and `sync changed` incremental contract in `agentic/src/agentic_kb/commands/sync.py`.
  - `task-801` - read-only stdio Search MCP server in `agentic/src/agentic_kb/cli.py` and the packaged MCP module.
  - `task-802` - finalized user-facing client launcher/setup contract in `agentic/README.md`.
- Planning and research sources reviewed for this task:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `agentic/README.md`
  - `.agent/readme.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `.agent/plans/agentic/research/task-802-client-setup-docs.md`
  - `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-503-search-cli-and-status-commands.md`
- Downstream boundary to preserve:
  - `task-804` owns the follow-up verification and cleanup of agent index docs after this workflow document is finalized.

## Files Expected To Change

- `.agent/workflows/agentic-kb.md` - primary target for workflow reconciliation against shipped CLI, sync, snapshot, and MCP behavior.
- `.agent/plans/agentic/task-plans/task-803.md` - canonical plan document for this task.
- `.agent/plans/agentic/task-plans/task-803-plan-review.md` - append-only planning review log.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - only if task metadata is updated when implementation completes.
- `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md` - only if implementation uncovers a durable tracking clarification that is materially new.
- `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md` or a new follow-up tracking artifact - only if implementation needs to record the missing-`git` image/runtime limitation as a durable follow-up beyond the workflow doc itself.

## Implementation Approach

- **Treat the workflow as operator documentation, not a second README**: keep `.agent/workflows/agentic-kb.md` focused on how Daedalus contributors boot, inspect, sync, snapshot, and connect to the KB stack, while avoiding duplication of the full client-config examples already finalized in `agentic/README.md`.
- **Anchor every workflow claim to shipped commands**: re-read the workflow against the actual CLI surface in `agentic/src/agentic_kb/cli.py`, making sure the documented commands are limited to `status`, `search`, `entity get`, `sync <subcommand>`, `snapshot <subcommand>`, `service`, and `mcp-search`.
- **Reconcile status/search behavior with the implemented command contracts**:
  - `status` should describe runtime config, mount visibility, dependency reachability, and live DB inspection when `DATABASE_URL` is usable.
  - `status --healthcheck` should be documented as lightweight and dependency-focused rather than full schema inspection.
  - `status --json`, `search --json`, repeated `--entity-type`, repeated `--filter key=value`, and `entity get` failure semantics should be mentioned where operator-facing scriptability matters.
- **Lock sync sections to the shipped task-701 contract**:
  - `sync docs`, `sync code`, `sync github`, and `sync project` are explicit source syncs.
  - `sync changed` is the general incremental command for an already-seeded KB and must not be documented as an empty-KB bootstrap substitute.
  - The workflow must clearly state the current baseline requirements for `sync changed`, the git-baseline caveat for docs/code deltas, the shared-watermark caveat for GitHub, and the cursor-continuation-only caveat for Project.
  - The workflow must also call out the current Compose/image limitation for `sync changed`: the documented `docker compose ... run --rm kb-tools sync changed` path depends on `git` because `commands/sync.py` shells out to `git`, but the current `agentic/Dockerfile` does not install `git`, so this path is not fully usable inside the shipped `kb-tools` container until a follow-up runtime task lands.
  - `sync all` must remain documented as only `docs -> code -> github -> project` in that fixed order, stopping on first failure.
- **Lock snapshot sections to the shipped task-602 contract**:
  - export writes a `.dump` plus sibling `.manifest.json` into `/workspace/agentic/snapshots` by default.
  - import accepts either the dump path or the manifest path, validates manifest plus dump identity first, and remains destructive to the `agentic` schema with required `--yes` acknowledgement.
  - the workflow should continue to warn users to restore only into isolated or disposable KB databases.
- **Keep MCP wording aligned with task-801/task-802 reality**:
  - document `agentic-kb mcp-search` as a stdio-only MCP server.
  - keep the supported client launcher contract pinned to `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
  - explain that the Compose `mcp-search` service is a parity/smoke harness, not a background network endpoint for clients.
  - prefer linking readers to `agentic/README.md` for copy-pasteable OpenCode and Claude Code snippets rather than duplicating large config blocks in the workflow.
- **Remove stale aspirational setup wording instead of editing around it**: update `.agent/workflows/agentic-kb.md` to remove future-tense or plan-era MCP/setup language such as `Expected tools`, `The implementation should ship`, and `Required environment variables will likely include ...` when those statements conflict with the already shipped task-801/task-802 docs. Replace them with current-state wording that matches `agentic/README.md`, including the fact that `GITHUB_TOKEN` is optional for read-only MCP search/status and still required for `sync github` and `sync project`.
- **Preserve minimal scope**: only touch additional docs or tracking files if `.agent/workflows/agentic-kb.md` cannot be made accurate without them.

## Acceptance Criteria

- `.agent/workflows/agentic-kb.md` accurately reflects the shipped `agentic-kb` CLI and `docker-compose.agentic.yml` behavior for boot, status, search, sync, snapshot, and MCP usage.
- The workflow no longer contains aspirational or outdated wording that conflicts with the actual task-503, task-602, task-701, task-801, or task-802 outcomes.
- The workflow documents `sync changed` as an incremental command that requires existing successful baselines and does not imply first-sync fallback on empty or partially seeded KBs.
- The workflow explicitly documents the current shipped Compose limitation for `sync changed`: the command depends on `git`, but the current `kb-tools` image does not install `git`, so the containerized path needs either a documented caveat or a separately tracked follow-up and must not be presented as fully working without that caveat.
- The workflow explicitly keeps the current GitHub and Project limitations visible: shared-watermark GitHub incrementals with client-filtered `pulls` and `review_comments`, and Project cursor continuation that can miss edits to already-seen items.
- The workflow explains that `snapshot import` is destructive, requires `--yes`, validates the manifest/dump pair before restore, and should target a disposable KB database.
- The workflow describes the MCP server as stdio-only and does not imply `docker compose ... up -d mcp-search` is the user-facing MCP client setup path.
- The workflow points readers at `agentic/README.md` for concrete client config examples instead of restating a conflicting setup contract.
- The workflow removes stale future-tense MCP/setup wording and replaces it with shipped-current statements about the actual tool surface and env contract.
- Any tracker updates stay minimal and limited to `task-803` metadata.

## Verification Plan

- Re-read the final workflow doc against `agentic/src/agentic_kb/cli.py` to confirm the documented command list and top-level CLI shape are exact.
- Re-read the workflow's status/search sections against `agentic/src/agentic_kb/commands/status.py` and `agentic/src/agentic_kb/commands/search.py`, including `--healthcheck`, `--json`, `--entity-type`, `--filter`, and operator-facing scriptability notes.
- Re-read the workflow's sync section against `agentic/src/agentic_kb/commands/sync.py` and `.agent/plans/agentic/research/task-701-sync-commands.md`, especially the `sync changed` baseline contract, `sync all` ordering/failure behavior, and GitHub/Project caveats.
- Re-read the documented Compose/container workflow against `agentic/Dockerfile` and `docker-compose.agentic.yml` to verify image prerequisites for the user-facing commands. In particular, confirm the workflow accurately describes that `sync changed` relies on `git` in `commands/sync.py` while the current `kb-tools` image does not install `git`.
- Re-read the workflow's snapshot section against `agentic/src/agentic_kb/commands/snapshot.py` and `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`, especially sibling manifest resolution, validation-before-restore behavior, and destructive import acknowledgement.
- Re-read the workflow's MCP wording against `agentic/README.md`, `docker-compose.agentic.yml`, and `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md` plus `.agent/plans/agentic/research/task-802-client-setup-docs.md` so the stdio-only launcher contract stays consistent across docs.
- If feasible during implementation, run a lightweight documented Compose-path sanity check such as `docker compose -f docker-compose.agentic.yml run --rm kb-tools --help` and compare the result plus the image package contract against any workflow claims about in-container command usability; if a full `sync changed` smoke is not feasible, the workflow must still preserve the verified missing-`git` caveat.
- If the workflow references `status` output details or migration/index expectations, verify those lines against the current implementation in `agentic/src/agentic_kb/commands/status.py` instead of older plan text.

## Risks / Open Questions

- **Workflow duplication risk**: `.agent/workflows/agentic-kb.md` and `agentic/README.md` now overlap around MCP setup. The workflow should summarize the launcher contract and link to the README for full client snippets so the two docs do not drift again.
- **Freshness wording boundary**: the workflow currently contains future-facing freshness language, but task-702 stale-index detection is still pending. The update needs to keep only currently accurate caveats and avoid implying shipped stale-detection features.
- **Status detail drift**: `status` now reports specific migration versions, searchable tables, indexes, and sync-state summaries. The workflow should describe those at the right level without overfitting to implementation details that may change in a later migration.
- **Missing `git` in shipped image**: the current documented Compose path for `sync changed` is runtime-incomplete because `agentic/src/agentic_kb/commands/sync.py` shells out to `git` while `agentic/Dockerfile` does not install it. Task-803 should document this limitation clearly and may need to leave a follow-up breadcrumb for runtime ownership rather than trying to fix the image here.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the source of truth for the intended task-803 workflow reconciliation scope until implementation is complete.
- Planning review history lives in `.agent/plans/agentic/task-plans/task-803-plan-review.md`.
- Implementation review history should be recorded in `.agent/plans/agentic/task-plans/task-803-impl-review.md` once the task is built and reviewed.
- `.agent/workflows/agentic-kb.md` is the expected implementation target.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` should only be updated for `task-803` status metadata when the implementation actually lands.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-803-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-803-impl-review.md`

## Final Outcome

- Outcome: completed and approved.
- `.agent/workflows/agentic-kb.md` now documents the shipped `agentic-kb` workflow in current-state language instead of plan-era future tense.
- The workflow now aligns with `agentic/README.md`, `docker-compose.agentic.yml`, and the shipped CLI/command modules for `status`, `search`, `entity get`, `sync`, `snapshot`, and `mcp-search`.
- The final workflow explicitly documents the current shipped `sync changed` container limitation: `commands/sync.py` requires `git` for docs/code delta calculation, while the current `kb-tools` image built from `agentic/Dockerfile` does not install `git`.
- Verification completed by source-to-doc consistency review plus Compose/image sanity checks recorded in `.agent/plans/agentic/task-plans/task-803-impl-review.md`.
- Final review result: planning approved in `task-803-plan-review.md`; implementation approved in `task-803-impl-review.md`.
