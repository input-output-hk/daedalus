---
description: Run the agentic knowledge base, sync it, and connect agents through MCP
---

# Agentic Knowledge Base Workflow

This workflow guides booting the Daedalus agentic knowledge base, syncing local and GitHub knowledge into it, exporting/importing snapshots for team sharing, and connecting local agents through the read-only Search MCP.

## Status

This workflow documents the current Daedalus agentic platform shape described in `.agent/plans/agentic/knowledge-base-platform.md`.

Current implementation note: `docker-compose.agentic.yml` boots the infrastructure scaffold for `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`. `kb-tools` now ships as a packaged `agentic-kb` CLI with implemented `status`, `status --json`, local `search`, generic `entity get`, `service`, `snapshot export`, destructive `snapshot import`, and the full packaged `sync` command family (`sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, and `sync all`). `mcp-search` remains a placeholder service until its follow-up task lands. For schema bootstrap, `agentic/schema/init.sql` remains the single first-boot entrypoint and delegates the task-203 search-index phase to `agentic/schema/create_indexes.sql`; existing initialized DB volumes still require a manual `psql -f agentic/schema/create_indexes.sql` apply because Docker init scripts do not retrofit existing volumes.

## Goals

- Start the whole platform with Docker Compose
- Search docs, code, issues, PRs, and GitHub Project context from one place
- Share knowledge base snapshots between multiple human developers
- Keep the knowledge base fresh as the repo and project change
- Expose read-only search tools to OpenCode, Claude Code, and similar agent tools

## Core Components

The planned stack consists of:

- `paradedb` — PostgreSQL 18 + `pg_search` + `pgvector`
- `ollama` — local embedding service
- `kb-tools` — ingest, sync, snapshot, and search commands
- `mcp-search` — read-only Search MCP server
- GitHub Issues + Project 5 — coordination layer via `gh` CLI

## Commands

The current CLI surface for these task-owned commands is:

```bash
# Start the stack
docker compose -f docker-compose.agentic.yml up -d

# Check status
docker compose -f docker-compose.agentic.yml ps
docker compose -f docker-compose.agentic.yml run --rm kb-tools status

# Sync the whole knowledge base
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all

# Sync only changes since last update
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed

# Search locally
docker compose -f docker-compose.agentic.yml run --rm kb-tools search "mithril bootstrap"

# Export a portable snapshot for another developer
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot export

# Restore a shared snapshot into a disposable KB database
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/latest.dump --yes
```

Today:

- `status` reports runtime config, mount visibility, dependency reachability, and live KB database inspection when `DATABASE_URL` is usable.
- `status --healthcheck` stays lightweight and exit-code-oriented for Compose healthchecks; it does not require schema inspection.
- `status --json` emits one machine-readable JSON object on stdout for scripts.
- `search` runs real BM25, vector, or hybrid KB queries with optional `--entity-type`, repeated `--filter key=value`, and `--json`.
- `entity get <entity_type> <id>` fetches one indexed row by stable id, returns exit code `2` for invalid entity types, and returns exit code `4` for not-found rows.
- `snapshot export` creates a real custom-format `pg_dump` of the `agentic` schema, writes to `/workspace/agentic/snapshots` by default, and emits a schema-valid sibling `.manifest.json` sidecar.
- `snapshot import` accepts either the dump path or the sibling manifest path, validates the manifest plus dump size/hash before restore, and still requires `--yes` acknowledgement.
- `sync docs` refreshes the allowlisted docs corpus from the current checkout and removes stale indexed rows for allowlisted docs that are no longer discovered.
- `sync code` refreshes the supported code corpus from the current checkout and prunes stale indexed rows for supported files that are now deleted or excluded.
- `sync github` runs the existing four-stream GitHub ingest path for `issues`, `pulls`, `issue_comments`, and `review_comments`. Incremental runs reuse the earliest stored GitHub watermark as one shared lower bound; upstream `since` still applies only to `issues` and `issue_comments`, while `pulls` and `review_comments` remain client-filtered after fetch.
- `sync project` seeds from `after_cursor=None` on its first explicit run and later continues only from the stored Project cursor. Repeated runs at the current end cursor succeed as no-op continuation and still do not guarantee replay of edits to already-seen items.
- `sync changed` is now the general incremental command for an already-seeded KB. It still supports the post-import fast-start workflow, but it now requires successful existing baselines for docs, code, all four GitHub stream rows, and Project cursor state before it does any work.
- `sync all` runs only `sync docs`, `sync code`, `sync github`, and `sync project` in that fixed order and stops on the first failure.

## Status Behavior

Use `status` for a quick operator view of both service readiness and KB DB readiness:

- runtime config summary for `DATABASE_URL`, `OLLAMA_BASE_URL`, `OLLAMA_EMBED_MODEL`, and optional `GITHUB_TOKEN`
- mount visibility for `/workspace` and `/workspace/agentic/snapshots`
- dependency reachability for ParadeDB TCP plus Ollama API/model checks
- KB DB inspection for migration versions `1`, `2`, and `3`
- presence of the current `agentic` tables, the seven searchable tables, and their BM25/HNSW indexes
- searchable row counts and grouped `kb_sync_state` summaries

`status` is intentionally limited to current repo-backed contracts; it does not require sync orchestration, GitHub API calls, or MCP readiness.

## Team Sharing Workflow

The platform is designed for multiple human developers, not just one local machine.

### Recommended Team Pattern

1. One developer or CI publishes a fresh baseline snapshot from `develop`
2. Another developer imports that snapshot into a fresh or otherwise disposable KB database
3. The developer runs `sync changed` to add branch-local docs/code changes and any new GitHub updates
4. The team repeats this instead of rebuilding the whole knowledge base from scratch every time

Incremental caveats for `sync changed`:

- it is intentionally not a first-sync substitute on an empty or partially seeded KB; run `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all` first when baselines are missing
- docs and code deltas are computed from each source's stored baseline commit to current `HEAD`, so rebases can include broader catch-up than purely branch-local edits
- GitHub incremental runs use one shared lower bound across all four streams, but upstream `since` still applies only to `issues` and `issue_comments`; `pulls` and `review_comments` remain ordered-fetch streams with client-side filtering
- Project refresh is cursor continuation only from stored `after_cursor`; it does not detect updates to already-seen items yet

### Snapshot Publication

The default publication channel for shared baseline snapshots should be GitHub Actions artifacts.

- CI publishes the latest baseline snapshot from `develop`
- developers download that artifact, import it into a disposable KB database with `snapshot import ... --yes`, and then run `sync changed`
- GitHub Releases assets are out of scope for KB snapshot sharing

### Why Snapshot Sharing Matters

- Full ingestion can be expensive
- GitHub history grows over time
- Shared snapshots keep teams aligned on a common baseline
- Developers can still index local branch deltas after import

### Snapshot Safety Boundary

- `snapshot export` is schema-scoped to `agentic`; it is not a full-cluster backup.
- `snapshot import` drops and recreates the `agentic` schema before restore.
- Only run import against fresh, isolated, or otherwise disposable KB databases.
- The canonical manifest contract lives at `agentic/config/snapshot-manifest.schema.json`; export now writes that manifest beside the dump, and import validates the manifest contract plus dump identity before any destructive restore.

## Freshness Rules

The knowledge base should not quietly drift stale.

It should detect:

- local docs/code indexed from an older commit than current `HEAD`
- GitHub issues/PRs behind their latest `updatedAt`
- project items behind their latest project field updates

Agents should prefer the knowledge base when it is current and fall back to direct repo reads when the KB reports staleness.

## MCP Setup

The first version should only expose read-only search tools.

Expected tools:

- `search`
- `search_docs`
- `search_code`
- `search_github`
- `get_entity`
- `find_related`
- `kb_status`

### OpenCode / Claude Code Setup

The implementation should ship copy-pasteable examples for:

- local `.mcp.json`
- OpenCode service configuration
- Claude Code MCP configuration

Required environment variables will likely include:

- `DATABASE_URL`
- `OLLAMA_BASE_URL`
- `GITHUB_TOKEN`

For project ingestion, `GITHUB_TOKEN` must be able to read organization ProjectV2 data for `DripDropz` Project 5, not just repository issues and pull requests.

## GitHub Coordination

Use GitHub Issues and Project 5 for planning and execution tracking.

`gh` CLI is the supported coordination interface. A GitHub MCP server is explicitly out of scope for this platform.

### Notes

- Project operations may require `gh auth refresh -s read:project,project`
- Search MCP remains read-only even though coordination happens in GitHub

## Related Files

- Plan: `.agent/plans/agentic/knowledge-base-platform.md`
- Tasks: `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- Agent index: `.agent/readme.md`
- Root agent instructions: `AGENTS.md`
- Claude Code entry: `CLAUDE.md`
