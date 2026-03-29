---
description: Run the agentic knowledge base, sync it, and connect agents through MCP
---

# Agentic Knowledge Base Workflow

This workflow guides booting the Daedalus agentic knowledge base, syncing local and GitHub knowledge into it, exporting/importing snapshots for team sharing, and connecting local agents through the read-only Search MCP.

## Status

This workflow documents the current shipped Daedalus agentic KB behavior.

`docker-compose.agentic.yml` boots `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`. `kb-tools` ships the packaged `agentic-kb` CLI with `status`, `search`, `entity get`, `sync <subcommand>`, `snapshot <subcommand>`, `service`, and the stdio MCP entrypoint `mcp-search`. The Compose `mcp-search` service is a parity/smoke harness for that same stdio process and is not a separate daemon or network endpoint.

For schema bootstrap, `agentic/schema/init.sql` remains the first-boot entrypoint and delegates index creation to `agentic/schema/create_indexes.sql`. Existing initialized DB volumes still require a manual `psql -f agentic/schema/create_indexes.sql` apply because Docker init scripts do not retrofit existing volumes.

## Goals

- Start the whole platform with Docker Compose
- Search docs, code, issues, PRs, and GitHub Project context from one place
- Share knowledge base snapshots between multiple human developers
- Keep the knowledge base fresh as the repo and project change
- Expose read-only search tools to OpenCode, Claude Code, and similar agent tools

## Core Components

The shipped stack consists of:

- `paradedb` — PostgreSQL 18 + `pg_search` + `pgvector`
- `ollama` — local embedding service
- `kb-tools` — ingest, sync, snapshot, and search commands
- `mcp-search` — read-only Search MCP server
- GitHub Issues + Project 5 — coordination layer via `gh` CLI

## Commands

The current operator-facing Compose path is:

```bash
# Start the stack
docker compose -f docker-compose.agentic.yml up -d

# Check status
docker compose -f docker-compose.agentic.yml ps
docker compose -f docker-compose.agentic.yml run --rm kb-tools status

# Machine-readable readiness proof
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json

# Sync the whole knowledge base
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all

# Search locally
docker compose -f docker-compose.agentic.yml run --rm kb-tools search "mithril bootstrap"

# Export a portable snapshot for another developer
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot export

# Restore a snapshot into a disposable KB database
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/latest.dump --yes

# Prove imported data is queryable without waiting on hybrid/vector readiness
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

`agentic/.env.example` now documents the optional Compose overrides. The supported clean-bootstrap path works with the defaults in `docker-compose.agentic.yml`; copying an env file is optional unless you need custom ports, credentials, model selection, or a `GITHUB_TOKEN` for GitHub-backed sync commands.

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

`sync changed` caveats:

- `agentic/src/agentic_kb/commands/sync.py` shells out to `git` for docs/code delta calculation, and the shipped `kb-tools` image now includes `git` plus a per-command safe-directory override for the bind-mounted `/workspace` checkout
- `sync changed` still is not part of the clean-machine bootstrap acceptance path for this workflow; use it only after the KB already has successful docs, code, GitHub, and Project baselines
- even with the container-path git fix, the broader import-then-`sync changed` team fast-start story remains distinct from the narrower task-901 clean-bootstrap contract and from the separate manual shared-baseline publication workflow

## Status Behavior

Use `status` for a quick operator view of both service readiness and KB DB readiness:

- runtime config summary for `DATABASE_URL`, `OLLAMA_BASE_URL`, `OLLAMA_EMBED_MODEL`, and optional `GITHUB_TOKEN`
- mount visibility for `/workspace` and `/workspace/agentic/snapshots`
- dependency reachability for ParadeDB TCP plus Ollama API/model checks
- KB DB inspection for migration versions `1`, `2`, and `3`
- presence of the current `agentic` tables, the seven searchable tables, and their BM25/HNSW indexes
- searchable row counts and grouped `kb_sync_state` summaries

`status` is intentionally limited to current repo-backed contracts; it does not require sync orchestration, GitHub API calls, or MCP readiness.

## Clean-Machine Bootstrap

The currently validated narrow bootstrap contract is:

1. Start a fresh isolated stack with `docker compose -f docker-compose.agentic.yml up -d`
2. Import a valid snapshot with `docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import <dump-or-manifest> --yes`
3. Prove readiness with `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json`
4. Prove imported data is searchable with `docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"`

This is the current clean-machine acceptance path. It intentionally does not require `sync changed`.

## Team Sharing Workflow

The platform is designed for multiple human developers, not just one local machine.

### Recommended Team Pattern

1. Publish the canonical shared baseline manually from `develop` with `.github/workflows/agentic-kb-sync.yml`
2. Download the successful workflow artifact `agentic-kb-develop-baseline-<github-sha>`
3. Extract the `.dump` plus sibling `.manifest.json` pair into `agentic/snapshots/`
4. Import the snapshot into a fresh or otherwise disposable KB database
5. Validate the imported KB with `status --json` plus the deterministic BM25 documents-only proof
6. Treat `sync changed` as a follow-on incremental command, not part of the clean-machine bootstrap success path

Incremental caveats for `sync changed`:

- it is intentionally not a first-sync substitute on an empty or partially seeded KB; run `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all` first when baselines are missing
- docs and code deltas are computed from each source's stored baseline commit to current `HEAD`, so rebases can include broader catch-up than purely branch-local edits
- GitHub incremental runs use one shared lower bound across all four streams, but upstream `since` still applies only to `issues` and `issue_comments`; `pulls` and `review_comments` remain ordered-fetch streams with client-side filtering
- Project refresh is cursor continuation only from stored `after_cursor`; it does not detect updates to already-seen items yet

### Snapshot Publication

The canonical shared baseline is published manually with GitHub Actions workflow `.github/workflows/agentic-kb-sync.yml`.

- trigger shape for task-603 is `workflow_dispatch` only
- canonical publication is restricted to `refs/heads/develop`; manual dispatch from any other ref fails clearly and does not publish a canonical branch snapshot
- the workflow bootstraps the stack with `docker-compose.agentic.yml`, runs `sync all`, exports a fresh snapshot pair, verifies `status --json`, runs `search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"`, and uploads the artifact only after those checks pass
- the required artifact name is `agentic-kb-develop-baseline-<github-sha>`
- the artifact payload is exactly the exported `.dump` plus sibling `.manifest.json` pair; no extra repo-specific archive layer is added
- the workflow requires explicit secret `AGENTIC_KB_SYNC_GITHUB_TOKEN` because `sync github` and `sync project` need token-backed GitHub reads and ProjectV2 access cannot be assumed from the default Actions token
- GitHub Releases assets are out of scope for KB snapshot sharing

Consumption path after download:

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

Expected validation shape after import:

- `status --json` reports `ok = true`
- search JSON reports `mode = "bm25"`
- search returns at least one hit
- the first hit has `entity_type = "documents"`
- the first hit has `fields.source_path = ".agent/workflows/agentic-kb.md"`

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

## Freshness Guidance

Current shipped freshness handling is operational rather than automatic:

- use `status` to inspect current sync-state summaries and indexed row counts
- use `sync changed` only after successful source baselines already exist
- use `sync all` or explicit source syncs when the KB needs reseeding

Automatic stale-index detection is not part of the current shipped workflow.

## MCP Setup

`agentic-kb mcp-search` is the shipped read-only Search MCP server over stdio.

Implemented tool surface:

- `search`
- `search_docs`
- `search_code`
- `search_github`
- `get_entity`
- `find_related`
- `kb_status`

### OpenCode / Claude Code Setup

Use this repo-supported launcher contract for MCP clients:

```bash
docker compose -f docker-compose.agentic.yml run --rm -T mcp-search
```

Do not use `docker compose -f docker-compose.agentic.yml up -d mcp-search` as a client setup path. That service is only a parity/smoke harness for the same stdio process.

Copy-pasteable OpenCode, Claude Code, and local `.mcp.json` examples live in `agentic/README.md`.

Environment notes:

- `DATABASE_URL` and `OLLAMA_BASE_URL` are required for direct local `agentic-kb mcp-search` launches; the Compose launcher wires them automatically
- `OLLAMA_EMBED_MODEL` is an optional override and should stay aligned with the embedding model used for the indexed KB
- `GITHUB_TOKEN` is optional for read-only MCP search and `status`
- `GITHUB_TOKEN` is still required for `sync github` and `sync project`

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
