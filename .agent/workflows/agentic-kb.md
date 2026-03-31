---
description: Run the agentic knowledge base, sync it, and connect agents through MCP
---

# Agentic Knowledge Base Workflow

This workflow guides booting the Daedalus agentic knowledge base, syncing local and GitHub knowledge into it, exporting/importing snapshots for team sharing, and connecting local agents through the read-only Search MCP.

## Status

This workflow documents the current shipped Daedalus agentic KB behavior.

`docker-compose.agentic.yml` boots `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`. `kb-tools` ships the packaged `agentic-kb` CLI with `status`, `search`, `entity get`, `sync <subcommand>`, `snapshot <subcommand>`, `service`, and the stdio MCP entrypoint `mcp-search`. The Compose `mcp-search` service is a parity/smoke harness for that same stdio process and is not a separate daemon or network endpoint.

For schema bootstrap, `agentic/schema/init.sql` remains the first-boot entrypoint and delegates index creation to `agentic/schema/create_indexes.sql`. Existing initialized DB volumes are treated as disposable in v1: when schema contracts change, recreate the KB volume and import a compatible snapshot or rebuild locally instead of attempting manual retrofit.

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

# Publish the current local baseline into a local Dropbox-synced Daedalus_KB folder
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish

# Fetch one explicit shared snapshot pair back into agentic/snapshots/
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- agentic-kb-<timestamp>

# Restore a snapshot into a disposable KB database
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes

# Prove imported data is queryable without waiting on hybrid/vector readiness
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

### Convenience Scripts

For common operations, the package.json convenience scripts source `agentic/.env` automatically:

```bash
# Start/stop the stack
yarn agentic:kb:up          # docker compose up -d
yarn agentic:kb:down        # docker compose down
yarn agentic:kb:down:clean  # docker compose down -v

# Sync operations
yarn agentic:kb:sync:all     # sync docs + code + github + project
yarn agentic:kb:sync:changed  # incremental sync after import/baseline

# Snapshot sharing
yarn agentic:kb:publish                  # sync all + export + copy to Dropbox
yarn agentic:kb:fetch -- <snapshot>     # copy from Dropbox to snapshots/
yarn agentic:kb:import                  # import snapshot into fresh KB
```

`agentic/.env.example` now documents the optional Compose overrides. The supported clean-bootstrap path works with the defaults in `docker-compose.agentic.yml`; copying an env file is optional unless you need custom ports, credentials, model selection, or a `GITHUB_TOKEN` for GitHub-backed sync commands.

The shipped publish/fetch helpers stay intentionally narrow:

- they require `AGENTIC_KB_SHARED_DIR` to point to a locally accessible Dropbox-synced folder named `Daedalus_KB`
- `yarn agentic:kb:publish` wraps `sync all` plus `snapshot export`, then copies one `.dump` and sibling `.manifest.json` pair into that local folder
- `yarn agentic:kb:fetch -- <snapshot-basename>` copies one explicit sibling pair back into `agentic/snapshots/`
- they do not call Dropbox APIs, bootstrap Dropbox accounts, or maintain any repo-owned latest-artifact registry

Today:

- `status` reports runtime config, mount visibility, dependency reachability, and live KB database inspection when `DATABASE_URL` is usable.
- `status --healthcheck` stays lightweight and exit-code-oriented for Compose healthchecks; it does not require schema inspection.
- `status --json` emits one machine-readable JSON object on stdout for scripts, including a separate `embedding_compatibility` object so snapshot compatibility does not change top-level readiness semantics.
- `search` runs real BM25, vector, or hybrid KB queries with optional `--entity-type`, repeated `--filter key=value`, and `--json`.
- `entity get <entity_type> <id>` fetches one indexed row by stable id, returns exit code `2` for invalid entity types, and returns exit code `4` for not-found rows.
- `snapshot export` creates a real custom-format `pg_dump` of the `agentic` schema, writes to `/workspace/agentic/snapshots` by default, and emits a schema-valid sibling `.manifest.json` sidecar.
- `snapshot import` accepts either the dump path or the sibling manifest path, validates the manifest plus dump size/hash, rejects legacy or incompatible embedding contracts before restore, enforces the disposable-target boundary, and still requires `--yes` acknowledgement.
- `sync docs` refreshes the allowlisted docs corpus from the current checkout, removes stale indexed rows for allowlisted docs that are no longer discovered, skips unchanged docs before embedding by comparing deterministic stored chunk hashes, and reports actual work with `updated_paths`, `skipped_paths`, and `deleted_paths` while `processed_count` remains the stored markdown chunk rows rewritten in that run.
- `sync code` refreshes the supported code corpus from the current checkout and prunes stale indexed rows for supported files that are now deleted or excluded.
- `sync github` runs the existing four-stream GitHub ingest path for `issues`, `pulls`, `issue_comments`, and `review_comments`. Incremental runs reuse the earliest stored GitHub watermark as one shared lower bound; upstream `since` still applies only to `issues` and `issue_comments`, while `pulls` and `review_comments` remain client-filtered after fetch.
- `sync project` seeds from `after_cursor=None` on its first explicit run and later continues only from the stored Project cursor. Repeated runs at the current end cursor succeed as no-op continuation and still do not guarantee replay of edits to already-seen items.
- `sync project --full` forces a full re-ingest from `after_cursor=None` even when a stored Project cursor already exists, enabling manual convergence for edits to already-seen Project items.
- `sync changed` is now the general incremental command for an already-seeded KB. It still supports the post-import fast-start workflow, but it now requires successful existing baselines for docs, code, all four GitHub stream rows, and Project cursor state before it does any work.
- `sync changed` also refuses to extend an imported baseline when the latest imported snapshot manifest is legacy, malformed, or incompatible with the local runtime embedding contract. KBs with no imported snapshot history still follow the local-only baseline path.
- `sync all` runs only `sync docs`, `sync code`, `sync github`, and `sync project` in that fixed order and stops on the first failure.

`sync changed` caveats:

- `agentic/src/agentic_kb/commands/sync.py` shells out to `git` for docs/code delta calculation, and the shipped `kb-tools` image now includes `git` plus a per-command safe-directory override for the bind-mounted `/workspace` checkout
- `sync changed` still is not part of the clean-machine bootstrap acceptance path for this workflow; use it only after the KB already has successful docs, code, GitHub, and Project baselines
- even with the container-path git fix, the broader import-then-`sync changed` team fast-start story remains distinct from the narrower task-901 clean-bootstrap contract and from the separate manual shared-baseline publication workflow

## Canonical Embedding Contract

Shared baselines use one canonical embedding contract at a time. In v1, that is not a per-developer preference.

- the canonical contract is the exact enforced `embedding_contract` tuple recorded in snapshot manifests: `contract_id`, `embedding_model`, and `embedding_dimension`
- this workflow is the current operator-facing policy source of truth for the v1 canonical-contract shape and republish rule; the repo does not expose a separate dedicated canonical tuple registry
- before importing or extending a specific shared baseline, operators should inspect that artifact's published `.manifest.json` to see the exact tuple it was built with, then confirm local post-import compatibility through `status --json` and its `embedding_compatibility` object
- the checked-in schema and example manifest document the contract shape and an example v1 tuple, but they do not by themselves announce that every published shared baseline currently uses that tuple
- local `OLLAMA_EMBED_MODEL` overrides are acceptable only for disposable local rebuild experiments; they are out of contract for publishing or extending the shared team baseline and must not be treated as a canonical-contract registry
- any intentional change to the canonical contract on `develop` requires a fresh canonical snapshot rebuild and republish from `develop` on the trusted baseline-publisher machine before further team handoff resumes
- snapshots built before an intentional canonical-contract change are no longer valid shared baselines for import-then-`sync changed` continuation under the new contract
- ordinary content refreshes or baseline rebuilds that keep the same canonical tuple do not redefine the contract or require a policy change announcement
- mismatch handling is operator-visible by design: `snapshot import` fails before restore for legacy or incompatible manifests, `status` keeps top-level readiness separate from `embedding_compatibility`, and `sync changed` refuses to extend an imported baseline whose latest imported manifest is legacy, malformed, or incompatible

## Status Behavior

Use `status` for a quick operator view of both service readiness and KB DB readiness:

- runtime config summary for `DATABASE_URL`, `OLLAMA_BASE_URL`, `OLLAMA_EMBED_MODEL`, and optional `GITHUB_TOKEN`
- mount visibility for `/workspace` and `/workspace/agentic/snapshots`
- dependency reachability for ParadeDB TCP plus Ollama API/model checks
- KB DB inspection for migration versions `1`, `2`, and `3`
- presence of the current `agentic` tables, the seven searchable tables, and their BM25/HNSW indexes
- searchable row counts and grouped `kb_sync_state` summaries

`status` is intentionally limited to current repo-backed contracts and does not require sync orchestration or MCP readiness. GitHub and Project freshness probes run only on normal `status` when `GITHUB_TOKEN` is configured; without a token, those remote freshness entries report `skipped` and status remains usable. When imported snapshot metadata exists, `status` also reports the latest imported snapshot's embedding-contract compatibility separately from readiness. Compatible imports report `compatible`; legacy imported manifests report `unsupported_legacy_manifest`; malformed persisted metadata reports `unavailable` with detail instead of being silently normalized.

For shared-baseline workflows, treat `status --json` as the post-import confirmation step, not as a canonical tuple discovery mechanism. It tells you whether the latest imported snapshot metadata is compatible with the local runtime contract, but the artifact's published manifest remains the source for the exact tuple a specific snapshot was built with.

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

1. Build the canonical shared baseline locally from `develop` on a trusted GPU-capable developer machine
2. Run `AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish` to refresh the local baseline, export one `.dump` plus sibling `.manifest.json` pair into `agentic/snapshots/`, and copy that pair into the local Dropbox-synced shared folder
3. On another machine, run `AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>` to copy that explicit pair back into `agentic/snapshots/`
5. Import the snapshot into a fresh or otherwise disposable KB database
6. Validate the imported KB with `status --json` plus the deterministic BM25 documents-only proof
7. Treat `sync changed` as a follow-on incremental command, not part of the clean-machine bootstrap success path

Incremental caveats for `sync changed`:

- it is intentionally not a first-sync substitute on an empty or partially seeded KB; run `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all` first when baselines are missing
- docs and code deltas are computed from each source's stored baseline commit to current `HEAD`, so rebases can include broader catch-up than purely branch-local edits
- GitHub incremental runs use one shared lower bound across all four streams, but upstream `since` still applies only to `issues` and `issue_comments`; `pulls` and `review_comments` remain ordered-fetch streams with client-side filtering
- Project refresh is cursor continuation only from stored `after_cursor`; it does not detect updates to already-seen items yet
- eventual freshness is acceptable for Project 5 items in v1, but the final workflow must also document a manual full Project refresh path for re-convergence

### Snapshot Publication

The canonical shared baseline is published locally, not through GitHub Actions.

- publication runs from `develop` on a trusted GPU-capable developer machine
- publication builds a fresh baseline with `sync all`, exports a snapshot pair, validates it locally, and uploads it to the chosen private shared storage backend
- the portable payload remains exactly the exported `.dump` plus sibling `.manifest.json` pair; no extra repo-specific archive layer is required
- if `develop` intentionally changes the canonical embedding-contract tuple (`contract_id`, `embedding_model`, or `embedding_dimension`), publish a newly rebuilt canonical snapshot before any further team handoff; older snapshots remain valid provenance but are no longer the canonical import-then-sync baseline under the new contract
- if `develop` only changes content while keeping the same embedding-contract tuple, publication is an ordinary baseline refresh rather than a contract-policy change
- GitHub Actions artifacts and GitHub Releases assets are both out of scope for KB snapshot sharing in v1
- the selected v1 backend is Dropbox shared-folder storage
- the canonical shared location is the Dropbox shared folder `Daedalus_KB`
- Developer 1 creates `Daedalus_KB` in an existing Dropbox account and shares that folder with Developer 2 with write access; the team contract for this backend is one shared folder that is writable by both developers
- Developer 2 bootstrap is documented only to the current repo-truth minimum: Developer 2 must have a Dropbox access path that can accept the shared folder and verify write access; the repo now ships only local-path helper wrappers, not a deeper account-bootstrap SOP
- the helper path contract is local-file based: set `AGENTIC_KB_SHARED_DIR` to the local Dropbox-synced `Daedalus_KB` path and keep Dropbox desktop sync or equivalent local access working outside the repo
- artifact discovery remains intentionally explicit rather than automatic: `yarn agentic:kb:fetch` requires a snapshot basename or sibling filename and does not claim zero-argument latest selection or a richer canonical registry
- naming and location expectations stay minimal: upload the exported pair directly into `Daedalus_KB`, keep the `.dump` and sibling `.manifest.json` together, and do not rename only one file from the pair
- `yarn agentic:kb:publish` enforces that same sibling-pair contract locally by failing if export does not leave both files together before copy, and `yarn agentic:kb:fetch` fails if either side of the selected pair is missing in `Daedalus_KB`
- retention remains manual in v1: keep the current canonical pair in `Daedalus_KB` until a newer compatible pair has been uploaded, and keep the previous known-good pair available as a short-term fallback when possible
- post-download integrity stays on the shipped import contract: download both sibling files together and rely on `snapshot import` to validate manifest schema plus dump size/hash before any restore; if validation fails, discard the download and fetch both files again
- outage or latest-artifact-unavailable recovery is to use a last known-good compatible local pair or rebuild locally from `develop` on a trusted GPU-capable machine; do not switch to GitHub Actions artifacts, GitHub Releases, or another ad hoc publication channel

Consumption path after helper fetch from the Dropbox shared folder `Daedalus_KB`:

```bash
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

Expected validation shape after import:

- `status --json` reports `ok = true`
- `status --json` reports `embedding_compatibility.state = "compatible"`
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
- The enforced v1 import contract allows two target states only: a fresh database with no `agentic` schema yet, or an initialized KB where all state-bearing `agentic` tables are empty.
- Import fails before schema drop if searchable KB tables, `kb_sync_state`, or `kb_snapshot_manifest` already contain rows.
- The canonical manifest contract lives at `agentic/config/snapshot-manifest.schema.json`; export now writes that manifest beside the dump, and import validates the manifest contract plus dump identity before any destructive restore.
- Imported legacy `embedding_model`-only manifests and imported mismatched embedding contracts are hard failures before restore; the supported recovery path is to recreate the disposable KB and import a compatible snapshot or rebuild locally.

## Freshness Guidance

Current shipped freshness handling is status-driven rather than automatic sync:

- normal `status` and `status --json` now report a separate `freshness` section alongside readiness
- top-level `status.ok` still means runtime/dependency/database readiness only; stale or skipped freshness does not turn imported-baseline readiness into failure
- docs and code are `fresh` only when their last successful stored `repo_commit_hash` matches local `HEAD`; if the baseline is missing they report `missing_baseline`
- GitHub stream freshness compares the stored per-stream watermarks for `issues`, `pulls`, `issue_comments`, and `review_comments` against lightweight latest-watermark API probes when `GITHUB_TOKEN` is available
- Project freshness compares the stored project watermark against the latest observed Project item update when `GITHUB_TOKEN` is available
- if `GITHUB_TOKEN` is absent, GitHub and Project freshness report `skipped` instead of pretending those sources are fresh
- `status --healthcheck` remains lightweight and does not run git or GitHub freshness checks
- `sync changed` remains the follow-on command to refresh stale baselines after a seeded or imported KB, but Project freshness still cannot guarantee replay of edits to already-seen items under the current cursor-only sync contract
- eventual freshness for Project 5 items is acceptable in v1, and the remaining rollout work includes documenting a manual full Project refresh path for re-convergence

Bootstrap and broader team rollout remain separate acceptance contracts:

- clean-machine bootstrap succeeds when the documented import, `status --json`, and deterministic BM25 proof succeed on a fresh stack
- team-ready rollout additionally requires the private shared-storage handoff, canonical embedding-contract guidance, Project token guidance, and the manual full Project refresh path

## MCP Setup

`agentic-kb mcp-search-http` is the shipped read-only Search MCP server over HTTP/SSE.

Implemented tool surface:

- `search`
- `search_docs`
- `search_code`
- `search_github`
- `get_entity`
- `find_related`
- `kb_status`

### Starting the MCP Server

Start the MCP server as a long-running daemon:

```bash
docker compose -f docker-compose.agentic.yml up -d mcp-search-http
```

The server listens on `127.0.0.1:8765` by default.

### OpenCode / Claude Code Setup

The repo's `opencode.json` is pre-configured for OpenCode. Claude Code examples live in `agentic/README.md`.

Environment notes:

- `DATABASE_URL` and `OLLAMA_BASE_URL` are required; the Compose launcher wires them automatically
- `OLLAMA_EMBED_MODEL` is an optional override and should stay aligned with the embedding model used for the indexed KB
- for shared baseline publication and post-import sync, `OLLAMA_EMBED_MODEL` must match the current canonical embedding contract; treat ad hoc overrides as disposable local-only experiments
- `GITHUB_TOKEN` is optional for read-only MCP search and `status`
- `GITHUB_TOKEN` is still required for `sync github` and `sync project`

For project ingestion, `GITHUB_TOKEN` must be able to read organization ProjectV2 data for `DripDropz` Project 5, not just repository issues and pull requests.

## GitHub Coordination

Use GitHub Issues and Project 5 for planning and execution tracking.

`gh` CLI is the supported coordination interface. A GitHub MCP server is explicitly out of scope for this platform.

### Notes

- Project operations may require `gh auth refresh -s read:project,project`

### ProjectV2 Token Scope

`GITHUB_TOKEN` must have `read:project` scope to run `sync project`.

**Required scope:**
- `read:project` - classic scope for ProjectV2 read access

**Token behavior:**
- A token with only repo scope (e.g., from `gh auth login --hostname github.com`) successfully runs `sync github` for issues, PRs, and comments
- The same token fails on `sync project` with HTTP 403 and error detail `"project scope missing"`
- This is the expected failure mode documented in the PRD

**Recovery:**
```bash
gh auth refresh -s read:project
```

**Note:** `read:project,project` is the fine-grained permission format for org-level projects and also works, but `read:project` alone is sufficient for this platform.

- Search MCP remains read-only even though coordination happens in GitHub

## Related Files

- Plan: `.agent/plans/agentic/knowledge-base-platform-prd.md`
- Tasks: `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- Agent index: `.agent/readme.md`
- Root agent instructions: `AGENTS.md`
- Claude Code entry: `CLAUDE.md`
