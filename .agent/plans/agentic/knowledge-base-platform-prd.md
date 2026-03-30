# Agentic Knowledge Base and Workflow Platform

## Overview

Build a Docker Compose-based agentic development platform for Daedalus that combines GitHub Projects coordination, a local searchable knowledge base, read-only MCP search tools, and a documented workflow for keeping the knowledge current across multiple human developers and AI agents.

This work should adapt the useful parts of the `vibe-node` workflow to Daedalus without copying its full research pipeline. The result should be a practical developer platform for this repository: searchable docs, searchable code, searchable GitHub history, searchable project context, and a repeatable way to export, import, and refresh the shared knowledge base.

## Goals

- Use GitHub Issues + GitHub Project 5 (`Daedalus Maintenance`) as the coordination layer for agentic platform work.
- Run the full agentic stack with Docker Compose so contributors only need Docker, `docker compose`, and `gh`.
- Build a local knowledge base using ParadeDB + Ollama with BM25 keyword search, vector similarity search, and RRF ranking fusion.
- Ingest Daedalus-specific knowledge: docs, workflows, plans, SOPs, code, issues, pull requests, comments, and Project 5 items.
- Provide snapshot export/import so multiple developers can share a current knowledge base instead of re-ingesting everything from scratch.
- Keep the knowledge base fresh with explicit sync commands, staleness detection, and a lightweight local publication workflow for the shared baseline.
- Expose the knowledge base to agents through a read-only Search MCP and documented setup for OpenCode, Claude Code, and similar tools.

## Non-Goals

- Do not build or depend on a GitHub MCP server for project management. Use `gh` CLI for coordination.
- Do not make this part of the Daedalus runtime, wallet startup path, installer, or production build artifacts.
- Do not require pre-commit indexing or any other always-on local background daemon in v1.
- Do not give MCP write access to GitHub, the repository, or the knowledge database in v1.
- Do not treat GitHub-hosted Actions runners as the canonical embedding or snapshot publication environment in v1.
- Retire and remove any existing GitHub Actions-based KB snapshot publication path. It is not a supported fallback in v1.

## Requirements

- [x] Track the work as a GitHub issue in `DripDropz/daedalus` and add it to Project 5.
- [x] Store the implementation plan and machine-readable tasks in `.agent/plans/agentic/`.
- [x] Start the full knowledge stack with Docker Compose from the repo root.
- [x] Ingest and index agent documentation from `.agent/`, top-level repo docs, selected subsystem READMEs, and implementation plans.
- [x] Ingest and index Daedalus source code with symbol-aware chunking for TypeScript/TSX-heavy areas.
- [x] Ingest and index GitHub issues, pull requests, comments, and Project 5 item metadata.
- [x] Support BM25 keyword search, vector search, and RRF fusion across all indexed entity types.
- [x] Provide read-only Search MCP tools and setup instructions for OpenCode and Claude Code.
- [ ] Provide snapshot export/import plus a documented team-sharing workflow for multiple developers, including a selected private shared-storage backend.
- [x] Enforce snapshot/embed-model compatibility checks so import and post-import sync cannot silently mix incompatible embedding contracts.
- [x] Provide incremental sync commands and staleness detection so the knowledge base can be refreshed after repo or GitHub changes.
- [ ] Add an automated Compose boot smoke/regression check for the repo-root stack contract.
- [ ] Provide a documented local baseline publish/download workflow that uses GPU-capable developer machines and private shared snapshot storage outside git history.
- [ ] Provide a manual full Project 5 refresh path so eventual freshness converges even when edits land on already-seen items.
- [x] Document the v1 disposable-volume policy for schema changes and snapshot restore targets.
- [ ] Document ProjectV2 token scope requirements and the expected failure mode when repo reads work but org project reads do not.
- [x] Enforce that snapshot import only targets fresh, isolated, or otherwise disposable KB databases.
- [ ] Define and validate convergence rules for deleted or renamed repo content and removed or cleared Project 5 state, while treating GitHub history as append-only in v1.
- [ ] Add automated ingest fixtures, snapshot round-trip coverage, and MCP smoke tests so the documented v1 validation contract is executable.

## Technical Design

### Coordination Model

- GitHub Issues and Project 5 remain the source of truth for active work coordination.
- `.agent/plans/agentic/knowledge-base-platform-prd.md` is the durable implementation plan.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` tracks dependencies and implementation status in a machine-readable form.
- `gh` CLI is the supported interface for issue/project updates by humans and agents.
- The initial rollout should use a single epic issue in Project 5. Follow-up issues can be created from the task phases once implementation starts.

### Project 5 Field Conventions

The following Project 5 fields are the default coordination shape for this initiative:

- `Status` - existing project field (`Backlog`, `Ready`, `In progress`, `In review`, `Done`)
- `Priority` - existing project field (`P0`, `P1`, `P2`)
- `Size` - existing project field (`XS`, `S`, `M`, `L`, `XL`)
- `Work Type` - custom field (`epic`, `feature`, `chore`, `docs`, `infra`, `research`)
- `Area` - custom field (`coordination`, `infra`, `ingest`, `search`, `mcp`, `docs`, `automation`)
- `Phase` - custom field (`Phase 0` through `Phase 9`)
- `KB Impact` - custom field (`none`, `docs`, `code`, `github`, `project`, `all`)
- `Start date` / `Target date` - optional scheduling fields when a date-bound rollout is needed

`Type` could not be used as a field name because GitHub Project reserves it, so the project uses `Work Type` instead.

### Recommended Repository Layout

Keep the knowledge platform isolated from the Electron app and the Nix/Electron build flow:

```text
daedalus/
├── docker-compose.agentic.yml
├── agentic/
│   ├── README.md
│   ├── .env.example
│   ├── Dockerfile
│   ├── schema/
│   ├── snapshots/
│   ├── config/
│   └── src/
├── .agent/
│   ├── plans/agentic/
│   └── workflows/agentic-kb.md
```

Implementation tooling should live in an isolated developer-tools package/service so it can evolve independently of the wallet runtime. The preferred default is a dedicated Python-based tools container because it stays decoupled from the Electron/webpack toolchain and matches the shape of the ParadeDB/Ollama/MCP stack well.

### Compose Stack

The v1 Compose stack should include:

- `paradedb` - PostgreSQL 18 with `pg_search` and `pgvector`
- `ollama` - local embedding service
- `ollama-init` - pulls the configured embedding model on first boot
- `kb-tools` - ingestion, sync, snapshot, and CLI commands
- `mcp-search` - read-only MCP server that queries the knowledge base

Design rules:

- Everything needed for the platform starts from `docker compose -f docker-compose.agentic.yml up -d`.
- Ingestion and sync commands run through `docker compose run --rm kb-tools ...`.
- Persistent state lives in named Docker volumes plus `agentic/snapshots/` for portable exports.
- The stack should be safe to tear down and re-create without affecting the wallet app.

### Knowledge Sources

#### Repository Documentation

Prioritize documents that already guide humans and agents:

- `AGENTS.md`
- `CLAUDE.md`
- `.agent/readme.md`
- `.agent/workflows/**`
- `.agent/skills/**`
- `.agent/SOPs/**`
- `.agent/plans/**`
- `README.md`
- `tests/README.md`
- `installers/README.md`
- other subsystem READMEs as needed

#### Source Code

The first code ingestion pass should cover the entire repository immediately.

Search metadata and filters should still make it easy to focus on the code that agents most often need to navigate, including:

- `source/main/**`
- `source/common/**`
- `source/renderer/app/**`
- `storybook/**`
- `tests/**`
- selected `utils/**`

#### GitHub Knowledge

- `DripDropz/daedalus` issues and comments
- `DripDropz/daedalus` pull requests, review comments, and discussion threads
- GitHub Project 5 items and relevant custom field values

### Database Model

Start with a practical schema instead of a research-heavy one:

- `kb_documents` - docs, plans, workflows, SOPs, READMEs
- `kb_code_chunks` - code symbols or structured file chunks
- `kb_github_issues`
- `kb_github_issue_comments`
- `kb_github_prs`
- `kb_github_pr_comments`
- `kb_project_items`
- `kb_sync_state` - cursors, commit hashes, schema version, timestamps
- `kb_snapshot_manifest` - optional metadata for exported/imported snapshots

Common fields should include:

- stable id
- source type
- title / path / preview text
- content hash
- commit hash or `updated_at` watermark
- labels / project status / domain metadata where relevant
- embedding vector

### Ingestion and Chunking Strategy

#### Markdown / Docs

- Split on heading boundaries and preserve title hierarchy.
- Store document title, section title, subsection title, and source path.
- Preserve enough nearby context to make search previews useful.

#### TypeScript / TSX Code

- Use symbol-aware chunking for exported functions, classes, methods, stores, IPC definitions, and important constants.
- Prefer `ts-morph` or an equivalent TypeScript AST approach so results map back to real source symbols rather than arbitrary line windows.
- Add Daedalus-specific structured extractors later for:
  - IPC channels
  - MobX stores
  - workflows and skills
  - plan/task metadata

#### GitHub Issues / PRs / Project Items

- Store parent items and individual comments separately.
- Build a combined searchable text body for parent items.
- Capture Project 5 field values such as status, priority, type, and area when available.

### Search Stack

Use the same layered search approach as `vibe-node`, adapted for Daedalus:

- **BM25** for exact and keyword-heavy matching (`ipc`, `mithril`, store names, file paths, issue titles)
- **Vector search** for semantic similarity and natural-language queries
- **RRF (Reciprocal Rank Fusion)** to merge BM25 and vector result lists into one ranked output without trying to normalize their raw scores directly

Search should support filters by:

- entity type
- path prefix
- doc kind
- repo
- label / state
- project status / area
- source domain (`docs`, `code`, `github`, `project`)

### Compute Placement

- The canonical KB build and baseline snapshot publication flow should run on trusted developer machines, not on GitHub-hosted Actions runners.
- The expected team shape for v1 is two developers with local GPU access; the platform should take advantage of that instead of assuming CPU-only CI runners are the primary embedding environment.
- GPU is preferred for full rebuilds and canonical baseline publication because it keeps embedding latency practical as the corpus grows.
- CPU-only local operation must still work for import, status checks, BM25 queries, and targeted sync operations, even if full rebuilds are slower.
- GitHub-hosted Actions may remain useful later for lightweight verification, but they are not the source of truth for canonical KB snapshots in v1.
- The prior GitHub Actions-based snapshot publication path is retired and should be removed rather than preserved as a fallback.

### Export / Import and Multi-Developer Sharing

This is required for team use, not optional polish.

#### Snapshot Format

Each export should produce:

- a PostgreSQL custom-format `agentic` schema dump matching the current task-205 export contract (`pg_dump --format=custom --compress=6`)
- a manifest validated against the canonical checked-in schema at `agentic/config/snapshot-manifest.schema.json`; if runtime packaging needs an installed copy inside the Python package, that packaged copy must stay mechanically derived from the canonical root-level file rather than becoming a second independently edited source of truth
- a manifest JSON containing:
  - schema version
  - snapshot creation timestamp
  - basename-only sibling dump filename plus immutable artifact identity (`dump_format`, explicit compression metadata, exact byte size, and `sha256:<lowercase-hex>` content hash)
  - repo name plus docs/code commit baselines
  - GitHub per-stream watermarks and Project 5 cursor/watermark state
  - embedding contract metadata: embedding model name, expected vector dimensionality, and any versioned contract identifier needed to reject incompatible imports
  - row counts for the current seven entity types

#### Snapshot Compatibility Rules

- Snapshot import must fail fast when the manifest's embedding contract does not match the local KB contract expected by the running tooling.
- `status` should surface embedding-contract mismatches explicitly so operators can distinguish "imported successfully" from "safe to keep syncing".
- Post-import incremental commands such as `sync changed` must refuse to mix embeddings from incompatible contracts in the same KB.
- Snapshot import must also refuse restore targets that are not fresh, isolated, or otherwise explicitly disposable. The supported operator path is restore-into-empty, not merge-into-existing.
- The supported recovery path for an incompatible snapshot remains: recreate the disposable KB volume, then import a compatible snapshot or rebuild locally.

#### Canonical Embedding Contract

- v1 must define one canonical embedding contract for the shared team baseline rather than treating the embedding model as a per-developer preference.
- That contract must include at least the embedding model name, expected vector dimensionality, and a versioned contract identifier that changes whenever compatibility is intentionally broken.
- The canonical baseline publisher is responsible for republishing from `develop` whenever the embedding contract changes.
- Operator docs must state how developers discover the current canonical contract before import or sync, and what mismatch signals they should expect from `status`, snapshot import, and post-import sync commands.
- Ad hoc local overrides to `OLLAMA_EMBED_MODEL` are acceptable only for disposable local rebuild experiments; they are out of contract for shared baseline publication and team handoff.

#### Team Workflow

- Maintain a baseline team snapshot generated from `develop`.
- Developers can import the latest snapshot instead of re-running every ingest task from scratch.
- After import, developers run `sync changed` to capture local branch changes and any recent GitHub updates.
- Build and publish the canonical baseline on one of the trusted GPU-equipped developer machines.
- Store shared snapshots in durable private artifact storage outside git history.
- v1 selects Dropbox shared-folder storage as that private shared artifact store. The canonical location is the shared Dropbox folder `Daedalus_KB`.
- Developer 1 creates `Daedalus_KB` in an existing Dropbox account and shares it with Developer 2 with write access so both developers can publish to and fetch from the same folder.
- Developer 2 bootstrap is only specified to the truthful minimum so far: use a Dropbox access path that can accept the shared folder and confirm write access. Avoid inventing repo-owned bootstrap helpers until later tasks define them.
- Avoid Git LFS for rotating KB dumps.
- Selecting and documenting that private shared storage backend is a rollout gate for the team-sharing workflow, not optional follow-up polish.
- The selected backend must also document artifact discovery, authentication/bootstrap, naming/location expectations, manual retention expectations, integrity expectations after download, and the operator recovery path when the backend or latest artifact is unavailable.

This gives the team two supported modes:

1. **Fast start** - import latest shared snapshot, then sync deltas
2. **Full rebuild** - ingest from scratch on a local developer machine when validating the pipeline itself or republishing the canonical baseline

Shared-baseline publication is local-only in v1. GitHub Actions artifacts are not a supported publication channel and any transitional workflow in that direction should be removed.

### Freshness and Update Strategy

The knowledge base should be explicitly refreshable and able to detect staleness.

#### Sync Commands

Provide commands for:

- `sync docs`
- `sync code`
- `sync github`
- `sync project`
- `sync project --full` (or an equivalent explicit full-refresh mode) so Project 5 state can re-converge after edits to already-seen items
- `sync changed`
- `sync all`

#### Staleness Rules

- Compare indexed repo commit against local `HEAD` for docs/code sources.
- Compare GitHub watermarks (`updatedAt`, comment counts, project cursors) against last sync state.
- Warn agents and humans when local results are older than the current branch or GitHub state.
- For GitHub Project 5 items, eventual freshness is acceptable in v1. Cursor-based incremental sync does not need to guarantee immediate replay of edits to already-seen items as long as the platform also provides a manual full refresh path that re-converges project state.
- The manual full Project refresh path must be validated against an already-seeded KB by proving that an edit to a previously seen Project item is reflected after the explicit full refresh.

#### Convergence Rules For Removals

- Incremental repo sync must converge not only on changed content but also on removed or renamed tracked sources. Deleted docs or code files must not remain queryable as live KB records after the relevant sync scope completes.
- Project sync must define how removals or field clears are represented. At minimum, removed Project items and cleared field values must stop surfacing stale prior metadata after a full refresh.
- GitHub issue and pull-request history is append-only in v1. The platform does not need to retroactively delete previously ingested GitHub records as part of normal convergence, but it must still refresh newer comments, bodies, and metadata through the supported sync paths.
- Validation must cover already-seeded KBs so the platform proves it converges toward current source truth for repo and Project data rather than only appending new records.

#### Automation

- No GitHub-hosted scheduled rebuild or publication flow is required in v1.
- Canonical baseline refresh and publication should run as an explicit local workflow from `develop` on a trusted GPU-equipped developer machine.
- The repo should still carry an automated Compose boot smoke/regression check for `docker compose -f docker-compose.agentic.yml up -d`, `kb-tools status --healthcheck`, and basic teardown so the local stack contract does not silently drift.
- The shared storage backend should be durable, private, and independently discoverable by the two developers using the platform.
- Optional future automation should be evaluated only if it preserves GPU access, artifact durability, and the current security boundary.
- Local developers should not be forced through expensive indexing on every commit.
- Existing GitHub Actions-based publication automation should be removed as part of the rollout rather than maintained in parallel.

### MCP and Agent Integration

Only build read-only search MCP tools in v1.

Recommended tools:

- `search`
- `search_docs`
- `search_code`
- `search_github`
- `get_entity`
- `find_related`
- `kb_status`

The supported MCP client contract is stdio via `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`. The Compose `mcp-search` entry exists only as a parity or smoke harness for that same stdio process and is not a network daemon that clients connect to. Task and implementation wording should consistently describe it as a stdio tool container, not a long-lived service API.

Required documentation:

- OpenCode setup example
- Claude Code setup example
- local `.mcp.json` example
- environment variable reference for `DATABASE_URL`, `OLLAMA_BASE_URL`, `GITHUB_TOKEN`
- ProjectV2 token scope expectations and the operator-visible failure behavior when `GITHUB_TOKEN` can read repo issues/PRs but cannot read `DripDropz` Project 5 metadata

### Security and Operational Boundaries

- Keep MCP read-only in v1.
- Keep GitHub tokens in environment variables or local env files that are gitignored.
- Do not couple KB credentials or services to the wallet runtime.
- Treat snapshots as internal developer artifacts because they may contain copied GitHub discussions and project metadata.
- Keep shared snapshots out of normal git history and out of Git LFS in v1.
- Snapshot import is supported only against fresh, isolated, or otherwise disposable KB databases.
- In-place schema upgrades and manual retrofit procedures are not part of the supported v1 operator path. When schema contracts change, the supported recovery path is to recreate the KB volume and import a compatible snapshot or rebuild locally.

## Implementation Phases

### Phase 0 - Planning and Coordination

- Create the `.agent` plan and tasks JSON.
- Create the GitHub epic issue in `DripDropz/daedalus`.
- Add the epic to Project 5 and define project field conventions for follow-up issues.

### Phase 1 - Compose Infrastructure

- Add `docker-compose.agentic.yml`.
- Add the developer-tools container image and env examples.
- Add wrapper commands or scripts for booting the stack and running sync tasks.

### Phase 2 - Schema and Search Foundation

- Create the core schema.
- Create BM25 and HNSW indexes.
- Add DB status, snapshot export, and snapshot restore commands.

### Phase 3 - Documentation and Plan Ingestion

- Ingest `.agent/` and repo docs.
- Add markdown chunking, metadata extraction, and idempotency.

### Phase 4 - Code, GitHub, and Project Ingestion

- Add TypeScript symbol chunking.
- Add GitHub issues, PRs, comments, and Project 5 ingestion.
- Track sync watermarks and incremental refresh state.

### Phase 5 - Search and CLI

- Add the embedding client.
- Add BM25/vector/RRF search queries.
- Add a local CLI for search and KB status.

### Phase 6 - Snapshot Sharing

- Add snapshot manifest format.
- Add snapshot export/import commands.
- Add embedding-contract compatibility checks for import, status, and post-import sync safety.
- Add import-target safety checks so snapshot restore refuses non-disposable KB databases.
- Add a shared team workflow for locally publishing and consuming baseline snapshots through private shared storage.

### Phase 7 - Freshness and Automation

- Add `sync` commands.
- Add stale-index detection.
- Add deletion and field-clear reconciliation for docs/code/project entities so sync converges on source truth instead of stale supersets.
- Add an automated Compose boot smoke/regression check for the repo-root stack contract.
- Add a manual full Project 5 refresh path so eventual freshness can re-converge after edits to already-seen items.
- Add validation that the manual full Project 5 refresh path really replays edits to already-seen items.
- Add local baseline refresh/publication workflow docs, shared-storage fetch helpers, and smoke checks.

### Phase 8 - MCP and Agent Documentation

- Add read-only Search MCP.
- Add OpenCode and Claude Code setup docs.
- Add `.agent/workflows/agentic-kb.md` and update the main agent indexes.

### Phase 9 - Validation and Rollout

- Validate the narrow clean-machine bootstrap contract separately from the broader two-developer publication and handoff workflow.
- Validate snapshot import/export round-trips.
- Pilot with multiple developers and document SOPs for common failure modes.

## Testing Strategy

### Automated

- Compose boot smoke test
- Schema bootstrap and index creation test
- Docs/code/GitHub ingest fixtures
- Search regression tests for BM25, vector, and RRF ranking, backed by checked-in query fixtures and enforced as an automated regression gate
- Snapshot export/import round-trip tests
- Snapshot compatibility guard tests for embedding model/vector contract mismatches
- Snapshot import target-safety tests proving restore is refused for non-empty or non-disposable KBs
- Project full-refresh regression test proving replay of edits to already-seen items
- Repo/project removal reconciliation tests proving deleted content and cleared metadata stop surfacing after sync/full refresh
- MCP smoke tests against a seeded database

### Manual

- Fresh developer setup from clone
- Import latest shared snapshot
- Run `sync changed` on a local feature branch after the imported-baseline bootstrap succeeds
- Query the KB through CLI and MCP
- Confirm agent setup works in OpenCode and Claude Code
- Confirm repo-only `GITHUB_TOKEN` failures surface clearly for Project 5 reads and docs point operators to the required ProjectV2 scopes

## Rollout Plan

1. Ship the plan, task tracking, and GitHub epic
2. Build the local stack and docs/code ingestion first
3. Add GitHub/project ingestion and search
4. Add local snapshot sharing and baseline refresh workflow
5. Onboard a small set of developers before wider rollout

## Decisions Confirmed

- Shared snapshots should be published from trusted local GPU-equipped developer machines to private shared artifact storage outside git history.
- Project 5 is new enough that we can shape its metadata to fit this workflow; the initial field conventions are `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date`.
- The first code ingestor should cover the entire repository immediately, with metadata and filters preserving fast access to the most agent-relevant areas.
- Eventual freshness is acceptable for Project 5 items in v1 as long as the platform provides a manual full refresh path.
- GitHub issue and pull-request history is append-only in v1; repo and Project data still need explicit removal reconciliation.
- Disposable KB volumes are acceptable in v1; in-place schema upgrade support is not required.
- The shared baseline uses one canonical embedding contract at a time; changing that contract requires a deliberate republish from `develop` and explicit mismatch handling in import, status, and sync flows.

## References

- `vibe-node` docs: `docs/about/`, `docs/reference/workflow.md`, `docs/reference/cli-reference.md`, `docs/reference/pipelines/`
- Daedalus agent docs: `AGENTS.md`, `CLAUDE.md`, `.agent/readme.md`
- GitHub Project: `https://github.com/orgs/DripDropz/projects/5`

---

**Status:** 🚧 In Progress  
**Date:** 2026-03-27  
**Author:** OpenCode  
**Tracking:** GitHub epic `DripDropz/daedalus#22`; local tasks tracked in `knowledge-base-platform-tasks.json`  
**Notes:** GitHub issue created and added to Project 5 on 2026-03-27. Project field conventions are now established with `Work Type`, `Area`, `Phase`, and `KB Impact` custom fields. The repo-root Compose scaffold, core schema, search stack, docs/code/GitHub ingestion, sync commands, read-only stdio Search MCP, snapshot export/import, embedding-contract compatibility enforcement, disposable import-target safety, and primary KB workflow docs are all in place. Dropbox shared-folder storage is now selected and validated as the private shared snapshot backend for v1. Remaining rollout work is blocked on the documented local publish/download workflow, validated repo/Project removal reconciliation, Project 5 full re-convergence, and the remaining automated smoke/regression coverage promised by this plan.
