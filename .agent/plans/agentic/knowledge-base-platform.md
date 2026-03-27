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
- Keep the knowledge base fresh with explicit sync commands, staleness detection, and scheduled automation.
- Expose the knowledge base to agents through a read-only Search MCP and documented setup for OpenCode, Claude Code, and similar tools.

## Non-Goals

- Do not build or depend on a GitHub MCP server for project management. Use `gh` CLI for coordination.
- Do not make this part of the Daedalus runtime, wallet startup path, installer, or production build artifacts.
- Do not require pre-commit indexing or any other always-on local background daemon in v1.
- Do not give MCP write access to GitHub, the repository, or the knowledge database in v1.

## Requirements

- [x] Track the work as a GitHub issue in `DripDropz/daedalus` and add it to Project 5.
- [x] Store the implementation plan and machine-readable tasks in `.agent/plans/agentic/`.
- [ ] Start the full knowledge stack with Docker Compose from the repo root.
- [ ] Ingest and index agent documentation from `.agent/`, top-level repo docs, selected subsystem READMEs, and implementation plans.
- [ ] Ingest and index Daedalus source code with symbol-aware chunking for TypeScript/TSX-heavy areas.
- [ ] Ingest and index GitHub issues, pull requests, comments, and Project 5 item metadata.
- [ ] Support BM25 keyword search, vector search, and RRF fusion across all indexed entity types.
- [ ] Provide read-only Search MCP tools and setup instructions for OpenCode and Claude Code.
- [ ] Provide snapshot export/import plus a documented team-sharing workflow for multiple developers.
- [ ] Provide incremental sync commands and staleness detection so the knowledge base can be refreshed after repo or GitHub changes.

## Technical Design

### Coordination Model

- GitHub Issues and Project 5 remain the source of truth for active work coordination.
- `.agent/plans/agentic/knowledge-base-platform.md` is the durable implementation plan.
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
└── .github/workflows/
    └── agentic-kb-sync.yml
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

### Export / Import and Multi-Developer Sharing

This is required for team use, not optional polish.

#### Snapshot Format

Each export should produce:

- a compressed ParadeDB dump (`pg_dump --format=custom --compress=zstd`)
- a manifest JSON containing:
  - schema version
  - snapshot creation timestamp
  - repo commit used for docs/code ingestion
  - GitHub sync cursors and watermarks
  - embedding model name
  - row counts by entity type

#### Team Workflow

- Maintain a baseline team snapshot generated from `develop`.
- Developers can import the latest snapshot instead of re-running every ingest task from scratch.
- After import, developers run `sync changed` to capture local branch changes and any recent GitHub updates.
- Snapshot publication should be automatable from CI so the team can share a current baseline.

This gives the team two supported modes:

1. **Fast start** - import latest shared snapshot, then sync deltas
2. **Full rebuild** - ingest from scratch when validating the pipeline itself

### Freshness and Update Strategy

The knowledge base should be explicitly refreshable and able to detect staleness.

#### Sync Commands

Provide commands for:

- `sync docs`
- `sync code`
- `sync github`
- `sync project`
- `sync changed`
- `sync all`

#### Staleness Rules

- Compare indexed repo commit against local `HEAD` for docs/code sources.
- Compare GitHub watermarks (`updatedAt`, comment counts, project cursors) against last sync state.
- Warn agents and humans when local results are older than the current branch or GitHub state.

#### Automation

- Scheduled GitHub Action refreshes GitHub and project metadata.
- CI can publish a new shared snapshot on a regular cadence or after selected merges to `develop`.
- Local developers should not be forced through expensive indexing on every commit.

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

Required documentation:

- OpenCode setup example
- Claude Code setup example
- local `.mcp.json` example
- environment variable reference for `DATABASE_URL`, `OLLAMA_BASE_URL`, `GITHUB_TOKEN`

### Security and Operational Boundaries

- Keep MCP read-only in v1.
- Keep GitHub tokens in environment variables or local env files that are gitignored.
- Do not couple KB credentials or services to the wallet runtime.
- Treat snapshots as internal developer artifacts because they may contain copied GitHub discussions and project metadata.

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
- Add a shared team workflow for publishing and consuming baseline snapshots.

### Phase 7 - Freshness and Automation

- Add `sync` commands.
- Add stale-index detection.
- Add scheduled GitHub automation and smoke checks.

### Phase 8 - MCP and Agent Documentation

- Add read-only Search MCP.
- Add OpenCode and Claude Code setup docs.
- Add `.agent/workflows/agentic-kb.md` and update the main agent indexes.

### Phase 9 - Validation and Rollout

- Validate clean-machine setup.
- Validate snapshot import/export round-trips.
- Pilot with multiple developers and document SOPs for common failure modes.

## Testing Strategy

### Automated

- Compose boot smoke test
- Schema and migration test
- Docs/code/GitHub ingest fixtures
- Search regression tests for BM25, vector, and RRF ranking
- Snapshot export/import round-trip tests
- MCP smoke tests against a seeded database

### Manual

- Fresh developer setup from clone
- Import latest shared snapshot
- Run `sync changed` on a local feature branch
- Query the KB through CLI and MCP
- Confirm agent setup works in OpenCode and Claude Code

## Rollout Plan

1. Ship the plan, task tracking, and GitHub epic
2. Build the local stack and docs/code ingestion first
3. Add GitHub/project ingestion and search
4. Add snapshot sharing and freshness automation
5. Onboard a small set of developers before wider rollout

## Decisions Confirmed

- Shared snapshots should be published as GitHub Actions artifacts, not GitHub Releases assets.
- Project 5 is new enough that we can shape its metadata to fit this workflow; the initial field conventions are `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date`.
- The first code ingestor should cover the entire repository immediately, with metadata and filters preserving fast access to the most agent-relevant areas.

## References

- `vibe-node` docs: `docs/about/`, `docs/reference/workflow.md`, `docs/reference/cli-reference.md`, `docs/reference/pipelines/`
- Daedalus agent docs: `AGENTS.md`, `CLAUDE.md`, `.agent/readme.md`
- GitHub Project: `https://github.com/orgs/DripDropz/projects/5`

---

**Status:** 🚧 In Progress  
**Date:** 2026-03-27  
**Author:** OpenCode  
**Tracking:** GitHub epic `DripDropz/daedalus#22`; local tasks tracked in `knowledge-base-platform-tasks.json`  
**Notes:** GitHub issue created and added to Project 5 on 2026-03-27. Project field conventions are now established with `Work Type`, `Area`, `Phase`, and `KB Impact` custom fields. The Phase 1 compose scaffold now exists in `docker-compose.agentic.yml` with pinned images, localhost-only ports, healthchecks, and named volumes. `kb-tools` now builds from the local `agentic/` Python package with a packaged `agentic-kb` CLI, while `mcp-search` remains the placeholder service scheduled for `task-104`.
