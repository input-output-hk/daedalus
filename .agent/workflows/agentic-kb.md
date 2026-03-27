---
description: Run the agentic knowledge base, sync it, and connect agents through MCP
---

# Agentic Knowledge Base Workflow

This workflow guides booting the Daedalus agentic knowledge base, syncing local and GitHub knowledge into it, exporting/importing snapshots for team sharing, and connecting local agents through the read-only Search MCP.

## Status

This workflow documents the planned Daedalus agentic platform described in `.agent/plans/agentic/knowledge-base-platform.md`.

Until implementation lands, use this file as the source of truth for how the platform is expected to work and how future agents should think about its responsibilities.

Current implementation note: `docker-compose.agentic.yml` now exists and boots the infrastructure scaffold for `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`. The `kb-tools` and `mcp-search` services are still placeholders at this stage, so the sync, search, snapshot, and MCP commands documented below remain planned behavior until later tasks land.

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

## Planned Commands

The exact command names may evolve, but the supported workflow should converge on this shape:

```bash
# Start the stack
docker compose -f docker-compose.agentic.yml up -d

# Check status
docker compose -f docker-compose.agentic.yml ps

# Sync the whole knowledge base
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all

# Sync only changes since last update
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed

# Search locally
docker compose -f docker-compose.agentic.yml run --rm kb-tools search "mithril bootstrap"

# Export a portable snapshot for another developer
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot export

# Restore a shared snapshot
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/latest.dump
```

## Team Sharing Workflow

The platform is designed for multiple human developers, not just one local machine.

### Recommended Team Pattern

1. One developer or CI publishes a fresh baseline snapshot from `develop`
2. Another developer imports that snapshot locally
3. The developer runs `sync changed` to add branch-local docs/code changes and any new GitHub updates
4. The team repeats this instead of rebuilding the whole knowledge base from scratch every time

### Snapshot Publication

The default publication channel for shared baseline snapshots should be GitHub Actions artifacts.

- CI publishes the latest baseline snapshot from `develop`
- developers download that artifact, import it locally, and then run `sync changed`
- GitHub Releases assets are out of scope for KB snapshot sharing

### Why Snapshot Sharing Matters

- Full ingestion can be expensive
- GitHub history grows over time
- Shared snapshots keep teams aligned on a common baseline
- Developers can still index local branch deltas after import

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
