# Daedalus Agentic MCP Server

This document explains how the Daedalus MCP server works and how to ensure your queries route to the knowledge base instead of direct code scanning.

## Overview

The Daedalus agentic platform exposes a **read-only** MCP server (`mcp-search-http`) that provides access to a searchable knowledge base containing:

- Repository documentation (AGENTS.md, workflows, plans, SOPs)
- Source code with symbol-aware chunking
- GitHub issues, pull requests, and comments
- GitHub Project 5 (Daedalus Maintenance) items and field values

The MCP server is auto-discovered by OpenCode when running in the Daedalus repository via `opencode.json` at the project root.

## MCP Tools Available

| Tool | Description |
|------|-------------|
| `search` | General hybrid search across all entity types |
| `search_docs` | Search only documentation (md, markdown files) |
| `search_code` | Search only code chunks and symbols |
| `search_github` | Search GitHub issues, PRs, and comments |
| `get_entity` | Fetch a specific entity by type and ID |
| `find_related` | Find related entities for a given entity |
| `kb_status` | Check knowledge base health and freshness |

## How Queries Route to MCP

OpenCode automatically discovers and uses MCP servers defined in `opencode.json` at the project root. When the `daedalus-agentic-search` MCP server is enabled and the KB stack is running, OpenCode will route queries to it based on:

1. **Natural language queries about documentation** — Questions like "How do I run tests?", "What is the workflow for X?", or "Find information about Y" typically route to the MCP server's `search_docs`.

2. **Explicit search tool calls** — Using MCP tools like `search`, `search_docs`, `search_code`, `search_github` directly in your requests.

3. **Contextual hints** — Prefixing questions with phrases like:
   - "Search the KB for..."
   - "Find in the knowledge base..."
   - "What does the docs say about..."
   - "Look up in the knowledge base..."

## Ensuring Your Query Hits the MCP Server

### Do

- **Be specific about what you want**: "Search the docs for information about cardano-cli transactions" routes more reliably than "cardano-cli transactions"
- **Reference entity types**: "Find me the `search_docs` results for..." or "Use `search_github` to look up..."
- **Ask about KB content**: Questions about plans, workflows, SOPs, architecture decisions, or project history are strong MCP signals
- **Use the `kb_status` tool** to verify the MCP server is connected and the KB is healthy before querying

### Consider Using MCP For

- Questions about past decisions or PRDs (see `.agent/plans/`)
- Understanding workflows and SOPs (see `.agent/SOPs/`, `.agent/workflows/`)
- Finding relevant code patterns in the codebase
- GitHub issue/PR history and project context
- Questions that require searching across multiple source types

### Consider Code Scanning Instead

- Questions about **current** file contents or implementation details
- Finding specific functions, classes, or variables by exact name
- Understanding how code actually works right now
- Questions that require reading exact file paths or line numbers
- Refactoring or editing tasks

## Running the Stack

For the MCP server to be available, start the `mcp-search-http` service:

```bash
# Start the MCP server (daemon)
docker compose -f docker-compose.agentic.yml up -d mcp-search-http

# Verify services are up
docker compose -f docker-compose.agentic.yml ps

# Check MCP/KB health
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
```

## Environment Variables

The MCP server is configured via `opencode.json` at the project root and uses the Compose-launched container environment. Key variables wired automatically:

- `DATABASE_URL` — KB database connection
- `OLLAMA_BASE_URL` — Local embedding service
- `OLLAMA_EMBED_MODEL` — Embedding model (defaults to `all-minilm`)

For GitHub-backed queries, `GITHUB_TOKEN` must have `read:project` scope for Project 5 access.

## Troubleshooting

**OpenCode MCP server shows as "failed":**
Ensure the `mcp-search-http` service is running:

```bash
docker compose -f docker-compose.agentic.yml up -d mcp-search-http
docker compose -f docker-compose.agentic.yml ps mcp-search-http
```

**Manual MCP testing:**
```bash
# Verify the MCP server is running
curl http://127.0.0.1:8765 2>/dev/null || echo "MCP server not responding"
```

**KB status check:**
```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
```

**Stale results:**
```bash
# Sync recent changes
yarn agentic:kb:sync:changed

# Or do a full refresh
yarn agentic:kb:sync:all
```

**Import a fresh baseline:**
```bash
# Fetch latest from shared storage (requires AGENTIC_KB_SHARED_DIR set)
yarn agentic:kb:fetch -- <snapshot-basename>

# Import it
yarn agentic:kb:import
```

## Related Documentation

- [Agentic KB Workflow](./.opencode/workflows/agentic-kb.md) — Full workflow guide
- [Knowledge Base Platform PRD](./.agent/plans/agentic/knowledge-base-platform-prd.md) — Technical design and requirements
- [agentic/README.md](./agentic/README.md) — CLI reference and MCP setup examples
