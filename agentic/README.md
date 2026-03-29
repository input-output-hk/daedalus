# Agentic KB Tools

This package provides the `kb-tools` container for the Daedalus agentic knowledge-base stack.

Current command surface:

- `agentic-kb status` - implemented bootstrap, dependency, and KB DB status checks
- `agentic-kb search` - implemented local BM25/vector/hybrid KB search
- `agentic-kb entity get` - implemented generic entity inspection by type and id
- `agentic-kb sync <subcommand>` - implemented packaged sync commands
- `agentic-kb snapshot <subcommand>` - implemented export/import snapshot commands
- `agentic-kb service` - long-running service mode used by `docker compose up -d kb-tools`
- `agentic-kb mcp-search` - implemented read-only Search MCP server over stdio

Implemented now:

- `status`
- `search`
- `entity get`
- `sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, `sync all`
- `snapshot export`
- `snapshot import`
- `service`
- `mcp-search`

Examples:

```bash
# Build the tools image
docker compose -f docker-compose.agentic.yml build kb-tools

# Show packaged CLI help
docker compose -f docker-compose.agentic.yml run --rm kb-tools --help

# Check bootstrap status without claiming schema readiness
docker compose -f docker-compose.agentic.yml run --rm kb-tools status

# Start the long-running service mode used by compose
docker compose -f docker-compose.agentic.yml up -d kb-tools

# Start the packaged stdio MCP server in the compose parity service
docker compose -f docker-compose.agentic.yml up -d mcp-search
```

Container contract:

- Repository mount: `/workspace`
- Snapshot mount: `/workspace/agentic/snapshots`
- Default entrypoint: `agentic-kb`
- Default command: `service`

The `mcp-search` compose service now runs the same packaged stdio MCP process exposed locally as `agentic-kb mcp-search`; it does not expose a separate HTTP daemon.

The image uses `python:3.12-slim-bookworm` so later tasks can add PostgreSQL-adjacent tooling without reworking the base image away from Alpine.
