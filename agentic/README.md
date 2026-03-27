# Agentic KB Tools

This package provides the `kb-tools` container for the Daedalus agentic knowledge-base stack.

Current command surface:

- `agentic-kb status` - implemented bootstrap and dependency status checks
- `agentic-kb sync <subcommand>` - placeholder surface for later sync tasks
- `agentic-kb snapshot <subcommand>` - placeholder surface for later snapshot tasks
- `agentic-kb service` - long-running service mode used by `docker compose up -d kb-tools`

Implemented now:

- `status`
- `service`

Placeholder only in `task-103`:

- `sync all`
- `sync changed`
- `sync docs`
- `sync code`
- `sync github`
- `sync project`
- `snapshot export`
- `snapshot import`

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
```

Container contract:

- Repository mount: `/workspace`
- Snapshot mount: `/workspace/agentic/snapshots`
- Default entrypoint: `agentic-kb`
- Default command: `service`

The image uses `python:3.12-slim-bookworm` so later tasks can add PostgreSQL-adjacent tooling without reworking the base image away from Alpine.
