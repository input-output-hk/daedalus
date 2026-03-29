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

# Emit machine-readable readiness for scripts
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json

# Start the long-running service mode used by compose
docker compose -f docker-compose.agentic.yml up -d kb-tools

# Run the packaged stdio MCP server in a client-spawned session
docker compose -f docker-compose.agentic.yml run --rm -T mcp-search
```

## Clean Bootstrap

The currently validated clean-machine bootstrap path is intentionally narrow:

```bash
# Start a fresh stack
docker compose -f docker-compose.agentic.yml up -d

# Import a valid snapshot into a disposable KB database
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes

# Prove readiness
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json

# Prove imported data is queryable without waiting on hybrid/vector readiness
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

`agentic/.env.example` contains optional Compose overrides only. The default bootstrap path works without creating a local env file.

Current boundaries:

- `sync changed` is available as an incremental command after a seeded baseline, but it is not part of the clean bootstrap acceptance path

## Shared Baseline Snapshot Artifact

The canonical team baseline is local-only in v1.

Current contract:

- publication runs from `develop` on a trusted GPU-capable developer machine
- the portable payload is exactly one `.dump` plus its sibling `.manifest.json`
- snapshots are shared through a private storage backend outside git history
- GitHub Actions artifacts are retired and are not a supported publication channel
- the shared baseline uses one canonical embedding contract at a time; local model overrides are out of contract for team publication and handoff

Current local publication shape:

1. Start the stack locally.
2. Run `sync all` from a trusted developer machine.
3. Run `snapshot export` to produce the portable pair.
4. Upload the pair to the team's chosen private shared storage backend.

Current manual consumption path:

1. Download the shared snapshot pair from the private storage backend.
2. Place the two files into `agentic/snapshots/`.
3. Import either the `.dump` path or the sibling `.manifest.json` path.

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

Validation expectations after import:

- `status --json` should report `"ok": true`
- the search result JSON should report `"mode": "bm25"`
- the search result should contain at least one hit
- the first hit should have `entity_type = "documents"`
- the first hit should have `fields.source_path = ".agent/workflows/agentic-kb.md"`

`sync changed` remains optional follow-on work after import when a developer wants to refresh local deltas on top of the shared baseline.

The exact backend selection, naming, retention, publish helpers, and fetch helpers are still tracked as pending rollout work.

Pending rollout work also includes the backend authentication/bootstrap path, artifact discovery path, and outage recovery behavior.

## Canonical Embedding Contract

Shared baseline publication is pinned to one canonical embedding contract at a time rather than being a per-developer preference.

- the contract is the embedding model name plus the compatibility metadata recorded in the snapshot manifest and enforced by the tooling
- if the canonical contract changes, the trusted publisher must rebuild and republish from `develop` before the team resumes snapshot handoff
- `OLLAMA_EMBED_MODEL` overrides are acceptable for disposable local experiments only; do not use them when publishing or extending the shared baseline
- operators should expect incompatible contracts to fail fast during snapshot import and to block post-import incremental sync

Clean-machine bootstrap and full team rollout are separate acceptance levels:

- bootstrap only requires stack start, compatible snapshot import, `status --json`, and the deterministic BM25 proof query
- broader team rollout additionally requires the documented shared-storage handoff, Project token guidance, helper commands, and post-import sync workflow

## MCP Setup

`agentic-kb mcp-search` is a stdio MCP server. MCP clients should spawn it as a no-TTY child process and communicate over stdin/stdout.

Use this repo-supported launcher for MCP clients:

```bash
docker compose -f docker-compose.agentic.yml run --rm -T mcp-search
```

Do not point MCP clients at `docker compose -f docker-compose.agentic.yml up -d mcp-search`. That Compose service is only a parity/smoke harness for the same stdio process; it is not a network endpoint.

### Environment Variables

- `DATABASE_URL`: required for direct local `agentic-kb mcp-search` launches; the Compose launcher wires it automatically.
- `OLLAMA_BASE_URL`: required for direct local launches and needed for vector/hybrid MCP queries; the Compose launcher wires it automatically.
- `OLLAMA_EMBED_MODEL`: optional override. Keep it aligned with the embedding model used to index the current KB.
- `OLLAMA_EMBED_MODEL`: optional for disposable local rebuild experiments, but shared baseline publication and post-import sync must match the current canonical embedding contract.
- `GITHUB_TOKEN`: optional for read-only MCP search/status. It is still required for `sync github` and `sync project`, and Project 5 reads need a token that can read `DripDropz` ProjectV2 data.

### OpenCode

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "daedalus-agentic-search": {
      "type": "local",
      "command": [
        "docker",
        "compose",
        "-f",
        "docker-compose.agentic.yml",
        "run",
        "--rm",
        "-T",
        "mcp-search"
      ],
      "enabled": true
    }
  }
}
```

### Claude Code

```bash
claude mcp add --transport stdio --scope project daedalus-agentic-search -- \
  docker compose -f docker-compose.agentic.yml run --rm -T mcp-search
```

This writes the project-scoped MCP entry into `.mcp.json` for Claude Code.

### Local `.mcp.json`

```json
{
  "mcpServers": {
    "daedalus-agentic-search": {
      "type": "stdio",
      "command": "docker",
      "args": [
        "compose",
        "-f",
        "docker-compose.agentic.yml",
        "run",
        "--rm",
        "-T",
        "mcp-search"
      ]
    }
  }
}
```

All three examples intentionally resolve to the same launcher contract. If your client stores the process definition differently, preserve the same argv and keep `-T` so Docker does not allocate a TTY for the stdio MCP session.

Container contract:

- Repository mount: `/workspace`
- Snapshot mount: `/workspace/agentic/snapshots`
- Default entrypoint: `agentic-kb`
- Default command: `service`

The `mcp-search` compose service now runs the same packaged stdio MCP process exposed locally as `agentic-kb mcp-search`; it does not expose a separate HTTP daemon.

The image uses `python:3.12-slim-bookworm` so later tasks can add PostgreSQL-adjacent tooling without reworking the base image away from Alpine.
