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

## Compose Smoke Regression

Run the host-side Compose smoke regression only when you intentionally want to exercise the full repo-root boot contract:

```bash
AGENTIC_RUN_COMPOSE_SMOKE=1 PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_compose_bootstrap.py'
```

The explicit env gate keeps normal unittest discovery from unexpectedly booting Docker Compose and pulling the Ollama model.

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

Shipped local helper commands:

```bash
yarn agentic:kb:publish
yarn agentic:kb:fetch -- agentic-kb-<timestamp>
```

Helper contract:

- `AGENTIC_KB_SHARED_DIR` is required and must point to a locally accessible Dropbox-synced folder named `Daedalus_KB`
- Set it in `agentic/.env` (see `.env.example` for the variable name)
- `yarn agentic:kb:publish` runs `sync all`, exports one snapshot pair into `agentic/snapshots/`, and copies that `.dump` plus sibling `.manifest.json` into `Daedalus_KB`
- `yarn agentic:kb:fetch` copies one explicit sibling pair back from `Daedalus_KB` into local `agentic/snapshots/`
- fetch requires an explicit snapshot basename or sibling filename; it does not claim zero-argument latest discovery
- these helpers stay local-path based only: no Dropbox API usage, OAuth flow, remote listing, or account bootstrap automation is added here

Current contract:

- publication runs from `develop` on a trusted GPU-capable developer machine
- the portable payload is exactly one `.dump` plus its sibling `.manifest.json`
- snapshots are shared through Dropbox, using the shared folder `Daedalus_KB` outside git history
- GitHub Actions artifacts are retired and are not a supported publication channel
- the shared baseline uses one canonical embedding contract at a time; local model overrides are out of contract for team publication and handoff
- Developer 1 creates `Daedalus_KB` in an existing Dropbox account and shares it with Developer 2 with write access; the shared-folder contract for v1 requires that both developers can write to the same folder
- Developer 2 bootstrap is only documented to the truthful minimum today: Developer 2 must have a Dropbox access path that can accept the shared folder and verify writable access; the repo's shipped helper surface starts only after that local path exists
- artifact discovery stays explicit: select the intended `.dump` plus sibling `.manifest.json` basename in `Daedalus_KB` and pass that basename to `yarn agentic:kb:fetch`
- upload the exported pair directly into `Daedalus_KB` and keep the two sibling files together; do not rename only one file from the pair
- retention is manual in v1: keep the current canonical pair in `Daedalus_KB` until its replacement is uploaded, and keep the previous known-good pair available as a short-term fallback when possible
- after download, rely on `snapshot import` to validate manifest schema plus dump size/hash before restore; if import reports an artifact mismatch, redownload both files together
- if Dropbox is unavailable or the latest intended pair is unavailable, use a last known-good compatible local pair or perform a full local rebuild from `develop`; do not fall back to GitHub Actions artifacts or GitHub Releases assets

Current local publication shape:

1. Start the stack locally.
2. Ensure `AGENTIC_KB_SHARED_DIR` is set in `agentic/.env` (pointing to the local Dropbox-synced `Daedalus_KB` path).
3. Run `yarn agentic:kb:publish` from a trusted developer machine.
4. Confirm the helper copied both sibling files into the Dropbox shared folder `Daedalus_KB`.

Current local fetch and consumption path:

1. Ensure `AGENTIC_KB_SHARED_DIR` is set in `agentic/.env` (pointing to the local Dropbox-synced `Daedalus_KB` path).
2. Run `yarn agentic:kb:fetch -- <snapshot-basename>`.
3. Import either the `.dump` path or the sibling `.manifest.json` path.

```bash
yarn agentic:kb:fetch -- <snapshot-basename>
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

## Canonical Embedding Contract

Shared baseline publication is pinned to one canonical embedding contract at a time rather than being a per-developer preference.

- the contract is the embedding model name plus the compatibility metadata recorded in the snapshot manifest and enforced by the tooling
- if the canonical contract changes, the trusted publisher must rebuild and republish from `develop` before the team resumes snapshot handoff
- `OLLAMA_EMBED_MODEL` overrides are acceptable for disposable local experiments only; do not use them when publishing or extending the shared baseline
- operators should expect incompatible contracts to fail fast during snapshot import and to block post-import incremental sync

Clean-machine bootstrap and full team rollout are separate acceptance levels:

- bootstrap only requires stack start, compatible snapshot import, `status --json`, and the deterministic BM25 proof query
- broader team rollout additionally requires the documented shared-storage handoff, Project token guidance, and post-import sync workflow

## MCP Setup

The agentic KB provides two MCP server variants:

- `agentic-kb mcp-search` - stdio-based MCP server (for compatibility)
- `agentic-kb mcp-search-http` - HTTP/SSE-based MCP server (recommended)

The HTTP/SSE-based MCP server is the recommended approach as it avoids stdio-related issues in some MCP clients.

### Starting the MCP Server

Start the MCP server as a long-running daemon:

```bash
docker compose -f docker-compose.agentic.yml up -d mcp-search-http
```

The server listens on `127.0.0.1:8765` by default.

### Environment Variables

- `DATABASE_URL`: required; the Compose launcher wires it automatically.
- `OLLAMA_BASE_URL`: required for vector/hybrid search queries; the Compose launcher wires it automatically.
- `OLLAMA_EMBED_MODEL`: optional override. Keep it aligned with the embedding model used to index the current KB.
- `GITHUB_TOKEN`: optional for read-only MCP search/status. It is still required for `sync github` and `sync project`.

### OpenCode

The repo's `opencode.json` is pre-configured for OpenCode:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "daedalus-agentic-search": {
      "type": "remote",
      "url": "http://127.0.0.1:8765",
      "enabled": true,
      "timeout": 60000
    }
  }
}
```

### Claude Code (HTTP/SSE)

```bash
claude mcp add --transport http --scope project daedalus-agentic-search -- \
  http://127.0.0.1:8765
```

### Local `.mcp.json` (HTTP/SSE)

```json
{
  "mcpServers": {
    "daedalus-agentic-search": {
      "url": "http://127.0.0.1:8765"
    }
  }
}
```

Container contract:

- Repository mount: `/workspace`
- Snapshot mount: `/workspace/agentic/snapshots`
- Default entrypoint: `agentic-kb`
- Default command: `service`

The image uses `python:3.12-slim-bookworm`.
