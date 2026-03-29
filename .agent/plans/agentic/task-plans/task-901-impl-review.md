Implementation: Iteration 1
Timestamp: 2026-03-29T16:45:00Z
Outcome: implemented

- Summary of changes made: installed `git` in the `kb-tools` image, added per-command git `safe.directory` handling for bind-mounted repo operations, packaged the snapshot manifest schema into the installed Python package, added the missing optional `agentic/.env.example`, and narrowed the operator docs to the validated clean-bootstrap contract (`up -> snapshot import -> status --json -> bm25 search --json`) while explicitly marking CI snapshot publication as pending and keeping `sync changed` out of acceptance.
- Files touched: `agentic/Dockerfile`, `agentic/.env.example`, `agentic/pyproject.toml`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/src/agentic_kb/snapshot_manifest.py`, `agentic/src/agentic_kb/config/snapshot-manifest.schema.json`, `agentic/README.md`, `.agent/workflows/agentic-kb.md`, `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`, `.agent/plans/agentic/task-plans/task-901-impl-review.md`.
- Verification run: built fresh images; launched isolated source stack with `AGENTIC_DB_PORT=5545 OLLAMA_PORT=12434 docker compose -p task901-seed -f docker-compose.agentic.yml up -d`; verified deterministic source search with `docker compose ... run --rm kb-tools search --mode bm25 --entity-type documents --json "task-901 clean machine bootstrap"`; exported snapshot with `docker compose ... run --rm kb-tools snapshot export agentic/snapshots/task901-seed.dump`; launched isolated target stack with `AGENTIC_DB_PORT=5645 OLLAMA_PORT=13434 docker compose -p task901-target -f docker-compose.agentic.yml up -d`; imported with `docker compose ... run --rm kb-tools snapshot import agentic/snapshots/task901-seed.dump --yes`; proved readiness with `docker compose ... run --rm kb-tools status --json`; proved deterministic post-import search with `docker compose ... run --rm kb-tools search --mode bm25 --entity-type documents --json "task-901 clean machine bootstrap"`; ran one minimal MCP initialize smoke with `docker compose -p task901-target -f docker-compose.agentic.yml run --rm -T mcp-search` under the same port overrides.
- Verification outcome: import/readiness/search succeeded in the fresh target environment; `status --json` reported `ok: true`; the deterministic BM25 query returned the expected `doc:task901-bootstrap` hit after import; minimal MCP initialize smoke exited `0`.
- Deviations from approved plan: validation snapshot input came from a task-local isolated seeded environment because no shared published snapshot artifact exists yet; the local workstation required non-default host-port overrides to keep the isolated validation environments separate from existing services; exploratory seeding briefly exercised `sync docs`/`sync code`, which exposed unrelated ingestion behavior, but `sync changed` remained out of the acceptance path as approved.

Code Review: Iteration 1
Timestamp: 2026-03-29T16:39:48Z
Outcome: requires_changes
findings
- `.agent/workflows/agentic-kb.md:81-88` still documents `sync changed` as a shipped-container limitation because the `kb-tools` image supposedly does not install `git`, but `agentic/Dockerfile:9-18` now installs `git` and `agentic/src/agentic_kb/commands/sync.py:883-899` adds the per-command `safe.directory` fix for bind-mounted repo operations. That leaves the operator workflow materially inaccurate after this task, which conflicts with the approved plan's requirement to align the docs with the validated runtime reality and resolve the current documentation contradictions explicitly.
Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-03-29T16:40:42Z
Outcome: implemented

- Summary of changes made: recorded the narrow workflow-doc fix now applied in `.agent/workflows/agentic-kb.md` so it no longer claims the shipped `kb-tools` image lacks `git`, and instead describes the current `sync changed` caveat accurately against the validated runtime.
- Files touched: `.agent/plans/agentic/task-plans/task-901-impl-review.md`.
- Verification run: re-read `.agent/workflows/agentic-kb.md` against the code-review finding and confirmed the stale wording is gone; the workflow now states that the shipped `kb-tools` image includes `git` plus the per-command safe-directory override for `/workspace`, while still keeping `sync changed` outside the clean-machine bootstrap acceptance path.
- Verification outcome: the previously reported doc/runtime mismatch is resolved in the current workflow text.
- Deviations from approved plan: none for this iteration; this was an append-only implementation-log update for the already-applied narrow doc fix.

Code Review: Iteration 2
Timestamp: 2026-03-29T16:41:38Z
Outcome: approved
findings
- None. The current implementation now matches the approved task-901 plan: the clean-bootstrap contract is narrowed correctly in `agentic/README.md` and `.agent/workflows/agentic-kb.md`, the missing optional `agentic/.env.example` is present and documented accurately, the snapshot-manifest schema is packaged and loaded in an installed-image-safe way via `agentic/pyproject.toml`, `agentic/src/agentic_kb/config/snapshot-manifest.schema.json`, and `agentic/src/agentic_kb/snapshot_manifest.py`, and the in-container git/runtime fix in `agentic/Dockerfile` plus `agentic/src/agentic_kb/commands/sync.py` is now described correctly without pulling `sync changed` into the clean-machine acceptance path.
Decision: approved

(End of file - total 32 lines)
