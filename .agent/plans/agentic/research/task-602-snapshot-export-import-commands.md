# Task 602 Snapshot Export Import Commands Research

- Date: 2026-03-29
- Task: `task-602`
- Evidence: `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/src/agentic_kb/snapshot_manifest.py`, `agentic/tests/test_snapshot_command.py`, `agentic/tests/test_snapshot_command_db.py`, `.agent/workflows/agentic-kb.md`, `.agent/plans/agentic/task-plans/task-602.md`

## Durable Findings

- `agentic-kb snapshot export` now produces a portable sibling pair: `<snapshot-name>.dump` plus `<snapshot-name>.manifest.json` in the same directory, with the manifest locked to the approved task-601 schema and referencing the dump only by basename.
- Export consistency is now implementation-grade rather than aspirational: the command opens a repeatable-read PostgreSQL transaction, collects manifest metadata, calls `pg_export_snapshot()`, and passes that identifier to `pg_dump --snapshot=...`; if no usable snapshot id is returned, export fails loudly and cleans up partial files instead of emitting a potentially mismatched dump/manifest pair.
- Import resolution is deterministic in both supported entry modes. Passing a dump path resolves only the derived same-directory `.manifest.json` sibling; passing a manifest path resolves only the same-directory dump named by `artifact.filename`. The implementation does no directory scanning or cross-directory guessing.
- Manifest-path import now enforces the full sibling-dump contract, not just basename safety: `artifact.filename` must remain basename-only and must end with `.dump` before the file is accepted for pre-restore validation.
- Import validation order is safely locked: parse JSON, validate schema, resolve the same-directory dump, verify exact `size_bytes`, verify exact `sha256:<lowercase-hex>` content hash, then run the destructive filtered-schema restore. Size or hash mismatches fail before any schema drop.
- `agentic.kb_snapshot_manifest` persistence is now aligned with the richer external manifest without a schema change: raw manifest JSON is preserved in `manifest`, artifact digest maps to `content_hash`, GitHub stream watermarks map to `github_watermarks`, and `repo_commit_hash` stores the shared docs/code baseline only when both baselines are equal; otherwise it remains `NULL`.
- The DB-backed tests confirmed both branches of that repo-commit mapping: an export with only docs baseline populated stores `NULL`, while a later export with matching docs/code baselines stores the shared commit hash.
- Containerized DB verification in this repo needed an explicit readiness loop before launching the DB-backed unittest suite from `kb-tools`. Directly invoking the suite immediately after `docker compose up -d paradedb` could race the in-network endpoint and produce transient `connection refused` failures even after host-side health checks were green.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/snapshot.py" "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py" "agentic/tests/test_snapshot_manifest_schema.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -p task602-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb -e OLLAMA_EMBED_MODEL=all-minilm:l6-v2 kb-tools - <<'PY' ... unittest discover('agentic/tests', pattern='test_snapshot_command_db.py') ... PY` passed after an in-container DB readiness wait.

## No New Research Beyond Task Scope

- Task-602 intentionally does not add snapshot publication automation, import-then-sync orchestration, or stale-index detection; those remain later-task concerns even though export/import now provide the manifest and metadata those flows will build on.
