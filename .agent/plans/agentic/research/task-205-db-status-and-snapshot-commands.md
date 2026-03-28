# Task 205 DB Status And Snapshot Commands Research

- Date: 2026-03-28
- Task: `task-205`
- Evidence: `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/tests/test_status_command.py`, `agentic/tests/test_snapshot_command.py`, `agentic/tests/test_status_command_db.py`, `agentic/tests/test_snapshot_command_db.py`, `agentic/Dockerfile`, `.agent/workflows/agentic-kb.md`

## Durable Findings

- `agentic-kb status` now layers live KB DB inspection on top of the task-103 runtime checks: when `DATABASE_URL` is valid and reachable, it reports migration versions `1`/`2`/`3`, the current `agentic` table set, the seven searchable registry tables, the task-203 BM25/HNSW index names, per-table row counts, and grouped `kb_sync_state` summaries.
- `agentic-kb status --healthcheck` remains intentionally lighter than normal status output: it still checks config parsing, mount visibility, ParadeDB TCP reachability, and Ollama API/model reachability, but it skips schema inspection so Compose healthchecks do not depend on a fully bootstrapped DB query path.
- The durable searchable-table contract for status should come from `agentic_kb.search.config.list_search_entity_configs()` rather than a second hard-coded table registry; this keeps task-205 aligned with task-204's table vocabulary while still locking index-name expectations to the current task-203 naming pattern.
- Real snapshot behavior is now implemented behind the task-103 CLI surface: `snapshot export` uses `pg_dump --format=custom --schema=agentic`, and `snapshot import` uses `psql` to drop the `agentic` schema before `pg_restore` recreates it. Import is explicitly destructive and requires `--yes` acknowledgement.
- Restricting restore scope for arbitrary custom dumps requires more than `pg_restore --schema=agentic`. Live verification against a deliberately non-conforming full-database dump showed `pg_restore` can still attempt schema/extension-adjacent entries outside `agentic`; the durable fix is to generate a filtered `pg_restore --list` file and restore only TOC entries whose schema is exactly `agentic`.
- ParadeDB 18 requires matching PostgreSQL client major versions for snapshot tooling. The first attempt with Debian's default PostgreSQL 15 client packages failed with `pg_dump: aborting because of server version mismatch`; the `kb-tools` image therefore needs PostgreSQL 18 client binaries from the PGDG apt repository, not the distro-default client package.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/cli.py" "agentic/src/agentic_kb/config.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/commands/snapshot.py" "agentic/tests/test_status_command.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_status_command_db.py" "agentic/tests/test_snapshot_command_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'` passed locally.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed locally.
- Local DB-backed tests were skipped without `AGENTIC_TEST_DATABASE_URL`; the disposable live verification ran in-container instead.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed after adding PostgreSQL 18 client tools.
- `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_status_command.py'` passed.
- `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_status_command_db.py'` passed against an isolated ParadeDB volume.
- `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command_db.py'` passed against the same isolated ParadeDB volume.
- `AGENTIC_DB_PORT=5760 docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command_db.py'` passed after the review fix and proved that importing a non-conforming full-database custom dump restores only `agentic` schema objects.
- Smoke verification passed with `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools status`, `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot export /workspace/agentic/snapshots/task205-smoke.dump`, and `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot import /workspace/agentic/snapshots/task205-smoke.dump --yes`, followed by a post-restore count check showing the exported document row survived the destructive import.

## No New Research Beyond Task Scope

- Task-205 did not define an external snapshot manifest schema, sync orchestration UX, or search CLI behavior; the durable outcome is the accepted DB-inspection contract, destructive restore boundary, and PostgreSQL 18 client-tool requirement.
