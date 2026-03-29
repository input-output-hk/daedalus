# Task 207 Schema Bootstrap Regression Check Research

- Date: 2026-03-29
- Task: `task-207`
- Evidence: `agentic/schema/init.sql`, `agentic/schema/create_indexes.sql`, `agentic/tests/test_status_command_db.py`, `agentic/tests/test_snapshot_command_db.py`, `agentic/src/agentic_kb/commands/status.py`, `docker-compose.agentic.yml`

## Durable Findings

- The current DB-backed test helpers in `agentic/tests/test_status_command_db.py` and `agentic/tests/test_snapshot_command_db.py` remove `\ir` lines from `agentic/schema/init.sql` and then execute `agentic/schema/create_indexes.sql` separately, so they do not regress the fresh-bootstrap include contract by themselves.
- `agentic/src/agentic_kb/commands/status.py` already centralizes the concrete schema/bootstrap expectations that task-207 should verify against a fresh database: migration versions `(1, 2, 3)`, the ten `agentic` tables, the seven searchable tables from the registry, and the derived BM25/HNSW index names.
- The shipped first-boot contract in `docker-compose.agentic.yml` depends on mounting `agentic/schema/create_indexes.sql` as `/docker-entrypoint-initdb.d/includes/create_indexes.task-203.sql`, matching the `\ir includes/create_indexes.task-203.sql` line in `agentic/schema/init.sql`. A meaningful task-207 regression check should therefore execute the bootstrap path with that relative include layout intact instead of only replaying the two SQL files independently.
- Because `\ir` is a `psql` client meta-command, a real task-207 regression must execute the checked-in bootstrap root through `psql -f` (or an equivalent `psql`-driven path). Running `init.sql` through `psycopg` alone cannot cover the shipped include contract.
- The disposable DB reset for this regression should stay schema-scoped (`DROP SCHEMA IF EXISTS agentic CASCADE`). The reusable test database may already have `pg_search` and `vector` installed at the cluster level, so the test should rely on `CREATE EXTENSION IF NOT EXISTS` idempotence rather than assuming a globally pristine cluster.
- The landed regression remains minimal by creating a temporary bootstrap directory during the test, copying `init.sql` there unchanged, and copying `create_indexes.sql` to `includes/create_indexes.task-203.sql`; this reproduces the shipped Compose layout closely enough to prove the checked-in relative include contract without changing production SQL.
- On a fresh bootstrap executed through that `psql` path, `status.inspect_database()` reports the expected migration versions and schema/index surface while all searchable row counts remain zero and `kb_sync_state` summaries remain empty, which makes `status` a good single-source assertion surface for this regression.

## Verification Notes

- `python3 -m py_compile agentic/tests/test_schema_bootstrap.py` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task207-verify -f docker-compose.agentic.yml build kb-tools` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task207-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_schema_bootstrap.py'` passed against an isolated ParadeDB target.
- The first packaged test attempt hit the already-known ParadeDB first-boot restart race with a transient connection-refused error. Waiting for a successful `pg_isready` against the isolated DB before the unittest run resolved it, which reinforces the existing task-201/task-203 guidance that first healthy/container-start signals are not authoritative proof that init scripts have finished.
- no new research
