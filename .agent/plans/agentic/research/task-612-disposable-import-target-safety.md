# Task 612 Disposable Import Target Safety Research

- Date: 2026-03-29
- Task: `task-612`
- Evidence: `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/tests/test_snapshot_command.py`, `agentic/tests/test_snapshot_command_db.py`, `.agent/plans/agentic/task-plans/task-612.md`

## Durable Findings

- `snapshot import` now enforces the documented disposable-target boundary in code instead of relying only on workflow text. After manifest and dump validation succeed, the command inspects the target DB before any `DROP SCHEMA` step runs.
- The accepted v1 disposable-target contract is now implementation-grade and minimal: import is allowed when the database has no `agentic` schema at all, or when `agentic` exists but all state-bearing KB tables are empty.
- The state-bearing tables checked before restore are `agentic.kb_documents`, `agentic.kb_code_chunks`, `agentic.kb_github_issues`, `agentic.kb_github_issue_comments`, `agentic.kb_github_prs`, `agentic.kb_github_pr_comments`, `agentic.kb_project_items`, `agentic.kb_sync_state`, and `agentic.kb_snapshot_manifest`.
- Import now rejects partially seeded targets even when searchable content is empty but provenance/state remains. Leftover `kb_sync_state` or `kb_snapshot_manifest` rows are treated as non-disposable because they still represent an existing baseline that restore must not silently overwrite or merge with.
- Failure messaging now includes the offending table row counts and points operators to the supported recovery path: recreate the disposable KB volume and retry import.
- The guard is intentionally state-based rather than heuristic. It does not infer disposability from DB name, environment, or volume naming.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/snapshot.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `AGENTIC_DB_PORT=6545 docker compose -p task612-verify -f docker-compose.agentic.yml up -d paradedb` plus an in-container psycopg readiness loop, followed by `docker compose -p task612-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb -e OLLAMA_EMBED_MODEL=all-minilm:l6-v2 kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command_db.py'`, passed.
- The DB-backed suite proved both accepted restore targets and the rejection boundary: schema-missing fresh DBs succeed, initialized-but-empty KBs succeed, and existing rows in `kb_documents`, `kb_sync_state`, or `kb_snapshot_manifest` fail before restore.

## No New Research Beyond Task Scope

- Task-612 does not add embedding-contract compatibility checks, publication workflow changes, or broader bootstrap orchestration; it tightens only the import target-safety precondition for the existing snapshot restore path.
