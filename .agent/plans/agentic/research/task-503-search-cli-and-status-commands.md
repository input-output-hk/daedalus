# Task 503 Search CLI And Status Commands Research

- Date: 2026-03-29
- Task: `task-503`
- Evidence: `agentic/src/agentic_kb/cli.py`, `agentic/src/agentic_kb/commands/search.py`, `agentic/src/agentic_kb/commands/entity.py`, `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/commands/output.py`, `agentic/tests/test_search_command.py`, `agentic/tests/test_search_command_db.py`, `agentic/tests/test_entity_command.py`, `agentic/tests/test_entity_command_db.py`, `agentic/tests/test_status_command.py`

## Durable Findings

- The packaged local CLI now exposes three scriptable KB surfaces from `agentic/src/agentic_kb/cli.py`: `search <query>`, `status --json`, and `entity get <entity_type> <id>`; no second CLI module is needed.
- The accepted local search UX is intentionally generic and registry-driven: `--entity-type` can be repeated, `--filter key=value` can be repeated, and the command passes one `SearchRequest` into the existing `PostgresSearchStore` instead of duplicating search semantics in `argparse`.
- The durable JSON contract for `search --json`, `status --json`, and successful `entity get --json` is one stdout JSON object with stable top-level keys and stderr-only human errors. A shared serializer boundary needs to normalize enums, dates, timestamps, UUID/path-like values, mappings, and lists so the three commands stay aligned.
- Generic entity inspection works cleanly by reusing the task-204 registry metadata for `table_name` and `primary_key_column`, then issuing one exact-id `SELECT *` query with quoted identifiers. That keeps the command generic across current searchable tables without introducing per-entity lookup code.
- The durable failure contract for `entity get` is: invalid `entity_type` -> exit code `2`; valid-but-missing row -> exit code `4`; both failure classes emit no stdout JSON even when `--json` is passed and instead write concise diagnostics to stderr.
- Isolated DB-backed verification should wait for ParadeDB health before running command tests. A fresh Compose project is reliable, but immediate test startup can fail with connection refusal while the DB container is still in `health: starting`.

## Verification Notes

- `python3 -m py_compile` passed for all touched command modules and tests.
- Focused unit suites passed for `test_search_command.py`, `test_entity_command.py`, and `test_status_command.py`.
- Isolated DB-backed command suites passed for `test_search_command_db.py`, `test_entity_command_db.py`, and `test_status_command_db.py` using `docker compose -p task503-verify -f docker-compose.agentic.yml ...` with `AGENTIC_DB_PORT=5761`.
- Local help/smoke checks passed for `python3 -m agentic_kb.cli --help`, `python3 -m agentic_kb.cli search --help`, `python3 -m agentic_kb.cli entity --help`, and `python3 -m agentic_kb.cli status --help`.
- Packaged smoke checks passed for `kb-tools search ... --json`, `kb-tools entity get ... --json`, and `kb-tools status --json` against the isolated DB after using an actually seeded row id for the entity smoke probe.

## No New Research Beyond Task Scope

- Task-503 did not add MCP behavior, sync orchestration, snapshot freshness logic, or broader ranking changes. The durable outcome is the local CLI contract and its script-facing JSON/error semantics.
