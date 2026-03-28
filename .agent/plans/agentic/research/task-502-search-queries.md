# Task 502 Search Queries Research

- Date: 2026-03-28
- Task: `task-502`
- Evidence: `agentic/src/agentic_kb/search/query.py`, `agentic/tests/test_search_query.py`, `agentic/tests/test_search_query_db.py`, live ParadeDB probes against `docker-compose.agentic.yml`

## Durable Findings

- The pinned ParadeDB image accepts BM25 query predicates in the form `<indexed_text_column> @@@ %s` with `paradedb.score(id)` used in both `SELECT` and `ORDER BY`; for this task, registry-driven BM25 search is implemented as an `OR` across the entity's configured text columns, followed by a candidate-id join back to the base table where typed and prefix filters run as ordinary SQL predicates.
- Supported non-BM25-budget filters, especially `project_items.size`, must stay out of the BM25 predicate itself. The accepted contract is `BM25 candidate ids -> join base table -> apply SQL filters -> final order/limit`, which keeps project-item typed filters effective without pretending they are indexed BM25 fields.
- The pinned vector-query contract works with standard pgvector cosine distance syntax: `embedding <=> %s::vector` ordered ascending, with deterministic tie-breaking on the stable row id. Tests use injected `384`-dimension unit vectors and never call live Ollama.
- Hybrid ranking is implemented with Reciprocal Rank Fusion using a fixed constant `k = 60`. The merge key is `(entity_type, id)`, ties break by higher fused score, then better best modality rank, then better individual modality ranks, then stable `(entity_type, id)` ordering.
- Deterministic DB-backed verification needs a fresh isolated ParadeDB volume or an already bootstrapped task-202+ schema. A stale long-lived local `paradedb_data` volume can lack the `agentic` schema even when the current SQL files are correct, so task verification should use an isolated Compose project name as planned.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/search/query.py" "agentic/src/agentic_kb/search/__init__.py" "agentic/tests/test_search_query.py" "agentic/tests/test_search_query_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_query.py'` passed locally.
- Deterministic DB-backed local verification passed with a temporary editable venv plus `AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@127.0.0.1:5758/agentic_kb /tmp/task502-venv/bin/python -m unittest discover -s agentic/tests -p 'test_search_query_db.py'` against an isolated fresh ParadeDB stack.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- In-container verification passed with `docker compose -p agentic-task-502 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_search_query.py'` and the matching `test_search_query_db.py` run.

## No New Research Beyond Task Scope

- No CLI search commands, MCP handlers, live Ollama verification, or schema/index changes were required. The durable outcome is the packaged query contract, accepted pinned-image SQL syntax, deterministic embedding seam, and hybrid-ranking behavior.
