# Task 504 Search Quality Fixtures Research

- Date: 2026-03-29
- Task: `task-504`
- Evidence: `agentic/config/search-fixtures.yaml`, `agentic/tests/search_quality_helpers.py`, `agentic/tests/test_search_quality_fixtures.py`, `agentic/pyproject.toml`

## Durable Findings

- The accepted canonical fixture schema is a small repo-local YAML document with `version: 1` and a `fixtures` list. Each fixture uses `id`, `query_text`, `mode`, optional `limit`, optional registry-backed `filters`, optional `query_embedding_key`, and `expectations` expressed only as `top_hit`, `ordered_prefix`, and `contains_unordered`.
- Fixture loading should stay test-local for now. Keeping the loader in `agentic/tests/search_quality_helpers.py` avoids expanding packaged runtime surface while still validating the real packaged search contracts through `PostgresSearchStore` and `SearchRequest`.
- Deterministic `vector` and `hybrid` verification works cleanly by injecting fixed `384`-dimension query embeddings keyed by fixture metadata. The fixture suite should reject missing or unknown embedding keys during load rather than falling through to runtime embedding-provider behavior.
- Fixture validation is more reliable when it checks the active registry/search contract up front, not just YAML shape. Running each fixture through `SearchRequest` plus `select_search_entity_configs()` catches unsupported filters and incompatible entity/filter combinations before DB execution.
- BM25 CLI parity can stay narrow and deterministic by running the existing `search` command only for BM25 fixtures against the seeded corpus. That avoids live Ollama dependence on the CLI path while still proving the command matches direct search-store results.
- `PyYAML` needs to be an explicit `agentic` dependency for this task. The shell already had `yaml` installed, but relying on incidental availability would make isolated verification environments flaky.
- DB-backed seeded-corpus reset helpers need to be foreign-key-safe even when the fixture corpus only seeds parent tables. In the bootstrapped schema, `agentic.kb_github_issues` has FK children such as `agentic.kb_github_issue_comments`, so `TRUNCATE` must include `CASCADE` or explicitly include dependent tables or the fixture suite can fail before any ranking assertions run.
- Real DB-backed execution is still necessary even with a carefully seeded corpus. The first authoritative run showed that the hybrid cross-entity fixture's intended result set needed an explicit `entity_type` scope and a corrected `top_hit`; local unit discovery alone would not have surfaced that ranking mismatch because the DB-backed class was skipped there.

## Verification Notes

- `python3 -m py_compile "agentic/tests/search_quality_helpers.py" "agentic/tests/test_search_quality_fixtures.py" "agentic/tests/test_search_query_db.py"` passed.
- `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'` passed locally, with DB-backed tests skipped because `AGENTIC_TEST_DATABASE_URL` was unset for that invocation.
- Authoritative packaged DB-backed verification passed with `AGENTIC_DB_PORT=36047 docker compose -p task504-impl2 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'` after fixing FK-safe truncation and correcting the hybrid fixture expectation.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_query.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_command.py'` passed.

## No New Research Beyond Task Scope

- No new CLI verbs, MCP changes, sync behavior, or ranking-formula changes were required. The durable outcome is the accepted fixture schema, test-local loader boundary, deterministic embedding injection rule, and BM25-only CLI parity approach.
