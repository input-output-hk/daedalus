# Task 204 Search Config Registry Research

- Date: 2026-03-28
- Task: `task-204`
- Evidence: `agentic/src/agentic_kb/search/config.py`, `agentic/src/agentic_kb/search/__init__.py`, `agentic/tests/test_search_config.py`, `agentic/schema/create_indexes.sql`

## Durable Findings

- The stable public v1 entity vocabulary for downstream search work is the plural table-aligned set: `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.
- `entity_type` is a registry-level global filter, not a SQL column filter. It should narrow candidate entity configs before later query code builds per-entity SQL.
- `source_domain` is durable registry metadata for grouping entities by `docs`, `code`, `github`, and `project`, but it is not a universal shared DB column. Downstream code should read it from the registry config rather than assuming every table stores it.
- Path filtering should stay explicit and prefix-based in v1: docs use `source_path_prefix` backed by `source_path`, and code uses `repo_path_prefix` backed by `repo_path`. This keeps task-502 semantics clear without overcommitting to glob or regex matching.
- Project-item `size` is a supported typed filter because it is a stable typed column from task-404, but it is not part of the accepted task-203 BM25 column budget for `kb_project_items`. The registry must preserve that distinction so later query code does not assume every supported filter is BM25-backed.
- BM25-column parity is important enough to test directly: `agentic/tests/test_search_config.py` now locks all seven entity configs to the settled task-203 index definitions so registry/index drift fails focused tests early.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/search/config.py" "agentic/src/agentic_kb/search/__init__.py" "agentic/tests/test_search_config.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_config.py'` passed locally.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_search_config.py'` passed.

## No New Research Beyond Task Scope

- No query execution, ranking behavior, CLI UX, or MCP behavior was added here. The durable outcome is the registry/filter contract and its alignment with the existing schema and index definitions.
