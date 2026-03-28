# Task 404 Project Ingestion Research

- Date: 2026-03-28
- Task: `task-404`
- Evidence: `agentic/src/agentic_kb/ingest/project.py`, `agentic/tests/test_project_ingest.py`, `.agent/workflows/agentic-kb.md`

## Durable Findings

- The accepted Project 5 ingestor now lives at `agentic/src/agentic_kb/ingest/project.py` and uses GitHub GraphQL over the Python standard-library HTTP stack; no `gh` binary or extra GraphQL dependency was needed inside `kb-tools`.
- The library-first public surface for project ingestion is `ProjectFetchBounds`, `ProjectItemsPage`, `ProjectPageWriteResult`, `ProjectIngestResult`, `iter_project_item_pages`, `write_project_item_page`, `ingest_project_items`, and `ingest_project_items_from_config`, mirroring the reusable page/result contracts adopted in `task-403`.
- Canonical Project 5 single-select fields map by exact field name into dedicated columns: `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, and `KB Impact`; `Start date` and `Target date` map into the schema date columns and all field values are also preserved in `field_values` with raw type metadata.
- The ingestor treats nested `fieldValues` pagination as a loud failure: if GitHub reports `fieldValues.pageInfo.hasNextPage`, ingestion raises `ProjectFieldPaginationError` instead of silently truncating project metadata.
- `content_id` is populated only for linked `DripDropz/daedalus` issues and pull requests using the deterministic ids from `task-403`; draft issues and linked content from other repositories keep `content_id` null while preserving repo, url, node id, and content timestamps in row metadata.
- Searchable `body_text` is derived from project-item title, linked content or draft body, and a canonical field-summary block so project metadata remains searchable even before task-502 adds filter-aware project search.
- Project ingestion needs a `GITHUB_TOKEN` that can read organization ProjectV2 data for `DripDropz` Project 5; repo-only issue/PR access is not enough, so the workflow doc now states that token-scope requirement explicitly.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/ingest/project.py" "agentic/src/agentic_kb/ingest/__init__.py" "agentic/tests/test_project_ingest.py"` passed.
- Local unit tests passed: `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_project_ingest.py'`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- In-container unit tests passed: `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_project_ingest.py'`.
- Live GitHub Project smoke verification did not run in this implementation session because `GITHUB_TOKEN` was not available with project-read scope in the current environment.
