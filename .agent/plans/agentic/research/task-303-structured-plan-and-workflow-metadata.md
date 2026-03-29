# Task 303 Structured Plan And Workflow Metadata Research

- Date: 2026-03-29
- Task: `task-303`
- Evidence: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/search/config.py`, `agentic/src/agentic_kb/search/query.py`, `agentic/src/agentic_kb/mcp/search_server.py`, `agentic/tests/test_docs_ingest.py`, `agentic/tests/test_search_config.py`, `agentic/tests/test_search_query_db.py`, `agentic/tests/test_mcp_search_server.py`, `agentic/tests/test_mcp_search_server_db.py`

## Durable Findings

- Structured docs metadata stayed on the existing packaged docs-ingest path in `agentic/src/agentic_kb/ingest/docs.py`; no separate metadata module or schema migration was needed.
- Workflow docs under `.agent/workflows/**/*.md` now extract `workflow_description` only from leading YAML front matter. The shipped rule is intentionally narrow: if the file does not start with a bounded `--- ... ---` front matter block containing a simple `description:` scalar, no workflow description metadata is emitted.
- Canonical task-plan detection is path-based and intentionally excludes adjacent review logs: only `.agent/plans/agentic/task-plans/task-*.md` files that do not end in `-plan-review.md` or `-impl-review.md` are treated as canonical task plans.
- Canonical task-plan docs now persist stable metadata keys in `kb_documents.metadata`: `task_id`, `title`, `planning_status`, `build_status`, and `plan_type = canonical_task_plan`.
- Adjacent review logs now use the same narrow path-based classification seam without pretending to be canonical task plans: `-plan-review.md` maps to `plan_type = plan_review_log` and `-impl-review.md` maps to `plan_type = implementation_review_log`.
- Task-301 baseline metadata remains intact on every docs row (`relative_path`, `file_size_bytes`, `source_group`, `title_source`, `title_from_h1`), and task-302 chunking/task-304 unchanged-doc behavior remained unchanged because structured metadata is extracted once per source file and copied onto every prepared chunk draft deterministically.
- The searchable filter seam stayed explicit and docs-scoped. `agentic/src/agentic_kb/search/config.py` now defines docs-only filters for `task_id`, `planning_status`, `build_status`, and `plan_type`; it does not expose generic JSON-path filtering.
- Metadata-backed filtering is implemented through a static accessor seam in `agentic/src/agentic_kb/search/query.py` that compiles allowlisted docs filters to `base.metadata ->> '<key>'`. Callers cannot supply arbitrary metadata paths or SQL fragments.
- Workflow descriptions remain stored metadata and searchable context only for this task. No typed `workflow_description` filter was added, and the tracker wording was reconciled to match that narrower shipped contract.
- Because MCP search tool schemas hardcode per-surface filter allowlists, task-303 also updated `agentic/src/agentic_kb/mcp/search_server.py` so the generic `search` tool and docs-only `search_docs` tool accept the same new canonical-task-plan filter vocabulary.

## Verification Notes

- Focused unit coverage now verifies workflow front matter extraction, canonical task-plan metadata extraction, review-log exclusion/classification, docs filter vocabulary, and MCP docs-filter schema alignment.
- DB-backed search coverage verifies the new canonical-task-plan metadata filters against real `agentic.kb_documents` rows when `AGENTIC_TEST_DATABASE_URL` is available.

## Out Of Scope

- No generic metadata filter engine was introduced.
- No workflow-description typed filter was shipped.
- No changes were made to task-302 markdown chunk boundaries or task-304 unchanged-doc skip semantics.
