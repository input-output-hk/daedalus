Implementation: Iteration 1
Timestamp: 2026-03-29T00:00:00Z
Outcome: implemented

- Changes made: added deterministic workflow/task-plan metadata extraction to `agentic/src/agentic_kb/ingest/docs.py`; added explicit docs-only metadata-backed search filters for canonical task-plan fields in `agentic/src/agentic_kb/search/config.py` and `agentic/src/agentic_kb/search/query.py`; updated MCP docs filter allowlists/schemas in `agentic/src/agentic_kb/mcp/search_server.py`; expanded focused ingest/search/MCP tests; updated the task tracker, canonical task doc, and added the required durable research note.
- Files touched: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/search/config.py`, `agentic/src/agentic_kb/search/query.py`, `agentic/src/agentic_kb/mcp/search_server.py`, `agentic/tests/test_docs_ingest.py`, `agentic/tests/test_search_config.py`, `agentic/tests/test_search_query_db.py`, `agentic/tests/test_mcp_search_server.py`, `agentic/tests/test_mcp_search_server_db.py`, `.agent/plans/agentic/knowledge-base-platform-tasks.json`, `.agent/plans/agentic/research/task-303-structured-plan-and-workflow-metadata.md`, `.agent/plans/agentic/task-plans/task-303.md`, `.agent/plans/agentic/task-plans/task-303-impl-review.md`.
- Verification run: passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest`, `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_config`, `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_command`, `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_mcp_search_server`, and `python3 -m py_compile "agentic/src/agentic_kb/ingest/docs.py" "agentic/src/agentic_kb/search/config.py" "agentic/src/agentic_kb/search/query.py" "agentic/src/agentic_kb/mcp/search_server.py" "agentic/tests/test_docs_ingest.py" "agentic/tests/test_search_config.py" "agentic/tests/test_search_query_db.py" "agentic/tests/test_mcp_search_server.py" "agentic/tests/test_mcp_search_server_db.py"`; DB-backed `agentic.tests.test_search_query_db` and `agentic.tests.test_mcp_search_server_db` skipped cleanly in the current environment because `AGENTIC_TEST_DATABASE_URL` is not available.
- Deviations from the approved plan: none so far. Workflow `description` was intentionally kept as stored metadata/search context only, with tracker wording reconciled instead of adding a typed filter.

Code Review: Iteration 1
Timestamp: 2026-03-29T18:31:24Z
Outcome: requires_changes

- High: `agentic/src/agentic_kb/ingest/docs.py:740-753` still decides unchanged-doc skips from `content_hash` alone, and `content_hash` is computed from `source_path`, `chunk_index`, and chunk content only. Because the new structured fields are stored in `metadata` and are not part of that comparison, a KB indexed before task-303 will keep skipping unchanged canonical plans/workflows and never backfill `task_id`, `planning_status`, `build_status`, `plan_type`, or `workflow_description`. That violates the approved rollout requirement to make the new metadata/filter surface actually available while preserving task-304 semantics only when both content and extracted metadata are unchanged.
- Medium: `agentic/tests/test_docs_ingest.py` does not cover the migration/backfill path above. A regression test is needed to prove that previously indexed docs rows without structured metadata are rewritten once, while truly unchanged post-task-303 rows still skip.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-03-29T18:40:00Z
Outcome: review_fix_applied

- Changes made: fixed the docs unchanged-skip seam in `agentic/src/agentic_kb/ingest/docs.py` so stored rows are compared by both deterministic chunk `content_hash` and persisted `metadata`, which forces a one-time rewrite for legacy pre-task-303 rows that lack the new structured metadata and preserves task-304 skipping once content and extracted metadata both match; added a regression test proving one-time backfill plus subsequent skip behavior.
- Files touched: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/tests/test_docs_ingest.py`, `.agent/plans/agentic/task-plans/task-303.md`, `.agent/plans/agentic/task-plans/task-303-impl-review.md`.
- Verification run: passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest` and `python3 -m py_compile "agentic/src/agentic_kb/ingest/docs.py" "agentic/tests/test_docs_ingest.py"`.
- Deviations from the approved plan: none. The fix stayed narrow to docs-ingest backfill behavior and preserved task-304 semantics when both content and extracted metadata are unchanged.

Code Review: Iteration 2
Timestamp: 2026-03-29T18:34:39Z
Outcome: approved

- Verified the backfill fix in `agentic/src/agentic_kb/ingest/docs.py`: stored document versions now include persisted `metadata`, and `_document_drafts_match_versions(...)` compares both deterministic chunk `content_hash` and normalized metadata. That closes the rollout gap for legacy pre-task-303 docs rows while preserving task-304 skip behavior after backfill.
- Verified regression coverage in `agentic/tests/test_docs_ingest.py`: `test_ingest_docs_backfills_missing_structured_metadata_once_then_skips` exercises the exact one-time rewrite path for a legacy canonical task-plan row missing structured metadata, then confirms a subsequent rerun skips without re-embedding once metadata matches.
- No remaining mismatches found against the approved plan in the touched ingest/backfill seam.

Decision: approved
