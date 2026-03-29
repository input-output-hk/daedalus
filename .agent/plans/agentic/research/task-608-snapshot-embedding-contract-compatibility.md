# Task 608 Snapshot Embedding-Contract Compatibility Research

- Date: 2026-03-29
- Task: `task-608`
- Evidence: `agentic/src/agentic_kb/snapshot_manifest.py`, `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/tests/test_snapshot_manifest_schema.py`, `agentic/tests/test_snapshot_command.py`, `agentic/tests/test_snapshot_command_db.py`, `agentic/tests/test_status_command.py`, `agentic/tests/test_status_command_db.py`, `agentic/tests/test_sync_command.py`, `agentic/tests/test_sync_command_db.py`, `agentic/tests/test_mcp_search_server.py`, `.agent/plans/agentic/task-plans/task-608.md`, `.agent/plans/agentic/task-plans/task-608-impl-review.md`

## Durable Findings

- Snapshot compatibility is now enforced as an explicit three-field contract, not a loose model-name check. The required fields are `contract_id`, `embedding_model`, and `embedding_dimension`.
- Runtime contract construction is centralized in `snapshot_manifest.py` and derives vector dimensionality from the shipped embedding runtime constant (`EXPECTED_EMBEDDING_DIMENSION`) instead of introducing a new duplicate constant.
- The canonical root manifest schema and the packaged runtime schema copy both require `embedding_contract` with `additionalProperties: false`. Persisted manifests are treated with the same strictness at runtime, including rejection of unexpected extra keys.
- Legacy manifests that only contain top-level `embedding_model` are intentionally unsupported for compatibility-sensitive flows. Import rejects them before restore, `status` reports them as `unsupported_legacy_manifest`, and `sync changed` refuses to extend them.
- Status and post-import sync decisions must use only the latest imported `agentic.kb_snapshot_manifest` row (`imported_at IS NOT NULL`). Export-only rows remain provenance but do not drive current-KB compatibility decisions.
- Local-only KBs with no imported snapshot history remain supported. `status` reports `missing_imported_snapshot_metadata`, while `sync changed` does not invent a new snapshot requirement for those KBs.
- Malformed persisted compatibility metadata is not silently coerced. Wrong types, missing values, or extra `embedding_contract` keys are surfaced as invalid/unavailable metadata rather than normal compatibility comparisons.

## Constraints And Gotchas

- Compatibility checks are intentionally exact. A mismatch in contract identifier, embedding model, or vector dimensionality is a hard failure for import and for post-import incremental sync.
- Import safety depends on validation order. The shipped path validates manifest structure and dump identity, enforces disposable-target safety, then checks embedding compatibility before any destructive restore step.
- Top-level `status.ok` still means readiness, not compatibility. Scripts or operators that care about snapshot extension safety must inspect the separate `embedding_compatibility` object.
- MCP `kb_status` inherits that payload shape, so downstream consumers now see the embedding compatibility object alongside readiness and freshness data.

## Failed Or Rejected Approaches

- Permissive coercion of persisted manifest fields with `str()` and `int()` was rejected because it let schema-invalid stored manifests pass through runtime compatibility checks.
- Treating export-only snapshot manifest rows as the current compatibility source was rejected because it could misreport local non-imported KBs and ignore the actual imported baseline.
- Allowing extra `embedding_contract` keys in persisted manifests was rejected because it broke parity with the schema's `additionalProperties: false` rule.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/src/agentic_kb/commands/snapshot.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/commands/sync.py" "agentic/tests/test_snapshot_manifest_schema.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py" "agentic/tests/test_status_command.py" "agentic/tests/test_status_command_db.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py" "agentic/tests/test_mcp_search_server.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_command.py'` passed.
- DB-backed coverage was implemented for snapshot import compatibility rejection plus DB-backed status/sync compatibility inspection, but those suites were skipped in this environment because `AGENTIC_TEST_DATABASE_URL` was not set.

## Review History Note

- The implementation review log contains an append-only sequencing repair entry. `Implementation: Iteration 4` documents that `Code Review: Iteration 3` had been appended before `Code Review: Iteration 2`; no source-code changes were made in that repair step.
