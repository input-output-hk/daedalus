# Task 601 Snapshot Manifest Format Research

- Date: 2026-03-29
- Task: `task-601`
- Evidence: `agentic/config/snapshot-manifest.schema.json`, `agentic/config/snapshot-manifest.example.json`, `agentic/src/agentic_kb/snapshot_manifest.py`, `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/src/agentic_kb/sync/state.py`, `agentic/schema/init.sql`, `agentic/tests/test_snapshot_manifest_schema.py`

## Durable Findings

- The canonical external contract now lives at `agentic/config/snapshot-manifest.schema.json`, with `agentic/config/snapshot-manifest.example.json` as the checked-in v1 fixture that downstream export/import work should mirror.
- Manifest v1 is locked to `schema_version = 1` and requires the stable top-level object shape approved in planning: `$schema`, `schema_version`, `snapshot_name`, `snapshot_created_at`, `artifact`, `repo`, `embedding_model`, `entity_counts`, and `sync_state`.
- Artifact identity is now verification-grade and intentionally portable: `artifact.filename` must be a basename-only sibling dump filename, `dump_format` is fixed to `postgresql_custom`, `compression` is fixed to `{ "algorithm": "gzip", "level": 6 }` to match the current `pg_dump --format=custom --compress=6` behavior from `task-205`, `size_bytes` is an exact integer byte count, and `content_hash` is required.
- The v1 digest representation is explicitly locked to algorithm-prefixed lowercase hexadecimal `sha256:<64 hex chars>`. This is the durable external mapping target for `agentic.kb_snapshot_manifest.content_hash`, so `task-602` can compute and compare one deterministic representation without inventing another hash field or algorithm contract.
- Repo baseline is normalized into public manifest fields instead of exposing raw DB rows: `repo.name`, `repo.docs_commit_hash`, and `repo.code_commit_hash` capture the docs/code ingestion baselines without forcing importers to reinterpret internal sync-state storage.
- Entity counts are now explicitly locked to the current seven searchable KB entities and reject extras: `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.
- Exported sync-state is also normalized into manifest-friendly public fields and excludes transient operator history like `last_error`: docs/code expose `repo_commit_hash` plus `last_synced_at`, GitHub exposes one nested watermark record per current stream, and Project 5 exposes `owner`, `number`, `cursor`, and `updated_at_watermark`.
- The narrow helper in `agentic/src/agentic_kb/snapshot_manifest.py` captures the approved constants plus sync-state normalization rules for later `task-602` wiring without adding any manifest-aware export/import behavior in this task.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/tests/test_snapshot_manifest_schema.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.

## No New Research Beyond Task Scope

- Task-601 still does not add manifest-aware snapshot export/import behavior, DB writes to `agentic.kb_snapshot_manifest`, or runtime manifest-vs-dump enforcement; those remain `task-602` implementation work.
