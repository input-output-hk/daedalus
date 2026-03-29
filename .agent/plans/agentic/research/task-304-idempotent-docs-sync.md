# Task 304 Idempotent Docs Sync Research

- Date: 2026-03-29
- Task: `task-304`
- Evidence: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/src/agentic_kb/sync/state.py`, `agentic/tests/test_docs_ingest.py`, `agentic/tests/test_sync_command.py`, `agentic/tests/test_sync_command_db.py`

## Durable Findings

- Docs idempotency stayed narrowly scoped to allowlisted docs ingestion and docs sync orchestration; code, GitHub, and project sync behavior were not broadened.
- The durable unchanged-doc comparison contract now reuses the shipped task-302 row hash scheme directly. A docs path is unchanged only when the newly prepared chunk set matches stored rows exactly by `(source_path, chunk_index, content_hash)`.
- Pre-embedding skip detection is now mandatory in the shipped path. Docs ingest first prepares chunk drafts and deterministic hashes without embeddings, compares them to stored rows, and only requests embeddings for `updated_paths`.
- Skipped unchanged docs remain content-versioned in `agentic.kb_documents`: their stored `repo_commit_hash` and `source_updated_at` are intentionally left untouched, while repo-scoped docs sync state in `agentic.kb_sync_state` can still advance to the latest sync head commit.
- Explicit `sync docs` and incremental docs sync now distinguish candidate discovery from actual work performed. `candidate_paths` tracks considered docs, `updated_paths` tracks rewritten docs, `skipped_paths` tracks unchanged docs skipped before embedding, and `deleted_paths` tracks removals.
- Deleted docs remain a separate path from unchanged-doc skipping. Explicit docs sync still prunes allowlisted paths no longer discovered, and incremental docs sync still deletes docs removed by the repo delta.

## Verification Notes

- Focused docs ingest coverage now proves unchanged reruns are skipped before embedding and do not rewrite stored row metadata.
- Sync command coverage now proves explicit and incremental docs sync report `updated_paths` and `skipped_paths` separately and leave skipped row metadata unchanged while docs sync-state advances.
- DB-backed docs sync coverage was extended so unchanged incremental docs candidates can be verified at the actual SQL seam when `AGENTIC_TEST_DATABASE_URL` is available.
