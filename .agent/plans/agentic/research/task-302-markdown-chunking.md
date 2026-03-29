# Task 302 Markdown Chunking Research

- Date: 2026-03-29
- Task: `task-302`
- Evidence: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/tests/test_docs_ingest.py`, `agentic/tests/test_sync_command.py`, `agentic/tests/test_sync_command_db.py`

## Durable Findings

- The shipped markdown chunking implementation stayed in `agentic/src/agentic_kb/ingest/docs.py`; no standalone chunker module was needed.
- V1 chunking is deliberately narrow and ATX-heading-based only. It splits on `#` through `######` headings, keeps heading-less docs as one row, and preserves any body text before the first section heading as an intro chunk when present.
- ATX-looking lines inside fenced code blocks are ignored during heading parsing, so command comments and examples do not become synthetic sections in stored docs metadata.
- The first H1 remains the document `title` when it appears at the top of the file. That title heading is excluded from stored `heading_path`, so section hierarchy describes navigation beneath the document title instead of repeating it on every chunk.
- Stored docs rows now use deterministic ids and hashes per chunk: `id = docs:{source_path}#{chunk_index}` and `content_hash = sha256(source_path + NUL + chunk_index + NUL + chunk_content)`.
- `section_title` and `subsection_title` are derived from `heading_path[0]` and `heading_path[1]`; deeper nesting remains only in `heading_path`.
- Docs writes now use one atomic replacement seam per affected `source_path` set via `replace_documents_for_paths(...)`. That path is reused by library ingest, explicit `sync docs`, and incremental `_sync_docs_changed(...)`, so shrinking reruns remove stale higher chunk indexes without risking an empty document on a failed re-ingest.
- Docs result and sync-state metadata intentionally preserve file-unique `source_paths` while `processed_count` now reflects chunk-row count.

## Constraints And Limitations

- Setext headings and non-ATX markdown constructs are still out of scope for v1 because the current allowlisted corpus did not require a broader parser.
- Chunking changes stored row boundaries only at heading seams. Oversized sections still rely on the existing embedding-only segmentation fallback and are not split into smaller stored rows.

## Verification Notes

- Focused docs ingest and sync tests were expanded to cover heading chunking, nested heading paths, intro chunks, shrinking reruns, unique sync metadata, and failure-safe atomic replacement semantics.
- DB-backed sync coverage was updated so task-302 paths can verify that shrinking reruns leave only the current chunk rows for a source path when the optional database environment is available.
