Planner: Iteration 1
Timestamp: 2026-03-29T17:06:15Z
Outcome: initial_plan_documented

- Created the canonical `task-302` plan around repo reality in `agentic/src/agentic_kb/ingest/docs.py` rather than the stale tracker target `agentic/src/ingest/markdown_chunker.py`.
- Locked the task scope to minimal heading-boundary chunking for allowlisted markdown docs, preserving the task-301 contracts for source discovery, repo-relative paths, deterministic ids/hashes, and existing embedding constraints.
- Called out the main implementation-critical gap in the current docs ingestor: upsert-only writes are not enough once per-file chunk counts can shrink, so the implementation must replace rows per `source_path` rather than only upserting by `(source_path, chunk_index)`.
- Kept the boundary with neighboring tasks explicit: no task-303 structured metadata extraction, no task-304 generalized unchanged-doc skip logic, and no assumption that a new markdown chunker module is required.

Critiquer: Iteration 1
Timestamp: 2026-03-29T17:22:00Z
Outcome: requires_plan_changes

- The plan correctly identifies stale tracker drift and the need to replace per-file rows when chunk counts shrink, but it is still too soft about where that replacement must happen. In current repo reality, both explicit `sync docs` and incremental `_sync_docs_changed` bypass `ingest_docs(...)` and call `prepare_documents(...)` plus `docs_store.upsert_documents(...)` directly (`agentic/src/agentic_kb/commands/sync.py:242-248`, `agentic/src/agentic_kb/commands/sync.py:920-927`). The implementation section should be tightened from optional sync changes to an explicit requirement that all docs write paths use one source-path replacement seam.
- The replacement seam also needs to be atomic, not just logically equivalent. Current `PostgresDocsStore.delete_documents_for_paths(...)` and `upsert_documents(...)` each open their own transaction (`agentic/src/agentic_kb/ingest/docs.py:555-653`), so a delete-then-upsert sequence split across calls can leave a document temporarily or permanently empty on failure. The plan should require either a single `replace_documents_for_paths(...)` store helper or another one-transaction mechanism reused by `ingest_docs`, `sync docs`, and `sync changed`.
- Verification scope is missing the repo's real sync surfaces. Unit coverage in `agentic/tests/test_docs_ingest.py` is necessary but not sufficient because sync-state metadata is built separately in `agentic/src/agentic_kb/sync/state.py:155-179` and explicit/changed docs sync each construct their own result payloads. Add acceptance and verification language for chunked `sync docs` and `sync changed`: file-unique `source_paths`, `processed_count` intentionally meaning chunk rows, stale higher `chunk_index` removal on shrinking reruns, and correct sync-state metadata after changed-only docs updates.
- Docs/tracking updates should be expanded slightly. The plan already requires the task tracker fix and a task-302 research note, but current shipped workflow docs describe current docs-sync behavior in `.agent/workflows/agentic-kb.md`. Because `status` and sync summaries surface docs metadata from `build_docs_sync_state(...)`, the implementation should also refresh operator-facing docs if `processed_count` semantics change from file count to chunk count so the workflow does not become stale immediately after task-302 lands.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T17:11:17Z
Outcome: critique_addressed

- Revised the canonical `task-302` plan so source-path replacement is now an explicit implementation requirement across every docs write path that matters in current repo reality: `ingest_docs(...)`, `ingest_docs_from_config(...)`, explicit `sync docs`, and incremental `_sync_docs_changed(...)`.
- Tightened replacement semantics from a loose delete-or-equivalent note into a required one-transaction replacement seam. The plan now requires a shared atomic helper such as `replace_documents_for_paths(...)` or an equivalent centralized transaction boundary, and explicitly rejects split `delete_documents_for_paths(...)` plus `upsert_documents(...)` sequencing because it can leave a document empty on failure.
- Added failure-safety requirements to the implementation and acceptance sections: replacement must occur only after new chunk rows are fully prepared, old rows must remain intact if chunk preparation, embedding, or insert work fails, and no docs write path may keep its own ad hoc delete-then-upsert flow after task-302 lands.
- Expanded verification to cover the repo's real sync surfaces and metadata contracts, including explicit `sync docs` and `_sync_docs_changed(...)`, stale higher-`chunk_index` removal on shrinking reruns, unique file-path metadata, and `processed_count` intentionally meaning chunk rows rather than file count.
- Added the operator-doc update trigger explicitly: if docs sync summaries or workflow wording become stale because docs `processed_count` now reflects chunk rows, `.agent/workflows/agentic-kb.md` must receive the narrow task-owned wording update alongside the tracker and research-note updates.

Critiquer: Iteration 2
Timestamp: 2026-03-29T17:29:00Z
Outcome: approved

- Re-read against current repo reality, the revised plan is now adequately constrained and still minimal. It keeps task-302 centered on heading-boundary chunking inside `agentic/src/agentic_kb/ingest/docs.py`, preserves the task-301 allowlist and embedding contract, and does not broaden into task-303 metadata extraction or task-304 unchanged-content skip logic.
- The earlier blocking gaps are now addressed explicitly in the canonical plan: all relevant docs write paths are covered, atomic replacement is required rather than implied, failure safety is specified, and verification now includes the real sync paths plus sync-state metadata semantics.
- The remaining tracker/workflow notes are appropriately scoped as implementation follow-through rather than missing planning work. No additional plan changes are required before implementation.

Decision: approved
