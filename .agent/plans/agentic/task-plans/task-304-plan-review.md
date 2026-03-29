Planner: Iteration 1
Timestamp: 2026-03-29T18:25:00Z
Outcome: initial_plan_documented

- Created the first canonical `task-304` plan around current repo reality in `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/commands/sync.py`, and `agentic/src/agentic_kb/sync/state.py` instead of the stale tracker target `agentic/src/ingest/base.py`.
- Scoped the task narrowly to unchanged-doc skip behavior for allowlisted docs, explicitly building on the existing task-302 deterministic per-chunk `content_hash` contract already stored in `agentic.kb_documents`.
- Kept docs sync-state changes intentionally narrow: the plan treats `kb_documents` as the default source of per-path hash truth and only allows a small summary metadata addition in `kb_sync_state` if implementation proves it is necessary.
- Explicitly excluded scope creep into code ingestion idempotency, GitHub/project sync behavior, generalized hashing infrastructure, and per-document watermark redesign.
- Called out the main likely implementation seam: current docs preparation computes embeddings immediately, so task-304 may need a small two-phase refactor in `docs.py` to compare chunk hashes before embedding unchanged docs.

Critiquer: Iteration 1
Timestamp: 2026-03-29T17:30:21Z
Outcome: requires_changes

- The plan leaves the core no-reembed guarantee as a risk/open question instead of a required implementation step. In current repo reality, `prepare_documents(...)` always embeds while preparing rows in `agentic/src/agentic_kb/ingest/docs.py`, and both explicit and incremental docs sync call it before any comparison in `agentic/src/agentic_kb/commands/sync.py`. The plan should explicitly require a two-phase docs preparation/comparison path (or equivalent pre-embed hash-preparation helper) so unchanged docs are proven skipped before any embedding call.
- The unchanged-skip design does not resolve row-metadata staleness. Current docs rows persist `repo_commit_hash` and `source_updated_at`, and normal upserts refresh both fields. If unchanged paths are skipped entirely, those row fields stay at the old commit/mtime while repo-scoped docs sync state advances to the new head commit, creating inconsistent freshness signals. The plan should decide whether those row fields are intentionally content-versioned and allowed to remain stale, or require a metadata-only refresh path or downstream contract note.
- Result semantics and verification are underspecified for incremental docs sync. Current changed-sync output derives `updated=` from `len(result["changed_paths"])`, so adding skip logic without redefining `changed_paths` versus `updated_paths`/`skipped_paths` will misreport work performed. The plan should lock down the returned/result metadata shape and add tests that prove both no embedding calls and no row rewrite for unchanged reruns, including the DB-backed path.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T18:40:00Z
Outcome: plan_revised_for_critique

- Revised the canonical `task-304` plan to make pre-embedding unchanged detection mandatory, requiring a two-phase docs preparation/comparison path or equivalent helper so unchanged docs are skipped before any embedding call.
- Resolved skipped-doc row metadata semantics by defining `agentic.kb_documents.repo_commit_hash` and `source_updated_at` as content-versioned fields that remain unchanged for skipped paths even when repo-scoped docs sync state advances.
- Locked down result semantics for explicit and incremental docs sync so `updated_paths`, `skipped_paths`, and `deleted_paths` reflect actual work performed rather than candidate discovery, and expanded acceptance/verification language to require no embedding calls, no row rewrites, and no misreporting for unchanged paths.

Critiquer: Iteration 2
Timestamp: 2026-03-29T18:55:00Z
Outcome: approved

- The revised plan now makes pre-embedding skip detection a hard requirement instead of a possible implementation detail. The required execution sequence explicitly loads stored chunk hash state, prepares deterministic chunk metadata and `content_hash` values without embeddings, partitions `updated_paths` versus `skipped_paths`, and only then requests embeddings for `updated_paths`.
- The plan now adequately constrains skipped-row metadata semantics versus repo sync-state advancement. It explicitly defines `agentic.kb_documents.repo_commit_hash` and `source_updated_at` as content-versioned row metadata that remain unchanged for skipped paths, while `agentic.kb_sync_state` may advance independently as the repo-scoped sync record.
- Result/reporting semantics are now sufficiently explicit. The plan defines `updated_paths`, `skipped_paths`, and `deleted_paths` by actual work performed, forbids conflating candidate discovery with reported updates, and adds verification requirements for counts/path lists plus no-embed/no-rewrite behavior in both explicit and incremental docs sync.
- DB-backed verification and scope boundaries are also clear enough now. The verification section requires real DB validation at the SQL seam when the new read helper is added, and the scope remains tightly limited to allowlisted docs ingestion/sync without expanding into broader sync-state redesign or non-doc idempotency work.

Decision: approved
