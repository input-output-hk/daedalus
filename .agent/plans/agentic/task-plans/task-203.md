# Task Plan: task-203 Create BM25 and HNSW indexes

- Task ID: `task-203`
- Title: `Create BM25 and HNSW indexes`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-203` is the next unblocked critical-path schema/search task after `task-202`, which deliberately stopped at tables, constraints, and embedding columns without adding any standalone search indexes.
- `task-502` cannot deliver the planned BM25/vector/RRF query layer until the searchable tables have stable ParadeDB BM25 and pgvector ANN index contracts.
- This is also the right point to settle the fresh-bootstrap vs existing-volume behavior for search indexes while the schema is still small and the Docker init-script model is still simple.

## Scope

- Add ParadeDB BM25 indexes and pgvector HNSW indexes for every currently searchable KB entity table.
- Establish the canonical SQL entrypoint for standalone search-index creation at `agentic/schema/create_indexes.sql`.
- Ensure fresh database bootstrap applies the index SQL automatically under the existing Docker init-script model.
- Ensure an already initialized task-202-era local DB can be upgraded manually by applying the standalone index SQL file directly.
- Define stable index names and per-table index contracts that later search/query tasks can rely on.

## Non-Goals

- Do not add new tables, alter the core entity table shapes, or redesign the `agentic` schema; that remains owned by `task-202` unless implementation finds a hard blocker.
- Do not add search config registry code, filter registries, or query-building logic; that remains `task-204` and `task-502`.
- Do not add CLI migration/status behavior, migration-runner infrastructure, or automatic retrofit behavior for existing DB volumes; that remains later operational work.
- Do not add indexes for non-searchable operational tables such as `agentic.kb_sync_state` or `agentic.kb_snapshot_manifest`.
- Do not widen scope into ingestion changes beyond documenting the stable index-name and distance-operator contract future tasks should assume.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-101` - established the Compose-managed ParadeDB bootstrap path in `docker-compose.agentic.yml`
  - `task-201` - enabled `pg_search` and `vector` in `agentic/schema/init.sql`
  - `task-202` - created the searchable KB tables and intentionally left standalone search indexes for this task
  - `task-301`, `task-401`, `task-403`, `task-404` - now define the actual searchable entity shapes and filter-relevant columns for docs, code, GitHub, and Project items
  - `task-405` - established that existing initialized volumes still need explicit manual apply behavior until a real migration runner exists
  - `task-501` - confirmed the embedding contract remains `VECTOR(384)`
- Direct downstream tasks unblocked by this work:
  - `task-204` - search config registry can point at stable indexed columns and entity contracts
  - `task-502` - hybrid BM25/vector/RRF queries can assume one BM25 index and one vector ANN index per searchable table
  - `task-503`, `task-701`, `task-801` - search/status/agent surfaces can build on the finalized index layout
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-401-typescript-symbol-chunking.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-404-project-ingestion.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/task-plans/task-202.md`
  - `.agent/plans/agentic/task-plans/task-301.md`
  - `.agent/plans/agentic/task-plans/task-401.md`
  - `.agent/plans/agentic/task-plans/task-403.md`
  - `.agent/plans/agentic/task-plans/task-404.md`
  - `.agent/plans/agentic/task-plans/task-405.md`
  - `agentic/schema/init.sql`
  - `docker-compose.agentic.yml`
  - ParadeDB BM25 index docs confirming one BM25 index per table, mandatory `key_field`, and the need to include filter/sort columns in the BM25 index definition
  - pgvector docs confirming HNSW index syntax and `vector_cosine_ops` support for `vector(384)` embeddings

## Files Expected To Change

- `agentic/schema/create_indexes.sql` - new canonical standalone SQL for BM25 and HNSW index creation plus migration-row seeding for this phase
- `agentic/schema/init.sql` - keep as the single bootstrap entrypoint and add the task-203 delegation hook that sources `create_indexes.sql` during fresh bootstrap
- `docker-compose.agentic.yml` - mount `create_indexes.sql` where `init.sql` can source it during first boot without relying on a second independently auto-executed schema entrypoint

## Implementation Approach

- **Task ownership vs bootstrap entrypoint**: keep `agentic/schema/create_indexes.sql` as the task-owned source of truth for index DDL and migration-row `version = 3`, but preserve `agentic/schema/init.sql` as the single bootstrap entrypoint established by `task-201` and `task-202`.
- **Fresh-bootstrap delegation**: add a clearly delimited task-203 include/delegation block at the end of `agentic/schema/init.sql` so fresh bootstrap still starts from one root SQL file. `docker-compose.agentic.yml` should mount `create_indexes.sql` at a path `init.sql` can source during first boot, and the mount strategy should avoid introducing a second independently auto-executed schema-entry file.
- **Existing-volume behavior**: keep manual apply explicit for already initialized DB volumes. The recommended upgrade path for a task-202-era database should be `psql -f agentic/schema/create_indexes.sql` against a running DB, while `docker compose up` alone must not be described as retrofitting old volumes.
- **Idempotent SQL**: use `CREATE INDEX IF NOT EXISTS` where supported and `INSERT ... ON CONFLICT DO NOTHING` for migration-row seeding so the standalone file is safe to re-run manually after fresh bootstrap or manual upgrade.
- **Schema migration ledger**: append exactly one new `agentic.kb_schema_migrations` row from the standalone index file with `version = 3` and a description aligned to this task, for example `create bm25 and hnsw indexes`, using `ON CONFLICT (version) DO NOTHING`. Fresh bootstrap through `init.sql` should yield rows `1`, `2`, and `3`; manual apply against a real task-202-era DB should add only row `3`.
- **Searchable-table set**: create indexes only for the seven tables that currently hold searchable content and already have nullable `embedding VECTOR(384)` columns:
  - `agentic.kb_documents`
  - `agentic.kb_code_chunks`
  - `agentic.kb_github_issues`
  - `agentic.kb_github_issue_comments`
  - `agentic.kb_github_prs`
  - `agentic.kb_github_pr_comments`
  - `agentic.kb_project_items`
- **BM25 contract**: create exactly one ParadeDB BM25 index per searchable table because ParadeDB allows only one BM25 index per table. Use the table primary key `id` as the first indexed column and `key_field='id'` for every BM25 index.
- **BM25 column selection rule**: narrow v1 to core search text plus the smallest already-committed scalar filters from the current schema and ingestion contracts. Defer wider timestamp/date/line-number/JSONB coverage until `task-204` and `task-502` prove it is necessary, because ParadeDB allows only one BM25 index per table.
- **Vector contract**: create one partial HNSW index per searchable table on `embedding` using `vector_cosine_ops` and `WHERE embedding IS NOT NULL`. This matches the current embedding shape and gives task-502 a stable cosine-distance ANN contract without assuming vectors are pre-normalized.
- **Stable naming contract**: use one predictable naming scheme future tasks can rely on for diagnostics, verification, and operator notes:
  - BM25: `<table_name>_bm25_idx`
  - HNSW: `<table_name>_embedding_hnsw_idx`
  Query code in `task-502` should depend on table/operator semantics rather than index names for correctness, but tests and status tooling may assert these names.
- **Per-table BM25 intent**: keep the indexed column sets explicit in the SQL so later tasks do not have to reverse-engineer what v1 search/filter coverage means:
  - `kb_documents`: `id`, `source_domain`, `doc_kind`, `source_path`, `title`, `preview_text`, `content`
  - `kb_code_chunks`: `id`, `repo_path`, `language`, `symbol_name`, `symbol_kind`, `preview_text`, `content`
  - `kb_github_issues`: `id`, `repo`, `issue_number`, `state`, `title`, `preview_text`, `body_text`
  - `kb_github_issue_comments`: `id`, `repo`, `issue_number`, `preview_text`, `body_text`
  - `kb_github_prs`: `id`, `repo`, `pr_number`, `state`, `title`, `preview_text`, `body_text`
  - `kb_github_pr_comments`: `id`, `repo`, `pr_number`, `comment_type`, `repo_path`, `preview_text`, `body_text`
  - `kb_project_items`: `id`, `project_owner`, `project_number`, `content_type`, `repo`, `status`, `priority`, `work_type`, `area`, `phase`, `kb_impact`, `title`, `body_text`
- **No duplicated ownership**: do not duplicate the index DDL body inside `init.sql`. `init.sql` should delegate to `create_indexes.sql` for fresh bootstrap, and `create_indexes.sql` should remain directly runnable for task-scoped manual apply and verification.

## Acceptance Criteria

- `agentic/schema/create_indexes.sql` exists and is the canonical standalone SQL file for `task-203` index creation.
- `agentic/schema/init.sql` remains the single bootstrap entrypoint for fresh DB initialization and delegates to `create_indexes.sql` for the task-203 index phase.
- Fresh bootstrap applies the task-203 index SQL automatically through the updated `init.sql` path without requiring a second manual command.
- Manual apply against a real task-202-era DB volume using `agentic/schema/create_indexes.sql` succeeds and is documented as the supported upgrade path for pre-existing local volumes.
- `agentic.kb_schema_migrations` gains exactly one new row for this task's index phase, and that row is added exactly once on fresh bootstrap or manual apply.
- Each searchable table has exactly one BM25 index and exactly one partial HNSW vector index after task-203 lands.
- No standalone search indexes are created for non-searchable operational tables such as `agentic.kb_sync_state` or `agentic.kb_snapshot_manifest`.
- HNSW indexes target `embedding` with `vector_cosine_ops` and exclude null embeddings.
- BM25 indexes use `id` as the first indexed column with `key_field='id'` and cover only the narrowed core-text plus minimal scalar-filter columns documented in this plan.
- Stable index names follow the documented naming convention so later verification and search tasks can reference them predictably.
- The SQL is idempotent when re-run manually after either bootstrap path.
- The task does not add query logic, search registry code, CLI migration/status features, or schema redesign outside the index/bootstrap boundary.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the updated bootstrap mount wiring resolves cleanly.
- Validate fresh bootstrap with an isolated project name and fresh volume, for example `AGENTIC_DB_PORT=5756 docker compose -p agentic-task-203-fresh -f docker-compose.agentic.yml up -d paradedb`, then wait for the known ParadeDB first-boot restart handoff until `psql` succeeds.
- On the fresh-volume DB, verify `agentic.kb_schema_migrations` contains versions `1`, `2`, and `3` exactly once each.
- Verify fresh bootstrap still flows through `agentic/schema/init.sql` as the single root schema entrypoint and that `create_indexes.sql` is sourced once through that path rather than acting as a second independent bootstrap entry file.
- Inspect index metadata on the fresh-volume DB via `pg_indexes` plus `pg_class`/`pg_am` joins and confirm:
  - the seven searchable tables each have one BM25 index with the expected name
  - the seven searchable tables each have one HNSW index with the expected name
  - the access methods are `bm25` and `hnsw`
  - no standalone BM25/HNSW indexes exist for `kb_sync_state` or `kb_snapshot_manifest`
- Verify the BM25 definitions include `key_field='id'` and the HNSW definitions use `vector_cosine_ops` plus `WHERE embedding IS NOT NULL`.
- Before editing live SQL, save a temporary task-202-era copy of the predecessor bootstrap file so upgrade validation starts from a real pre-task-203 schema state.
- Start a separate isolated upgrade-test stack whose init mount points only at that saved task-202-era `init.sql`, confirm it has the task-202 tables and migration rows `1` and `2`, and confirm it has no standalone BM25/HNSW indexes yet.
- Apply `agentic/schema/create_indexes.sql` manually to that running upgrade-test DB, then re-run the migration and index metadata checks to confirm the standalone file alone adds the full task-203 index set plus migration row `3`.
- Re-run the same standalone index SQL a second time against the upgrade-test DB and confirm no duplicate indexes or migration rows are created; treat that as the idempotence check.
- Tear down the isolated verification stacks with `docker compose ... down -v` after validation.

## Risks / Open Questions

- **BM25 column breadth**: this revision intentionally narrows v1 to core text plus minimal scalar filters, but critique should still stress-test whether the selected per-table filter columns are the right minimum for task-204/task-502 without prematurely consuming the one-BM25-index-per-table budget.
- **Access-method compatibility on the pinned image**: the plan assumes the pinned ParadeDB/Postgres 18 image supports `USING bm25` and pgvector `USING hnsw ... vector_cosine_ops` exactly as documented, but implementation must verify the concrete SQL syntax on the repo's pinned image rather than trusting upstream docs alone.
- **Bootstrap delegation mechanics**: the plan now preserves `init.sql` as the single bootstrap entrypoint, but implementation still needs to choose the exact include/mount mechanics that let `init.sql` source `create_indexes.sql` once during first boot without turning it into a second independent bootstrap file.
- **Future query assumptions**: task-502 should use cosine-distance vector queries and table-level BM25 operators without depending on index-name internals for correctness. If critique sees a stronger contract needed for query/config code, that should be resolved before implementation.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when `task-203` is completed.
- Add a research note under `.agent/plans/agentic/research/` capturing the verified BM25/HNSW SQL syntax on the pinned image, the final indexed-column sets, the accepted naming convention, and the fresh-bootstrap/manual-apply caveats.
- Update `.agent/workflows/agentic-kb.md` or `agentic/README.md` only if implementation reveals a durable operator-facing change in how developers should bootstrap or manually apply KB index SQL.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-203-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-203-impl-review.md`

## Implementation Notes

- Added `agentic/schema/create_indexes.sql` as the standalone task-owned SQL for all task-203 BM25 and HNSW index DDL plus the migration-ledger seed for `version = 3` / `create bm25 and hnsw indexes`.
- Created exactly one ParadeDB BM25 index and one partial pgvector HNSW index for each currently searchable table: `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, `kb_github_pr_comments`, and `kb_project_items`.
- Kept the narrowed BM25 v1 contract from the approved plan: each BM25 index starts with `id`, uses `WITH (key_field = 'id')`, and includes only the approved core text plus minimal scalar filter columns for that table.
- Kept the vector contract stable for downstream search work: every ANN index uses `USING hnsw (embedding vector_cosine_ops) WHERE embedding IS NOT NULL`, matching the existing `VECTOR(384)` embedding shape without assuming pre-normalized vectors.
- Preserved `agentic/schema/init.sql` as the single fresh-bootstrap root entrypoint by adding a task-203 delegation block that sources `includes/create_indexes.task-203.sql` via `\ir` instead of duplicating index DDL inline.
- Updated `docker-compose.agentic.yml` to mount `agentic/schema/create_indexes.sql` under `/docker-entrypoint-initdb.d/includes/create_indexes.task-203.sql`, which keeps the file available for `init.sql` delegation during first boot without making it a second independently auto-executed root init script.
- Updated `.agent/workflows/agentic-kb.md` with the durable operator-facing note that fresh bootstrap flows through `init.sql`, while existing initialized DB volumes still require an explicit manual `psql -f agentic/schema/create_indexes.sql` apply.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` passed with the new `create_indexes.sql` mount wiring.
- Saved a task-202-era copy of `agentic/schema/init.sql` to `/tmp/agentic-task-203-task202-init.sql` and created `/tmp/agentic-task-203-compose-task202.yml` so the upgrade test booted from the real predecessor schema state rather than the live task-203 file.
- Fresh bootstrap verification used `AGENTIC_DB_PORT=5756 docker compose -p agentic-task-203-fresh -f docker-compose.agentic.yml up -d paradedb`, then waited for `psql` readiness. The resulting DB had migration rows `1`, `2`, and `3` exactly once each.
- Fresh bootstrap mount inspection via `ls -R /docker-entrypoint-initdb.d` inside the container showed `999-agentic-init.sql` at the root and `includes/create_indexes.task-203.sql` under a subdirectory, confirming `create_indexes.sql` was available only for `init.sql` delegation rather than as a second root init script.
- Fresh bootstrap index metadata checks via `pg_indexes` and `pg_class`/`pg_am` confirmed the seven searchable tables each have exactly one BM25 index and one HNSW index with the approved names, access methods `bm25` / `hnsw`, `key_field=id` in BM25 definitions, and `vector_cosine_ops` plus `WHERE embedding IS NOT NULL` in HNSW definitions.
- Fresh bootstrap checks also confirmed no BM25 or HNSW indexes exist on `agentic.kb_sync_state` or `agentic.kb_snapshot_manifest`, and `current_setting('search_path')` remained `public, paradedb`.
- Upgrade-path verification used `AGENTIC_DB_PORT=5757 docker compose -p agentic-task-203-upgrade -f /tmp/agentic-task-203-compose-task202.yml up -d paradedb`, confirmed the predecessor state had only migration rows `1` and `2` plus no standalone BM25/HNSW indexes, then applied `agentic/schema/create_indexes.sql` manually with `psql` against the running DB.
- After manual apply on the upgrade DB, the same migration and index metadata checks passed: row `3` was added exactly once, the full task-203 index set existed, and the operational tables still had zero search indexes.
- Re-running `agentic/schema/create_indexes.sql` against the upgrade DB produced only `already exists` notices plus `INSERT 0 0`, and the final migration counts remained `1`, `2`, and `3` exactly once each, confirming idempotence.

## Outcome

- `task-203` is complete for the approved scope: the KB schema now has stable BM25 and HNSW index contracts for all seven searchable entity tables, fresh bootstrap still flows through `agentic/schema/init.sql` as the single root entrypoint, and existing initialized DB volumes have a verified standalone manual-apply path through `agentic/schema/create_indexes.sql`.
- Downstream tasks can now rely on the documented naming convention, the narrowed BM25 column sets, the cosine-distance HNSW contract, and migration ledger row `version = 3` without widening scope into query logic, search registries, or migration-runner automation.

## Review Outcome

- Implementation completed without scope deviations from the approved canonical plan.
- Tracking and research artifacts were updated alongside the SQL changes so the local task plan, workflow note, research note, implementation review log, and machine-readable task registry stay aligned.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of the planning review explicitly approved this canonical plan before implementation.
- Build status is `completed` because the approved SQL/bootstrap wiring landed and passed fresh-bootstrap, real predecessor upgrade-path, and standalone idempotence verification against the pinned ParadeDB image.
