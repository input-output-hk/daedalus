# Task Plan: task-202 Create core knowledge tables

- Task ID: `task-202`
- Title: `Create core knowledge tables`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-202` is the next unblocked critical-path task after `task-201`, which already created the `agentic` schema, enabled `pg_search` and `vector`, seeded `agentic.kb_schema_migrations`, and wired `agentic/schema/init.sql` into the ParadeDB first-boot path.
- Later critical-path work depends on these tables existing before it can add indexes, search config, sync state handling, or ingestion logic: `task-203`, `task-204`, `task-205`, `task-301`, `task-401`, `task-403`, `task-404`, and `task-405`.
- The current `kb-tools` CLI intentionally does not inspect schema readiness yet, so the schema itself must now become concrete and reviewable through SQL before later CLI and ingest tasks build on it.

## Scope

- Extend `agentic/schema/init.sql` with the first real KB entity tables under schema `agentic`.
- Add the core tables named in the platform design: `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, `kb_github_pr_comments`, `kb_project_items`, `kb_sync_state`, and `kb_snapshot_manifest`.
- Define practical v1 column shapes for those tables so later ingestion, search, sync, and snapshot tasks can write into them without redesigning the schema.
- Keep the SQL idempotent and safe to re-run manually against an already initialized local database, because the current bootstrap path is still first-boot oriented.

## Non-Goals

- Do not add BM25 indexes, HNSW indexes, or other search indexes; that remains `task-203`.
- Do not add search config registry tables, filter registries, or query-layer behavior; that remains `task-204` and later search tasks.
- Do not add `kb-tools` status, snapshot, or migration-runner behavior; that remains `task-205` and later tasks.
- Do not implement embeddings, search queries, sync logic, document chunking, code chunking, GitHub ingestion, project ingestion, or snapshot export/import behavior; those remain `task-301+`, `task-401+`, `task-403+`, `task-404+`, `task-501+`, and `task-601+`.
- Do not change `docker-compose.agentic.yml` unless implementation discovers a real bootstrap defect; task-201 already mounted `agentic/schema/init.sql` into the ParadeDB init path.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-101` - established the Compose stack and ParadeDB service contract in `docker-compose.agentic.yml`
  - `task-103` - established the `kb-tools` image, CLI contract, runtime mounts, and the fact that schema status is not yet surfaced through the CLI
  - `task-201` - created `agentic/schema/init.sql`, enabled `pg_search` and `vector`, created schema `agentic`, and seeded `agentic.kb_schema_migrations`
- Direct downstream tasks unblocked by this work:
  - `task-203`, `task-204`, `task-205`
  - `task-301`, `task-401`, `task-403`, `task-404`, `task-405`
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-201-schema-foundation.md`
  - `.agent/plans/agentic/task-plans/task-201.md`
  - `agentic/schema/init.sql`
  - `docker-compose.agentic.yml`
  - `agentic/README.md`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/commands/status.py`

## Files Expected To Change

- `agentic/schema/init.sql` - extend the bootstrap SQL with the core KB tables and supporting constraints

## Implementation Approach

- **Bootstrap path**: keep `agentic/schema/init.sql` as the single schema bootstrap file. Continue using fully qualified `agentic.` table names and do not alter the long-lived ParadeDB `search_path`; task-201 already established that the base image should keep owning runtime `search_path` behavior.
- **Idempotent SQL**: use `CREATE TABLE IF NOT EXISTS` and `ON CONFLICT DO NOTHING` style patterns where seeding is necessary. Do not add any standalone `CREATE INDEX` statements in `task-202`; only the implicit indexes created by primary-key and unique constraints are permitted in this task. The file must remain re-runnable with `psql -f /docker-entrypoint-initdb.d/999-agentic-init.sql` against a running local DB after a database has already been initialized from the task-202-era file.
- **Schema-version contract**: `task-201` already owns migration row `version = 1`. This task must append exactly one new seed row to `agentic.kb_schema_migrations` with `version = 2` and `description = 'create core knowledge tables'`, using `ON CONFLICT (version) DO NOTHING`. Fresh bootstrap with the updated `init.sql` must yield rows `1` and `2` exactly once each. Manual apply against a true task-201-era database must preserve the existing version-`1` row and add version `2` exactly once. Re-running the updated SQL after either path must not duplicate either row.
- **Stable IDs**: give each KB entity table a deterministic `TEXT PRIMARY KEY` rather than introducing new UUID extensions. This matches the platform requirement for stable ids and lets future ingestion tasks compute repeatable ids in application code.
- **Metadata shape**: prefer explicit typed columns for fields already promised by the platform design, plus `JSONB NOT NULL DEFAULT '{}'::jsonb` or `JSONB NOT NULL DEFAULT '[]'::jsonb` for flexible metadata that will vary by source.
- **Embedding columns**: add nullable vector columns only on tables that will become searchable content entities: documents, code chunks, GitHub parent items, GitHub comments, and project items. Do not add vector indexes yet. The exact vector declaration must match what the pinned ParadeDB image accepts during implementation.
- **Timestamps**: include insert-default bookkeeping timestamps `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()` and `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`, but do not imply trigger-driven updates. In v1, `updated_at` is only auto-filled on insert; later writers must set it explicitly when they modify a row. Keep source-watermark timestamps such as `source_updated_at`, `source_created_at`, and source-specific fields like `repo_commit_hash` separate from those local bookkeeping columns.
- **GitHub source boundaries**: keep issue comments and PR comments distinct in schema shape rather than deciding that during implementation. `kb_github_issue_comments` stores comments for GitHub issues only. `kb_github_pr_comments` stores both top-level PR issue/discussion comments and inline review comments, with an explicit `comment_type` discriminator and a simple schema-level guardrail that only allows `issue_comment` or `review_comment`, so later ingest tasks do not need a table redesign and cannot silently persist unexpected values.
- **Sync-state contract**: make `kb_sync_state` a minimal but concrete generic ledger for later sync work. Use named cursor/watermark columns instead of placeholders so later tasks can store opaque cursors, text high-water marks, timestamps, repo commit hashes, and schema versions without redefining the table.
- **Minimal relational guardrails**: add primary keys, sensible unique constraints on natural source identifiers, and foreign keys from comment tables to their parent issue/PR rows. Do not add triggers, generated columns, or search-specific helper tables in this task.

### Table Shape Expectations

- `agentic.kb_documents`
  - Purpose: markdown/docs/plans/workflows/SOPs/READMEs chunk storage.
  - Required columns: stable `id`, `source_domain`, `doc_kind`, `source_path`, `title`, nullable section/heading metadata, `chunk_index`, `content`, `preview_text`, `content_hash`, nullable `repo_commit_hash`, nullable source file timestamp/watermark, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(source_path, chunk_index)`.
- `agentic.kb_code_chunks`
  - Purpose: symbol-aware or structured code chunks.
  - Required columns: stable `id`, `repo_path`, `language`, nullable `symbol_name`, nullable `symbol_kind`, nullable parent/container symbol metadata, `chunk_index`, `start_line`, `end_line`, `content`, `preview_text`, `content_hash`, nullable `repo_commit_hash`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(repo_path, chunk_index)`.
- `agentic.kb_github_issues`
  - Purpose: issue parent entities.
  - Required columns: stable `id`, `repo`, `issue_number`, nullable `github_node_id`, `title`, `state`, nullable `author_login`, `labels` JSONB array, `body_text`, `preview_text`, `html_url`, `source_created_at`, `source_updated_at`, nullable `source_closed_at`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(repo, issue_number)`.
- `agentic.kb_github_issue_comments`
  - Purpose: searchable comments for GitHub issues only; PR discussion comments do not belong here.
  - Required columns: stable `id`, `issue_id` referencing `agentic.kb_github_issues(id)`, `repo`, `issue_number`, `github_comment_id`, nullable `github_node_id`, nullable `author_login`, `body_text`, `preview_text`, `html_url`, `source_created_at`, `source_updated_at`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; foreign key to `kb_github_issues`; unique natural key on `(repo, github_comment_id)`.
- `agentic.kb_github_prs`
  - Purpose: pull request parent entities.
  - Required columns: stable `id`, `repo`, `pr_number`, nullable `github_node_id`, `title`, `state`, nullable `author_login`, nullable `base_branch`, nullable `head_branch`, `labels` JSONB array, `body_text`, `preview_text`, `html_url`, `source_created_at`, `source_updated_at`, nullable `source_closed_at`, nullable `source_merged_at`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(repo, pr_number)`.
- `agentic.kb_github_pr_comments`
  - Purpose: PR comments stored separately from PR parent rows, covering both top-level PR issue/discussion comments and inline review comments.
  - Required columns: stable `id`, `pr_id` referencing `agentic.kb_github_prs(id)`, `repo`, `pr_number`, `comment_type` (`issue_comment` or `review_comment`), `github_comment_id`, nullable `github_node_id`, nullable `author_login`, nullable review context columns `repo_path`, `commit_oid`, `original_commit_oid`, `diff_hunk`, `line`, `original_line`, `side`, `start_line`, `start_side`, `body_text`, `preview_text`, `html_url`, `source_created_at`, `source_updated_at`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; foreign key to `kb_github_prs`; unique natural key on `(repo, comment_type, github_comment_id)`; schema-level check constraint enforcing `comment_type IN ('issue_comment', 'review_comment')`.
- `agentic.kb_project_items`
  - Purpose: GitHub Project 5 item metadata and field values.
  - Required columns: stable `id`, `project_owner`, `project_number`, `project_item_node_id`, nullable `content_type`, nullable `content_id`, nullable `content_node_id`, `title`, `body_text`, nullable `repo`, nullable `status`, nullable `priority`, nullable `size`, nullable `work_type`, nullable `area`, nullable `phase`, nullable `kb_impact`, nullable `start_date`, nullable `target_date`, `field_values` JSONB, nullable `html_url`, `source_updated_at`, nullable `embedding`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `project_item_node_id`; keep `(project_owner, project_number)` as descriptive source scope, not the uniqueness contract.
- `agentic.kb_sync_state`
  - Purpose: sync cursors, repo commit hashes, schema-related watermarks, and task-level sync timestamps for later `task-405` and `task-701` work.
  - Required columns: stable `id`, `source_name`, `scope_key`, nullable `repo_commit_hash`, nullable `cursor_text`, nullable `watermark_text`, nullable `watermark_timestamp`, nullable `schema_version`, nullable `last_attempted_at`, nullable `last_succeeded_at`, nullable `last_error`, `metadata`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(source_name, scope_key)`; use a normalized non-null `scope_key` such as `global` for singleton streams rather than relying on nullable uniqueness behavior.
- `agentic.kb_snapshot_manifest`
  - Purpose: metadata rows describing exported/imported KB snapshots, not the dump payload itself.
  - Required columns: stable `id`, `snapshot_name`, `schema_version`, `snapshot_created_at`, nullable `repo_commit_hash`, nullable `embedding_model`, `entity_counts` JSONB, `github_watermarks` JSONB, `manifest` JSONB, nullable `source_path`, nullable `content_hash`, nullable `imported_at`, `created_at`, `updated_at`.
  - Constraints: primary key on `id`; unique natural key on `(snapshot_name, snapshot_created_at)`.

### Existing-Volume Handling

- PostgreSQL init scripts in `/docker-entrypoint-initdb.d/` still run only on first database initialization. Updating `agentic/schema/init.sql` will not modify an already populated `paradedb_data` volume by itself.
- Because a migration runner is explicitly out of scope here, the task should support two operator paths and document both in implementation notes:
  - `fresh volume` - the primary bootstrap path for clean validation, using a new Compose project name or `down -v`
  - `manual apply to existing local volume` - run the same SQL file explicitly with `psql -f /docker-entrypoint-initdb.d/999-agentic-init.sql` against a running `paradedb` container whose database was bootstrapped from the task-201-era schema
- The existing-volume verification must be a real upgrade-path test from a task-201-era schema state: schema `agentic`, extensions installed, `kb_schema_migrations` containing only version `1`, no core KB tables yet, and runtime `search_path` still `public, paradedb` as verified in task-201. Re-running the updated SQL against a database that already booted from the updated file is useful only for idempotence, not for upgrade validation.
- The SQL therefore must be safe to re-run manually and must not rely on first-boot-only side effects beyond file mounting.

## Acceptance Criteria

- `agentic/schema/init.sql` creates all nine planned core KB tables under schema `agentic` and seeds exactly one new `agentic.kb_schema_migrations` row for `version = 2` with description `create core knowledge tables`, while preserving the existing version-`1` bootstrap row from `task-201`.
- The schema includes `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, `kb_github_pr_comments`, `kb_project_items`, `kb_sync_state`, and `kb_snapshot_manifest`.
- Each table has a stable primary key and the concrete source-specific columns listed in this plan closely enough that later tasks can proceed without schema redesign.
- `kb_sync_state` uses the concrete v1 columns in this plan: `source_name`, normalized `scope_key`, `cursor_text`, `watermark_text`, `watermark_timestamp`, `repo_commit_hash`, `schema_version`, attempt/success timestamps, and error/metadata fields.
- Searchable content tables include nullable embedding columns, but no BM25, HNSW, or other standalone indexes are created; the only indexes permitted in `task-202` are the implicit indexes backing primary-key and unique constraints.
- Comment tables reference their parent issue/PR tables with foreign keys.
- `kb_github_pr_comments.comment_type` is protected by a schema-level constraint that permits exactly `issue_comment` and `review_comment`.
- Natural uniqueness is enforced where the source identity is already clear, including `(source_path, chunk_index)`, `(repo_path, chunk_index)`, `(repo, issue_number)`, `(repo, github_comment_id)` for issue comments, `(repo, comment_type, github_comment_id)` for PR comments, and `project_item_node_id` for project items.
- Fresh bootstrap against the updated file yields migration rows `1` and `2` exactly once each.
- Manual apply against a true task-201-era database yields the nine new tables, preserves migration row `1`, adds migration row `2` exactly once, and leaves runtime `search_path` unchanged.
- The SQL remains idempotent when re-run manually against an existing initialized local database after either bootstrap path.
- No migration runner, schema-aware CLI status behavior, snapshot command behavior, ingestion logic, or search registry tables are added.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the existing Compose wiring still resolves cleanly.
- Validate fresh-volume bootstrap with an isolated project name, for example `AGENTIC_DB_PORT=5647 docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml up -d paradedb`.
- Account for the known first-boot ParadeDB restart from `task-201`: do not trust the first healthy signal alone. Wait until `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT 1;"` succeeds after the restart handoff.
- List the schema tables with `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'agentic' ORDER BY table_name;"` and confirm the version table plus the nine new KB tables exist.
- Inspect the migration ledger on the fresh-volume DB with `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT version, description FROM agentic.kb_schema_migrations ORDER BY version;"` and confirm rows `1` / `initial kb schema bootstrap` and `2` / `create core knowledge tables` each appear exactly once.
- Inspect table columns with `information_schema.columns`, for example `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT table_name, column_name, data_type FROM information_schema.columns WHERE table_schema = 'agentic' ORDER BY table_name, ordinal_position;"`, and confirm the expected column families from this plan are present.
- Inspect foreign keys with `pg_constraint` or `information_schema.table_constraints` and confirm `kb_github_issue_comments` references `kb_github_issues` and `kb_github_pr_comments` references `kb_github_prs`.
- Inspect check constraints with `pg_constraint` and confirm `kb_github_pr_comments.comment_type` is restricted to exactly `issue_comment` and `review_comment`.
- Confirm search scope boundaries by querying index metadata and verifying the new tables have no standalone indexes at all; only the implicit primary-key/unique indexes created by constraints may exist, and there must be no BM25 or HNSW search indexes yet.
- Confirm runtime `search_path` remains untouched on the fresh-volume DB with `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT current_setting('search_path') AS search_path;"` and verify it is still `public, paradedb`.
- Before editing `agentic/schema/init.sql`, save a temporary local copy of the task-201-era SQL so upgrade verification can start from the real predecessor schema instead of from the updated file.
- Create a separate isolated upgrade-test stack whose init mount points at that saved task-201-era SQL copy, not at the live updated `agentic/schema/init.sql`. After boot, verify `agentic.kb_schema_migrations` contains only version `1`, verify `information_schema.tables` lists only `kb_schema_migrations` in schema `agentic`, and verify `current_setting('search_path')` is still `public, paradedb`.
- Apply the updated task-202 SQL to that already-bootstrapped upgrade-test DB from a different path/input than the mounted predecessor file, so the command cannot accidentally re-run the old `/docker-entrypoint-initdb.d/999-agentic-init.sql`. For example, feed the current repo file over stdin with `docker compose -p agentic-task-202-upgrade -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb < agentic/schema/init.sql`, or copy the updated file into a separate temporary path first. Then re-run the table-listing, migration-ledger, and `search_path` checks. Confirm the nine new KB tables now exist, rows `1` and `2` each exist exactly once in `agentic.kb_schema_migrations`, and runtime `search_path` is still `public, paradedb`.
- Re-run the same updated task-202 SQL input one more time against the same upgrade-test DB and confirm it succeeds without creating duplicate tables or duplicate migration rows; treat this as the idempotence check, not the upgrade-path check.
- Tear down the isolated verification stack with `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- The pinned ParadeDB image definitely provides the `vector` extension, but the exact column declaration the image accepts for future embedding data should be verified during implementation and captured in the task research note.
- `kb_snapshot_manifest` needs to be useful for `task-601` and `task-602` without prematurely locking the final manifest JSON shape; the plan keeps both explicit core fields and a flexible JSONB payload to reduce redesign risk.
- The existing-volume story remains manual until a real migration/apply mechanism exists. That is acceptable for this task, but implementation notes must be explicit so developers do not expect `docker compose up` alone to retrofit old local volumes.
- For PR comments, later ingest work must normalize GitHub API responses into the agreed `comment_type` plus `github_comment_id` contract so both discussion comments and review comments land in the same table without identity collisions, while matching the schema-level `comment_type` constraint.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when `task-202` is completed.
- Add a research note under `.agent/plans/agentic/research/` for durable findings such as accepted vector column syntax, useful constraint choices, and any first-boot/manual-apply caveats discovered during implementation.
- Only update `.agent/plans/agentic/knowledge-base-platform.md`, `.agent/workflows/agentic-kb.md`, or `agentic/README.md` if implementation reveals a durable design or workflow change beyond the already documented bootstrap and manual-apply boundary.

## Implementation Notes

- Extended `agentic/schema/init.sql` in place so it remains the single bootstrap/manual-apply SQL entrypoint established by `task-201`.
- Added the nine planned core tables under schema `agentic`: `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, `kb_github_pr_comments`, `kb_project_items`, `kb_sync_state`, and `kb_snapshot_manifest`.
- Used deterministic `TEXT PRIMARY KEY` ids across all entity tables and added the planned natural-key uniqueness constraints for documents, code chunks, GitHub parent rows, GitHub comments, project items, sync state, and snapshot manifests.
- Added nullable `VECTOR(384)` columns on the searchable content tables only: documents, code chunks, GitHub issues, GitHub issue comments, GitHub PRs, GitHub PR comments, and project items. Verification against the pinned ParadeDB image confirmed `VECTOR(384)` is accepted for this phase.
- Added the planned schema guardrails without crossing into later search work: foreign keys from issue comments to issues and PR comments to PRs, plus the `kb_github_pr_comments.comment_type` check constraint restricting values to `issue_comment` and `review_comment`.
- Preserved the task-201 manual-apply boundary: existing initialized volumes are not upgraded by `docker compose up` alone; they require explicit re-application of `agentic/schema/init.sql` with `psql` until a future migration runner exists.
- Seeded exactly one additional migration ledger row with `version = 2` and description `create core knowledge tables` using `ON CONFLICT (version) DO NOTHING`, leaving version `1` ownership unchanged.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` succeeded before verification.
- Saved a temporary copy of the task-201-era `agentic/schema/init.sql` from `HEAD` to `/tmp/agentic-task-202-task201-init.sql` and used a temporary compose file at `/tmp/agentic-task-202-compose-task201.yml` so upgrade verification started from the real predecessor schema instead of the updated file.
- `AGENTIC_DB_PORT=5647 docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml up -d paradedb` failed because host port `5647` was already allocated; after tearing that stack down, fresh-volume verification succeeded with `AGENTIC_DB_PORT=5747`.
- `docker compose -p agentic-task-202-fresh -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT 1;"` succeeded after the known first-boot restart handoff.
- Fresh-volume checks confirmed all expected task-202 tables exist in schema `agentic`, migration rows `1` and `2` each exist exactly once, `current_setting('search_path')` remains `public, paradedb`, the planned column families are present, the two comment-table foreign keys exist, and the only check constraint is the PR-comment `comment_type` guardrail.
- Fresh-volume index inspection via `pg_indexes` showed only the implicit primary-key and unique-constraint indexes; no BM25, HNSW, or other standalone search indexes were created.
- Upgrade-path verification booted a separate stack from `/tmp/agentic-task-202-compose-task201.yml` with `AGENTIC_DB_PORT=5748`, confirmed the predecessor state contained only `agentic.kb_schema_migrations` with version `1`, and confirmed `search_path` was still `public, paradedb` before applying the updated SQL.
- Manual apply against that true task-201-era database using `docker compose -p agentic-task-202-upgrade -f /tmp/agentic-task-202-compose-task201.yml exec -T paradedb psql -U agentic -d agentic_kb < agentic/schema/init.sql` succeeded, created the nine new tables, added migration row `2` exactly once, and preserved row `1` and runtime `search_path`.
- Re-running the same manual-apply command against the same upgrade-test DB succeeded with only `already exists` notices and left migration rows `1` and `2` at count `1` each, confirming idempotence.

## Outcome

- `task-202` implementation is complete for the approved scope: the core KB schema now exists under `agentic`, supports both fresh bootstrap and manual apply from a real task-201 predecessor state, and leaves search indexes, ingestion logic, migration-runner behavior, and CLI feature work for later tasks.
- Build-loop iteration 2 closed the remaining review-required follow-ups by updating the machine-readable tasks JSON and adding the task-202 research note for durable implementation findings and verification caveats.

## Review Outcome

- Final code review result: clean.
- Build-loop iteration 1 found only documentation/tracking follow-ups, which build-loop iteration 2 resolved by updating `.agent/plans/agentic/knowledge-base-platform-tasks.json` and adding `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`.
- No additional project metadata updates were required for this task beyond keeping the local task plan, tasks JSON, and research notes synchronized.

## Planning Status Rationale

- This plan is implementation-ready because it stays within the existing `init.sql` bootstrap path, defines the task-202 migration-ledger contract, names concrete v1 columns and uniqueness rules for the core tables, resolves the GitHub comment and project-item identity boundaries, adds the explicit schema-level guardrail for PR `comment_type`, clarifies non-trigger `updated_at` semantics, and explicitly distinguishes fresh bootstrap, true task-201-to-task-202 upgrade verification, and post-upgrade idempotence checks.
- The remaining uncertainties are implementation details to verify in the pinned ParadeDB image rather than blockers to starting the task.
