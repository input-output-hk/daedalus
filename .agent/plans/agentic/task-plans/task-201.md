# Task Plan: task-201 Create schema and version tables

- Task ID: `task-201`
- Title: `Create schema and version tables`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-201` is the next critical-path database task after the compose scaffold from `task-101` and the real `kb-tools` container from `task-103`.
- `task-202` cannot safely add knowledge entity tables until the stack has a durable namespace, extension bootstrap, and a migration/version record that later schema work can reference, even though existing volumes will still need an explicit upgrade story until real migration tooling exists.
- The current workflow already expects `docker compose -f docker-compose.agentic.yml up -d` to remain the primary bootstrap path, so the schema foundation should land before any ingest or search behavior is layered on top.

## Scope

- Add the first SQL bootstrap file under `agentic/schema/` for the knowledge-base database foundation.
- Initialize the database extensions required by the platform design but without creating searchable entity tables or indexes yet.
- Create a dedicated schema namespace for agentic KB objects rather than using `public`.
- Create and seed the schema-version tracking table that later migration tasks can read and extend.
- Wire the SQL bootstrap into the existing Compose-managed ParadeDB startup path.

## Non-Goals

- Do not add `kb_documents`, `kb_code_chunks`, GitHub tables, sync-state tables, snapshot-manifest tables, or any other core entity tables; that remains `task-202`.
- Do not add BM25 indexes, vector indexes, or search configuration; that remains `task-203` and later search tasks.
- Do not implement a general migration runner, `kb-tools` migration command, or schema-aware status command in this task.
- Do not change the MCP service, sync commands, snapshot commands, or wrapper scripts.
- Do not redesign the overall database model already documented in `.agent/plans/agentic/knowledge-base-platform.md`.

## Relevant Dependencies

- Completed dependencies:
  - `task-101` - compose scaffold and service contract in `docker-compose.agentic.yml`
  - `task-103` - built `kb-tools` image and stable container/runtime paths
- Direct downstream task unblocked by this work:
  - `task-202` - core knowledge tables
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `agentic/README.md`
  - `agentic/src/agentic_kb/commands/status.py`
  - `docker-compose.agentic.yml`

## Files Expected To Change

- `agentic/schema/init.sql` - new bootstrap SQL for extensions, schema namespace, and schema-version table
- `docker-compose.agentic.yml` - mount the bootstrap SQL into ParadeDB init flow

## Implementation Approach

- **Bootstrap mechanism**: use PostgreSQL container init scripts by mounting `agentic/schema/init.sql` into `/docker-entrypoint-initdb.d/` on the `paradedb` service. This keeps the bootstrap behind the existing `docker compose -f docker-compose.agentic.yml up -d` workflow and avoids inventing an early migration CLI before it is needed.
- **Fresh-volume expectation**: document in the task notes and verification steps that Postgres init scripts only run when the database volume is first created. Verification should therefore use an isolated Compose project name or a fresh volume, not an already-initialized `paradedb_data` volume.
- **Upgrade boundary**: make the first-boot limitation explicit in both implementation notes and verification notes. Because `task-202` is still planned to extend `agentic/schema/init.sql`, this task must not imply that `agentic.kb_schema_migrations` alone upgrades existing populated volumes; until a real migration/apply mechanism exists, follow-up schema tasks must either require fresh-volume recreation or provide a clearly documented manual apply path.
- **Extensions**: enable the extensions the platform design already depends on with idempotent SQL, specifically `pg_search` and `vector`, so later tasks can add searchable tables and indexes without revisiting bootstrap wiring.
- **Schema namespace**: create a dedicated schema named `agentic` and keep future KB objects under that namespace. This isolates the KB from `public` while still preserving the planned `kb_*` table names from the platform design.
- **Version tracking table**: create `agentic.kb_schema_migrations` as the durable schema-version ledger. Recommended shape:
  - `version BIGINT PRIMARY KEY`
  - `description TEXT NOT NULL`
  - `applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
  - `applied_by TEXT NOT NULL DEFAULT CURRENT_USER`
- **Bootstrap seed row**: insert the initial version record for this task (version `1`, description like `initial kb schema bootstrap`) as part of the same SQL file. The insert should be idempotent, for example by keying on `version` with `ON CONFLICT DO NOTHING`.
- **Scope guardrail**: do not create any non-version KB tables in `agentic/schema/init.sql`. The only table introduced in this task should be the schema-version ledger.
- **Compose change**: keep all current ports, env vars, healthchecks, and service dependencies intact. The only compose change expected here is the read-only mount that feeds the bootstrap SQL into `paradedb` startup.
- **CLI boundary**: do not update `agentic-kb status` to report schema readiness yet. `task-103` intentionally keeps status limited to config and dependency checks, and `task-205` owns real DB status/reporting behavior.

## Acceptance Criteria

- `agentic/schema/init.sql` exists and is the single SQL bootstrap entrypoint for this task.
- The SQL bootstrap creates `pg_search` and `vector` extensions with idempotent statements.
- The SQL bootstrap creates a dedicated `agentic` schema.
- The SQL bootstrap creates `agentic.kb_schema_migrations` with the agreed version-tracking columns and seeds version `1`.
- `docker-compose.agentic.yml` mounts the bootstrap SQL into the `paradedb` init path without changing the existing service contract for other containers.
- Booting ParadeDB against a fresh volume automatically produces the schema namespace and exactly one seeded version-`1` row.
- No core KB entity tables, sync-state tables, snapshot tables, or search indexes are added by this task.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to validate the compose changes.
- Start ParadeDB with an isolated project name so init scripts run on a fresh named volume, for example `docker compose -p agentic-task-201 -f docker-compose.agentic.yml up -d paradedb`.
- Inspect installed extensions from the running DB, for example with `docker compose -p agentic-task-201 -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT extname FROM pg_extension WHERE extname IN ('pg_search', 'vector') ORDER BY extname;"`.
- Inspect the schema and version ledger, for example with `docker compose -p agentic-task-201 -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT version, description FROM agentic.kb_schema_migrations ORDER BY version;"`.
- Confirm the bootstrap seed is not duplicated, for example with `docker compose -p agentic-task-201 -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT COUNT(*) AS version_1_rows FROM agentic.kb_schema_migrations WHERE version = 1;"`.
- Confirm scope boundaries with a concrete schema-table listing, for example with `docker compose -p agentic-task-201 -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'agentic' ORDER BY table_name;"`, and verify that only `kb_schema_migrations` exists.
- Tear down the isolated verification stack with `docker compose -p agentic-task-201 -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- **Extension availability**: the pinned ParadeDB image is expected to include `pg_search` and `vector`, but the implementation should verify the exact extension names exposed by that image during build/verification.
- **Fresh-volume-only bootstrap**: mounting `init.sql` into `/docker-entrypoint-initdb.d/` is right for first boot, but it does not by itself provide an upgrade path for an already-populated volume. That is acceptable for `task-201`, but follow-up tasks that keep extending `agentic/schema/init.sql` must explicitly document whether developers should recreate volumes or apply SQL manually until a real migration mechanism exists.
- **Schema naming**: use schema `agentic` for this task so KB objects stay out of `public` while future table names can still follow the platform's `kb_*` convention without redundant schema names.
- **Version numbering contract**: use a simple monotonic integer sequence starting at `1` for the bootstrap ledger. More complex migration identifiers are intentionally deferred until a real migration/apply mechanism exists.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when `task-201` is completed.
- Add a research note under `.agent/plans/agentic/research/` capturing any durable findings about ParadeDB extension names, init-script behavior, or version-table design chosen during implementation.
- If later schema tasks continue to extend `agentic/schema/init.sql` before migration tooling exists, document the expected existing-volume behavior in their task plans and implementation notes instead of implying automatic upgrades.
- Only update `.agent/workflows/agentic-kb.md`, `agentic/README.md`, or `.agent/plans/agentic/knowledge-base-platform.md` if implementation reveals a durable workflow/design change beyond the currently planned Compose bootstrap path.

## Implementation Notes

- Added `agentic/schema/init.sql` as the single bootstrap SQL entrypoint for this phase.
- The bootstrap explicitly creates `pg_search` and `vector`, creates schema `agentic`, creates `agentic.kb_schema_migrations`, and seeds version `1` with `ON CONFLICT DO NOTHING`.
- Mounted the SQL file into the `paradedb` service at `/docker-entrypoint-initdb.d/999-agentic-init.sql` as a read-only bind mount so the task bootstrap runs after the image's built-in ParadeDB initialization, with no other compose contract changes.
- Removed the earlier persistent `ALTER DATABASE` / `ALTER ROLE ... IN DATABASE ... SET search_path` approach. The corrected fix preserves the image's long-lived runtime `search_path` behavior and instead relies on init-script ordering so the `agentic` schema is created after ParadeDB finishes setting its defaults.
- Verification used a fresh isolated Compose project and volume recreation, because `/docker-entrypoint-initdb.d/` scripts only run on first database initialization.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` to mark `task-201` completed on `2026-03-27`.
- Added `.agent/plans/agentic/research/task-201-schema-foundation.md` to capture durable findings about extension names, init ordering, fresh-volume limits, and first-boot readiness behavior.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` succeeded after the compose mount change.
- `docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml up -d paradedb` first failed on host port `5546` already being allocated; verification was rerun successfully with `AGENTIC_DB_PORT=5646`.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT extname FROM pg_extension WHERE extname IN ('pg_search', 'vector') ORDER BY extname;"` returned `pg_search` and `vector`.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT version, description FROM agentic.kb_schema_migrations ORDER BY version;"` returned the single seeded row for version `1`.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT COUNT(*) AS version_1_rows FROM agentic.kb_schema_migrations WHERE version = 1;"` returned `1`.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'agentic' ORDER BY table_name;"` returned only `kb_schema_migrations` once the init script was reordered to run after ParadeDB's built-in bootstrap.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT current_setting('search_path') AS search_path;"` returned `public, paradedb`, confirming task-201 is no longer overriding the long-lived runtime `search_path`.
- The ParadeDB image performs an internal restart during first boot; early `psql` attempts during that handoff failed with `No such file or directory` / `the database system is shutting down`, so the successful verification commands were run only after startup settled.
- Related readiness caveat: Compose-level health and early `pg_isready` success can happen before all init scripts have finished on first boot, so schema verification must wait for startup to settle rather than assuming initial healthy status is sufficient.
- `AGENTIC_DB_PORT=5646 docker compose -p agentic-task-201-fix -f docker-compose.agentic.yml down -v` completed successfully after validation.

## Outcome

- `task-201` scope is implemented and verified: the knowledge-base database now bootstraps its required extensions, dedicated `agentic` schema, and initial schema-version ledger on first boot against a fresh volume.
- No entity tables, sync-state tables, snapshot tables, DB status behavior, or search indexes were introduced.
- Required task-compliance artifacts are now in place: the machine-readable tasks file marks `task-201` completed and the research note records the durable bootstrap findings and readiness caveat.

## Review Outcome

- Final code review result: clean.
- No further build iterations were required after the compliance-only tracking and research updates landed.
- No additional project metadata updates were needed for this local task beyond keeping the plan, tasks JSON, and research notes synchronized.

## Planning Status Rationale

- This plan is implementation-ready because it fixes the bootstrap mechanism, names the schema namespace and version table, defines the seeded initial version, preserves the existing Compose workflow, explicitly defers all task-202 entity-table work, and states the important limitation that existing populated volumes are not auto-upgraded by this bootstrap alone.
- The remaining risks are verification items around environment behavior rather than blockers to implementation.
