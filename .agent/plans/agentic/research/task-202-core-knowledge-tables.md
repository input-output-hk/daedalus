# Task 202 Core Knowledge Tables Research

- Date: 2026-03-27
- Task: `task-202`
- Evidence: `agentic/schema/init.sql`, `.agent/plans/agentic/task-plans/task-202.md`, `.agent/plans/agentic/knowledge-base-platform-tasks.json`

## Durable Findings

- The pinned ParadeDB image accepted embedding columns declared as `VECTOR(384)` for the task-202 searchable tables.
- Keeping `agentic/schema/init.sql` as the single schema entrypoint worked for both clean bootstrap and manual upgrade application, as long as the SQL stayed idempotent with `CREATE TABLE IF NOT EXISTS` and `ON CONFLICT DO NOTHING` migration seeding.
- The task-202 core schema can be safely re-applied to an already initialized local database without duplicating schema objects or migration rows; expected repeat-run output is limited to benign `already exists` notices.

## Verification Caveats

- Updating `agentic/schema/init.sql` does not retrofit an existing ParadeDB volume by itself because `/docker-entrypoint-initdb.d/` scripts run only on first database initialization.
- A true upgrade-path verification must start from a fresh database bootstrapped with the task-201-era SQL, then manually apply the updated task-202 SQL into that running database; re-running the updated file against a database that already booted from the updated file checks idempotence only, not upgrade behavior.
- The operator paths that matter for this phase are `fresh volume` for first-boot validation and `manual apply` for already-initialized local volumes until a real migration runner exists.
- ParadeDB still has the known first-boot restart handoff from task-201, so verification should wait for a successful `psql` query after the restart instead of trusting the first healthy signal alone.

## No New Research Beyond Task Scope

- No additional durable design changes were discovered beyond the accepted `VECTOR(384)` syntax, the fresh-volume/manual-apply boundary, and the idempotent re-apply behavior required for local upgrades.
