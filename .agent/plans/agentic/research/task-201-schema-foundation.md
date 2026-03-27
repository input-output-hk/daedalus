# Task 201 Schema Foundation Research

- Date: 2026-03-27
- Task: `task-201`
- Evidence: `agentic/schema/init.sql`, `docker-compose.agentic.yml`, `.agent/plans/agentic/task-plans/task-201.md`

## Durable Findings

- The required ParadeDB extension names for this phase are confirmed as `pg_search` and `vector`.
- Task bootstrap should be mounted as `/docker-entrypoint-initdb.d/999-agentic-init.sql` so it runs after the image's built-in `10_bootstrap_paradedb.sh`; that ordering keeps the runtime `search_path` behavior owned by the base image while still allowing `agentic` schema creation and version-table seeding.
- PostgreSQL init scripts in `/docker-entrypoint-initdb.d/` run only on first database initialization, so task-201 behavior is a fresh-volume bootstrap, not an upgrade path for already-populated volumes.
- On first boot, the ParadeDB container performs internal initialization and restart steps after the container is already running; because of that, Compose service health and early `pg_isready` success do not guarantee that all init scripts have finished.

## Gotchas And Constraints

- Verification commands that query schema state may fail during the first-boot handoff with errors like `No such file or directory` or `the database system is shutting down` if they run before ParadeDB finishes its internal restart.
- Follow-up schema tasks that extend `agentic/schema/init.sql` must continue documenting the fresh-volume/manual-apply limitation until a real migration mechanism exists.

## No New Research

- No additional durable findings beyond extension naming, init ordering, first-boot limits, and first-boot readiness behavior.
