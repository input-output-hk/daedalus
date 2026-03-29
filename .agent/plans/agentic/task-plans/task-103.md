# Task Plan: task-103 Build kb-tools service image

- Task ID: `task-103`
- Title: `Build kb-tools service image`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-103` is the next pending critical-path task after the compose scaffold from `task-101`.
- Later schema, ingest, snapshot, search, and wrapper tasks all need a real Python tools image with a stable CLI contract instead of the current inline shell placeholder.

## Scope

- Create the first real `agentic/` Python tools package used by the `kb-tools` Compose service.
- Add a Docker build for that package and make `docker compose -f docker-compose.agentic.yml build kb-tools` succeed locally.
- Replace the placeholder `kb-tools` service in `docker-compose.agentic.yml` with a build-backed service that uses the packaged CLI.
- Ship a minimal but real CLI contract for `status`, `sync <subcommand>`, `snapshot <subcommand>`, and `service`, where `status` is implemented for bootstrap/readiness checks and the grouped `sync` / `snapshot` surfaces are explicit placeholders for later phase-scoped tasks.
- Wire the service for future repo ingestion and snapshot export by mounting the repository and `agentic/snapshots/` in a way later tasks can reuse without redesigning the container.

## Non-Goals

- Do not implement schema creation, migrations, search queries, embeddings, real sync behavior, or snapshot export/import logic in this task.
- Do not create or revise helper wrappers in `package.json`; that remains `task-105`.
- Do not replace the placeholder `mcp-search` service; that remains `task-104`.
- Do not introduce `.env.example`; environment examples remain `task-102`, though this task must honor the environment contract already defined by `task-101`.
- Do not make MCP writable or add any non-read-only MCP behavior; v1 remains read-only by project decision.

## Relevant Dependencies

- Completed dependency: `task-101` (`docker-compose.agentic.yml` scaffold and environment contract)
- Adjacent task to preserve boundaries with: `task-102` (`agentic/.env.example`)
- Downstream tasks unblocked by this work: `task-104`, `task-105`, `task-201`, `task-205`, `task-501`
- Reference docs reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `docker-compose.agentic.yml`

## Files Expected To Change

- `agentic/Dockerfile`
- `agentic/.dockerignore`
- `agentic/README.md`
- `agentic/pyproject.toml`
- `agentic/src/agentic_kb/__init__.py`
- `agentic/src/agentic_kb/cli.py`
- `agentic/src/agentic_kb/config.py`
- `agentic/src/agentic_kb/commands/__init__.py`
- `agentic/src/agentic_kb/commands/service.py`
- `agentic/src/agentic_kb/commands/status.py`
- `agentic/src/agentic_kb/commands/sync.py`
- `agentic/src/agentic_kb/commands/snapshot.py`
- `agentic/snapshots/.gitkeep`
- `docker-compose.agentic.yml`
- `.agent/workflows/agentic-kb.md`

## Implementation Approach

- **Package layout**: create an isolated Python package under `agentic/src/agentic_kb/` and use `pyproject.toml` with a console entrypoint named `agentic-kb`. Keep dependencies minimal and prefer standard-library modules unless one small runtime dependency is clearly justified.
- **Docker image**: build `kb-tools` from `agentic/Dockerfile` using a Python base image suitable for future Postgres and CLI tooling work. The image should install the local package, set a predictable working directory, set `ENTRYPOINT ["agentic-kb"]`, and set `CMD ["service"]`. That contract must ensure `docker compose up -d kb-tools` starts service mode by default, while `docker compose run --rm kb-tools --help` invokes the packaged CLI as `agentic-kb --help` rather than bypassing it.
- **CLI contract**: implement top-level commands `status`, `sync`, `snapshot`, and `service`.
- **Real command in this task**: `status` should parse config, report the resolved environment contract, and perform lightweight readiness checks against configured services that are safe before schema tasks land. It must not claim the KB is fully initialized.
- **Grouped placeholder commands**: scaffold the command shapes expected by the workflow doc now: `sync all`, `sync changed`, `sync docs`, `sync code`, `sync github`, `sync project`, plus `snapshot export` and `snapshot import`. Each should print explicit "not implemented in task-103" guidance that references the later owning task(s) and exit non-zero so automation cannot mistake placeholders for working features.
- **Service mode**: `service` should be a benign long-running mode for `docker compose up -d` that starts through the CLI package, keeps the container alive, and handles SIGTERM cleanly. It may reuse the same bootstrap/config checks as `status`, but it must not perform ingest, search, or schema setup.
- **Configuration module**: centralize parsing for `DATABASE_URL`, `OLLAMA_BASE_URL`, `OLLAMA_EMBED_MODEL`, and `GITHUB_TOKEN`, and define defaults only where `task-101` already established them in Compose. For this task, `DATABASE_URL`, `OLLAMA_BASE_URL`, and `OLLAMA_EMBED_MODEL` are required for `status --healthcheck` and `service`; `GITHUB_TOKEN` remains optional and should be reported as absent rather than treated as an error. Placeholder `sync` / `snapshot` commands may parse config but must not require GitHub- or schema-specific settings yet.
- **Compose update**: replace the current `kb-tools` image/command with `build:` pointing at `agentic/`, keep the existing dependency ordering on `paradedb` and `ollama-init`, mount the repository at `/workspace`, mount `agentic/snapshots/` at `/workspace/agentic/snapshots`, and remove the `kb_tools_data` named volume because this scaffold does not require container-private mutable state.
- **Healthcheck**: replace the temp-file healthcheck with a CLI-driven readiness check such as `agentic-kb status --healthcheck`. The healthcheck should verify process/config bootstrap and optional service reachability only; it must not require schema tables, embeddings, sync state, or MCP availability.
- **Task boundary with later CLI work**: treat the task-103 CLI as the durable bootstrap contract. Later tasks such as `task-205`, `task-701`, and `task-602` should extend the already-present `status`, `sync`, and `snapshot` commands with real behavior rather than replacing the surface introduced here.
- **Documentation**: add `agentic/README.md` with the container purpose, current command surface, the distinction between implemented vs placeholder commands, and example `docker compose` invocations consistent with the workflow doc. Update `.agent/workflows/agentic-kb.md` to remove the statement that `kb-tools` is still a placeholder service and instead describe it as a real container with placeholder subcommands where appropriate.

## Acceptance Criteria

- `agentic/Dockerfile` and `agentic/pyproject.toml` exist and produce a runnable local `kb-tools` image.
- The repo contains a Python package under `agentic/src/agentic_kb/` with a documented `agentic-kb` CLI entrypoint.
- `docker compose -f docker-compose.agentic.yml build kb-tools` succeeds without relying on the old placeholder service definition.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools --help` shows CLI help from the packaged entrypoint, and `docker compose -f docker-compose.agentic.yml up -d kb-tools` enters `service` mode without needing an explicit command override.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools status` exits successfully and clearly reports bootstrap/readiness information without claiming schema or search readiness.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all`, `sync changed`, `sync docs`, `sync code`, `sync github`, `sync project`, `snapshot export`, and `snapshot import` all exist and fail with explicit placeholder messaging that points to later tasks.
- `docker-compose.agentic.yml` runs `kb-tools` from the local build, preserves the `task-101` environment contract, replaces the temp-file healthcheck with a CLI-based readiness check, mounts the repository at `/workspace`, mounts snapshots at `/workspace/agentic/snapshots`, and removes the obsolete `kb_tools_data` volume.
- The `kb-tools` container can see the repository and `agentic/snapshots/` at the documented stable mount paths.
- `.agent/workflows/agentic-kb.md` no longer describes `kb-tools` as a placeholder service, while still making clear that most sync and snapshot behavior remains future work.
- The implementation keeps scope to the tools container only and does not alter `mcp-search`, wrapper commands, or shared snapshot publication behavior.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to validate the updated Compose definition.
- Run `docker compose -f docker-compose.agentic.yml build kb-tools` to verify the image build.
- Run `docker compose -f docker-compose.agentic.yml run --rm kb-tools --help` to verify the console entrypoint wiring.
- Run `docker compose -f docker-compose.agentic.yml run --rm --entrypoint sh kb-tools -lc 'pwd && test -d /workspace && test -d /workspace/agentic/snapshots'` to verify the documented mount paths are visible in-container.
- Run `docker compose -f docker-compose.agentic.yml run --rm kb-tools status` to verify config parsing and bootstrap/readiness behavior.
- Run `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all`, `sync changed`, `sync docs`, `sync code`, `sync github`, `sync project`, `snapshot export`, and `snapshot import`, and confirm each exits non-zero with explicit placeholder guidance.
- Run `docker compose -f docker-compose.agentic.yml up -d paradedb ollama ollama-init` before service-level verification so `status` and the healthcheck are exercising the intended dependency state rather than implicit `run` behavior.
- Run `docker compose -f docker-compose.agentic.yml up -d kb-tools` and then `docker compose -f docker-compose.agentic.yml ps kb-tools` to verify the long-running service mode and healthcheck behavior.

## Risks / Open Questions

- The image base should avoid creating future friction for PostgreSQL client tooling and Python dependencies; the implementation should pick a base image with that follow-on work in mind and document the choice in the task outcome.
- The `status` healthcheck must stay narrow enough that it remains green before schema/search tasks land, but useful enough to catch broken config or missing dependencies.
- The repo and snapshot mounts should be chosen once here and then reused by later tasks; changing mount paths later would create avoidable churn across docs and automation.

## Required Docs / Tracking / Research Updates
- Update this task plan doc during implementation so build status, decisions, verification notes, and final outcome match the actual work.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when `task-103` is completed.
- Only update `.agent/plans/agentic/knowledge-base-platform-prd.md` if implementation requires a durable change to the documented tools-container or Compose design; no design update is expected from this task as currently planned.
- Update `.agent/workflows/agentic-kb.md` to reflect that `kb-tools` is now a real built service even though most task-specific commands remain placeholder scaffolding.

## Implementation Notes

- Created a real Python package in `agentic/src/agentic_kb/` with a console entrypoint named `agentic-kb`.
- Used `python:3.12-slim-bookworm` for `agentic/Dockerfile` so later tasks can add PostgreSQL-adjacent tooling without Alpine-specific friction.
- Kept the task-103 CLI surface limited to `status`, grouped placeholder `sync` subcommands, grouped placeholder `snapshot` subcommands, and `service`.
- Implemented `status` as a narrow bootstrap check for config parsing, mount visibility, ParadeDB TCP reachability, Ollama API reachability, and configured embedding-model presence.
- Implemented `service` as benign long-running mode behind the packaged CLI with SIGTERM and SIGINT handling.
- Updated `docker-compose.agentic.yml` so `kb-tools` uses local `build:`, defaults to service mode through image `CMD`, mounts the repo at `/workspace`, mounts snapshots at `/workspace/agentic/snapshots`, and uses `agentic-kb status --healthcheck` for its healthcheck.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` - passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` - passed.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools --help` - passed; packaged CLI help rendered from the image entrypoint.
- `docker compose -f docker-compose.agentic.yml run --rm --entrypoint sh kb-tools -lc 'pwd && test -d /workspace && test -d /workspace/agentic/snapshots'` - passed; mount paths were visible in-container at the documented locations.
- `docker compose -f docker-compose.agentic.yml up -d paradedb ollama ollama-init` - passed; dependency services reached the expected running/healthy state.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools status` - passed; reported bootstrap config, mount visibility, ParadeDB TCP reachability, Ollama API reachability, and embedding-model presence without claiming schema readiness.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync docs` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync code` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync github` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync project` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot export` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import` - passed as placeholder behavior; exited non-zero with explicit task guidance.
- `docker compose -f docker-compose.agentic.yml up -d kb-tools && docker compose -f docker-compose.agentic.yml ps kb-tools` - passed after waiting for the healthcheck interval; service came up in default `service` mode and reached `healthy`.

## Outcome

- Completed. `kb-tools` now builds from the local `agentic/` package, exposes the durable task-103 CLI contract, defaults to `service` mode in Compose, and keeps later schema/sync/snapshot/search work explicitly out of scope.
- Scribe updates completed in `.agent/workflows/agentic-kb.md`, `.agent/plans/agentic/knowledge-base-platform-tasks.json`, and `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`.
