# Task Plan: task-709 Add Compose boot smoke regression check

- Task ID: `task-709`
- Title: `Add Compose boot smoke regression check`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`
- Required User Inputs: `None`
- Required Manual Test Steps: `None for task completion; authoritative verification is an automated host-side Docker smoke run`
- Evidence Needed Back From User: `None`
- Implementation Can Proceed Before User Interaction: `Yes`

## Why This Task Was Chosen Now

- `task-709` was selected as a high-priority critical-path Phase 7 task because the PRD explicitly promised an automated Compose boot smoke/regression check for the repo-root KB stack contract.
- The core stack pieces it depends on already exist: `docker-compose.agentic.yml` boots the full stack, `kb-tools` exposes `status --healthcheck`, and the workflow docs already describe that lightweight readiness contract.
- Downstream rollout and validation work such as `task-901` should not be the first place that discovers Compose drift in `up -d`, healthcheck wiring, or teardown behavior.

## Scope

- Add one focused automated regression test at `agentic/tests/test_compose_bootstrap.py` for the repo-root Compose boot contract.
- Prove the minimal promised slice only:
- `docker compose -f docker-compose.agentic.yml up -d` succeeds for an isolated test project
- lightweight readiness succeeds through `docker compose -f docker-compose.agentic.yml run --rm --no-deps kb-tools status --healthcheck`
- the isolated project is torn down cleanly afterward
- Reuse the existing repo testing style where possible: Python `unittest`, stdlib `subprocess`, explicit skip/gating behavior, and small targeted assertions.

## Non-Goals

- Do not add broader ingest, snapshot, search, or MCP validation to this task; those belong to their own regression tasks.
- Do not redesign the Compose file, service healthchecks, image build strategy, or operator workflow unless a narrow fix is required to make the smoke check truthful.
- Do not add a second bespoke shell test harness when the existing `agentic/tests/` Python unittest pattern is sufficient.
- Do not treat this task as the place to wire a full CI pipeline or broad repo test command around the smoke suite unless a tiny discoverability change is truly necessary.

## Relevant Dependencies

- Declared completed dependencies:
- `task-101` established the repo-root Compose stack and the five-service contract in `docker-compose.agentic.yml`.
- `task-103` established the `kb-tools` image and the `status --healthcheck` readiness contract used by Compose healthchecks.
- `task-205` expanded `status` while preserving the lightweight `--healthcheck` mode as the intended dependency/readiness probe.
- Current repo context reviewed for planning:
- `.agent/plans/agentic/knowledge-base-platform-prd.md`
- `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- `.agent/workflows/agentic-kb.md`
- `.agent/plans/agentic/research/task-101-compose-foundation.md`
- `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
- `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md`
- `docker-compose.agentic.yml`
- `agentic/src/agentic_kb/commands/status.py`
- `agentic/tests/test_schema_bootstrap.py`
- `agentic/tests/test_snapshot_command_db.py`
- `agentic/tests/test_sync_command_db.py`

## Repo Constraints Reconciled

- Before implementation, the PRD and task tracker both promised an automated Compose boot smoke gate, but no `task-709` plan/log docs or `agentic/tests/test_compose_bootstrap.py` existed yet.
- `docker-compose.agentic.yml` already encodes the exact contract this task should exercise: repo-root `up -d`, `kb-tools`/`mcp-search` healthchecks driven by `agentic-kb status --healthcheck`, and named-volume lifecycle managed by Compose.
- The same Compose file also binds ParadeDB and Ollama to fixed loopback host ports by default (`127.0.0.1:${AGENTIC_DB_PORT:-5445}` and `127.0.0.1:${OLLAMA_PORT:-11434}`), so project-name isolation alone is not enough to avoid collisions with an already running local stack.
- Existing automated coverage is strong at the command and DB-backed level, but it stops short of shelling out to the real repo-root `docker compose` contract from a host test.
- Existing task patterns show two useful constraints for this task:
- DB-backed tests commonly use `unittest`, `subprocess`, skip gating, and isolated disposable targets
- authoritative verification for this specific task must run from the host, not from inside `kb-tools`, because the test itself needs access to the host Docker daemon and repo-root `docker compose`
- Planning-time environment check on this workstation found `docker compose` available and the Docker daemon reachable, so there is no current blocker that would force a non-autonomous task classification.

## Files Changed

- `agentic/tests/test_compose_bootstrap.py` - host-side Compose smoke regression suite.
- `agentic/README.md` - focused smoke-suite invocation and env-gate note.
- `.agent/plans/agentic/task-plans/task-709.md` - canonical task plan and final task record.
- `.agent/plans/agentic/task-plans/task-709-plan-review.md` - append-only planning review log.
- `.agent/plans/agentic/task-plans/task-709-impl-review.md` - append-only implementation review log.
- `.agent/plans/agentic/research/task-709-compose-boot-smoke.md` - durable Compose smoke findings.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - `task-709` completion status metadata.

## Implementation Approach

- Keep the task `autonomous`. This is an internal regression-check task with no required user decision, manual operator checkpoint, or external approval gate.
- Implement the smoke check as a Python `unittest` module under `agentic/tests/`, not as a shell script. That matches the current repo pattern and keeps the regression easy to run with focused unittest discovery.
- Make the suite explicitly host-side and environment-gated:
- require `docker` plus `docker compose` availability and a reachable Docker daemon
- use one explicit opt-in environment flag such as `AGENTIC_RUN_COMPOSE_SMOKE=1` so generic local unittest discovery does not unexpectedly trigger a full Compose boot and first-run model pull
- skip cleanly rather than failing when the environment gate is not satisfied
- Use an isolated Compose project name and mandatory per-run `AGENTIC_DB_PORT` plus `OLLAMA_PORT` overrides so the smoke suite does not collide with a developer's normal KB stack or other local services. This should be treated as part of the core test contract, not as an optional implementation detail, because the checked-in Compose file publishes fixed loopback host ports by default.
- Exercise the smallest truthful stack contract directly from the repo root:
- choose currently unused host ports, then invoke `docker compose -p <project> -f docker-compose.agentic.yml up -d` with those per-run environment overrides in the subprocess environment
- wrap the `up -d` command itself in an explicit generous timeout, planned as 15 minutes, because cold-cache `ollama-init` model pulls are part of the real boot contract and can materially extend startup time
- poll `docker compose -p <project> -f docker-compose.agentic.yml run --rm --no-deps kb-tools status --healthcheck` until it succeeds or a bounded timeout expires
- always run `docker compose -p <project> -f docker-compose.agentic.yml down -v` in cleanup so the isolated test project is removed even on failure
- Capture useful failure context for both `up -d` and the readiness poll, including the failing command's stdout/stderr plus follow-up `docker compose ps` and bounded `docker compose logs --no-color` output for the isolated project, instead of turning the suite into a large helper framework.
- Reuse the same implementation style already present in `test_schema_bootstrap.py`, `test_snapshot_command_db.py`, and `test_sync_command_db.py`: stdlib `subprocess`, small local helpers, and explicit environment-based skip conditions.
- Keep local verification and automated coverage responsibilities separate:
- automated test code should own the real repo-root boot, readiness poll, and teardown behavior
- implementation-time verification should run that focused host-side suite directly; it should not try to nest the smoke suite inside `kb-tools`, because that would stop testing the real host Compose contract

## Acceptance Criteria

- A new automated regression suite exists at `agentic/tests/test_compose_bootstrap.py`.
- The suite shells out to the real repo-root `docker compose -f docker-compose.agentic.yml` contract rather than mocking Compose behavior.
- The suite uses mandatory per-run `AGENTIC_DB_PORT` and `OLLAMA_PORT` overrides in addition to a unique Compose project name so host-port collisions do not invalidate isolation.
- The suite proves that an isolated Compose project can boot with `up -d` and reach lightweight readiness through `kb-tools status --healthcheck`.
- The suite enforces an explicit timeout for `docker compose ... up -d` itself and captures actionable failure context if boot stalls, times out, or exits non-zero.
- The suite performs teardown for the isolated project even when the readiness probe fails or times out.
- The implementation reuses the existing Python unittest pattern and does not introduce a second custom shell harness for this task.
- The task remains autonomous: no user input, manual approval, or hidden human checkpoint is required to land the automated smoke check.
- If the suite is environment-gated, the gate is explicit and documented narrowly enough that the regression remains discoverable and intentionally runnable.

## Verification Plan

- Run `python3 -m py_compile agentic/tests/test_compose_bootstrap.py` and any touched helper files.
- Run the focused smoke suite from the host with Docker available and the explicit opt-in gate enabled, for example:
- `AGENTIC_RUN_COMPOSE_SMOKE=1 PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_compose_bootstrap.py'`
- Use the focused host-side run as the authoritative proof for this task rather than trying to execute the suite inside `kb-tools`.
- During authoritative verification, confirm the smoke run actually used per-run `AGENTIC_DB_PORT` and `OLLAMA_PORT` overrides rather than the default loopback bindings from `docker-compose.agentic.yml`.
- Treat `docker compose ... up -d` as a separately bounded step during verification, with the planned 15-minute timeout and failure-context capture still enabled even before the later readiness poll begins.
- If implementation touches `docker-compose.agentic.yml` or status healthcheck behavior, rerun the most relevant focused command tests, especially `test_status_command.py`, and rerun the smoke suite afterward.
- If implementation adds a small operator-facing note about the smoke command, re-read `agentic/README.md` and `.agent/workflows/agentic-kb.md` against the final test entrypoint so docs do not invent a different invocation path.

## Risks / Open Questions

- First-run cold-cache behavior is the main execution risk. Because `ollama-init` pulls the configured embedding model, authoritative smoke runs can be slow and may depend on network availability if the model is not already cached locally.
- The test must stay isolated from a developer's normal stack. Random or task-specific project naming is necessary but insufficient on its own; explicit per-run `AGENTIC_DB_PORT` and `OLLAMA_PORT` overrides are also required because the checked-in Compose file binds fixed loopback ports by default.
- A host-side opt-in gate is likely the minimal truthful way to keep the suite useful without making every generic unittest discovery unexpectedly boot Docker Compose. That should remain an explicit implementation choice, not an implicit local convention.
- Later repo-level test-command or CI wiring may still be desirable, but it is not required to implement the minimal `task-709` regression slice described in the tracker.

## Docs / Tracking / Research Outcome

- This canonical task plan is maintained at `.agent/plans/agentic/task-plans/task-709.md`.
- Planning-review history lives at `.agent/plans/agentic/task-plans/task-709-plan-review.md`.
- Implementation-review history lives at `.agent/plans/agentic/task-plans/task-709-impl-review.md`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` records `task-709` as `completed` with `completedAt: 2026-03-30`.
- `.agent/plans/agentic/research/task-709-compose-boot-smoke.md` captures the durable Compose, timeout, and isolation findings from implementation.
- `agentic/README.md` was updated with the explicit smoke-suite invocation; `.agent/workflows/agentic-kb.md` did not require a behavior change for task-709 consistency.

## Final Outcome

- Final implementation status: `implemented_and_verified`, with the teardown-enforcement follow-up landed and re-verified.
- Final review outcome: `approved`; see `.agent/plans/agentic/task-plans/task-709-impl-review.md` for the preserved review sequence and terminal approval state.
- Final build state for this task: `completed`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-709-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-709-impl-review.md`

## Implementation Notes

- Added `agentic/tests/test_compose_bootstrap.py` as the host-side regression for the repo-root Compose boot contract.
- The suite is explicitly gated by `AGENTIC_RUN_COMPOSE_SMOKE=1`, skips cleanly when Docker Compose or the daemon is unavailable, and uses both a unique Compose project name and mandatory per-run `AGENTIC_DB_PORT` plus `OLLAMA_PORT` overrides.
- Boot verification now treats `docker compose -f docker-compose.agentic.yml up -d` as its own bounded step with a 15-minute timeout and actionable failure context, then polls `docker compose ... run --rm --no-deps kb-tools status --healthcheck` until readiness or timeout.
- The smoke test also asserts the overridden host port bindings through `docker compose port` and always cleans up the isolated project with `docker compose ... down -v`.

## Verification Notes

- `python3 -m py_compile agentic/tests/test_compose_bootstrap.py` passed.
- `AGENTIC_RUN_COMPOSE_SMOKE=1 PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_compose_bootstrap.py'` passed as the authoritative host-side Compose smoke run.
- The authoritative run used per-run `AGENTIC_DB_PORT` and `OLLAMA_PORT` overrides verified by `docker compose port` assertions in the test instead of relying on the default loopback bindings from `docker-compose.agentic.yml`.
