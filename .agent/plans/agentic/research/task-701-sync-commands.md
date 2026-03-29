# Task 701 Sync Commands Research

- Date: 2026-03-29
- Task: `task-701`
- Evidence: `agentic/src/agentic_kb/commands/sync.py`, `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/cli.py`, `agentic/tests/test_sync_command.py`, `agentic/tests/test_sync_command_db.py`

## Durable Findings

- The packaged sync surface now lives entirely in `agentic/src/agentic_kb/commands/sync.py`; no separate un-packaged sync module was needed.
- `sync docs` and `sync code` are explicit source refresh commands that seed and update their own `kb_sync_state` rows, while `sync docs` now prunes stale allowlisted document paths by comparing discovered allowlist paths against currently stored KB document paths.
- `sync github` now uses the existing general `ingest_github(...)` path across `issues`, `pulls`, `issue_comments`, and `review_comments` on both initial and incremental runs. The durable incremental contract is one shared `updated_since` lower bound derived from the earliest stored stream watermark across all four rows.
- GitHub stream asymmetry remains explicit in the command contract: upstream REST `since` applies only to `issues` and `issue_comments`, while `pulls` and `review_comments` still depend on ordered fetch plus client-side filtering under that same lower bound.
- Initial explicit `sync github` can seed empty-state stream rows even when a stream writes no rows by falling back to the sync completion timestamp as the seeded watermark. Later incremental runs require successful existing watermarks on all four stream rows.
- `sync project` now uses the existing cursor-based `ingest_project_items(...)` path for both initial explicit seeding from `after_cursor=None` and later continuation from stored `cursor_text`. Repeated end-cursor runs remain valid no-op continuations and do not imply replay of already-seen items.
- After task-701, `sync changed` is the general incremental sync command for an already-seeded KB. It still supports the snapshot-import bootstrap workflow from task-604, but it now requires all four source baselines to already exist and be complete; there is no source-local fallback into first-sync behavior.
- `sync all` is a thin orchestrator over `sync docs`, `sync code`, `sync github`, and `sync project` in that fixed order, and it stops on the first failure without attempting later sources.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/sync.py" "agentic/src/agentic_kb/cli.py" "agentic/src/agentic_kb/ingest/docs.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py"` passed.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command` passed locally after the iteration-2 GitHub regression fix (`Ran 13 tests`, `OK`, `1 skipped` for optional tree-sitter coverage when unavailable).
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db` was environment-gated locally (`Ran 3 tests`, `OK`, all skipped) because `AGENTIC_TEST_DATABASE_URL` was not set in the host environment.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src kb-tools -m unittest agentic.tests.test_sync_command` failed in-container because the current `kb-tools` image still lacks the `git` binary required by the new git-delta sync tests.
- An in-container DB-backed sync suite attempt timed out after bringing `paradedb` up; no authoritative `kb-tools` DB-backed pass was completed in this session.

## No New Research Beyond Task Scope

- Task-701 did not add stale-index detection, scheduled automation, or any new GitHub/Project ingestion primitives beyond command-layer orchestration and small docs-store support needed for explicit docs refresh.
