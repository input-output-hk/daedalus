# Task Plan: task-612 Enforce disposable import target safety

- Task ID: `task-612`
- Title: `Enforce disposable import target safety`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-612` is the next direct safety hardening step after `task-602` made manifest-aware snapshot import real but still restores into any reachable KB database after artifact validation.
- The PRD explicitly requires snapshot import to refuse targets that are not fresh, isolated, or otherwise disposable, and it names restore-into-empty as the only supported operator path in v1.
- This task is on the critical path for `task-613`, `task-714`, and `task-901`, because clean bootstrap and round-trip validation are only trustworthy if import cannot silently merge into a seeded KB.
- Current workflow docs already warn operators to use a disposable KB database, but `agentic/src/agentic_kb/commands/snapshot.py` does not yet enforce that boundary in code.

## Scope

- Add an explicit pre-restore disposable-target guard to snapshot import.
- Define the minimal implementation-grade meaning of a supported import target in current repo terms.
- Fail import before any schema drop when the KB target appears seeded or otherwise non-disposable.
- Add focused unit and DB-backed tests for accepted empty/fresh targets and rejected seeded targets.

## Non-Goals

- Do not implement embedding-contract compatibility checks; that remains `task-608` and `task-609`.
- Do not broaden this task into publication workflow, private storage selection, or `sync changed` bootstrap work.
- Do not add in-place schema upgrade support or retrofit procedures; the supported recovery path remains recreate-and-import or rebuild.
- Do not change the external snapshot manifest schema unless implementation uncovers a concrete blocker.
- Do not update global tracking files yet beyond the task-specific planning docs created in this planning step.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-205` established the destructive restore flow and filtered `agentic`-schema restore behavior.
  - `task-206` documented the v1 disposable-volume policy.
  - `task-602` added manifest-aware export/import with pre-restore dump validation.
  - `task-901` research confirmed the narrow clean-bootstrap contract depends on a fresh isolated target.
- Direct downstream tasks unblocked by this work:
  - `task-613` import target-safety regression coverage.
  - `task-714` snapshot round-trip regression tests on the supported disposable-target path.
  - `task-901` clean-machine bootstrap validation.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md`
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/tests/test_snapshot_command.py`
  - `agentic/tests/test_snapshot_command_db.py`

## Files Expected To Change

- `agentic/src/agentic_kb/commands/snapshot.py` - add the disposable-target inspection and fail-fast import guard before schema drop.
- `agentic/tests/test_snapshot_command.py` - add narrow unit coverage for the target-safety decision logic and failure ordering.
- `agentic/tests/test_snapshot_command_db.py` - extend DB-backed coverage to prove import is rejected for seeded KB targets and still succeeds for both supported fresh-target paths: schema-missing disposable DBs and initialized-but-empty KBs.
- `.agent/workflows/agentic-kb.md` - only if a small wording update is needed so operator guidance matches the enforced import contract exactly.
- `.agent/plans/agentic/research/task-612-disposable-import-target-safety.md` - capture durable findings and verification notes once implementation lands.

## Implementation Approach

- Preserve the current CLI surface: keep `agentic-kb snapshot import <dump-or-manifest> --yes` as the operator entrypoint and add the safety guard inside the existing import flow.
- Keep validation ordering strict: manifest parsing, schema validation, sibling dump resolution, size/hash verification, and disposable-target inspection must all happen before any destructive `DROP SCHEMA` step.
- Define the supported disposable-target contract using current DB state, not operator intent strings. The minimal safe rule for v1 is:
  - import may proceed when the target database is truly fresh and has no existing `agentic` schema at all
  - import may also proceed when the target already has `agentic` bootstrapped but all state-bearing `agentic` tables are empty
  - import must reject only non-disposable targets, meaning any target where state-bearing `agentic` tables already contain rows
  - specifically, when the `agentic` schema exists, import must reject any target where `agentic.kb_documents`, `agentic.kb_code_chunks`, `agentic.kb_github_issues`, `agentic.kb_github_issue_comments`, `agentic.kb_github_prs`, `agentic.kb_github_pr_comments`, `agentic.kb_project_items`, `agentic.kb_sync_state`, or `agentic.kb_snapshot_manifest` contains rows
- Treat both of these as supported fresh/disposable paths:
  - a schema-missing fresh database, because the dump already carries the `agentic` schema objects that restore will recreate
  - an initialized-but-empty KB database, which matches the current documented clean-bootstrap workflow after stack boot
- Reject partially seeded targets, including cases where searchable tables are empty but sync-state or prior snapshot-manifest rows remain. Those still indicate an existing baseline and would silently merge provenance/history if import proceeded.
- Keep the guard minimal and explicit rather than heuristic. Do not infer disposability from database name, volume name, or environment variables.
- For the schema-missing path, the guard should inspect whether `agentic` exists first; absence of the schema should be treated as fresh/disposable rather than as an automatic failure.
- Reuse `status.inspect_database()` patterns where helpful, but keep the import guard self-contained inside snapshot code unless a tiny shared helper clearly reduces duplication.
- Fail with operator-readable messages that name the offending tables or row-count summary so the recovery path is obvious: recreate the disposable KB volume, then retry import.
- Preserve the current filtered restore implementation once the target passes validation; `task-612` should tighten the preconditions, not redesign restore internals.
- Keep scope aligned to task-612 only. Supporting schema-missing fresh targets should reuse the current restore mechanics rather than turning this task into a broader bootstrap redesign.

## Acceptance Criteria

- `snapshot import` fails before schema drop when the target KB contains any existing indexed content, sync state, or prior snapshot-manifest rows.
- `snapshot import` succeeds for a fresh disposable target where the `agentic` schema is absent.
- `snapshot import` also succeeds for the supported initialized-but-empty KB target.
- The failure happens after artifact validation but before destructive restore commands run.
- Rejection messaging clearly states that import is supported only for fresh, isolated, or otherwise disposable KB databases and points operators to recreate the KB volume.
- The task does not weaken the existing manifest validation or filtered `agentic`-schema restore behavior from `task-602` and `task-205`.
- Tests prove both rejection of seeded targets and success on both supported fresh-target paths.

## Verification Plan

- Run `python3 -m py_compile` on touched Python modules and tests.
- Run focused unit coverage with `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'`.
- Add unit coverage for:
  - disposable-target inspection over schema-missing, empty, and non-empty states
  - rejection messaging content
  - failure ordering proving schema drop is not invoked when the target is non-disposable
- Extend DB-backed verification with `AGENTIC_TEST_DATABASE_URL` to prove:
  - import into a fresh database with no `agentic` schema succeeds
  - import into a bootstrapped-but-empty KB succeeds
  - import into a seeded KB with document rows fails before restore
  - import into a target with only `kb_sync_state` or only `kb_snapshot_manifest` rows also fails before restore
- If workflow wording changes, verify the documented import example still matches the shipped CLI contract.

## Risks / Open Questions

- The biggest contract choice is how strict to make “fresh/isolated/disposable” in implementation terms. This plan intentionally chooses the minimal safe rule of empty state-bearing `agentic` tables because looser heuristics risk silent merges.
- The main implementation nuance is distinguishing a truly fresh schema-missing target from a non-disposable target without falling back to heuristics. The plan treats missing `agentic` schema as allowed and any existing non-empty `agentic` state tables as disallowed.
- There is a tradeoff between reporting every offending table and keeping errors concise. The implementation should prefer one clear summary plus enough table detail for operator recovery.
- If current DB-backed tests rely on truncating only a subset of tables before import, they may need adjustment to satisfy the stronger non-empty-target guard without weakening the production rule.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final implementation notes, verification notes, outcome, and approved statuses once the task lands.
- Append future planning review decisions to `.agent/plans/agentic/task-plans/task-612-plan-review.md`.
- Create the implementation review log only when implementation begins; do not create it during planning.
- Add `.agent/plans/agentic/research/task-612-disposable-import-target-safety.md` after implementation with the accepted disposable-target rule, failure ordering, and verification evidence.
- Update `.agent/workflows/agentic-kb.md` only if implementation changes or sharpens operator-visible wording.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` later only for `task-612` task metadata when implementation is complete.

## Current Build State

- `agentic/src/agentic_kb/commands/snapshot.py` now inspects the target DB after manifest/artifact validation and before any schema drop.
- The shipped import guard allows both supported disposable paths from the approved plan: a schema-missing fresh DB and an initialized-but-empty KB.
- Import now rejects any existing rows in the state-bearing KB tables, including `kb_sync_state` and `kb_snapshot_manifest`, with an operator-readable recovery message.
- Focused unit coverage and DB-backed tests now prove both accepted and rejected target states against the current restore implementation.

## Implementation Outcome

- Added `SnapshotImportTargetInspection`, `inspect_snapshot_import_target()`, and `ensure_disposable_import_target()` in `agentic/src/agentic_kb/commands/snapshot.py`.
- Wired the new guard into `import_snapshot()` so manifest parsing, schema validation, dump resolution, size/hash validation, and disposable-target inspection all occur before `DROP SCHEMA IF EXISTS agentic CASCADE`.
- Kept the accepted disposable-target rule intentionally narrow and explicit: allow schema-missing fresh DBs and initialized-but-empty KBs, reject any non-empty state-bearing KB tables.
- Extended `agentic/tests/test_snapshot_command.py` with focused unit coverage for success and rejection paths plus failure ordering.
- Extended `agentic/tests/test_snapshot_command_db.py` with DB-backed coverage for schema-missing success, initialized-but-empty success, and rejection for existing `kb_documents`, `kb_sync_state`, and `kb_snapshot_manifest` rows.

## Verification Results

- `python3 -m py_compile "agentic/src/agentic_kb/commands/snapshot.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `AGENTIC_DB_PORT=6545 docker compose -p task612-verify -f docker-compose.agentic.yml up -d paradedb` with an in-container psycopg readiness wait, followed by `docker compose -p task612-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb -e OLLAMA_EMBED_MODEL=all-minilm:l6-v2 kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command_db.py'`, passed.

## User Inputs And Manual Validation

- Required user inputs: none for implementation planning or the expected code/test changes.
- Required manual test steps: none required to begin implementation; automated unit and DB-backed tests should be sufficient for this task's acceptance boundary.
- Evidence needed back from user: none before implementation can proceed.
- Can implementation proceed before user interaction: yes.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-612-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-612-impl-review.md`

## Review Outcome

- Planning review is complete and approved in `.agent/plans/agentic/task-plans/task-612-plan-review.md`.
- Implementation review is complete and approved in `.agent/plans/agentic/task-plans/task-612-impl-review.md`.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-612-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because iteration 1 of `.agent/plans/agentic/task-plans/task-612-impl-review.md` ended with `Decision: approved` after the implementation, focused tests, and durable research update landed.
