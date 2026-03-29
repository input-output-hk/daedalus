# Task Plan: task-701 Implement sync commands for all sources

- Task ID: `task-701`
- Title: `Implement sync commands for all sources`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-701` is the next unblocked critical-path task after `task-604`: the packaged CLI already exposes `sync` subcommands in `agentic/src/agentic_kb/commands/sync.py`, but `sync all`, `sync docs`, `sync code`, `sync github`, and `sync project` are still placeholders while only the bootstrap-only `sync changed` path is real.
- The required foundations are already in repo reality: docs ingestion (`task-301`), full-repository code ingestion (`task-402`), GitHub ingestion (`task-403`), Project 5 ingestion (`task-404`), sync-state persistence (`task-405`), search/status CLI (`task-503`), snapshot export/import (`task-602`), and the post-import bootstrap behavior (`task-604`).
- This task closes the main operator gap between the workflow and the packaged CLI by turning the full sync command family into real orchestration on top of those existing ingestion and sync-state contracts.
- The tracker currently drifts from repo reality by pointing `task-701.targetPath` at stale `agentic/src/sync/commands.py` instead of the packaged command module `agentic/src/agentic_kb/commands/sync.py`, so the canonical plan needs to anchor implementation against the real surface before code work begins.

## Scope

- Implement real packaged `agentic-kb sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, and `sync all` behavior in `agentic/src/agentic_kb/commands/sync.py`.
- Reuse the current ingestion entrypoints and `kb_sync_state` model rather than introducing a second sync framework.
- Keep `sync docs` and `sync code` focused on explicit source-scoped refresh of the current local checkout, including removals of previously indexed rows that are now out of scope for the source.
- Keep `sync github` focused on the current GitHub ingestion contracts: one general four-stream ingest path, server-side `since` where supported, ordered fetch plus client-side filtering where not, and state persistence per stream.
- Keep `sync project` focused on the current Project 5 ingestion contract: initial full ingest from `after_cursor=None` when explicitly requested, then cursor-based continuation on later runs without claiming replay of already-seen items.
- Redefine `sync changed` from the narrower task-604 bootstrap path into the general incremental operator command for an already-seeded KB, while preserving the fast post-import workflow as a supported case.
- Implement `sync all` as the minimal orchestration layer that runs the source-specific sync commands in a predictable order and reports a combined result.
- Add focused command and DB-backed tests that prove source-specific orchestration and sync-state updates through the packaged CLI surface.

## Non-Goals

- Do not implement stale-index detection, freshness warnings, or status-driven staleness UX; that remains `task-702`.
- Do not implement scheduled GitHub Actions refresh, CI snapshot publication cadence, or other automation; that remains `task-703`.
- Do not implement MCP behavior or agent-facing read-only search tools; that remains `task-801` and follow-ups.
- Do not redesign GitHub ingestion beyond the current REST-stream model unless a minimal command-layer change proves impossible without it.
- Do not redesign Project ingestion into full update detection for already-seen items if current cursor-based ingestion cannot support that within task scope.
- Do not silently turn `sync changed` into a first-sync substitute for an empty or partially seeded KB; missing source baselines must fail clearly instead of widening into source-local full sync.
- Do not absorb broader docs hashing/idempotency redesign, deeper remote replay optimization, or schema changes unless a concrete blocker is found and documented.

## Relevant Dependencies

- Declared dependencies already satisfied:
  - `task-301` - docs ingestion exists in `agentic/src/agentic_kb/ingest/docs.py`.
  - `task-402` - full repository code ingestion exists in `agentic/src/agentic_kb/ingest/code.py`.
  - `task-403` - GitHub issues, PRs, and comment ingestion exists in `agentic/src/agentic_kb/ingest/github.py`.
  - `task-404` - Project 5 ingestion exists in `agentic/src/agentic_kb/ingest/project.py`.
  - `task-405` - sync-state persistence exists in `agentic/src/agentic_kb/sync/state.py`.
- Practical completed prerequisites already present:
  - `task-503` - packaged CLI routing is stable in `agentic/src/agentic_kb/cli.py`.
  - `task-602` - snapshot import/export provides the seeded KB workflow that `sync changed` builds on.
  - `task-604` - current `sync changed` already proves the packaged command surface and bootstrap-only delta behavior.
- Downstream boundaries to preserve:
  - `task-702` owns stale-index detection.
  - `task-703` owns scheduled automation and snapshot refresh workflow.
  - `task-801` owns MCP exposure.
- Tracker drift to reconcile when implementation lands:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently lists `task-701.targetPath` as `agentic/src/sync/commands.py`, but repo reality uses `agentic/src/agentic_kb/commands/sync.py`.
- Primary references reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-402-repository-code-coverage.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-404-project-ingestion.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/research/task-503-search-cli-and-status-commands.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/cli.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-701.targetPath` to the packaged sync module and update only `task-701` metadata when implementation lands.
- `agentic/src/agentic_kb/commands/sync.py` - replace placeholder sync verbs with real orchestration and revise `sync changed` from bootstrap-only behavior into the task-701 incremental command contract.
- `agentic/src/agentic_kb/cli.py` - only if small help text updates are needed so the CLI no longer describes `sync` as reserved for later tasks.
- `agentic/src/agentic_kb/ingest/docs.py` - only if a narrow full-source sync helper or prune-capable path is needed for docs refresh.
- `agentic/src/agentic_kb/ingest/code.py` - only if a narrow command-facing full-source or incremental helper is needed beyond the current targeted ingest path.
- `agentic/src/agentic_kb/ingest/github.py` - only if a narrow reusable helper is needed to expose current per-stream incremental behavior more cleanly to the sync command layer.
- `agentic/src/agentic_kb/ingest/project.py` - only if a narrow reusable helper is needed for cursor-based project sync orchestration.
- `agentic/src/agentic_kb/sync/state.py` - only if small helper additions are needed to load and persist source-specific sync state consistently.
- `agentic/tests/test_sync_command.py` - extend focused unit coverage for all sync verbs, task-701 `sync changed` semantics, and `sync all` orchestration.
- `agentic/tests/test_sync_command_db.py` - extend DB-backed verification for source-scoped and combined sync behavior.
- `.agent/workflows/agentic-kb.md` - update only the sync-command wording needed so shipped `sync` behavior no longer contradicts the workflow doc; broader workflow finalization remains `task-803`.
- `.agent/plans/agentic/research/` - add a task-701 research note capturing durable command-contract decisions and remote-source caveats discovered during implementation.

## Implementation Approach

- **Use the packaged sync surface**: implement all task-701 command behavior in `agentic/src/agentic_kb/commands/sync.py` and do not create any parallel un-packaged sync module.
- **Preserve source-specific orchestration**: treat docs, code, GitHub, and Project as separate sync units with their own attempt/success/failure state and operator output. `sync all` should compose those units rather than bypassing them.
- **Docs sync contract**: `sync docs` should refresh the current allowlisted docs corpus from the current checkout and remove rows for previously indexed allowlisted docs that no longer exist or are no longer discovered by the task-301 allowlist. Prefer the smallest implementation that reuses existing docs preparation/store logic.
- **Code sync contract**: `sync code` should refresh the supported code corpus from the current checkout using the task-402 discovery rules, and remove rows for previously indexed supported paths that are now deleted or excluded by current support rules. Prefer existing code-ingest pruning behavior instead of adding a second deletion model.
- **GitHub sync contract**: `sync github` should use the existing general `ingest_github(...)` path across all four streams, not the narrower task-604 selective-stream path. On first explicit `sync github`, absent GitHub state should trigger a full four-stream ingest with `updated_since=None` and then seed all four per-stream rows. On later runs, the command should derive one shared lower-bound `updated_since` from the earliest stored GitHub watermark across `issues`, `pulls`, `issue_comments`, and `review_comments`, call the same general ingest path once, let `issues` and `issue_comments` use upstream `since`, and let `pulls` and `review_comments` rely on ordered fetch plus client-side filtering from that same lower bound. Operator output and tests must keep that stream asymmetry explicit.
- **Project sync contract**: `sync project` should use the existing cursor-based `ingest_project_items(...)` path. On first explicit `sync project`, absent project state should trigger a full ingest from `after_cursor=None` and then seed the project sync-state row. On later runs, the command should continue only from the stored cursor. If the stored cursor is already at the end, repeated runs should succeed as no-op continuation with unchanged cursor when no new pages exist. The command contract must stay explicit that current Project ingestion does not replay earlier pages and may miss edits to already-seen items.
- **Task-701 `sync changed` contract**: broaden `sync changed` beyond the task-604 import bootstrap case. After task-701, `sync changed` should mean: use stored sync state where available to refresh only source-specific deltas since the last successful sync for docs, code, GitHub, and Project; when the KB was seeded from `snapshot import`, that imported baseline remains a valid starting point for this same command. The command should stop describing itself as bootstrap-only.
- **`sync changed` local-source semantics**: for docs and code, compute deltas from each source's last successful `repo_commit_hash` to current `HEAD`, including deletions and renames, using the same path filters task-604 already established.
- **`sync changed` remote-source semantics**: for GitHub, use the same four-stream `ingest_github(...)` path as `sync github`, but require existing successful per-stream watermarks and derive the shared lower-bound `updated_since` from the earliest stored watermark instead of widening to an unbounded replay. For Project, continue from the stored cursor and be explicit that this is continuation, not full stale-item reconciliation.
- **`sync changed` missing-baseline contract**: `sync changed` is never the first-sync substitute. It should fail the command before any source work starts when any required baseline is missing or incomplete, with source-specific guidance to run `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all` first. Lock the source rules explicitly: docs require an existing successful `docs` sync-state row with non-empty `repo_commit_hash`; code requires an existing successful `code` row with non-empty `repo_commit_hash`; GitHub requires all four per-stream rows with non-null successful watermarks; Project requires an existing successful project row with non-empty `cursor_text`. There is no source-local fallback from `sync changed` into initial full sync.
- **`sync all` semantics**: `sync all` is the explicit full-refresh orchestrator for `sync docs`, `sync code`, `sync github`, then `sync project` in that order. It does not call or embed `sync changed`. It should execute sequentially and stop on the first source failure, returning non-zero while reporting earlier successes plus the first failing source; later sources remain unattempted.
- **Output contract**: keep operator output concise and source-specific. It should show what ran, which baselines or bounds were used, what was updated/deleted/skipped, and any explicit limitations for GitHub `pulls`/`review_comments` and Project cursor-only behavior.
- **Minimal tracker cleanup**: when implementation starts or lands, fix `task-701.targetPath` in the tasks JSON, but do not broaden that cleanup into unrelated tracker churn.
- **Task boundary with task-604**:
  - `task-604` delivered a trustworthy post-import bootstrap path.
  - `task-701` should keep that path working, but widen `sync changed` into the general incremental command for already-seeded KBs.
  - `task-701` should not remove the snapshot-import fast-start workflow; it should fold it into the broader command semantics.
  - `task-701` should not absorb stale detection, scheduled automation, or deeper remote-ingestion redesign unless a concrete blocker requires a narrowly documented exception.

## Acceptance Criteria

- `agentic-kb sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, and `sync all` are implemented in the packaged CLI and no longer return placeholder guidance.
- `sync docs` refreshes the current allowlisted docs corpus and removes stale indexed rows for allowlisted docs that were deleted or are no longer discovered.
- `sync code` refreshes the current supported code corpus and removes stale indexed rows for supported files that were deleted or are no longer supported.
- `sync docs`, `sync code`, `sync github`, and `sync project` can each run as an explicit initial source sync when their own sync-state row or rows are absent; `sync changed` cannot.
- `sync github` uses the current general `ingest_github(...)` flow across `issues`, `pulls`, `issue_comments`, and `review_comments` rather than preserving the task-604 selective-stream behavior.
- On incremental `sync github` runs, the command derives one shared lower-bound `updated_since` from the earliest stored GitHub watermark across the four streams, performs one four-stream ingest, and keeps the current research-backed contract explicit: upstream `since` applies only to `issues` and `issue_comments`, while `pulls` and `review_comments` rely on ordered fetch plus client-side filtering.
- `sync project` reuses stored project cursor state, succeeds as a no-op continuation when rerun at the current end cursor with no new pages, and keeps the current cursor-based limitation explicit rather than claiming full update detection for already-seen items.
- After task-701, `sync changed` is documented and implemented as the general incremental sync command for an already-seeded KB, not only as the task-604 post-import bootstrap path.
- The task-604 snapshot-import workflow remains supported: after importing a validated snapshot, `sync changed` can still use the restored sync-state baseline as its starting point.
- `sync changed` computes docs/code deltas from each source's own stored `repo_commit_hash` to current `HEAD`, including deletions and renames.
- `sync changed` fails clearly before doing work when any required docs/code/GitHub/Project baseline is missing or incomplete, and its error text names the missing source contract instead of widening into a silent first-sync fallback.
- `sync changed` reuses GitHub watermarks and Project cursor state without overclaiming guarantees beyond the current ingestion implementations.
- `sync all` orchestrates only `sync docs`, `sync code`, `sync github`, and `sync project` in that fixed order, explicitly excludes `sync changed`, and stops on the first failing source.
- Successful sync commands persist refreshed `kb_sync_state` rows using the existing task-405 attempt/success/failure contract.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is corrected to point `task-701.targetPath` at `agentic/src/agentic_kb/commands/sync.py` before the task is marked complete.

## Verification Plan

- Run `python3 -m py_compile` on all touched Python modules and tests.
- Extend focused unit coverage in `agentic/tests/test_sync_command.py` for:
  - parser routing for every sync verb
  - non-placeholder behavior for `sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, and `sync all`
  - source-specific initial-sync behavior for explicit source commands versus source-specific missing-baseline failure for `sync changed`
  - task-701 `sync changed` semantics versus the older bootstrap-only messaging
  - `sync all` excluding `sync changed`, fixed ordering, stop-on-first-failure behavior, and aggregation of already-completed earlier sources
  - docs/code delta selection, deletion, and rename handling
  - GitHub bounds derivation from sync state, with assertions that task-701 uses the general four-stream ingest path once per command, that only `issues` and `issue_comments` send upstream `updated_since`, and that `pulls` and `review_comments` stay client-filtered under the same lower bound
  - explicit operator messaging around `pulls`/`review_comments` limitations and the chosen task-701 contract versus the narrower task-604 defer behavior
  - Project initial full sync from empty state, repeated cursor-continuation no-op behavior when no new pages exist, and explicit messaging that already-seen item edits can still be missed under current cursor-only limits
- Extend DB-backed coverage in `agentic/tests/test_sync_command_db.py` to prove:
  - source-scoped sync commands update the real KB tables and sync-state rows
  - `sync changed` still works from an imported-style seeded baseline
  - `sync changed` fails before writing when one required source baseline is missing
  - `sync all` runs the expected source sequence, excludes `sync changed`, stops after the first failing source, and persists only the attempted source results
  - stale docs/code rows are removed on source-scoped syncs
  - initial `sync github` and `sync project` seed their own empty-state sync rows, while later incremental runs reuse previously stored state rather than behaving like empty-state runs
  - incremental `sync github` updates all four stream rows through the general ingest path and preserves the stream-specific watermark semantics in DB state
  - repeated `sync project` runs at the current end cursor leave the cursor unchanged and do not falsely claim older edited items were refreshed
- Rebuild and run the touched suites inside `kb-tools` so packaged runtime behavior matches local source execution.
- Update only the task-701-owned sync-command wording in `.agent/workflows/agentic-kb.md` and verify those lines match the shipped CLI semantics after implementation without broadening into the full workflow finalization reserved for `task-803`.

## Risks / Residual Constraints

- **Docs full-sync helper shape**: task-301 proved file-level docs ingest, but task-701 may need a narrow prune-capable helper to make `sync docs` trustworthy without broadening into unrelated docs-ingest redesign.
- **GitHub stream asymmetry remains real**: task-403 confirmed only `issues` and `issue_comments` support upstream `since`; `pulls` and `review_comments` still require ordered fetch plus client-side filtering, so task-701 must be careful not to overstate incremental guarantees.
- **Project continuation limits remain real**: task-404 and task-604 research confirm current project sync is cursor-based and does not fully detect updates to already-seen items. Task-701 should surface that limitation clearly instead of hiding it behind `sync project` wording.
- **Scope pressure from automation/staleness follow-ups**: operator-facing sync work can easily bleed into `task-702` and `task-703`; the plan should keep those boundaries explicit during critique.

## Required Docs / Tracking / Research Updates

- This canonical task plan now records the final approved implementation notes, verification notes, outcome, and review references for `task-701`.
- Planning review history is preserved in `.agent/plans/agentic/task-plans/task-701-plan-review.md`.
- Implementation review history is preserved in `.agent/plans/agentic/task-plans/task-701-impl-review.md`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` now points `task-701.targetPath` at `agentic/src/agentic_kb/commands/sync.py` and marks the task `completed`.
- `.agent/plans/agentic/research/task-701-sync-commands.md` captures the durable sync-command contract and final verification state.
- `.agent/workflows/agentic-kb.md` contains only the task-701-owned sync-command wording changes needed to match shipped behavior; broader workflow finalization remains `task-803`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-701-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-701-impl-review.md`

## Implementation Notes

- Implemented the full packaged sync family in `agentic/src/agentic_kb/commands/sync.py`: `sync docs`, `sync code`, `sync github`, `sync project`, `sync changed`, and `sync all` now run real orchestration instead of placeholder guidance.
- Kept the task-701 behavior entirely on the packaged CLI surface and reused the existing ingestion entrypoints plus `kb_sync_state` attempt/success/failure persistence instead of introducing a second sync framework.
- `sync docs` now refreshes the allowlisted docs corpus from the current checkout and prunes stale allowlisted KB rows by comparing discovered allowlist paths against currently stored document paths. This required a narrow `list_document_paths()` read helper in `agentic/src/agentic_kb/ingest/docs.py`.
- `sync code` now reuses the existing full-repository code ingest path with `prune_missing=True` so explicit code sync updates current supported files and removes stale supported-file rows without a new deletion model.
- `sync github` now uses the existing general four-stream `ingest_github(...)` path for both initial and incremental runs. Initial explicit sync can seed empty-state stream rows; later runs derive one shared lower-bound `updated_since` from the earliest stored watermark across `issues`, `pulls`, `issue_comments`, and `review_comments`.
- `sync project` now uses the existing cursor-based `ingest_project_items(...)` path for both initial seeding from `after_cursor=None` and later cursor continuation. Repeated end-cursor runs remain valid no-op continuations and still keep the already-seen-item edit limitation explicit.
- `sync changed` is now the general incremental sync command for an already-seeded KB. It still supports the task-604 snapshot-import workflow, but now requires complete successful baselines for docs, code, all four GitHub streams, and Project cursor state before doing any work.
- `sync all` now orchestrates only `sync docs`, `sync code`, `sync github`, and `sync project` in that fixed order and stops on the first failure.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/sync.py" "agentic/src/agentic_kb/cli.py" "agentic/src/agentic_kb/ingest/docs.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py"` passed.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command` passed locally after the iteration-2 GitHub regression fix (`Ran 13 tests`, `OK`, `1 skipped`).
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db` was environment-gated locally and skipped because `AGENTIC_TEST_DATABASE_URL` was unset.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src kb-tools -m unittest agentic.tests.test_sync_command` failed in-container because the current `kb-tools` image still lacks the `git` binary required by the new git-delta sync tests.
- An in-container DB-backed sync suite attempt after bringing up `paradedb` did not complete before the session timeout, so no authoritative packaged DB-backed sync pass was recorded in this environment.

## Outcome

- `task-701` is implemented on the approved packaged surface. The full sync command family now exists, source-specific sync-state rows are seeded and updated through the packaged commands, and `sync changed` now enforces the approved missing-baseline contract without the iteration-1 GitHub `NameError` regression.
- The final approved state includes the iteration-2 regression fix in `agentic/src/agentic_kb/commands/sync.py` plus the iteration-3 tracker reconciliation that left `task-701` marked `completed` with the packaged `targetPath` in `.agent/plans/agentic/knowledge-base-platform-tasks.json`.
- The workflow note, durable research note, and canonical plan now match the shipped task-701 behavior while leaving broader workflow finalization scoped to `task-803`.

## Review Outcome

- Planning review is complete and approved in `.agent/plans/agentic/task-plans/task-701-plan-review.md`.
- Implementation review is complete and approved in `.agent/plans/agentic/task-plans/task-701-impl-review.md`.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-701-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the implementation, iteration-2 regression fix, iteration-3 tracker reconciliation, focused verification, workflow sync wording, and durable research capture are complete.
