# Task Plan: task-604 Implement import-then-sync-changed bootstrap flow

- Task ID: `task-604`
- Title: `Implement import-then-sync-changed bootstrap flow`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-604` is the next critical-path gap after `task-602`: the workflow already tells developers to import a shared snapshot and then run `sync changed`, but the live packaged CLI still exposes only placeholder `sync` commands in `agentic/src/agentic_kb/commands/sync.py`.
- `task-405` now persists durable repo/GitHub/Project sync state, and `task-602` now imports a validated snapshot that restores that `kb_sync_state` baseline plus records the imported manifest. That makes the post-import bootstrap flow implementation-ready without waiting for the full sync command family.
- This task closes the immediate developer bootstrap gap for the shared-snapshot workflow while keeping broader `sync docs`, `sync code`, `sync github`, `sync project`, and `sync all` orchestration deferred to `task-701`.
- The task tracker still points `task-604.targetPath` at stale un-packaged code, so the plan needs to anchor implementation against the actual packaged command surface before any work starts.

## Scope

- Implement a real packaged `agentic-kb sync changed` command for the post-import bootstrap path.
- Consume imported snapshot baseline state from the restored `agentic.kb_sync_state` rows, with the imported `agentic.kb_snapshot_manifest` row treated as provenance that the baseline came from a validated snapshot import.
- Refresh docs and code only for repo paths that changed since the imported docs/code baseline commit hashes.
- Refresh remote state only to the extent current ingestion contracts can do so safely without expanding task-604 into a broader remote-sync redesign.
- For GitHub, bound refresh in task-604 only for the streams whose `updated_since` watermark is actually sent upstream today: `issues` and `issue_comments`.
- For Project 5, treat task-604 refresh as cursor-continuation only from the restored baseline, not as a complete bounded-update solution for already-seen items.
- Keep the implementation narrow enough that a developer can go from `snapshot import ... --yes` to `sync changed` without a full rebuild.
- Add focused command-level and DB-backed verification for the bootstrap path.

## Non-Goals

- Do not implement `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all`; those remain `task-701`.
- Do not make `sync changed` a general first-sync substitute on an empty KB. If the required baseline rows are missing, fail clearly instead of silently widening into full-ingest behavior.
- Do not add stale-index detection, operator warnings about freshness, or status UX changes; that remains `task-702` and related follow-ups.
- Do not broaden this task into snapshot publication automation, CI artifact workflow, or baseline distribution; that remains `task-603`.
- Do not redesign manifest or sync-state schemas unless implementation proves a concrete blocker.
- Do not make task-604 responsible for adding true bounded delta support for GitHub `pulls` or `review_comments`, or for Project updates to already-seen items; those remain follow-up remote-ingestion work outside this bootstrap task.

## Relevant Dependencies

- Declared task dependencies already satisfied:
  - `task-405` - `agentic.kb_sync_state` now persists repo commit baselines, GitHub watermarks, and Project cursor state in `agentic/src/agentic_kb/sync/state.py`.
  - `task-602` - `snapshot import` now restores that state from a validated dump/manifest pair and records imported snapshot provenance in `agentic.kb_snapshot_manifest`.
- Practical completed prerequisites already present in repo reality:
  - `task-301` - docs ingestion exists in `agentic/src/agentic_kb/ingest/docs.py`.
  - `task-402` - full-repository code ingestion exists in `agentic/src/agentic_kb/ingest/code.py`.
  - `task-403` - GitHub ingestion already accepts bounded fetch input in `agentic/src/agentic_kb/ingest/github.py`.
  - `task-404` - Project ingestion already accepts cursor-bounded fetch input in `agentic/src/agentic_kb/ingest/project.py`.
  - `task-503` - the packaged CLI surface in `agentic/src/agentic_kb/cli.py` is stable and already routes `sync` through `agentic/src/agentic_kb/commands/sync.py`.
- Downstream boundary to preserve:
  - `task-701` still owns the full sync command family and the general orchestration surface.
- Tracker drift to capture when implementation lands:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently lists `task-604.targetPath` as `agentic/src/sync/commands.py`, but the live packaged command surface is `agentic/src/agentic_kb/commands/sync.py`.
  - Nearby `task-701.targetPath` also still points at the stale un-packaged path; note that drift, but keep tracker cleanup scoped to `task-604` unless adjacent correction is required for consistency.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/src/agentic_kb/snapshot_manifest.py`
  - `agentic/src/agentic_kb/sync/state.py`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/code.py`
  - `agentic/src/agentic_kb/ingest/github.py`
  - `agentic/src/agentic_kb/ingest/project.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-604.targetPath` to the packaged sync command module and update only `task-604` status metadata when implementation completes.
- `agentic/src/agentic_kb/commands/sync.py` - replace the `sync changed` placeholder with real bootstrap behavior while leaving the other sync verbs explicitly deferred to `task-701`.
- `agentic/src/agentic_kb/sync/state.py` - only if small read helpers are needed to load repo/GitHub/Project baseline rows cleanly for the command.
- `agentic/src/agentic_kb/ingest/docs.py` - add the minimum targeted docs update/delete support needed for changed-path sync after import.
- `agentic/src/agentic_kb/ingest/code.py` - add or expose the minimum targeted changed-path and deleted-path handling needed for `sync changed`.
- `agentic/tests/test_sync_command.py` - new focused unit coverage for command routing, baseline validation, changed-path selection, bounded remote sync decisions for supported streams, and explicit deferral/limitation behavior for unsupported remote deltas.
- `agentic/tests/test_sync_command_db.py` - new DB-backed verification for import-then-sync-changed behavior against a real KB database.
- `.agent/workflows/agentic-kb.md` - update the workflow note so `sync changed` is documented as real bootstrap behavior instead of a future placeholder.
- `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md` - capture durable findings about the accepted baseline contract, bounded refresh behavior, and any implementation caveats.

## Implementation Approach

- **Use the packaged command surface**: implement `sync changed` in `agentic/src/agentic_kb/commands/sync.py` and do not create a parallel un-packaged sync command path.
- **Trust the imported baseline through live state, not manifest replay**: after `snapshot import`, the operational baseline should come from restored `agentic.kb_sync_state` rows. The imported `kb_snapshot_manifest` row proves the DB came from a validated snapshot, but `sync changed` should read current sync-state rows as its source of truth so later `task-701` can reuse the same read path.
- **Bootstrap-only precondition**: require the expected sync-state rows for docs, code, GitHub streams, and Project 5 to exist before running bounded refresh. If that baseline is missing, fail with guidance to import a snapshot or wait for the fuller `task-701` sync family rather than widening into an implicit full sync.
- **Per-source repo baseline**: use the restored docs and code repo commit hashes independently. The command should not assume docs and code were indexed at the same commit because task-602 manifest/export explicitly allows separate docs/code baselines.
- **Local delta detection**: compute changed repo paths from each source baseline commit to current `HEAD` using git history rather than mtimes. For docs, intersect the diff with the existing docs allowlist; for code, intersect with the existing supported code-path rules.
- **Changed docs support**: `task-604` should add only the minimal targeted docs store behavior needed to replace or delete rows by `source_path` so the KB can reflect added, modified, renamed, and deleted allowlisted docs after import. This is narrower than general docs sync orchestration.
- **Changed code support**: reuse or slightly extend the existing per-path code replacement contract so changed files can be re-indexed and deleted files can have their rows removed without running full-repository prune mode.
- **GitHub remote contract for task-604**: derive `GithubFetchBounds(updated_since=...)` from restored sync-state rows only for `issues` and `issue_comments`, because those are the only streams whose watermark is sent upstream today. `pulls` and `review_comments` should not be claimed as bounded by this task; `sync changed` should either skip them with explicit operator output or otherwise leave them deferred, rather than silently performing a broader replay while claiming a bounded bootstrap.
- **Project remote contract for task-604**: derive `ProjectFetchBounds(after_cursor=...)` from restored sync-state rows and treat the resulting fetch as append-only cursor continuation from the imported baseline. Do not claim that task-604 detects updates to already-seen Project items, because current ingestion does not use the stored watermark for that. Operator output and acceptance criteria should describe this as cursor-based continuation only.
- **Atomicity boundary**: the command does not need one giant cross-source transaction, but each source update should keep the existing ingest-plus-sync-state success/failure contract from `task-405`. A later source failure must not silently erase a prior source's successful baseline update.
- **Clear operator output**: report which sources were updated, skipped because no local diff was found, or blocked by missing baseline. Output should make it obvious that this is the post-import bootstrap path, not the eventual full sync family.
- **Task-604 versus task-701 boundary**:
  - `task-604` should deliver one trustworthy `sync changed` path specifically for an already-imported KB with restored baseline state.
  - `task-604` may add small reusable helpers needed to compute changed paths and load baseline rows.
  - `task-604` should narrow remote guarantees to what current ingestion can support safely: GitHub `issues`/`issue_comments` with upstream `updated_since`, and Project cursor continuation only.
  - `task-604` should leave all other sync verbs as placeholders and should not add a generic full-sync fallback.
  - `task-701` should later generalize the sync orchestration surface, factor shared helpers where useful, and add the remaining sync subcommands on top of the bootstrap behavior proven here, while any deeper remote-ingestion redesign stays separately scoped unless intentionally pulled in.

## Acceptance Criteria

- `agentic-kb sync changed` is implemented in the packaged CLI and no longer returns the current placeholder guidance for that subcommand.
- The command requires a trustworthy imported or otherwise pre-existing sync-state baseline and fails clearly if the needed `kb_sync_state` rows are absent.
- After a validated `snapshot import`, `sync changed` uses the restored docs/code repo commit hashes as the local baseline and the restored GitHub/project watermarks or cursor as the remote baseline.
- Docs refresh only allowlisted repo paths that changed since the docs baseline commit, including deletion handling so stale rows for removed docs do not remain.
- Code refresh only supported code paths that changed since the code baseline commit, including deletion handling so stale rows for removed code files do not remain.
- GitHub refresh in task-604 is guaranteed to be watermark-bounded only for the `issues` and `issue_comments` streams, using stored `updated_since` state that is sent upstream by current ingestion.
- `sync changed` does not claim bounded bootstrap refresh for GitHub `pulls` or `review_comments`; implementation must leave those streams explicitly deferred or skipped with clear operator output rather than implying they were safely bounded.
- Project refresh in task-604 is guaranteed only as cursor-based continuation from the stored `after_cursor` baseline.
- `sync changed` does not claim detection of updates to already-seen Project 5 items; if no minimum ingestion change is added for that behavior, operator output and tests must reflect that this task guarantees only append-style continuation beyond the imported cursor.
- Successful source updates persist refreshed `kb_sync_state` rows using the existing task-405 attempt/success/failure contract.
- The implementation does not add `sync docs`, `sync code`, `sync github`, `sync project`, or `sync all` behavior; those remain explicitly deferred to `task-701`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` records the packaged `task-604.targetPath` rather than the stale un-packaged path when implementation lands.

## Verification Plan

- Run `python3 -m py_compile` on all touched Python modules and tests.
- Add focused unit coverage for:
  - `sync changed` command routing and non-placeholder behavior
  - missing-baseline rejection with clear operator guidance
  - loading docs/code/GitHub/project baseline rows from `kb_sync_state`
  - per-source changed-path selection from baseline commit to `HEAD`
  - changed docs allowlist filtering, including deleted-path handling
  - changed code filtering and deleted-path handling
  - bounded `GithubFetchBounds(updated_since=...)` derivation only for GitHub `issues` and `issue_comments`
  - explicit skip/defer behavior for GitHub `pulls` and `review_comments` so task-604 does not overclaim bounded support
  - `ProjectFetchBounds(after_cursor=...)` derivation from restored sync-state rows with assertions limited to cursor continuation semantics
  - task-boundary behavior where other sync verbs remain placeholders
- Add DB-backed verification that:
  - imports a validated snapshot into an isolated ParadeDB database
  - mutates local repo fixtures or uses controlled changed-path fixtures
  - runs `sync changed`
  - proves only changed docs/code rows are updated or removed
  - proves GitHub `issues`/`issue_comments` sync uses prior watermarks rather than empty-state behavior
  - proves any GitHub `pulls`/`review_comments` handling matches the narrowed contract (explicitly skipped/deferred, or otherwise not advertised as bounded)
  - proves Project sync resumes from the prior cursor rather than empty-state behavior, without asserting already-seen item update detection unless task-604 explicitly adds that implementation work
  - proves refreshed sync-state rows are written back after success
- Reuse the existing snapshot command tests or fixtures where possible so the DB-backed bootstrap path starts from a real imported baseline rather than synthetic hand-written state.
- Rebuild and run the touched suites inside `kb-tools` so packaged runtime behavior matches local source execution.

## Risks / Open Questions

- **Docs deletion support is missing today**: `agentic/src/agentic_kb/ingest/docs.py` currently upserts documents but does not expose a path-level delete/replace contract, so `task-604` likely needs a small docs-store extension to keep `sync changed` trustworthy for removed docs.
- **Baseline commit availability**: the imported baseline commit hash may be absent from a developer's local clone in edge cases such as shallow history. The implementation should fail clearly rather than guessing a fallback diff base.
- **Diff semantics after rebases**: using the stored baseline commit to compute local deltas is correct for the import-then-sync bootstrap goal, but a rebased branch can cause the diff to include both branch-local work and newer upstream changes missing from the imported snapshot. That is acceptable for `task-604`, but it should be documented as baseline catch-up rather than purely branch-only change detection.
- **Manifest versus live state precedence**: task-602 restores both `kb_sync_state` and `kb_snapshot_manifest`. This plan intentionally treats live sync-state rows as the operational baseline and the imported manifest as audit/provenance only; implementation should keep that precedence explicit.
- **GitHub stream asymmetry**: current ingestion only sends `updated_since` upstream for `issues` and `issue_comments`. The plan therefore narrows task-604's bounded GitHub guarantee to those streams and avoids silently expanding the task into pull/review-comment ingestion redesign.
- **Project update blind spot**: current `ProjectFetchBounds` is cursor-only, so task-604 can safely promise continuation after the imported cursor but not refreshed updates to already-seen items. That limitation should stay explicit until a follow-up task expands the remote contract.
- **Boundary pressure from task-701**: the shared helpers added here should stay narrowly bootstrap-oriented so `task-604` does not accidentally absorb the remaining sync command family.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final implementation notes, verification notes, outcome, and final statuses when `task-604` lands.
- Append planning review decisions in `.agent/plans/agentic/task-plans/task-604-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-604-impl-review.md` yet; that starts only when implementation begins.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-604` metadata, including the packaged `targetPath` correction and final status, unless tightly scoped adjacent drift must be fixed for consistency.
- Add `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md` with durable findings about the accepted baseline contract and bounded refresh behavior.
- Update `.agent/workflows/agentic-kb.md` so the documented fast-start workflow matches the real implemented `sync changed` bootstrap path.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-604-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-604-impl-review.md`

## Implementation Notes

- Landed the real packaged `agentic-kb sync changed` bootstrap path in `agentic/src/agentic_kb/commands/sync.py` while keeping `sync all`, `sync docs`, `sync code`, `sync github`, and `sync project` as explicit placeholders for `task-701`.
- The command now requires restored `kb_sync_state` rows for docs, code, all GitHub stream keys, and Project 5 before it runs, using live sync-state rows as the operational baseline and not replaying the imported manifest.
- Docs and code delta detection now uses per-source baseline commits to current `HEAD`, with docs filtered through the existing allowlist and code filtered through existing supported code-path rules.
- Rename handling now treats git renames as delete-plus-add for both docs and code so stale old-path rows are removed while the new path is re-ingested.
- Added path-level docs deletion support in `agentic/src/agentic_kb/ingest/docs.py` so removed allowlisted docs no longer leave stale indexed rows after bootstrap sync.
- GitHub bootstrap behavior is intentionally narrowed to bounded `issues` and `issue_comments` refresh only, with `pulls` and `review_comments` left explicitly deferred in operator output and persisted sync-state metadata.
- Supported GitHub bootstrap streams now require restored `watermark_timestamp` values; `sync changed` fails clearly if `issues` or `issue_comments` lacks that baseline instead of widening to `updated_since=None`.
- Project bootstrap behavior resumes only from a restored non-empty stored `after_cursor` baseline and fails clearly if `cursor_text` is missing or empty; it does not claim already-seen item update detection.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/sync.py" "agentic/src/agentic_kb/ingest/docs.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py"`
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command`
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db`
- The focused unit suite passed.
- The focused unit suite now also covers rename handling for docs and explicit rejection when a supported GitHub stream baseline exists without a restored watermark.
- The focused unit suite now also covers explicit rejection when the restored Project baseline row exists but `cursor_text` is missing.
- The DB-backed suite was strengthened to exercise `sync_changed` directly with a real KB database plus patched remote ingestors, verifying rename cleanup for docs/code, supported GitHub watermark reuse, deferred unsupported GitHub streams, and Project cursor continuation semantics.
- The DB-backed suite now also includes a gated failure-mode case proving `sync changed` rejects a restored Project row with `cursor_text=None` instead of widening to `after_cursor=None`.
- In this environment the DB-backed suite still could not execute because `AGENTIC_TEST_DATABASE_URL`, `psycopg`, and optional tree-sitter parser dependencies were all absent.

## Outcome

- `task-604` remains implemented in a narrowly bootstrap-oriented form matching the approved plan: packaged `sync changed` is real, local docs/code sync only changed paths since their own baselines including deletions and renames, GitHub bounded guarantees are limited to `issues` and `issue_comments` with required restored watermarks, Project behavior now requires a restored non-empty cursor for continuation-only semantics, and all other sync verbs remain deferred.
