# Task Plan: task-702 Add stale-index detection

- Task ID: `task-702`
- Title: `Add stale-index detection`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-702` is the next high-priority Phase 7 gap after `task-701`: the KB can now sync all sources, but the shipped workflow still says automatic stale-index detection is not implemented.
- The required foundations already exist in repo reality: repo and remote baselines live in `agentic.kb_sync_state`, `sync changed` already depends on those baselines, and `status` plus MCP `kb_status` already provide the operator-facing inspection surface.
- Landing stale detection now unlocks the remaining freshness work in `task-703` and `task-704` without inventing a second status or sync interface later.

## Scope

- Add packaged stale-index detection logic for docs, code, GitHub, and Project sources.
- Reuse existing `kb_sync_state` rows as the source of truth for last successful baselines instead of introducing new persistence.
- Surface stale or missing-baseline results through the existing normal `agentic-kb status` report and MCP `kb_status` payload.
- Keep freshness separate from the current top-level readiness contract so imported-baseline/bootstrap success remains valid even when freshness lags.
- Keep local repo freshness focused on docs/code baselines versus current local `HEAD`.
- Keep remote freshness focused on lightweight GitHub and Project watermark checks versus stored sync-state watermarks.

## Non-Goals

- Do not add a new long-running daemon, background watcher, or auto-sync behavior.
- Do not change `sync` command semantics or absorb scheduled automation; those remain `task-701` and `task-703`.
- Do not redesign `kb_sync_state` unless implementation finds a concrete blocker that cannot be solved with the current fields.
- Do not turn `status --healthcheck` into a GitHub- or git-dependent freshness check.
- Do not repurpose top-level `StatusReport.ok` away from its current runtime/dependency/database readiness meaning.
- Do not claim perfect replay detection for already-seen Project items; current project ingestion remains cursor-continuation based.

## Relevant Dependencies

- Declared dependencies already satisfied:
  - `task-405` - repo commits, GitHub stream watermarks, and Project cursor state are already persisted in `agentic/src/agentic_kb/sync/state.py`.
  - `task-701` - the packaged sync command family now seeds and updates those baselines consistently.
- Practical upstream surfaces already shipped:
  - `task-503` - `agentic-kb status` exists in `agentic/src/agentic_kb/commands/status.py`.
  - `task-801` - MCP `kb_status` already reuses the status report serializer.
  - `task-803` - `.agent/workflows/agentic-kb.md` currently documents that stale detection is not yet shipped and will need a narrow freshness update when this task lands.
  - `task-901` - the clean-bootstrap contract is intentionally narrower than freshness detection and should stay that way.
- Tracker drift to reconcile when implementation lands:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently points `task-702.targetPath` at stale un-packaged path `agentic/src/sync/staleness.py`; repo reality should use packaged path `agentic/src/agentic_kb/sync/staleness.py`.

## Files Expected To Change

- `agentic/src/agentic_kb/sync/staleness.py` - new packaged stale-detection helpers and serializable result types.
- `agentic/src/agentic_kb/commands/status.py` - integrate stale detection into normal status collection and JSON/text output while keeping `--healthcheck` lightweight.
- `agentic/src/agentic_kb/ingest/github.py` - add a narrow metadata-only latest-watermarks helper for GitHub stream freshness checks.
- `agentic/src/agentic_kb/ingest/project.py` - add a narrow metadata-only latest-watermark helper for Project freshness checks.
- `agentic/src/agentic_kb/mcp/search_server.py` - only if a narrow adjustment is needed so `kb_status` exposes the extended serialized status shape cleanly.
- `agentic/tests/test_status_command.py` - add unit coverage for fresh, stale, missing, and skipped freshness states plus JSON serialization.
- `agentic/tests/test_github_ingest.py` - add mocked-payload unit coverage for the metadata-only GitHub watermark helper.
- `agentic/tests/test_project_ingest.py` - add mocked-payload unit coverage for the metadata-only Project watermark helper.
- `agentic/tests/test_mcp_search_server.py` - verify MCP `kb_status` includes stale-detection payloads without turning degraded freshness into a transport error.
- `agentic/tests/test_mcp_search_server_db.py` - extend DB-backed payload checks if the final status JSON shape changes materially.
- `.agent/workflows/agentic-kb.md` - replace the current "stale detection not shipped" note with the shipped freshness behavior and caveats.
- `.agent/plans/agentic/research/task-702-stale-index-detection.md` - capture durable decisions and verification outcomes.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update only `task-702` metadata and `targetPath` when implementation lands.

## Implementation Approach

- **Keep this library-first and packaged**: add one new packaged module, `agentic/src/agentic_kb/sync/staleness.py`, and have `status` consume it rather than embedding stale logic directly into CLI formatting code.
- **Reuse current sync-state rows**: docs and code should read repo-scoped rows keyed by `repo_scope_key(DEFAULT_SYNC_REPO)`. GitHub should read the existing four per-stream rows. Project should read the existing single project row. No new table or new sync-state row type should be added unless critique identifies a blocker.
- **Docs/code stale rule**: compare the stored successful `repo_commit_hash` for docs and code against the current local `HEAD`. If the source row is missing, has no successful baseline, or has no stored commit hash, report that source as missing baseline rather than fresh. If the stored commit differs from `HEAD`, report stale. If it matches, report fresh.
- **GitHub stale rule**: add one narrow metadata-only helper in `agentic/src/agentic_kb/ingest/github.py` that returns the latest remote watermark per stream for `issues`, `pulls`, `issue_comments`, and `review_comments`. It should make only the minimum API requests needed to read the newest `updated_at` values, return plain timestamps, and explicitly avoid embeddings, entity preparation, DB writes, or reuse of the full ingest path.
- **Project stale rule**: add one narrow metadata-only helper in `agentic/src/agentic_kb/ingest/project.py` that returns the latest observed remote Project item watermark only. It should use a minimal GraphQL metadata query, return just the latest timestamp needed for freshness comparison, and explicitly avoid full page/entity ingest, embeddings, or DB writes.
- **Remote optionality rule**: because `GITHUB_TOKEN` remains optional for status usage, remote GitHub/Project freshness should report `skipped` or `unavailable` when no token is configured instead of pretending those sources are fresh. This should not change `status --healthcheck` behavior.
- **Healthcheck boundary**: keep `status --healthcheck` unchanged and lightweight. Stale detection belongs only in normal `status` and MCP `kb_status`, not the healthcheck path used by Compose.
- **Status integration**: extend the normal status report with an explicit freshness section or equivalent freshness payload separate from readiness. Keep the existing top-level `StatusReport.ok` scoped to runtime/dependency/database readiness, and add a distinct freshness aggregate if one is needed.
- **Imported-baseline regression boundary**: preserve the shipped `task-901` bootstrap/readiness contract. After `snapshot import`, `status --json` must still be able to report readiness success (`ok = true`) even when freshness independently reports stale or skipped sources because the imported baselines are older than local `HEAD` or remote watermarks.
- **Degraded-but-readable behavior**: stale or missing-baseline freshness should be visible to humans and agents through the new freshness payload, but it must not silently broaden the current readiness contract or the non-healthcheck exit-code behavior.
- **Project limitation stays explicit**: project freshness can detect that the stored watermark lags behind the latest observed Project update, but it must not claim that current cursor-based sync can reconcile every older edited item.
- **Keep `task-701` boundaries intact**: this task adds detection and warnings only. It should not auto-run `sync changed`, auto-reseed missing baselines, or mutate `kb_sync_state` during freshness checks.

## Acceptance Criteria

- A packaged stale-detection module exists at `agentic/src/agentic_kb/sync/staleness.py` and is used by normal `agentic-kb status` reporting.
- Normal `agentic-kb status` reports freshness for docs, code, GitHub, and Project sources using the existing sync-state baselines.
- Freshness is reported separately from readiness: task-702 does not repurpose the existing top-level `status.ok`, and any aggregate freshness signal uses a distinct field or section.
- Docs and code are reported stale when their stored successful `repo_commit_hash` differs from current local `HEAD`, fresh when it matches, and missing-baseline when the required sync-state row is absent or incomplete.
- GitHub freshness compares stored per-stream watermarks against a metadata-only helper that returns the latest remote watermark for `issues`, `pulls`, `issue_comments`, and `review_comments` without performing full ingest, embedding, or DB writes.
- Project freshness compares the stored project watermark against a metadata-only helper that returns only the latest remote Project watermark without performing full ingest, embedding, or DB writes.
- When `GITHUB_TOKEN` is absent, GitHub and Project freshness are reported as skipped or unavailable rather than fresh, and normal status still remains usable.
- `status --healthcheck` remains lightweight and does not require git history inspection, GitHub API calls, or freshness evaluation.
- MCP `kb_status` returns the same extended status payload shape, including freshness information.
- The shipped `task-901` imported-baseline path remains valid: after snapshot import, `status --json` can still report readiness success while freshness separately reports lagging docs/code/GitHub/Project baselines.
- The workflow doc no longer states that stale detection is unshipped and instead documents the actual stale-detection behavior and caveats.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is corrected to packaged `task-702.targetPath` before the task is marked complete.

## Verification Plan

- Run `python3 -m py_compile` on the touched Python modules and tests.
- Add focused unit coverage for the new stale-detection helper covering:
  - fresh versus stale docs/code baselines,
  - missing or incomplete local baselines,
  - GitHub per-stream stale versus fresh outcomes,
  - Project stale versus fresh outcomes,
  - skipped remote checks when `GITHUB_TOKEN` is absent,
  - serialization-friendly result shapes.
- Add mocked-payload unit coverage for the new metadata-only remote helpers covering:
  - GitHub latest-watermark extraction for all four streams from minimal API payloads,
  - Project latest-watermark extraction from the minimal GraphQL metadata payload,
  - transport or malformed-payload failures staying scoped to freshness rather than triggering ingest behavior.
- Extend `agentic/tests/test_status_command.py` to verify:
  - normal status includes freshness output,
  - stale freshness does not change top-level readiness `report.ok`,
  - readiness and freshness serialize as separate report concerns,
  - healthcheck mode still skips freshness inspection,
  - JSON output includes the new freshness payload shape.
- Extend `agentic/tests/test_mcp_search_server.py` to verify degraded freshness still returns a successful `kb_status` tool payload and the freshness fields are preserved.
- Add an explicit imported-baseline regression test, aligned with `task-901`, proving that a seeded/imported historical baseline can produce `status --json` with readiness `ok = true` while freshness independently reports stale lag.
- Extend DB-backed coverage only as needed to confirm the real `kb_status` payload still serializes against a seeded database after the status shape change.
- Update `.agent/workflows/agentic-kb.md` and verify its freshness section matches the shipped behavior and still preserves the clean-bootstrap boundary from `task-901`.

## Risks / Open Questions

- **Remote-probe seam**: current GitHub and Project ingestors are ingest-oriented. Implementation may need a narrow metadata-only fetch helper so stale detection does not pay the cost or risk of a full ingest.
- **Freshness/readiness separation**: the implementation must keep new freshness fields clearly distinct from current readiness fields so existing operator and MCP consumers do not accidentally treat lagging freshness as bootstrap/runtime failure.
- **Project freshness limits**: a newer Project watermark can prove the KB is stale, but a matching watermark does not guarantee that cursor-only continuation fully covers all historical edits to already-seen items.
- **Repo-state assumptions**: docs/code stale detection assumes the local clone can resolve current `HEAD`. If the workspace is not a valid git checkout, status should degrade clearly rather than hiding the problem.
- **Status UX pressure**: adding freshness to the same `StatusReport` can easily blur runtime readiness and data freshness. The implementation should keep those concerns distinguishable in the output.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final implementation notes, verification notes, outcome, and final statuses when `task-702` lands.
- Append planning review decisions in `.agent/plans/agentic/task-plans/task-702-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-702-impl-review.md` during this planning step.
- Add `.agent/plans/agentic/research/task-702-stale-index-detection.md` with durable findings about the chosen stale rules, remote-probe contract, and verification results.
- Update `.agent/workflows/agentic-kb.md` so freshness guidance reflects shipped behavior instead of future-tense placeholder wording.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-702` metadata and packaged `targetPath` when implementation lands.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-702-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-702-impl-review.md`

## Implementation Notes

- Added the packaged stale-detection module at `agentic/src/agentic_kb/sync/staleness.py` with serializable `FreshnessItem` and `FreshnessReport` shapes plus rules for local repo baselines, GitHub stream watermarks, and Project watermarks.
- Wired normal `agentic-kb status` to collect and serialize a separate `freshness` payload while preserving the shipped meaning of top-level readiness `ok` and leaving `status --healthcheck` unchanged.
- Added one metadata-only GitHub helper that probes the latest watermark for each of the four GitHub streams without running ingest, embeddings, or DB writes, and one metadata-only Project helper that returns only the latest project-item watermark.
- Kept remote freshness optional: when `GITHUB_TOKEN` is absent, GitHub and Project freshness report `skipped` rather than `fresh`, and readiness remains usable for imported or partially offline environments.
- Preserved the task-901 clean-bootstrap boundary by allowing `status --json` to report `ok = true` while freshness separately reports stale imported baselines.
- MCP `kb_status` required no special transport change beyond reusing the updated status serializer, but focused tests now verify the freshness payload is present without turning stale results into tool failure.

## Verification Notes

- `python3 -m py_compile` was run against the touched Python modules and tests for syntax validation.
- Focused unit coverage was run for status, GitHub ingest, Project ingest, and MCP search-server status behavior.
- The DB-backed MCP status test remains environment-gated by `AGENTIC_TEST_DATABASE_URL` and `psycopg`; it now asserts that the real payload shape includes `freshness` when run in a provisioned environment.

## Verification Summary

- Syntax validation passed for the touched packaged freshness/status modules and their focused tests.
- Focused unit coverage passed for `agentic.tests.test_status_command`, `agentic.tests.test_github_ingest`, `agentic.tests.test_project_ingest`, and `agentic.tests.test_mcp_search_server`.
- The follow-up fix for the mixed GitHub issues-plus-PR latest-watermark case was reviewed and approved in the implementation review log.
- DB-backed MCP payload verification is still the only environment-gated gap in this session; the test coverage is in place and skips cleanly when `AGENTIC_TEST_DATABASE_URL` and/or `psycopg` are unavailable.

## Outcome

- `task-702` now ships packaged stale-index detection for docs, code, GitHub, and Project sources using existing `kb_sync_state` baselines.
- Normal status and MCP `kb_status` surface freshness separately from readiness, while `status --healthcheck` stays lightweight and unchanged.
- The workflow doc, task tracker metadata, and durable research note now reflect shipped freshness behavior and its limits.

## Review Outcome

- Planning review is complete and approved in `.agent/plans/agentic/task-plans/task-702-plan-review.md`.
- Implementation review is complete and approved in `.agent/plans/agentic/task-plans/task-702-impl-review.md`.

## Final Signoff

- Canonical source of truth for final task-702 scope, outcome, and verification remains this task plan.
- Final approved review artifacts are the planning log at `.agent/plans/agentic/task-plans/task-702-plan-review.md` and the implementation/code-review log at `.agent/plans/agentic/task-plans/task-702-impl-review.md`.
- Task tracker state is reconciled: `task-702` is completed and points at packaged target path `agentic/src/agentic_kb/sync/staleness.py`.
- Durable shipped behavior and caveats are captured in `.agent/plans/agentic/research/task-702-stale-index-detection.md` and `.agent/workflows/agentic-kb.md`.

## Planning Status Rationale

- Planning status is `approved` because iteration 4 of `.agent/plans/agentic/task-plans/task-702-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the packaged freshness implementation, focused tests, workflow update, research note, and task-tracker reconciliation are now in place.
