# Task Plan: task-903 Pilot with multiple developers

- **Task ID**: `task-903`
- **Title**: `Pilot with multiple developers`
- **Planning Status**: `approved`
- **Build Status**: `completed`
- **Interaction Mode**: `manual_execution`

## Why This Task Was Chosen Now

- `task-903` is the next critical-path task in Phase 9 (Validation and Rollout) after `task-901` (clean-machine bootstrap) and `task-902` (security boundaries) completed.
- All technical prerequisites are now in place: snapshot export/import (`task-602`), embedding-contract compatibility enforcement (`task-608`), disposable import-target safety (`task-612`), local publish/fetch helpers (`task-705`), Dropbox shared-folder selection (`task-605`), and team sharing workflow documentation (`task-606`).
- The PRD Phase 9 explicitly calls for: "Pilot with multiple developers and document SOPs for common failure modes."
- This task produces the operator-facing SOPs and documentation needed to run the actual two-developer pilot. The pilot itself is human-executed and cannot be truthfully completed by an agent.

## Scope

This task creates documentation and SOP templates that enable the two-developer pilot to be executed by human operators. The deliverables are:

- **Operator-facing instructions** for Developer 1 (publisher) and Developer 2 (importer)
- **SOP templates** in `.agent/SOPs/agentic/` covering the pilot workflow, common failure modes, and recovery procedures
- **Expected outputs** documentation showing what successful pilot execution looks like
- **Rollback notes** for each stage of the pilot

The task does NOT execute the pilot itself. It produces the artifacts that make the pilot runnable.

## Non-Goals

- Do not execute the actual two-developer pilot (requires two human developers on separate machines)
- Do not implement new features or fix bugs discovered during pilot planning
- Do not modify the KB stack, Compose configuration, or helper scripts
- Do not broaden into task-904 (canonical baseline ownership), task-905 (validate handoff), or task-906 (team-ready rollout)
- Do not attempt to simulate or mock the two-developer workflow

## Relevant Dependencies

| Task | Title | Status | Purpose |
|------|-------|--------|---------|
| `task-901` | Validate clean-machine bootstrap | ✅ Completed | Proves import + status + search path works on fresh machine |
| `task-902` | Review security and shared-storage boundaries | ✅ Completed | Confirms security constraints before team rollout |
| `task-705` | Add local publish and fetch helper commands | ✅ Completed | Provides `yarn agentic:kb:publish` and `yarn agentic:kb:fetch` |
| `task-605` | Select private shared snapshot storage backend | ✅ Completed | Dropbox `Daedalus_KB` shared folder selected |
| `task-606` | Document local baseline publish/download workflow | ✅ Completed | Workflow documented in `.agent/workflows/agentic-kb.md` |
| `task-608` | Enforce snapshot embedding-contract compatibility | ✅ Completed | Import rejects incompatible manifests |
| `task-612` | Enforce disposable import target safety | ✅ Completed | Import refuses non-disposable KB databases |
| `task-701` | Implement sync commands for all sources | ✅ Completed | `sync changed` available for post-import refresh |

## Files Expected To Change

- `.agent/SOPs/agentic/pilot-two-developer.md` — Primary SOP for the two-developer pilot workflow
- `.agent/SOPs/agentic/pilot-failure-modes.md` — Common failure modes and recovery procedures
- `.agent/SOPs/agentic/pilot-evidence-template.md` — Template for capturing pilot evidence
- `.agent/SOPs/agentic/pilot-rollback.md` — Rollback procedures for each pilot stage
- `.agent/SOPs/agentic/pilot-coordination.md` — Coordination and communication SOP between developers
- `.agent/plans/agentic/task-plans/task-903.md` — This canonical task plan
- `.agent/plans/agentic/task-plans/task-903-plan-review.md` — Append-only planning review log
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` — Update task-903 status on completion

## Implementation Approach

### SOP 1: Pilot Two-Developer Workflow (`.agent/SOPs/agentic/pilot-two-developer.md`)

Documents the complete end-to-end pilot workflow:

**Pre-pilot checklist:**
- Both developers have Dropbox installed and running
- Developer 1 created `Daedalus_KB` shared folder and shared with Developer 2 (write access)
- Developer 2 accepted the shared folder invitation and confirmed local sync
- **Developer 2 Dropbox pre-flight**: Developer 2 must confirm they can create/accept a shared folder before the pilot date (test by accepting a dummy shared folder or confirming Dropbox account has no sharing restrictions)
- **Docker volume hygiene check**: Developer 2 runs `docker volume ls | grep agentic` to check for stale volumes from prior KB work; if any exist, run `docker compose -f docker-compose.agentic.yml down -v` to clean up before pilot
- **GITHUB_TOKEN scope validation**: Developer 2 runs `gh auth status` or checks token scopes to confirm `GITHUB_TOKEN` has `read:project` scope (required for `sync changed`); a repo-only token will cause HTTP 403 failures
- Both developers have current `develop` branch checked out
- Developer 1 has GPU-capable machine (or accepts longer CPU embedding time)
- Developer 2 may be CPU-only (import and `sync changed` work on CPU)

**Developer 1 (Publisher) steps:**
1. Start stack: `docker compose -f docker-compose.agentic.yml up -d`
2. Publish baseline: `AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish`
3. Confirm both `.dump` and `.manifest.json` appear in local `Daedalus_KB` folder
4. Wait for Dropbox sync to complete (confirm files appear on Developer 2's machine)

**Developer 2 (Importer) steps:**
1. Confirm snapshot pair visible in local `Daedalus_KB` folder
2. **HUMAN CHECKPOINT**: Developer 2 must visually confirm Dropbox sync completed (both `.dump` and `.manifest.json` present and fully synced — check Dropbox icon shows "Up to date") before proceeding
3. Clean stack start: `docker compose -f docker-compose.agentic.yml down -v` (ensures clean import target per disposable safety contract), then `docker compose -f docker-compose.agentic.yml up -d`
4. Fetch snapshot: `AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>`
5. Import snapshot: `docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/<snapshot>.dump --yes`
6. Validate import: `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json`
7. **HUMAN CHECKPOINT**: Developer 2 must visually confirm `status --json` reports `ok: true` and `embedding_compatibility.state: "compatible"` before proceeding to BM25 proof
8. Run deterministic BM25 proof: `docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"`
9. **HUMAN CHECKPOINT**: Developer 2 must visually confirm BM25 proof query returns at least one hit with `entity_type: "documents"` before proceeding to sync
10. Run incremental refresh (conditional): **Only if** import validation (step 7) and BM25 proof (step 9) passed, run `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed`. **If validation failed**, follow `pilot-failure-modes.md` before attempting sync.

**Success criteria:**
- Developer 1: `yarn agentic:kb:publish` exits 0, both sibling files present in `Daedalus_KB`
- Developer 2: `status --json` reports `ok: true` and `embedding_compatibility.state: "compatible"`
- Developer 2: BM25 proof query returns at least one hit with `entity_type: "documents"`
- Developer 2: `sync changed` completes successfully (or reports no changes if baselines current)

### SOP 2: Failure Modes (`.agent/SOPs/agentic/pilot-failure-modes.md`)

Documents common failure modes with symptoms and recovery:

| Failure Mode | Symptom | Recovery |
|-------------|---------|----------|
| Dropbox sync delay | Developer 2 cannot see files in `Daedalus_KB` | Force sync, verify sharing permissions, confirm write access |
| Incompatible embedding contract | `snapshot import` rejects manifest with contract mismatch | Developer 1 republishes from `develop`; Developer 2 recreates KB volume and re-imports |
| Non-disposable import target | `snapshot import` refuses target with existing data | `docker compose down -v` then `up -d`, retry import |
| `sync changed` fails after import | Missing baseline for one or more sources | Run individual sync commands (`sync docs`, `sync code`, `sync github`, `sync project`) first |
| `GITHUB_TOKEN` scope failure (HTTP 403) | `sync changed` returns HTTP 403 "project scope missing" | Developer 2 must update `GITHUB_TOKEN` to include `read:project` scope; repo-only tokens are insufficient for project-level sync |
| `yarn agentic:kb:publish` fails | Missing `AGENTIC_KB_SHARED_DIR` or stack not running | Set env var, start stack, retry |
| `yarn agentic:kb:fetch` fails | Missing snapshot basename or pair incomplete | Verify both files exist together in `Daedalus_KB`, use correct basename |
| `status --json` reports `ok: false` | Service or DB readiness issue | Check `docker compose ps`, inspect logs, restart stack |
| BM25 proof query returns zero hits | Import may have failed or KB empty | Re-run import with `--yes`, check `status --json` row counts |

### SOP 3: Evidence Template (`.agent/SOPs/agentic/pilot-evidence-template.md`)

Structured template for capturing pilot results:

- Pilot date and participants (Developer 1, Developer 2)
- Developer 1 machine details (OS, GPU availability, Docker version)
- Developer 2 machine details (OS, GPU availability, Docker version)
- Snapshot basename and manifest hash
- Timestamps for each pilot stage
- Command outputs (truncated to relevant lines)
- Errors encountered and resolution
- Final validation results from both machines
- Notes on workflow clarity and documentation accuracy
- **Structured JSON artifact capture**: Save full raw JSON outputs as files for downstream task-905 validation:
  - `status--json.json` — full output of `status --json` (task-905 needs `ok: true`, `embedding_compatibility.state: "compatible"`, row counts)
  - `bm25-proof-result.json` — full output of BM25 proof search (task-905 needs hit structure, `entity_type: "documents"` confirmation)
  - `sync-changed-result.json` — full output of `sync changed` (task-905 needs sync completion status and any per-source results)
- **GITHUB_TOKEN scope evidence**: Capture output of `gh auth status` or token scope check confirming `read:project` scope is present
- **Task-905 field mapping**: Each captured field is explicitly mapped to downstream validation needs:
  - `status --json` → task-905 acceptance: import health, embedding compatibility
  - BM25 proof → task-905 acceptance: search functionality on imported baseline
  - `sync changed` → task-905 acceptance: incremental refresh works post-import
  - Token scope → task-905 acceptance: credential requirements for sync operations

### SOP 4: Rollback Procedures (`.agent/SOPs/agentic/pilot-rollback.md`)

Rollback for each pilot stage:

- **After failed publish**: Developer 1 retries; no cleanup needed on Developer 2 side
- **After failed fetch**: Delete partial files from `agentic/snapshots/`, retry fetch
- **After failed import**: Recreate disposable KB volume (`docker compose down -v && up -d`), retry import
- **After failed `sync changed`**: No data loss risk (additive); fix baseline gaps and retry
- **Full rollback**: Both developers tear down stacks (`docker compose down -v`), clear `agentic/snapshots/`, restart from scratch

### SOP 5: Pilot Coordination and Communication (`.agent/SOPs/agentic/pilot-coordination.md`)

Documents how the two developers coordinate during the pilot:

- **Pre-pilot sync**: Both developers confirm availability and complete pre-flight checklist before starting
- **Publish notification**: Developer 1 notifies Developer 2 (via agreed channel — Slack, email, etc.) when snapshot is published to `Daedalus_KB`, including snapshot basename and manifest hash
- **Sync confirmation**: Developer 2 confirms receipt of notification and verifies files appear in local Dropbox folder before starting import
- **Checkpoint acknowledgments**: Developer 2 reports back after each HUMAN CHECKPOINT step (import validation, BM25 proof, sync changed) — success or failure with error details
- **Failure escalation**: If any step fails, Developer 2 shares error output immediately; both developers consult `pilot-failure-modes.md` before retrying
- **Evidence handoff**: Developer 2 shares all captured JSON artifacts and evidence template with Developer 1 for review before pilot is considered complete
- **Post-pilot debrief**: Both developers review evidence template together, note any SOP improvements, and confirm readiness for task-905 validation

## Acceptance Criteria

- [ ] `.agent/SOPs/agentic/pilot-two-developer.md` exists with complete pilot workflow documentation
- [ ] `.agent/SOPs/agentic/pilot-failure-modes.md` exists with common failure modes and recovery procedures
- [ ] `.agent/SOPs/agentic/pilot-evidence-template.md` exists with structured evidence capture template
- [ ] `.agent/SOPs/agentic/pilot-rollback.md` exists with rollback procedures for each stage
- [ ] All SOPs reference current commands from `agentic/README.md` and `.agent/workflows/agentic-kb.md`
- [ ] Task plan explicitly acknowledges this is a human-executed workflow
- [ ] Task plan references downstream tasks (task-904, task-905, task-906)
- [ ] No implementation changes to KB stack, Compose config, or helper scripts

## Verification Plan

- Verify each SOP file exists in `.agent/SOPs/agentic/` and contains required sections
- Cross-reference SOP commands against `agentic/README.md` and `.agent/workflows/agentic-kb.md` for accuracy
- Confirm failure mode table covers known failure paths from existing tooling
- Confirm rollback procedures are safe and do not risk data loss
- Confirm evidence template captures all information needed for downstream task validation

## Risks / Open Questions

- **Dropbox sync reliability**: Pilot depends on Dropbox desktop sync completing before Developer 2 can fetch. Network issues or sync conflicts could block progress.
- **Embedding contract drift**: If `develop` changes the embedding contract between planning and execution, published snapshot may be incompatible. Developer 1 should verify current contract before publishing.
- **Developer 2 machine readiness**: Developer 2 may have stale Docker volumes or conflicting Compose projects from prior KB work. Pre-flight check needed.
- **GPU availability asymmetry**: Developer 1 expected to have GPU access for efficient full rebuilds. If not, pilot still works but takes longer.
- **Evidence capture discipline**: Pilot value for downstream tasks depends on capturing structured evidence during execution, not after.

## Final Outcome

task-903 delivered five SOP files in `.agent/SOPs/agentic/` that enable human operators to execute the two-developer pilot:

1. **pilot-two-developer.md** — End-to-end pilot workflow with pre-pilot checklist (Dropbox pre-flight, Docker volume hygiene, GITHUB_TOKEN scope validation), Developer 1 (publisher) steps, Developer 2 (importer) steps with 3 HUMAN CHECKPOINT callouts, conditional sync gating, and success criteria.
2. **pilot-failure-modes.md** — Common failure mode reference table with 9 entries covering Dropbox sync delay, incompatible embedding contract, non-disposable import target, missing baseline, GITHUB_TOKEN scope failure (HTTP 403), publish/fetch/status failures, and zero-hit BM25 proof; includes escalation path.
3. **pilot-evidence-template.md** — Structured evidence capture template with pilot metadata, machine details, snapshot info, timestamps, structured JSON artifacts (`status--json.json`, `bm25-proof-result.json`, `sync-changed-result.json`), and explicit task-905 field mapping.
4. **pilot-rollback.md** — Rollback procedures for each pilot stage (failed publish, fetch, import, sync changed) plus full rollback procedure with safety notes.
5. **pilot-coordination.md** — Coordination SOP covering pre-pilot sync, publish notification with example message, checkpoint acknowledgments, failure escalation, evidence handoff, and post-pilot debrief.

No code, Compose files, or helper scripts were modified. All commands cross-referenced against `agentic/README.md` and `.agent/workflows/agentic-kb.md` for accuracy.

## Review Log References

- Planning review log: `.agent/plans/agentic/task-plans/task-903-plan-review.md` (2 iterations, approved)
- Implementation review log: `.agent/plans/agentic/task-plans/task-903-impl-review.md` (1 iteration, approved)

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for task-903 planning state and final outcome
- Planning review history lives in `.agent/plans/agentic/task-plans/task-903-plan-review.md`
- Implementation review history lives in `.agent/plans/agentic/task-plans/task-903-impl-review.md` (created during implementation)
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when task-903 status changes
- No research file update expected; this task produces SOPs rather than research findings

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-903-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-903-impl-review.md`

## Downstream Dependencies

- **task-904** — Document canonical baseline ownership and fallback (builds on pilot learnings)
- **task-905** — Validate two-developer publish-download-import handoff (executes the pilot using these SOPs)
- **task-906** — Validate team-ready publication and handoff workflow (broader rollout validation)
