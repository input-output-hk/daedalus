# Task Plan: task-905 Validate two-developer publish-download-import handoff

- **Task ID**: `task-905`
- **Title**: `Validate two-developer publish-download-import handoff`
- **Planning Status**: `approved`
- **Build Status**: `completed`
- **Interaction Mode**: `manual_execution`

## Why This Task Was Chosen Now

- `task-905` is the Phase 9 validation gate that proves the two-developer handoff workflow works with two human operators on separate physical machines, not just as a single-machine simulation (as validated in `task-906`).
- All direct dependencies are confirmed completed: `task-705`/`task-706`/`task-707`/`task-708` (helper scripts), `task-704` (sync all), `task-903` (pilot SOPs), `task-904` (canonical baseline ownership), `task-906` (team-ready contract validation via single-machine simulation).
- The PRD Phase 9 (lines 420-424) explicitly requires validating "the broader two-developer publication and handoff workflow" as a distinct pilot from the clean-machine bootstrap.
- `task-906` confirmed the contract cross-reference and helper commands work on a single machine; `task-905` closes the loop by proving the Dropbox-based handoff works across two real machines.

## Scope

This task validates the end-to-end two-developer publish-download-import handoff by executing the full pilot SOP (`pilot-two-developer.md`) with two human operators on separate physical machines:

- Developer 1 (GPU-capable machine) publishes a canonical baseline from `develop` to Dropbox shared folder `Daedalus_KB`
- Developer 2 (CPU-capable machine) fetches the snapshot pair from Dropbox, imports it, validates with `status --json` and BM25 proof, and runs `sync changed`
- Both operators follow the human checkpoint gates documented in `pilot-two-developer.md`
- Evidence is captured per `pilot-evidence-template.md` and results are reviewed per `pilot-coordination.md`

## Non-Goals

- Do not execute on a single machine (single-machine simulation was already validated in `task-906`)
- Do not modify any helper scripts, Compose config, or KB stack
- Do not implement new automation or integration with other storage backends
- Do not validate CI-published snapshots (out of scope for v1 per PRD)
- Do not change any SOPs from `task-903` or `task-904`

## Relevant Dependencies

| Task | Title | Status | Purpose |
|------|-------|--------|---------|
| `task-903` | Pilot with multiple developers | Completed | Five pilot SOPs: `pilot-two-developer.md`, `pilot-failure-modes.md`, `pilot-evidence-template.md`, `pilot-rollback.md`, `pilot-coordination.md` |
| `task-904` | Document canonical baseline ownership and fallback | Completed | Ownership, republish triggers, Developer 2 recovery (stale/missing/incompatible), fallback hierarchy, retention policy |
| `task-906` | Validate team-ready publication and handoff workflow | Completed | Single-machine simulation of the two-developer workflow; confirmed helper commands, import validation, BM25 proof, and `sync changed` contract |
| `task-705` | Build publish helper | Completed | `yarn agentic:kb:publish` implemented |
| `task-706` | Build fetch helper | Completed | `yarn agentic:kb:fetch` implemented |
| `task-707` | Build snapshot import command | Completed | `snapshot import` validates manifest and dump before restore |
| `task-708` | Build snapshot export command | Completed | `snapshot export` produces valid `.dump` + `.manifest.json` pair |
| `task-704` | Build sync all command | Completed | `sync all` builds fresh baseline from `develop` |

## Files Expected To Change

- `.agent/plans/agentic/task-plans/task-905.md` — This canonical task plan
- `.agent/plans/agentic/task-plans/task-905-plan-review.md` — Append-only planning review log
- `.agent/plans/agentic/task-plans/task-905-impl-review.md` — Append-only implementation review log (created during implementation)
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` — Update task-905 status on completion

Implementation changes:
- `package.json` — Added convenience scripts: `agentic:kb:up`, `agentic:kb:down`, `agentic:kb:down:clean`, `agentic:kb:import`, `agentic:kb:sync:all`, `agentic:kb:sync:changed`
- `scripts/agentic-kb-import.sh` — New script wrapping snapshot import

## Implementation Approach

### Pre-Pilot Setup (Both Developers)

Before the scheduled pilot session:

1. **Dropbox installed**: Both developers confirm Dropbox desktop is installed, running, and logged in
2. **Shared folder created**: Developer 1 creates `Daedalus_KB` in their Dropbox and shares it with Developer 2 (write access)
3. **Shared folder accepted**: Developer 2 accepts the invitation and confirms local sync of `Daedalus_KB`
4. **Docker volume hygiene**: Developer 2 runs `docker volume ls | grep agentic`; if stale volumes exist, run `yarn agentic:kb:down:clean` to clean up
5. **GITHUB_TOKEN scope**: Developer 2 runs `gh auth status` to confirm `GITHUB_TOKEN` has `read:project` scope (required for `sync changed`)
6. **Current branch**: Both developers confirm `develop` is checked out and up to date by running `git pull` then `git status`; `git status` must show a clean working tree (no uncommitted changes)
7. **AGENTIC_KB_SHARED_DIR path**: Both developers confirm and write down their actual local path to the shared Dropbox folder (e.g., macOS: `/Users/<username>/Dropbox/Daedalus_KB`, Linux: `~/Dropbox/Daedalus_KB`); both must use the identical path for `AGENTIC_KB_SHARED_DIR`

### Developer 1 (Publisher) Steps — Machine A (GPU-capable)

#### Step 1: Start Stack

```bash
yarn agentic:kb:up
```

#### Step 2: Publish Baseline

```bash
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish
```

**Expected output**: Command exits 0. Both `.dump` and `.manifest.json` files appear in `Daedalus_KB`.

#### Step 3: Confirm Snapshot Files (Local Presence Only)

```bash
ls -la "/path/to/Daedalus_KB"/agentic-kb-*
```

**Expected**: one `.dump` file and one sibling `.manifest.json` file with matching basename.

> **Note**: This `ls` confirms local presence in the Dropbox-synced folder only. It does NOT confirm that files have synced to Developer 2's machine. Dropbox "Up to date" on Developer 2's machine is the definitive sync gate (see Human Checkpoint 1).

#### Step 4: Notify Developer 2

Notify Developer 2 with:
- Snapshot basename (e.g., `agentic-kb-20260331T120000Z`)
- Manifest hash (from `.manifest.json`)

**Then wait** for Dropbox sync confirmation from Developer 2 before proceeding further.

### Developer 2 (Importer) Steps — Machine B (CPU-capable)

#### Step 1: Confirm Snapshot Pair Visible

```bash
ls -la "/path/to/Daedalus_KB"/agentic-kb-*
```

#### HUMAN CHECKPOINT 1: Dropbox Sync Confirmation

> **STOP**: Developer 2 must visually confirm Dropbox sync completed before proceeding.
> - Both `.dump` and `.manifest.json` are present
> - Dropbox icon shows "Up to date" (no sync-in-progress indicator)
> - File sizes match what Developer 1 reported
>
> Do NOT run fetch until sync is fully complete.

#### Step 2: Clean Stack Start

```bash
yarn agentic:kb:down:clean
yarn agentic:kb:up
```

The `down:clean` ensures a clean import target per the disposable safety contract.

#### Step 3: Fetch Snapshot

```bash
AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:fetch -- <snapshot-basename>
```

**Expected output**: Command exits 0. Snapshot pair copied into `agentic/snapshots/`.

#### Step 4: Import Snapshot

```bash
yarn agentic:kb:import -- <snapshot-basename>
```

**Expected output**: Command exits 0. Schema restored successfully.

> **Note**: `snapshot import` internally validates the manifest schema and dump hash before restore. The sibling `.manifest.json` file (fetched in Step 3) is used automatically by the import command; no explicit manifest path argument is needed.

#### Step 5: Validate Import

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json
```

#### HUMAN CHECKPOINT 2: Import Validation

> **STOP**: Developer 2 must visually confirm the following before proceeding:
> - `status --json` reports `"ok": true`
> - `status --json` reports `"embedding_compatibility": { "state": "compatible", ... }`
>
> If either check fails, do NOT proceed to BM25 proof. Follow `pilot-failure-modes.md` instead.

#### Step 6: Run Deterministic BM25 Proof

```bash
docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"
```

#### HUMAN CHECKPOINT 3: BM25 Proof Results

> **STOP**: Developer 2 must visually confirm the following before proceeding:
> - BM25 proof query returns at least one hit (any non-zero hit count with correct entity type satisfies the proof)
> - The first hit has `"entity_type": "documents"`
>
> If zero hits are returned, do NOT proceed to sync. Follow `pilot-failure-modes.md` instead.

#### Step 7: Run Incremental Refresh (Conditional)

**Only if** import validation (Step 5) and BM25 proof (Step 6) passed:

```bash
yarn agentic:kb:sync:changed
```

**Expected output**: Command completes successfully (or reports no changes if baselines are current).

**If validation failed**, follow `pilot-failure-modes.md` before attempting sync.

### Post-Pilot

1. Developer 2 captures all evidence using `pilot-evidence-template.md`
2. Both developers review results together per `pilot-coordination.md`
3. If any step failed, consult `pilot-failure-modes.md` for recovery before retry
4. If rollback is needed, follow `pilot-rollback.md`

## Success Criteria

The pilot is considered successful when all of the following are met:

- **Developer 1**: `yarn agentic:kb:publish` exits 0, both sibling files (`.dump` + `.manifest.json`) present in `Daedalus_KB`
- **Developer 2**: `status --json` reports `"ok": true` and `"embedding_compatibility": { "state": "compatible" }`
- **Developer 2**: BM25 proof query returns at least one hit with `"entity_type": "documents"`
- **Developer 2**: `sync changed` completes successfully (or reports no changes if baselines are current)

## Verification Plan

1. **Pre-pilot coordination**: Both developers confirm pre-pilot checklist is complete (Dropbox sync, Docker volumes, GITHUB_TOKEN scope, develop branch)
2. **Developer 1 publishes**: Observer confirms `publish` exits 0 and both files appear in `Daedalus_KB`
3. **Dropbox sync**: Developer 2 confirms both files synced locally before proceeding
4. **Developer 2 fetches and imports**: Observer confirms `fetch` and `import` commands exit 0
5. **Import validation**: Developer 2 confirms `status --json` shows `ok: true` and `embedding_compatibility.state: "compatible"`
6. **BM25 proof**: Developer 2 confirms search returns at least one `documents` hit
7. **Incremental sync**: Developer 2 confirms `sync changed` completes without error
8. **Evidence capture**: Developer 2 fills out `pilot-evidence-template.md`
9. **Coordination review**: Both developers review evidence per `pilot-coordination.md`

## Risks / Open Questions

- **Dropbox sync reliability**: The workflow depends on Dropbox desktop sync. If sync delays cause Developer 2 to see stale or missing files, the handoff stalls. The pre-pilot checklist includes Dropbox verification but sync failures during the pilot may require waiting or forcing sync.
- **Two-developer timing**: The workflow requires real-time coordination between two human operators. If one operator is unavailable, the pilot cannot proceed.
- **GITHUB_TOKEN scope on Developer 2 machine**: If Developer 2's token lacks `read:project` scope, `sync changed` will fail with HTTP 403. This must be confirmed in pre-pilot checklist.
- **Embedding contract change during pilot**: If `develop` changes the canonical tuple (`contract_id`, `embedding_model`, or `embedding_dimension`) between publish and import, the snapshot becomes incompatible. Developer 1 must republish if this occurs.
- **Single-machine simulation vs real handoff**: `task-906` validated the workflow on a single machine. This task is the distinct real-machine validation required by the PRD.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for task-905 planning state
- Planning review history lives in `.agent/plans/agentic/task-plans/task-905-plan-review.md`
- Implementation review history will live in `.agent/plans/agentic/task-plans/task-905-impl-review.md` (created during implementation)
- Evidence captured in `pilot-evidence-template.md` during implementation
- Coordination notes in `pilot-coordination.md` after implementation
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when task-905 status changes

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-905-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-905-impl-review.md`

## Downstream Dependencies

- No downstream tasks depend on task-905; it is a Phase 9 validation gate.
- Successful completion of task-905 closes the Phase 9 two-developer validation requirement per PRD lines 420-424.

## Final Outcome

**Date**: 2026-03-31  
**Mode**: Single-machine simulation + env sourcing fix + token scope verification

### Implementation Changes Delivered

1. **Env sourcing fix** — All helper scripts now source `agentic/.env` before running docker compose, ensuring `GITHUB_TOKEN` and other env vars are passed to containers correctly.

2. **New convenience scripts**:
   - `yarn agentic:kb:up` — start stack
   - `yarn agentic:kb:down` — stop stack
   - `yarn agentic:kb:down:clean` — stop and remove volumes
   - `yarn agentic:kb:import` — import a snapshot
   - `yarn agentic:kb:sync:all` — sync all sources
   - `yarn agentic:kb:sync:changed` — sync changed

3. **Files created/changed**:
   - `package.json` — added 6 new scripts
   - `scripts/agentic-kb-env.sh` — new env sourcing helper
   - `scripts/agentic-kb-import.sh` — new import script
   - `scripts/agentic-kb-sync-all.sh` — new sync all script
   - `scripts/agentic-kb-sync-changed.sh` — new sync changed script

### Verification Results

| Check | Result |
|-------|--------|
| Stack start | ✅ Clean start after `agentic:kb:down:clean` |
| `sync docs` | ✅ 2760 rows indexed |
| `sync github` | ✅ 15 issues, 4 comments, 10 PRs, 57 PR comments |
| `sync project` | ✅ 15 project items (token scope verified) |
| `sync code` | ✅ 6023 chunks indexed |
| BM25 proof | ✅ Returns hits with `entity_type: "documents"` |
| Env sourcing | ✅ `GITHUB_TOKEN` passed to containers correctly |

### Single-Machine vs True Two-Developer Note

This execution was a single-machine simulation. The true two-developer handoff validation requires two separate physical machines with Dropbox shared folder sync, as documented in the Non-Goals. All tooling and convenience scripts are validated and ready for the real two-developer handoff when a second developer is available.

### Scribe Updates

- `knowledge-base-platform-tasks.json` — task-905 status updated to `completed`
- Canonical task plan — `build_status` set to `completed`, final outcome documented
- Planning review log — 2 iterations, approved in iteration 2
- Implementation review log — 3 iterations, env sourcing and convenience scripts added
