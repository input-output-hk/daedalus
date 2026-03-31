# Task Plan: task-906 Validate team-ready publication and handoff workflow

- **Task ID**: `task-906`
- **Title**: `Validate team-ready publication and handoff workflow`
- **Planning Status**: `approved`
- **Build Status**: `completed`
- **Interaction Mode**: `interactive_validation`

## Why This Task Was Chosen Now

- `task-906` is the final validation gate in Phase 9 (Validation and Rollout) after `task-903` (pilot SOPs), `task-904` (canonical baseline ownership), and `task-901` (clean-machine bootstrap) are all completed.
- All direct dependencies are confirmed completed: `task-606` (publish/download workflow), `task-705`/`task-706`/`task-707`/`task-710` (helper scripts), `task-611` (embedding contract), `task-901` (bootstrap), and `task-904` (ownership/fallback SOP).
- The PRD Phase 9 explicitly calls for validating the broader two-developer rollout contract: canonical publish from `develop`, private storage handoff via Dropbox, import on a second machine, explicit mismatch handling, Project token guidance, and post-import incremental refresh behavior.
- `task-906` closes the loop on the team-ready workflow by confirming that the documented contract matches shipped reality and that operators can execute the full publish→handoff→import→sync workflow without discovering new gaps.

## Scope

This task validates and confirms the team-ready publication and handoff workflow by:

- Cross-referencing the PRD Phase 9 team-sharing requirements against shipped documentation and tooling
- Confirming `yarn agentic:kb:publish` runs `sync all`, exports a valid snapshot pair, and copies both files to `Daedalus_KB`
- Confirming `yarn agentic:kb:fetch` retrieves an explicit sibling pair from `Daedalus_KB` into `agentic/snapshots/`
- Confirming `snapshot import` validates manifest schema and dump identity before restore
- Confirming `status --json` reports `ok: true` and `embedding_compatibility.state: "compatible"` after import
- Confirming the deterministic BM25 proof query returns the expected hit after import
- Confirming `sync changed` runs successfully after import and correctly computes deltas from stored baseline commits
- Documenting explicit mismatch handling when embedding contracts differ between publisher and importer
- Documenting Project token guidance for `sync project` and `sync changed` after import
- Confirming the manual full Project refresh path for re-convergence is documented
- Verifying `agentic/README.md` reflects the validated team-sharing workflow

The deliverable is a validation report and any needed documentation corrections in `agentic/README.md`.

## Non-Goals

- Do not execute the full two-developer publish→handoff on separate physical machines (requires two human operators)
- Do not implement new helper commands, automation, or Dropbox API integrations
- Do not change snapshot format, import/export behavior, or embedding contract
- Do not modify existing SOPs from `task-903` or `task-904`
- Do not validate CI-published snapshots (task-603 remains pending)

## Relevant Dependencies

| Task | Title | Status | Purpose |
|------|-------|--------|---------|
| `task-901` | Validate clean-machine bootstrap | Completed | Clean-machine bootstrap validated: stack start, snapshot import, `status --json`, BM25 proof |
| `task-903` | Pilot with multiple developers | Completed | Pilot SOPs document end-to-end two-developer workflow with human checkpoints |
| `task-904` | Document canonical baseline ownership and fallback | Completed | Ownership, republish triggers, and Developer 2 recovery procedures documented |
| `task-606` | Document local baseline publish/download workflow | Completed | Publish/fetch helper commands and consumption path in `.agent/workflows/agentic-kb.md` |
| `task-705` | Build publish helper | Completed | `yarn agentic:kb:publish` helper implemented |
| `task-706` | Build fetch helper | Completed | `yarn agentic:kb:fetch` helper implemented |
| `task-707` | Build snapshot import command | Completed | `snapshot import` validates manifest and dump before restore |
| `task-710` | Snapshot export command | Completed | `snapshot export` produces valid `.dump` + `.manifest.json` pair |
| `task-611` | Define canonical embedding contract | Completed | Single embedding contract enforced; mismatch handling enforced at import |

## Files Expected To Change

- `agentic/README.md` — Update team-sharing section if any discrepancies found between documented contract and shipped tooling
- `.agent/plans/agentic/task-plans/task-906.md` — This canonical task plan
- `.agent/plans/agentic/task-plans/task-906-plan-review.md` — Append-only planning review log
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` — Update task-906 status on completion

No implementation changes to KB stack, Compose config, or helper scripts expected.

## Implementation Approach

### Phase 1: Contract Cross-Reference

Grounded in the PRD Phase 9 team-sharing requirements and the shipped documentation:

1. Compare `.agent/workflows/agentic-kb.md` (lines 145-241) against the PRD Phase 9 team-sharing contract
2. Compare `agentic/README.md` (lines 83-145) against `.agent/workflows/agentic-kb.md` for consistency
3. Confirm all helper command contracts (`publish`, `fetch`, `snapshot import`) match documented behavior
4. Confirm the sibling-pair contract (`.dump` + `.manifest.json`) is enforced by both `publish` and `fetch`
5. Confirm mismatch handling is enforced at `snapshot import` (embedding contract check before restore)
6. Confirm Project token guidance is documented for `sync project` and `sync changed`

### Phase 2: Helper Command Contract Validation

Validate that shipped helpers behave as documented:

**`yarn agentic:kb:publish`** (per root `package.json` scripts and `scripts/agentic-kb-publish.sh`):
- Runs `sync all` to build fresh baseline
- Exports snapshot pair into `agentic/snapshots/`
- Copies both `.dump` and sibling `.manifest.json` into `AGENTIC_KB_SHARED_DIR`
- Fails if sibling pair is incomplete before copy

**`yarn agentic:kb:fetch`** (per root `package.json` scripts and `scripts/agentic-kb-fetch.sh`):
- Copies explicit sibling pair from `AGENTIC_KB_SHARED_DIR` into `agentic/snapshots/`
- Requires explicit snapshot basename (no auto-latest)
- Fails if either side of the selected pair is missing in `AGENTIC_KB_SHARED_DIR`

**`snapshot import`** (per `agentic/src/agentic_kb/commands/snapshot.py`):
- Validates manifest schema before restore
- Validates dump size/hash identity before restore
- Rejects incompatible embedding contracts before schema drop
- Refuses import into non-disposable target (existing rows in `agentic` tables)

### Phase 3: Post-Import Incremental Refresh Validation

Confirm `sync changed` behavior after import:

- `sync changed` computes deltas from stored baseline commits to current `HEAD`
- Deltas are bounded by the stored `repo_commit_hash` per source, not unbounded fan-out
- GitHub incremental uses one shared lower bound across all four streams
- `sync changed` is not a first-sync substitute on an empty or partially seeded KB
- Project refresh is cursor continuation only; does not detect updates to already-seen items
- Manual full Project refresh path documented via `sync project --full` flag (sync.py:128-132; workflow doc:94,163-164); `agentic/README.md` does not explicitly call out `--full` as the re-convergence procedure — documentation gap identified

### Phase 4: Documentation Discrepancy Report

If any discrepancy is found between documented contract and shipped reality:

- Document the specific discrepancy
- Recommend a disposition (documentation fix or tooling fix)
- Do not implement tooling fixes unless the discrepancy is a correctness issue (not a documentation issue)

## Acceptance Criteria

- [ ] All PRD Phase 9 team-sharing requirements cross-referenced against shipped docs and tooling
- [ ] `yarn agentic:kb:publish` contract confirmed: `sync all` → export → sibling-pair copy to `AGENTIC_KB_SHARED_DIR`
- [ ] `yarn agentic:kb:fetch` contract confirmed: explicit sibling-pair copy from `AGENTIC_KB_SHARED_DIR` → `agentic/snapshots/`
- [ ] `snapshot import` contract confirmed: manifest schema validation + dump identity + embedding compatibility check + disposable-target enforcement
- [ ] `status --json` post-import shape confirmed: `ok: true` and `embedding_compatibility.state: "compatible"`
- [ ] BM25 proof query shape confirmed: at least one hit, `entity_type: "documents"`, `fields.source_path: ".agent/workflows/agentic-kb.md"`
- [ ] `sync changed` post-import behavior confirmed: delta computation from stored baseline commits, not unbounded fan-out
- [ ] Mismatch handling confirmed: incompatible embedding contracts fail at `snapshot import` with clear error
- [ ] Project token guidance confirmed: `GITHUB_TOKEN` must have `read:project` scope for `sync project` and `sync changed`
- [ ] Manual full Project refresh path confirmed via `sync project --full` (sync.py:128-132); `agentic/README.md` gap identified — does not explicitly call out `--full` as re-convergence procedure
- [ ] `agentic/README.md` reflects validated team-sharing workflow with one documentation gap (Project `--full` flag not called out explicitly)
- [ ] Validation report produced with any recommended documentation corrections

## Verification Plan

1. **Contract cross-reference**: Read `.agent/workflows/agentic-kb.md` lines 145-241 and `agentic/README.md` lines 83-145. Confirm each team-sharing requirement has a corresponding shipped command or documented SOP.

2. **Helper command validation**: Inspect root `package.json` to confirm `agentic:kb:publish` and `agentic:kb:fetch` scripts exist and call `scripts/agentic-kb-*.sh`. Inspect the actual script implementations to confirm they match the documented contract.

3. **Snapshot import validation**: Inspect `agentic/src/agentic_kb/commands/snapshot.py` to confirm manifest validation, dump identity validation, embedding compatibility check, and disposable-target enforcement are all implemented.

4. **Post-import sync validation**: Inspect `agentic/src/agentic_kb/commands/sync.py` to confirm `sync changed` reads from stored baseline commits and does not fan out unboundedly.

5. **Documentation discrepancy check**: Compare the team-sharing section of `agentic/README.md` against the workflow doc. Flag any statement that does not have a corresponding shipped command.

6. **Present manual test steps**: Since `interactive_validation` requires human operators for the two-developer publish→handoff, prepare a step-by-step manual test plan that Developer 1 and Developer 2 can execute to validate the full workflow end-to-end.

## Risks / Open Questions

- **`sync changed` baseline computation**: The workflow doc says deltas are computed from stored baseline commits to current `HEAD`, but the shipped `sync changed` implementation needs verification to confirm this is not unbounded fan-out. Risk: implementation differs from doc.
- **Project refresh cursor-only limitation**: The workflow doc states Project refresh is cursor continuation only and does not detect updates to already-seen items. Verify this is accurately documented in `agentic/README.md` and that the manual full Project refresh path exists as documented.
- **Two-developer validation requires human operators**: The full publish→handoff→import workflow cannot be executed by an agent alone since it requires two separate machines with Dropbox sync. The agent can only validate the contract cross-reference and command implementations; actual end-to-end validation must be presented as manual test steps to the user.
- **Dropbox sync reliability**: The team-sharing workflow depends on Dropbox desktop sync being reliable. If sync delays cause Developer 2 to see stale or missing files, the workflow doc should include guidance on forcing sync or waiting.
- **`task-603` CI snapshots remain pending**: The team-sharing workflow explicitly excludes CI-published snapshots. Any validation should note this boundary.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for task-906 planning state
- Planning review history lives in `.agent/plans/agentic/task-plans/task-906-plan-review.md`
- Implementation review history will live in `.agent/plans/agentic/task-plans/task-906-impl-review.md` (created during implementation)
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when task-906 status changes
- No new research file expected; this task synthesizes existing research into a validation report

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-906-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-906-impl-review.md`

## Downstream Dependencies

- No downstream tasks depend on task-906; it is the final gate in Phase 9 validation.

## Final Outcome

- Validation report confirming all team-sharing contract elements are correctly documented and implemented
- Any discrepancies between documented contract and shipped tooling documented with recommended dispositions
- Manual test steps prepared for human operators to validate the full two-developer publish→handoff→import→sync workflow
- `agentic/README.md` updated if any documentation corrections are needed
- Planning status set to `approved` after review
- Build status set to `completed` after implementation and review

## Validation Executed

**Date**: 2026-03-31  
**Mode**: Orchestrator-executed two-developer simulation on single machine

**Results**:
- ✅ snapshot publish→Dropbox→fetch→import→status→BM25 proof round-trip succeeded
- ⚠️ `sync project` and `sync project --full` blocked by token scope (expected — `gh auth refresh -s read:project` required)
- ⚠️ Stale image issue: `kb-tools` container required rebuild to surface `--full` flag in CLI
- ⚠️ Root-owned snapshot files in bind-mounted `agentic/snapshots/` (operational consideration)

**Token scope issue confirmed as documented expected behavior** per workflow doc lines 286-305.
