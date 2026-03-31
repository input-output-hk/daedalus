# Task Plan: task-904 Document canonical baseline ownership and fallback

- **Task ID**: `task-904`
- **Title**: `Document canonical baseline ownership and fallback`
- **Planning Status**: `approved`
- **Build Status**: `completed`
- **Interaction Mode**: `autonomous`

## Why This Task Was Chosen Now

- `task-904` is the next critical-path task in Phase 9 (Validation and Rollout) after `task-903` (pilot with multiple developers) completed.
- All direct dependencies are completed: `task-605` (Dropbox shared backend), `task-606` (publish/download workflow), `task-611` (embedding contract policy), and `task-902` (security boundaries).
- The PRD Phase 9 explicitly calls for documenting canonical baseline ownership and recovery procedures after the pilot produces operational learnings.
- The pilot SOPs (`task-903`) cover the happy-path execution but do not define durable ownership rules, republish triggers, or structured recovery when the shared snapshot is stale, missing, or incompatible. Those gaps block `task-905` (validate two-developer handoff) and `task-906` (team-ready rollout).
- This task consolidates scattered decisions from the PRD, workflow doc, and research files into a single authoritative SOP that operators can follow without reconstructing policy from multiple sources.

## Scope

This task produces documentation that defines:

- **Canonical baseline ownership**: who is allowed to publish the canonical baseline, under what conditions, and on what machine
- **Republish policy**: when to republish from `develop` (embedding contract changes, content refresh, stale baseline)
- **Recovery procedures for Developer 2**: how to recover when the latest shared snapshot is stale, missing from Dropbox, or incompatible with the local embedding contract
- **Fallback hierarchy**: the ordered recovery path from preferred (fetch latest compatible) to last resort (full local rebuild)
- **Retention policy**: how long to keep old snapshots in `Daedalus_KB` and who is responsible for cleanup

The deliverable is one SOP file in `.agent/SOPs/agentic/` plus updates to this task plan and the planning review log.

## Non-Goals

- Do not implement new helper commands, automation, or Dropbox API integrations
- Do not change the embedding contract, snapshot format, or import/export behavior
- Do not broaden into `task-905` (validate two-developer handoff) or `task-906` (team-ready rollout)
- Do not define a richer multi-developer (>2) policy; v1 is explicitly a two-developer workflow
- Do not introduce GitHub Actions or any automated publication path; v1 is local-only
- Do not modify existing SOPs from `task-903`; this SOP complements them with ownership and recovery rules

## Relevant Dependencies

| Task | Title | Status | Purpose |
|------|-------|--------|---------|
| `task-605` | Select private shared snapshot storage backend | Completed | Dropbox `Daedalus_KB` shared folder selected; artifact discovery, retention, and recovery expectations defined |
| `task-606` | Document local baseline publish/download workflow | Completed | Publish/fetch helper commands and consumption path documented in `.agent/workflows/agentic-kb.md` |
| `task-611` | Define canonical embedding contract and republish policy | Completed | Single embedding contract policy, mismatch handling, and republish trigger documented |
| `task-902` | Review security and shared-storage boundaries | Completed | Security constraints confirmed: read-only MCP, token handling, snapshot boundaries, KB isolation |
| `task-903` | Pilot with multiple developers | Completed | Pilot SOPs provide the execution workflow that this task's ownership/recovery rules sit on top of |

## Files Expected To Change

- `.agent/SOPs/agentic/canonical-baseline-ownership.md` — New SOP for baseline ownership, republish policy, and recovery procedures
- `.agent/plans/agentic/task-plans/task-904.md` — This canonical task plan
- `.agent/plans/agentic/task-plans/task-904-plan-review.md` — Append-only planning review log
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` — Update task-904 status on completion

No implementation changes to KB stack, Compose config, helper scripts, or existing SOPs.

## Implementation Approach

### SOP: Canonical Baseline Ownership and Fallback (`.agent/SOPs/agentic/canonical-baseline-ownership.md`)

The SOP will consolidate the following policy areas from existing sources:

#### 1. Canonical Baseline Publisher

- **Who**: One trusted GPU-capable developer machine is designated as the canonical baseline publisher for v1
- **Where**: Publication runs from the `develop` branch only
- **What**: The publisher runs `sync all`, exports a snapshot pair, validates locally, and uploads to `Daedalus_KB`
- **Why local-only**: GPU access keeps embedding latency practical; GitHub Actions are explicitly retired as a publication channel in v1
- **Ad hoc overrides**: Local `OLLAMA_EMBED_MODEL` overrides are acceptable only for disposable local rebuild experiments; they are out of contract for shared baseline publication

#### 2. Republish Triggers

- **Embedding contract change**: If `develop` intentionally changes the canonical tuple (`contract_id`, `embedding_model`, or `embedding_dimension`), the publisher must rebuild and republish a fresh canonical snapshot before any further team handoff resumes. Older snapshots under the previous contract are no longer valid shared baselines for import-then-`sync changed` continuation.
- **Content refresh**: Ordinary content refreshes or baseline rebuilds that keep the same canonical tuple are routine baseline refreshes, not contract-policy changes. Republish when the team's imported baseline is meaningfully stale relative to `develop`.
- **Staleness threshold**: No automated staleness detection exists in v1. The publisher should republish at a cadence agreed with the second developer, or when the second developer reports that `sync changed` after import is catching too many deltas. Practical heuristic for "too large": if `sync changed` reports >500 document changes or takes >15 minutes to complete, consider the baseline stale and request a republish rather than continuing incremental sync.
- **Post-pilot**: After the `task-903` pilot completes successfully, the publisher should republish a clean baseline that reflects any SOP corrections or workflow adjustments discovered during the pilot.

#### 3. Developer 2 Recovery Procedures

Three failure scenarios with distinct recovery paths:

**Scenario A: Latest shared snapshot is stale**
- Symptom: Developer 2 imports the latest `Daedalus_KB` snapshot but `sync changed` catches an unexpectedly large delta
- Recovery: Import is still valid (same embedding contract). Run `sync changed` to catch up, or request Developer 1 republish a fresher baseline if the delta is too large for incremental sync to handle efficiently. Heuristic: if `sync changed` reports >500 document changes or takes >15 minutes, treat as stale and request republish. Also check for files matching `* (conflicted copy)*` in `Daedalus_KB` — these are Dropbox conflict artifacts from simultaneous writes; ignore them and use only the canonical `.dump` + `.manifest.json` pair.

**Scenario B: Latest shared snapshot is missing from Dropbox**
- Symptom: Developer 2 opens `Daedalus_KB` and finds no recent `.dump` + `.manifest.json` pair, or the folder is empty
- Recovery:
  1. Check for Dropbox conflict copies (files matching `* (conflicted copy)*`) — these are stale artifacts from simultaneous writes; do not import them
  2. Check for a previous known-good compatible pair still present in `Daedalus_KB` (retention policy keeps the prior pair as short-term fallback)
  3. If a previous compatible pair exists, fetch and import it, then run `sync changed` to catch up
  4. If no compatible pair exists in `Daedalus_KB`, notify Developer 1 to republish from `develop`
  5. As last resort, Developer 2 can rebuild locally from `develop`. Per the PRD, CPU-only local operation remains fully functional for import, status checks, BM25 queries, and targeted sync operations — a full local rebuild on CPU is feasible but will take significantly longer than GPU. Embedding generation is the primary bottleneck; expect 3-10x slower throughput depending on document count.

**Scenario C: Latest shared snapshot is incompatible**
- Symptom: `snapshot import` rejects the manifest with an embedding contract mismatch error
- Recovery:
  1. Do not attempt to force import; the incompatibility is intentional and enforced by design
  2. Check for Dropbox conflict copies (files matching `* (conflicted copy)*`) — these are stale artifacts; do not import them
  3. Check if Developer 1 has already republished under the new contract (look for a newer pair in `Daedalus_KB`)
  4. If a compatible pair exists under the new contract, recreate the KB volume (`docker compose down -v && up -d`) and import the new snapshot
  5. If no compatible pair exists yet, notify Developer 1 that a republish is required due to the contract change
  6. As last resort, Developer 2 can rebuild locally from `develop` using the current canonical embedding contract. CPU-only rebuild is feasible per the PRD — import, status checks, BM25 queries, and targeted sync remain functional; full rebuild will be slower but completes successfully.

#### 4. Fallback Hierarchy

Ordered from preferred to last resort:

1. **Fetch latest compatible pair** from `Daedalus_KB` and import
2. **Fetch previous known-good compatible pair** from `Daedalus_KB` (if retention kept it) and import, then `sync changed`
3. **Request republish** from Developer 1
4. **Full local rebuild** from `develop` on Developer 2's machine (GPU preferred, CPU acceptable but slower)

#### 5. Retention Policy

- Retention is manual in v1
- Keep the current canonical pair in `Daedalus_KB` until a newer compatible pair has been uploaded and confirmed working by Developer 2
- Keep the previous known-good pair available as a short-term fallback when possible
- Do not accumulate unbounded history in `Daedalus_KB`; remove pairs older than the previous known-good once the new pair is confirmed. "Confirmed" means Developer 2 has successfully imported the pair and completed `sync changed` without errors, or 48 hours have elapsed since the new pair was published — whichever comes first. The developer who performed the import (typically Developer 2) initiates cleanup, or the designated publisher may initiate it after confirming successful handoff.
- Both developers share write access to `Daedalus_KB`, so either can clean up stale pairs, but only the designated publisher should upload new canonical pairs. When performing cleanup, avoid deleting files that are still syncing (check Dropbox sync status indicator) and never delete the current or previous known-good pairs.

#### 6. Ownership Boundaries

- Only the designated publisher uploads new canonical pairs to `Daedalus_KB`
- Developer 2 may fetch, import, and provide feedback, but does not publish canonical baselines
- If Developer 2 needs to publish temporarily (e.g., Developer 1 is unavailable and a contract change requires republish), Developer 2 becomes the temporary publisher and must run from `develop` on a GPU-capable machine
- **Permanent publisher role transfer**: If Developer 1 becomes permanently unavailable (e.g., leaves the team, extended absence with no return date), Developer 2 assumes the publisher role permanently. This transfer must be documented in the team coordination channel with a message stating: "Publisher role transferred from Developer 1 to Developer 2 effective [date]. Developer 2 is now the designated canonical baseline publisher." The new publisher should perform a fresh publish from `develop` within one week of assuming the role to establish a new canonical baseline under their ownership.
- Both developers can clean up stale pairs from `Daedalus_KB` as part of retention

## Acceptance Criteria

- [ ] `.agent/SOPs/agentic/canonical-baseline-ownership.md` exists with complete ownership, republish, and recovery documentation
- [ ] SOP defines who is allowed to publish the canonical baseline
- [ ] SOP defines when to republish from `develop` (embedding contract change, content staleness, post-pilot)
- [ ] SOP defines recovery procedures for all three Developer 2 failure scenarios (stale, missing, incompatible)
- [ ] SOP defines the ordered fallback hierarchy from preferred to last resort
- [ ] SOP defines the manual retention policy for `Daedalus_KB`
- [ ] SOP is consistent with existing decisions in the PRD, `.agent/workflows/agentic-kb.md`, and `task-605`/`task-606`/`task-611` research files
- [ ] No implementation changes to KB stack, Compose config, or helper scripts
- [ ] SOP references the existing pilot SOPs (`pilot-two-developer.md`, `pilot-failure-modes.md`) where appropriate without duplicating their content
- [ ] SOP cross-references to pilot SOPs use correct file names and SOP IDs (verified against actual files in `.agent/SOPs/agentic/`)

## Verification Plan

- Verify the SOP file exists in `.agent/SOPs/agentic/` and contains all required sections
- Cross-reference ownership rules against the PRD (lines 219-224, 255-259, 263-280, 316-321)
- Cross-reference republish triggers against `.agent/workflows/agentic-kb.md` (Canonical Embedding Contract section, lines 107-118)
- Cross-reference recovery procedures against `task-605-dropbox-shared-backend.md` (recovery findings) and `pilot-failure-modes.md` (existing failure mode table)
- Confirm the fallback hierarchy is consistent with the PRD's stated recovery path (recreate KB volume, import compatible snapshot, or rebuild locally)
- Confirm retention policy matches the PRD's statement that retention is manual in v1
- Confirm no contradictions with existing SOPs from `task-903`
- **Verify SOP cross-references**: Confirm that all references to pilot SOPs (`pilot-two-developer.md`, `pilot-failure-modes.md`, and `pilot-rollback.md`) use correct file names and SOP IDs. Check that each cross-reference points to an existing file in `.agent/SOPs/agentic/` and that the referenced SOP ID matches the file's declared ID.

## Risks / Open Questions

- **Publisher designation**: The PRD says "trusted GPU-capable developer machine" but does not name a specific developer. The SOP should leave the designation as a team decision rather than hardcoding a person.
- **Staleness threshold**: No objective metric exists in v1 for "how stale is too stale." The SOP should acknowledge this and recommend a team-agreed cadence or delta-size heuristic.
- **Developer 2 as temporary publisher**: The PRD assumes Developer 1 is the publisher, but the SOP should cover the case where Developer 2 needs to take over temporarily.
- **Dropbox conflict handling**: If both developers write to `Daedalus_KB` simultaneously (e.g., retention cleanup overlapping with a new publish), Dropbox may create conflict copies. The SOP should note this risk and include guidance on identifying and avoiding conflicted copies in all recovery scenarios.
- **No automated staleness detection**: v1 has no mechanism to automatically detect when the shared baseline is stale. This is a known gap that future automation could address.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for task-904 planning state
- Planning review history lives in `.agent/plans/agentic/task-plans/task-904-plan-review.md`
- Implementation review history will live in `.agent/plans/agentic/task-plans/task-904-impl-review.md` (created during implementation)
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when task-904 status changes
- No new research file expected; this task synthesizes existing research into an SOP

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-904-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-904-impl-review.md`

## Downstream Dependencies

- **task-905** — Validate two-developer publish-download-import handoff (depends on task-904; uses the ownership and recovery SOPs during validation)
- **task-906** — Validate team-ready publication and handoff workflow (depends on task-904; broader rollout requires clear ownership rules)

## Final Outcome

- SOP created at `.agent/SOPs/agentic/canonical-baseline-ownership.md` documenting publisher ownership, republish triggers, Developer 2 recovery procedures, fallback hierarchy, retention policy, and ownership boundaries
- Planning review log at `.agent/plans/agentic/task-plans/task-904-plan-review.md` — 2 iterations: Planner→Critiquer (requires_changes)→Planner→Critiquer (approved)
- Implementation review log at `.agent/plans/agentic/task-plans/task-904-impl-review.md` — 1 iteration: Implementation→Code Review (approved)
- All acceptance criteria satisfied: SOP covers all six required policy areas, cross-references pilot SOPs correctly, no implementation changes to KB stack
- Task completed 2026-03-31
