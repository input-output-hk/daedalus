# SOP: Canonical Baseline Ownership and Fallback

**SOP ID**: `canonical-baseline-ownership`
**Related Task**: `task-904`
**Version**: 1.0

---

## Purpose

This SOP defines who is allowed to publish the canonical KB baseline, when to republish, how Developer 2 recovers when the shared snapshot is stale/missing/incompatible, the ordered fallback hierarchy, retention policy, and ownership boundaries. It sits on top of the pilot SOPs (`pilot-two-developer.md`, `pilot-failure-modes.md`, `pilot-rollback.md`) and provides the durable ownership and recovery rules that those execution workflows assume.

---

## Canonical Baseline Publisher

### Who

One trusted GPU-capable developer machine is designated as the canonical baseline publisher for v1. The designation is a team decision, not hardcoded to a specific person. In the default two-developer shape, this is Developer 1.

### Where

Publication runs from the `develop` branch only. The publisher must have their local checkout on `develop` and up to date before publishing.

### What

The publisher performs the following steps:

1. Run `sync all` to build a fresh baseline
2. Export a snapshot pair (`.dump` + `.manifest.json`)
3. Validate locally (confirm `status --json` reports `ok: true` and `embedding_compatibility.state: "compatible"`)
4. Upload the pair to the Dropbox shared folder `Daedalus_KB` using:
   ```bash
   AGENTIC_KB_SHARED_DIR="/path/to/Daedalus_KB" yarn agentic:kb:publish
   ```

### Why Local-Only

GPU access keeps embedding latency practical as the corpus grows. GitHub Actions are explicitly retired as a publication channel in v1 (PRD line 224). Publication runs on trusted developer machines, not CI runners.

### Ad Hoc Overrides

Local `OLLAMA_EMBED_MODEL` overrides are acceptable only for disposable local rebuild experiments. They are out of contract for shared baseline publication and must not be treated as a canonical-contract registry. The shared baseline always uses the canonical embedding contract (`contract_id`, `embedding_model`, `embedding_dimension`).

---

## Republish Triggers

### Embedding Contract Change

If `develop` intentionally changes the canonical tuple (`contract_id`, `embedding_model`, or `embedding_dimension`), the publisher **must** rebuild and republish a fresh canonical snapshot before any further team handoff resumes.

Older snapshots built under the previous contract are no longer valid shared baselines for import-then-`sync changed` continuation. They remain valid provenance but cannot be imported into a KB running the new contract.

### Content Refresh

Ordinary content refreshes or baseline rebuilds that keep the same canonical tuple are routine baseline refreshes, not contract-policy changes. Republish when the team's imported baseline is meaningfully stale relative to `develop`.

### Staleness Threshold

No automated staleness detection exists in v1. The publisher should republish at a cadence agreed with the second developer, or when the second developer reports that `sync changed` after import is catching too many deltas.

**Practical heuristic for "too large":** if `sync changed` reports **>500 document changes** or takes **>15 minutes** to complete, consider the baseline stale and request a republish rather than continuing incremental sync.

### Post-Pilot

After the `task-903` pilot completes successfully, the publisher should republish a clean baseline that reflects any SOP corrections or workflow adjustments discovered during the pilot.

---

## Developer 2 Recovery Procedures

Three failure scenarios with distinct recovery paths. In all scenarios, check for Dropbox conflict artifacts before proceeding.

### Scenario A: Latest Shared Snapshot Is Stale

**Symptom:** Developer 2 imports the latest `Daedalus_KB` snapshot but `sync changed` catches an unexpectedly large delta.

**Recovery:**

1. The import is still valid (same embedding contract). Run `sync changed` to catch up.
2. If the delta is too large for incremental sync to handle efficiently, request Developer 1 republish a fresher baseline.
   - **Heuristic:** if `sync changed` reports >500 document changes or takes >15 minutes, treat as stale and request republish.
3. Check for Dropbox conflict copies: look for files matching `* (conflicted copy)*` in `Daedalus_KB`. These are Dropbox conflict artifacts from simultaneous writes. **Do not import them.** Use only the canonical `.dump` + `.manifest.json` pair.
4. If a fresher baseline is published, fetch and import it, then run `sync changed`.

**See also:** `pilot-failure-modes.md` for operational failure modes (Dropbox sync delay, token scope issues).

### Scenario B: Latest Shared Snapshot Is Missing from Dropbox

**Symptom:** Developer 2 opens `Daedalus_KB` and finds no recent `.dump` + `.manifest.json` pair, or the folder is empty.

**Recovery:**

1. Check for Dropbox conflict copies (files matching `* (conflicted copy)*`). These are stale artifacts from simultaneous writes. **Do not import them.**
2. Check for a previous known-good compatible pair still present in `Daedalus_KB` (the retention policy keeps the prior pair as short-term fallback).
3. If a previous compatible pair exists:
   - Fetch and import it
   - Run `sync changed` to catch up to current `develop`
4. If no compatible pair exists in `Daedalus_KB`, notify Developer 1 to republish from `develop`.
5. As last resort, Developer 2 can rebuild locally from `develop` on their own machine.
   - GPU preferred; CPU acceptable but slower.
   - Per the PRD, CPU-only local operation remains fully functional for import, status checks, BM25 queries, and targeted sync operations.
   - Full local rebuild on CPU is feasible but will take significantly longer than GPU. Embedding generation is the primary bottleneck; expect 3-10x slower throughput depending on document count.

**See also:** `pilot-failure-modes.md` for `yarn agentic:kb:fetch` failure modes; `pilot-rollback.md` for full rollback procedures.

### Scenario C: Latest Shared Snapshot Is Incompatible

**Symptom:** `snapshot import` rejects the manifest with an embedding contract mismatch error.

**Recovery:**

1. **Do not attempt to force import.** The incompatibility is intentional and enforced by design.
2. Check for Dropbox conflict copies (files matching `* (conflicted copy)*`). These are stale artifacts. **Do not import them.**
3. Check if Developer 1 has already republished under the new contract (look for a newer pair in `Daedalus_KB`).
4. If a compatible pair exists under the new contract:
   - Recreate the KB volume: `docker compose -f docker-compose.agentic.yml down -v && up -d`
   - Import the new snapshot
   - Validate with `status --json` and the deterministic BM25 proof query
5. If no compatible pair exists yet, notify Developer 1 that a republish is required due to the contract change.
6. As last resort, Developer 2 can rebuild locally from `develop` using the current canonical embedding contract.
   - CPU-only rebuild is feasible per the PRD — import, status checks, BM25 queries, and targeted sync remain functional.
   - Full rebuild will be slower but completes successfully. Embedding generation is the primary bottleneck; expect 3-10x slower throughput on CPU.

**See also:** `pilot-failure-modes.md` for "Incompatible embedding contract" failure mode; `pilot-rollback.md` for failed import rollback procedures.

---

## Fallback Hierarchy

Ordered from preferred to last resort:

| Priority | Action | Notes |
|----------|--------|-------|
| 1 | Fetch latest compatible pair from `Daedalus_KB` and import | Fastest path; requires Dropbox sync and compatible contract |
| 2 | Fetch previous known-good compatible pair from `Daedalus_KB` and import, then `sync changed` | Requires retention kept the prior pair; import succeeds but needs catch-up sync |
| 3 | Request republish from Developer 1 (the designated publisher) | Requires Developer 1 availability and GPU-capable machine |
| 4 | Full local rebuild from `develop` on Developer 2's machine | Last resort; GPU preferred, CPU acceptable but 3-10x slower |

---

## Retention Policy

Retention is manual in v1. There is no automated cleanup.

### What to Keep

- **Current canonical pair:** Keep in `Daedalus_KB` until a newer compatible pair has been uploaded and confirmed working by Developer 2.
- **Previous known-good pair:** Keep available as a short-term fallback when possible.
- **Do not accumulate unbounded history:** Remove pairs older than the previous known-good once the new pair is confirmed.

### Confirmed Definition

A new pair is "confirmed" when **either**:
- Developer 2 has successfully imported the pair and completed `sync changed` without errors, **or**
- 48 hours have elapsed since the new pair was published

Whichever comes first.

### Cleanup Initiator

The developer who performed the import (typically Developer 2) initiates cleanup, or the designated publisher may initiate it after confirming successful handoff.

### Cleanup Safety

Both developers share write access to `Daedalus_KB`, so either can clean up stale pairs. When performing cleanup:
- Avoid deleting files that are still syncing (check Dropbox sync status indicator)
- Never delete the current or previous known-good pairs
- Only remove pairs older than the previous known-good after the new pair is confirmed

---

## Ownership Boundaries

### Designated Publisher

- Only the designated publisher uploads new canonical pairs to `Daedalus_KB`
- Developer 2 may fetch, import, and provide feedback, but does not publish canonical baselines
- The publisher runs from `develop` on a GPU-capable machine

### Temporary Handoff

If Developer 2 needs to publish temporarily (e.g., Developer 1 is unavailable and a contract change requires republish):
- Developer 2 becomes the temporary publisher
- Must run from `develop` on a GPU-capable machine
- Should notify the team coordination channel of the temporary handoff

### Permanent Role Transfer

If Developer 1 becomes permanently unavailable (e.g., leaves the team, extended absence with no return date), Developer 2 assumes the publisher role permanently. This transfer must be documented in the team coordination channel with a message stating:

> "Publisher role transferred from Developer 1 to Developer 2 effective [date]. Developer 2 is now the designated canonical baseline publisher."

The new publisher should perform a fresh publish from `develop` within one week of assuming the role to establish a new canonical baseline under their ownership.

### Shared Cleanup Rights

Both developers can clean up stale pairs from `Daedalus_KB` as part of retention. This does not confer publication rights — only the designated publisher uploads new canonical pairs.

---

## CPU-Only Rebuild Scoping Notes

When GPU is not available and Developer 2 must rebuild locally:

- **Fully functional:** import, status checks, BM25 queries, and targeted sync operations
- **Feasible but slow:** full local rebuild from `develop`
- **Primary bottleneck:** embedding generation
- **Expected slowdown:** 3-10x slower throughput depending on document count
- **No functional degradation:** the resulting KB is identical; only wall-clock time differs

CPU-only rebuild is a valid last resort in the fallback hierarchy (Scenario B and C recovery). It is not a substitute for the canonical GPU-published baseline for team handoff, but it unblocks individual developer work when the shared baseline is unavailable.

---

## Cross-References

| SOP | File | When to Consult |
|-----|------|-----------------|
| Two-Developer Pilot Workflow | `pilot-two-developer.md` (`pilot-two-developer`) | Happy-path publish/fetch/import/sync execution steps |
| Pilot Failure Modes | `pilot-failure-modes.md` (`pilot-failure-modes`) | Operational failures: Dropbox sync delay, token scope, command failures, import errors |
| Pilot Rollback Procedures | `pilot-rollback.md` (`pilot-rollback`) | Rollback by stage (publish, fetch, import, sync) and full rollback |
| Agentic KB Workflow | `.agent/workflows/agentic-kb.md` | Canonical embedding contract policy, Team Sharing Workflow, Snapshot Publication rules |
| Platform PRD | `.agent/plans/agentic/knowledge-base-platform-prd.md` | Compute placement, embedding contract, team workflow, export/import design |
