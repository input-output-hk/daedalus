Implementation: Iteration 1
Timestamp: 2026-03-31T01:30:00Z
Outcome: all_sop_artifacts_created_and_verified

- Created `.agent/SOPs/agentic/pilot-two-developer.md` — Primary SOP with pre-pilot checklist (9 items including Dropbox pre-flight, Docker volume hygiene, GITHUB_TOKEN scope validation), Developer 1 (Publisher) steps (4 steps), Developer 2 (Importer) steps (7 steps with 3 explicit HUMAN CHECKPOINT callouts), conditional sync gating, and success criteria
- Created `.agent/SOPs/agentic/pilot-failure-modes.md` — Failure mode reference table with 9 entries covering Dropbox sync delay, incompatible embedding contract, non-disposable import target, missing baseline for sync changed, GITHUB_TOKEN scope failure (HTTP 403), publish failure, fetch failure, status failure, and zero-hit BM25 proof; includes escalation path
- Created `.agent/SOPs/agentic/pilot-evidence-template.md` — Structured evidence capture template with pilot metadata, machine details, snapshot info, timestamps, pre-flight checks, command outputs, structured JSON artifacts (status--json.json, bm25-proof-result.json, sync-changed-result.json) with task-905 field mapping, validation results, errors/resolution log, and notes sections
- Created `.agent/SOPs/agentic/pilot-rollback.md` — Rollback procedures for each stage (failed publish, failed fetch, failed import, failed sync changed) plus full rollback procedure with safety notes
- Created `.agent/SOPs/agentic/pilot-coordination.md` — Coordination SOP covering pre-pilot sync, publish notification with example message, sync confirmation, 4 checkpoint acknowledgment templates, failure escalation path, evidence handoff, post-pilot debrief, and communication log template

### Verification
- Read `pilot-two-developer.md` (159 lines) — confirmed all required sections: pre-pilot checklist, Developer 1 steps, Developer 2 steps, 3 HUMAN CHECKPOINT callouts, success criteria
- Read `pilot-failure-modes.md` (46 lines) — confirmed failure mode table with 9 entries including HTTP 403 GITHUB_TOKEN scope failure, escalation path, and notes
- Read `pilot-evidence-template.md` (228 lines) — confirmed structured JSON artifact capture, task-905 field mapping, GITHUB_TOKEN scope evidence section, validation results
- Read `pilot-rollback.md` (138 lines) — confirmed rollback for each stage plus full rollback procedure with safety notes
- Read `pilot-coordination.md` (137 lines) — confirmed all coordination sections: pre-pilot sync, publish notification, checkpoint acknowledgments, failure escalation, evidence handoff, post-pilot debrief
- Cross-referenced all commands against `agentic/README.md` and `.agent/workflows/agentic-kb.md` — all commands match shipped reality (docker compose paths, env vars, yarn agentic:kb:publish/fetch, snapshot import, status --json, BM25 search, sync changed, gh auth refresh)
- No code, Compose files, or helper scripts were modified

### Deviations from Approved Plan
- None. All deliverables match the approved plan in task-903.md and the revised plan in task-903-plan-review.md.

### User Handoff
(none required - documentation-only task)

Code Review: Iteration 1
Timestamp: 2026-03-31T01:28:24Z
Outcome: approved

- Completeness: All 5 SOP files exist with required sections — pilot-two-developer.md (pre-pilot checklist, Dev 1/Dev 2 steps, 3 HUMAN CHECKPOINT callouts, success criteria), pilot-failure-modes.md (9 failure modes with recovery), pilot-evidence-template.md (structured JSON capture, task-905 field mapping), pilot-rollback.md (per-stage and full rollback), pilot-coordination.md (pre-pilot sync, publish notification, checkpoint acks, escalation, evidence handoff, post-pilot debrief)
- Command accuracy: All commands cross-referenced and match shipped reality in agentic-kb.md and agentic/README.md — docker compose paths, AGENTIC_KB_SHARED_DIR env var, yarn agentic:kb:publish/fetch, snapshot import, status --json, BM25 search query, sync changed, gh auth refresh -s read:project
- Pre-pilot checklist: Includes Dropbox pre-flight (line 22), Docker volume hygiene with `docker volume ls | grep agentic` and `down -v` cleanup (line 23), GITHUB_TOKEN scope validation with `gh auth status` and `read:project` requirement (line 24)
- Developer 2 steps: Includes `down -v` before `up -d` for clean import target (lines 85-86), conditional sync changed gating — only if import validation and BM25 proof passed (lines 131-139), 3 explicit HUMAN CHECKPOINT callouts (Dropbox sync confirmation, import validation, BM25 proof results)
- Failure modes: Includes HTTP 403 GITHUB_TOKEN scope failure with symptom "project scope missing" and recovery via `gh auth refresh -s read:project` (line 23)
- Evidence template: Includes JSON artifact capture (status--json.json, bm25-proof-result.json, sync-changed-result.json) with explicit task-905 field mapping for each artifact (lines 136-185), GITHUB_TOKEN scope evidence section
- Coordination SOP: Includes publish notification with example message template (lines 27-47), 4 checkpoint acknowledgment templates (lines 61-79), failure escalation path (lines 83-95), evidence handoff procedure, post-pilot debrief
- No code changes: Confirmed — no modifications to KB stack, Compose config, or helper scripts; documentation-only deliverables
- Consistency: All SOPs are internally consistent and consistent with existing workflow docs in agentic-kb.md and agentic/README.md

Decision: approved
