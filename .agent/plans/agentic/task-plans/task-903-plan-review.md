Planner: Iteration 1
Timestamp: 2026-03-31T00:00:00Z
Outcome: initial_plan_documented

- Drafted the canonical `task-903` plan as a documentation and SOP creation task for the two-developer pilot workflow.
- Set `planning_status: draft` and `build_status: in_progress`; this is a planning-only pass so implementation has not started.
- Marked `interaction_mode: manual_execution` explicitly because the actual pilot requires two human developers on separate machines — Developer 1 publishing to Dropbox shared folder, Developer 2 fetching/importing on a second machine. The agent cannot truthfully execute this alone.
- Defined the scope narrowly: produce operator-facing SOPs in `.agent/SOPs/agentic/` that enable the pilot to be run, not execute the pilot itself.
- Specified four SOP artifacts to create:
  - `pilot-two-developer.md` — end-to-end pilot workflow with pre-pilot checklist, Developer 1 (publisher) steps, Developer 2 (importer) steps, and validation criteria
  - `pilot-failure-modes.md` — common failure modes table with symptoms and recovery procedures
  - `pilot-evidence-template.md` — structured template for capturing pilot evidence and results
  - `pilot-rollback.md` — rollback procedures for each pilot stage
- Listed all completed prerequisite dependencies: task-901 (bootstrap validation), task-902 (security boundaries), task-705 (publish/fetch helpers), task-605 (Dropbox backend), task-606 (publish/download workflow docs), task-608 (embedding contract enforcement), task-612 (disposable target safety), task-701 (sync commands).
- Documented the pilot workflow steps grounded in the current shipped commands from `agentic/README.md` and `.agent/workflows/agentic-kb.md`: `yarn agentic:kb:publish`, `yarn agentic:kb:fetch`, `snapshot import`, `status --json`, BM25 proof query, `sync changed`.
- Captured known risks: Dropbox sync reliability, embedding contract drift, Developer 2 machine readiness, GPU availability asymmetry, and evidence capture discipline.
- Identified downstream dependencies: task-904 (baseline ownership), task-905 (validate handoff), task-906 (team-ready rollout).
- Non-goals are explicit: no pilot execution, no new feature implementation, no scope broadening into task-904/905/906, no Compose or helper script modifications.

Critiquer: Iteration 1
Timestamp: 2026-03-31T12:00:00Z
Outcome: requires_changes

- **Gaps — Pre-pilot checklist missing Dropbox account bootstrap detail**: The plan assumes both developers already have Dropbox desktop installed and running, but `agentic/README.md` and `agentic-kb.md` explicitly state Developer 2 bootstrap is documented only to the "truthful minimum" — a Dropbox access path that can accept the shared folder. The SOP should include a pre-flight step for Developer 2 to confirm they can actually create/accept a shared folder before the pilot date, not assume it on the day.
- **Gaps — No pre-flight Docker volume hygiene for Developer 2**: The plan mentions Developer 2 may have "stale Docker volumes" as a risk but does not bake a concrete pre-flight cleanup step into the SOP workflow (e.g., `docker compose down -v` or `docker volume ls | grep agentic` check before starting). This should be in the pre-pilot checklist, not just the rollback SOP.
- **Gaps — Evidence template missing `GITHUB_TOKEN` scope validation**: The PRD Phase 9 and `agentic-kb.md` both emphasize that `GITHUB_TOKEN` must have `read:project` scope for `sync project` and `sync changed` to work on an imported baseline. The evidence template should capture whether Developer 2's token has the right scopes, since a repo-only token would cause silent `sync changed` failures that look like baseline problems.
- **Gaps — Missing `sync changed` baseline precondition in Developer 2 steps**: The plan lists `sync changed` as step 7 for Developer 2 but does not explicitly gate it on the prior success of `status --json` and the BM25 proof query. The SOP should make this a conditional step: "only run `sync changed` if import validation passed; otherwise follow failure-modes SOP."
- **Complexity — Failure modes table is appropriate but missing one common case**: The table does not cover the `GITHUB_TOKEN` scope failure (HTTP 403 "project scope missing") which is a documented expected failure mode in `agentic-kb.md`. This should be added since it will block `sync changed` on a fresh import if Developer 2 only has repo-scoped credentials.
- **Command accuracy — Minor inconsistency in Developer 2 step 2**: The plan says Developer 2 should "Start fresh stack: `docker compose -f docker-compose.agentic.yml up -d`" but does not explicitly state this should be a clean stack (no existing `agentic` volume). The `agentic/README.md` clean bootstrap path assumes a fresh stack. The SOP should clarify `docker compose down -v && docker compose -f docker-compose.agentic.yml up -d` for the import-target safety contract.
- **Evidence capture — Template lacks machine-readable artifact capture**: The evidence template captures "command outputs (truncated to relevant lines)" but does not specify capturing the raw `status --json` and BM25 search JSON outputs as structured artifacts. Downstream task-905 validation will need these exact JSON blobs to confirm `ok: true`, `embedding_compatibility.state: "compatible"`, and BM25 hit structure. Recommend adding a "capture full JSON outputs as files" step.
- **Downstream readiness — Evidence template does not reference task-905 acceptance criteria**: The evidence template should explicitly map its captured fields to the downstream validation needs of task-905 (validate handoff). Without this mapping, pilot operators may capture irrelevant details while missing what task-905 needs to validate.
- **Interaction mode — Correctly classified but missing human checkpoint documentation**: `manual_execution` is correct. However, the plan does not document where the human-in-the-loop checkpoints are within the SOP itself (e.g., "Developer 2 must visually confirm Dropbox sync before running fetch"). This is important because an agent executing the SOP creation task should not accidentally automate steps that require human verification.
- **Scope — Plan stays within stated non-goals**: No issues here. The plan correctly limits itself to documentation/SOP creation without executing the pilot or modifying code.
- **Missing — No SOP for pilot coordination/communication**: The plan does not include an SOP for how the two developers coordinate during the pilot (e.g., "Developer 1 notifies Developer 2 when snapshot is published," "Developer 2 confirms import success"). This is a minor gap but could cause friction during actual pilot execution.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-31T12:30:00Z
Outcome: revised_plan_addresses_review_feedback

- Added Developer 2 Dropbox pre-flight verification step to pre-pilot checklist (confirm shared folder acceptance capability before pilot date)
- Added Docker volume hygiene check to pre-pilot checklist (`docker volume ls | grep agentic` with cleanup via `down -v`)
- Added `GITHUB_TOKEN` scope validation to pre-pilot checklist (`gh auth status` to confirm `read:project` scope)
- Added three explicit HUMAN CHECKPOINT callouts in Developer 2 steps (Dropbox sync confirmation, import validation, BM25 proof results)
- Updated Developer 2 step 2 to include `docker compose -f docker-compose.agentic.yml down -v` before `up -d` for clean import target
- Added conditional gating on `sync changed` — only run if import validation and BM25 proof passed; otherwise follow failure-modes SOP
- Added `GITHUB_TOKEN` scope failure (HTTP 403 "project scope missing") to failure modes table with recovery procedure
- Enhanced evidence template with structured JSON artifact capture requirements (`status--json.json`, `bm25-proof-result.json`, `sync-changed-result.json`)
- Added task-905 field mapping to evidence template, explicitly mapping each captured field to downstream validation needs
- Added SOP 5: Pilot Coordination and Communication covering pre-pilot sync, publish notification, checkpoint acknowledgments, failure escalation, evidence handoff, and post-pilot debrief
- Updated Files Expected To Change to include new `pilot-coordination.md` SOP

Critiquer: Iteration 2
Timestamp: 2026-03-31T01:26:00Z
Outcome: approved

- All 11 findings from Iteration 1 are addressed: Dropbox pre-flight (line 69), Docker volume hygiene (line 70), GITHUB_TOKEN scope validation (lines 71, 133), conditional sync gating (line 92), HTTP 403 failure mode (line 110), clean stack with `down -v` (line 85), JSON artifact capture (lines 129-132), task-905 field mapping (lines 134-138), 3 HUMAN CHECKPOINT callouts (lines 84, 89, 91), and SOP 5 coordination (lines 150-160).
- No new issues introduced by revisions. All added commands match shipped reality in `agentic-kb.md`. Docker compose paths, env vars, and tool invocations are consistent with the workflow doc.
- SOPs remain scoped appropriately — all are documentation-only artifacts in `.agent/SOPs/agentic/`, no implementation changes to KB stack or Compose config.
- Evidence template JSON artifacts (`status--json.json`, `bm25-proof-result.json`, `sync-changed-result.json`) correctly map to task-905 acceptance criteria fields.
- GITHUB_TOKEN scope validation uses `gh auth status` which matches the recovery path (`gh auth refresh -s read:project`) documented in `agentic-kb.md:302`.
- The conditional sync gate correctly references `pilot-failure-modes.md` for failure escalation.
- Plan is complete, internally consistent, and ready for implementation.

Decision: approved
