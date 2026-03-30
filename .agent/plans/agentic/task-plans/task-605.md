# Task Plan: task-605 Select private shared snapshot storage backend

- Task ID: `task-605`
- Title: `Select private shared snapshot storage backend`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `interactive_decision`

## Why This Task Was Chosen Now

- `task-602` shipped the snapshot export/import contract, and `task-611` documented the canonical embedding-contract policy, but the repo still does not name one concrete private shared storage backend for the two-developer handoff workflow.
- The PRD treats this backend selection as a rollout gate, not optional polish, and current docs still refer only to a future or already-chosen private backend without identifying one.
- Downstream work such as `task-606`, `task-705`, `task-706`, `task-902`, `task-904`, and `task-906` should not invent upload, fetch, retention, or recovery behavior until this task records the actual storage decision.

## Scope

- Select one concrete private shared storage backend for canonical KB snapshot sharing between the two developers.
- Document the backend choice and the minimum operator contract needed to use it safely:
- artifact discovery
- authentication/bootstrap
- artifact naming/location expectations
- retention expectations
- integrity expectations after download
- outage and latest-artifact-unavailable recovery
- Keep the task limited to backend selection and decision documentation that unblocks later workflow and helper-command tasks.

## Non-Goals

- Do not implement upload/download helper commands or wrappers; that remains `task-705`.
- Do not write the full local publish/download operator workflow; that remains `task-606`.
- Do not add new snapshot export/import behavior, manifest rules, or embedding-contract behavior; those are already covered by `task-602`, `task-608`, `task-611`, and `task-612`.
- Do not provision cloud resources, rotate secrets, or execute any manual storage setup on behalf of the user in this planning task.
- Do not broaden the task into team ownership/fallback SOP work beyond the minimum outage-recovery statement needed for the backend decision.

## Relevant Dependencies

- Completed upstream work:
- `task-602` implemented the portable `.dump` plus `.manifest.json` snapshot pair and import-side integrity validation.
- `task-611` documented the canonical embedding-contract discovery and republish policy.
- `task-803` updated `.agent/workflows/agentic-kb.md` to the current shipped KB workflow.
- Relevant repo context reviewed:
- `.agent/plans/agentic/knowledge-base-platform-prd.md`
- `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- `.agent/workflows/agentic-kb.md`
- `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md`
- `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
- `.agent/plans/agentic/research/task-611-canonical-embedding-contract-policy.md`
- Downstream work blocked or shaped by this task:
- `task-606` local baseline publish/download workflow docs
- `task-705` local publish/fetch helper commands
- `task-706` local baseline publication smoke checks
- `task-902` security and shared-storage boundary review
- `task-904` canonical baseline ownership and fallback
- `task-906` team-ready publication and handoff validation

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-prd.md` - record the chosen backend at the platform-plan level if implementation confirms that the higher-level rollout doc should name it explicitly.
- `.agent/workflows/agentic-kb.md` - replace the current placeholder wording about a future or external backend with the selected backend's operator contract.
- `agentic/README.md` - document the selected backend at the developer entrypoint level only if needed to keep setup and handoff instructions truthful.
- `.agent/plans/agentic/task-plans/task-605.md` - keep this canonical task plan current through implementation and final outcome.
- `.agent/plans/agentic/task-plans/task-605-plan-review.md` - append-only planning review transcript.
- `.agent/plans/agentic/task-plans/task-605-impl-review.md` - append-only implementation review log created when implementation begins.
- `.agent/plans/agentic/research/task-605-dropbox-shared-backend.md` - durable note for the final backend decision, constraints, and evidence pointers.

## Implementation Approach

- Classify this task as `interactive_decision`, not `autonomous`, because the repository does not already contain a concrete approved backend choice and that choice depends on an external infrastructure/security decision that cannot be derived truthfully from repo state alone.
- Keep the required human checkpoint explicit. Do not hide backend selection inside an autonomous implementation loop.
- Before any implementation claims that one backend is the approved team backend, obtain a direct user/maintainer decision naming the backend to use.
- The received decision for this task is: Dropbox, using a shared Dropbox folder named `Daedalus_KB`, created by Developer 1 in an existing Dropbox account and shared to Developer 2 with write access for both developers.
- After that decision, make the smallest truthful doc updates needed to capture:
- the exact backend name
- how each developer discovers the latest canonical snapshot artifact
- how each developer bootstraps access/authentication
- what naming or path convention identifies canonical artifacts
- what retention expectation applies
- what post-download integrity checks are expected before import
- what recovery path applies when the backend or latest artifact is unavailable
- Keep Dropbox wording narrow and truthful: document the shared-folder contract, the writable-by-both-developers requirement, and the truthful minimum for Developer 2 bootstrap, but do not invent helper commands, broader SOP ownership, or unstated Dropbox account details.
- Keep integrity wording aligned with shipped behavior from `task-602` and `task-612`: downloaded snapshot pairs are validated by manifest schema plus dump size/hash during `snapshot import`, and restore targets remain disposable-only.
- Keep canonical-contract wording aligned with `task-611`: backend selection does not change embedding-contract policy, canonical republish policy, or compatibility enforcement.
- If implementation records selection rationale beyond the workflow docs, keep it narrow and decision-oriented rather than turning this task into a broad vendor evaluation memo.

## Acceptance Criteria

- The canonical plan records `Interaction Mode: interactive_decision` and explicitly states that backend selection requires user input before implementation can be completed.
- The repo documents Dropbox as the one concrete private shared snapshot storage backend instead of placeholder wording about a future choice.
- The documented backend contract covers artifact discovery, authentication/bootstrap, naming/location expectations, retention, integrity expectations after download, and outage recovery for the two-developer workflow.
- The docs are explicit that Dropbox usage is shared-folder based through `Daedalus_KB` and that the folder must be writable by both developers.
- The documentation stays truthful about current implementation boundaries and does not smuggle in `task-606` workflow mechanics or `task-705` helper-command behavior.
- The documented integrity and compatibility expectations remain consistent with the shipped manifest/import and embedding-contract policies.

## Verification Plan

- Planning verification: confirm the canonical plan is explicit that this task is blocked on a human backend decision and does not pretend the repo already contains one.
- Implementation verification after backend selection:
- review `.agent/workflows/agentic-kb.md`, `agentic/README.md`, and any PRD update for consistency with the selected backend and current shipped snapshot contracts
- confirm the docs name exactly one backend and one discovery/bootstrap path for the two developers
- confirm the docs keep later helper commands and broader workflow automation out of scope
- review the implementation review log and research note for consistency with the selected Dropbox shared-folder contract
- Manual validation required for final acceptance:
- one developer with publisher access can authenticate to the selected backend and locate the intended artifact location
- the second developer can independently authenticate, discover the same artifact location, and download the snapshot pair
- both developers can confirm that the shared Dropbox folder `Daedalus_KB` is writable by both developers
- the downloaded pair can be validated by the existing documented import path on a disposable KB

## Risks / Open Questions

- The backend decision and required manual validation are complete for task-605 scope, but the repository still does not record richer Developer 2 account/bootstrap details beyond needing a Dropbox access path that can accept the shared writable folder.
- Dropbox folder hygiene remains manual in v1. Later tasks may refine publication helpers or ownership/fallback SOPs, but task-605 should not invent them early.
- This task stays narrow: selecting the backend, documenting the operator contract, and recording the completed validation evidence is enough. Upload/fetch command design and broader publication workflow automation remain later tasks.

## Required Docs / Tracking / Research Updates

- Create and maintain this canonical task plan at `.agent/plans/agentic/task-plans/task-605.md`.
- Append all planning-review decisions to `.agent/plans/agentic/task-plans/task-605-plan-review.md`.
- Create `.agent/plans/agentic/task-plans/task-605-impl-review.md` when implementation begins and keep it append-only.
- Update `.agent/workflows/agentic-kb.md` during implementation once the user chooses the backend.
- Update `agentic/README.md` only if needed to keep the developer-facing instructions aligned with the selected backend.
- Update `.agent/plans/agentic/knowledge-base-platform-prd.md` only if the higher-level rollout doc should explicitly record the final backend choice.
- Add a durable research note under `.agent/plans/agentic/research/` capturing the Dropbox decision, constraints, gotchas, and evidence pointers.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` later only when implementation is complete and task metadata changes are being recorded.

## Current Outcome

- Dropbox is the selected private shared snapshot storage backend for the two-developer workflow.
- The repository docs record the shared-folder location as `Daedalus_KB`, the writable-by-both-developers expectation, the truthful minimum bootstrap path, and the manual discovery/retention/recovery contract.
- User validation completed end to end: once the separately-fixed docs persistence issue was out of the way, the Dropbox shared-folder workflow successfully exported, transferred, imported, and validated a canonical snapshot pair on a fresh disposable KB.

## Final Outcome

- Completed as a narrow backend-selection documentation update plus durable task tracking and research updates. Dropbox is the approved private shared snapshot backend, using shared folder `Daedalus_KB`, and the documented workflow has now been validated successfully end to end for task-605 scope.

## Verification Outcome

- Manual validation succeeded after the KB snapshot workflow became healthy again from a separately-fixed docs persistence bug outside task-605 scope.
- `sync docs` persisted `1822` rows in `agentic.kb_documents` before export.
- `snapshot export` succeeded for `agentic-kb-20260330T154354Z.dump` and its sibling manifest.
- A fresh disposable KB reset via `docker compose -f docker-compose.agentic.yml down -v` and `up -d` was followed by successful `snapshot import ... --yes`.
- Post-import `SELECT COUNT(*) AS documents FROM agentic.kb_documents;` returned `1822`.
- `status --json` returned `"ok": true`.
- BM25 search for `GitHub Releases assets are out of scope for KB snapshot sharing` returned hits, including `.agent/workflows/agentic-kb.md`.

## Review Outcome

- Planning review is approved in `.agent/plans/agentic/task-plans/task-605-plan-review.md`.
- Implementation review log `.agent/plans/agentic/task-plans/task-605-impl-review.md` now records both the documentation update and the completed user handoff validation evidence.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-605-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-605-impl-review.md`

## Required User Inputs

- None remaining.
- Backend decision received and implemented: Dropbox.
- Validation evidence received: shared Dropbox folder `Daedalus_KB` with Developer 1 sharing write access to Developer 2, followed by successful disposable-KB export/import validation.

## Required Manual Test Steps

- None remaining for task-605.
- The required Dropbox shared-folder and disposable-KB import validation steps were completed manually and are captured in the verification outcome and implementation review log.

## What Evidence Is Needed Back From The User

- None remaining.

## Whether Implementation Can Proceed Before That User Interaction

- No further user interaction is required for task-605. The interactive decision and the required manual validation evidence have both been received.

## Planning Status Rationale

- Planning status is `approved` because the canonical plan was reviewed and approved in `.agent/plans/agentic/task-plans/task-605-plan-review.md`.
- Build status is `completed` because the documentation implementation is landed and the required Dropbox shared-folder plus disposable-KB import validation evidence has now been received and recorded in `.agent/plans/agentic/task-plans/task-605-impl-review.md`.
