# Task Plan: task-606 Document local baseline publish and download workflow

- Task ID: `task-606`
- Title: `Document local baseline publish and download workflow`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-605` selected Dropbox as the private shared snapshot storage backend and that decision is now validated and recorded.
- `task-608` enforced embedding compatibility and that contract is now ship-tested.
- `task-611` defined the canonical embedding-contract policy and that policy is now documented in the workflow doc.
- The workflow doc at `.agent/workflows/agentic-kb.md` already contains extensive coverage of the two-developer publish/upload/download/import workflow through the Recommended Team Pattern (lines 148-156), Snapshot Publication policy (lines 165-185), Consumption path (lines 187-194), and Expected validation shape (lines 196-204).
- This task confirms that existing coverage is sufficient for task-606 scope and that no additional workflow mechanics need to be invented or added.

## Scope

- Confirm that `.agent/workflows/agentic-kb.md` already documents the complete two-developer local GPU-backed publication, upload, download, and import workflow using the Dropbox backend from task-605.
- Verify that the existing coverage is internally consistent with the task-605 backend decision and task-611 embedding-contract policy.
- Record the finding that no gap-fill documentation work is needed beyond confirming the existing doc is sufficient.
- Update this canonical task plan to reflect the confirmed sufficiency of existing coverage.

## Non-Goals

- Do not implement new helper commands, upload scripts, or download wrappers; those belong to `task-705` and `task-706`.
- Do not modify snapshot export/import behavior; those are covered by `task-602` and `task-612`.
- Do not redefine the embedding-contract policy; that is covered by `task-611`.
- Do not change the Dropbox backend selection; that is the delivered output of `task-605`.
- Do not document the full `sync changed` incremental workflow; that is a separate future task.

## Relevant Dependencies

- Completed upstream work:
  - `task-605` selected and validated Dropbox shared-folder storage in `Daedalus_KB` as the v1 private shared snapshot backend.
  - `task-608` enforced embedding compatibility in snapshot import and `sync changed`.
  - `task-611` defined the canonical embedding-contract policy and republish rule.
- Files reviewed:
  - `.agent/workflows/agentic-kb.md` (lines 60-67, 74-79, 144-204)
  - `.agent/plans/agentic/task-plans/task-605-impl-review.md`
  - `.agent/plans/agentic/task-plans/task-611-impl-review.md`

## Files Expected To Change

- `.agent/plans/agentic/task-plans/task-606.md` - this canonical task plan
- `.agent/plans/agentic/task-plans/task-606-plan-review.md` - append-only planning review transcript
- No changes to `.agent/workflows/agentic-kb.md` are required because existing coverage is sufficient

## Implementation Approach

- This is an autonomous documentation confirmation task. The existing workflow doc already contains comprehensive coverage of the two-developer publish/upload/download/import workflow.
- Verify the existing coverage is internally consistent by re-reading lines 144-204 and 60-79 of `.agent/workflows/agentic-kb.md`.
- Confirm that the existing workflow wording correctly reflects the Dropbox backend (from task-605) and the embedding-contract tuple (from task-611).
- If no gaps are found, record the finding and update this plan accordingly.
- If gaps are found, make the smallest truthful gap-fill edits needed to the workflow doc.

## Acceptance Criteria

- The canonical plan records that no additional workflow doc edits are required because lines 144-204 of `.agent/workflows/agentic-kb.md` already document the complete two-developer local GPU-backed publication, upload, download, and import workflow.
- The documented workflow is internally consistent with the task-605 Dropbox backend decision and the task-611 embedding-contract policy.
- The plan confirms that `yarn agentic:kb:publish` and `yarn agentic:kb:fetch` commands are documented with their constraints.
- The plan confirms that the consumption path (fetch + import + validate) is documented with expected validation shape.
- The plan confirms that snapshot publication policy (Dropbox as v1 backend, `Daedalus_KB` folder, artifact pair contract, retention, integrity, outage recovery) is documented.

## Verification Plan

- Planning verification:
  - Re-read `.agent/workflows/agentic-kb.md` lines 144-204 and 60-79 to confirm existing coverage.
  - Confirm the workflow doc correctly names Dropbox and `Daedalus_KB` as the backend.
  - Confirm the workflow doc uses correct embedding-contract field names (`contract_id`, `embedding_model`, `embedding_dimension`) from task-611.
  - Confirm the consumption path and expected validation shape are documented.
- If verification finds gaps, make the smallest truthful gap-fill edits needed.
- If verification finds no gaps, update this plan to record the confirmed sufficiency.

## Risks / Open Questions

- No risks identified. The workflow doc already contains comprehensive coverage of the task-606 scope.
- No open questions remain. The Dropbox backend is selected and validated (task-605), embedding compatibility is enforced (task-608), and the embedding-contract policy is defined (task-611).

## Required Docs / Tracking / Research Updates

- Create and maintain this canonical task plan at `.agent/plans/agentic/task-plans/task-606.md`.
- Append all planning-review decisions to `.agent/plans/agentic/task-plans/task-606-plan-review.md`.
- No other doc updates are required for task-606 scope.

## Current Outcome

- Task completed. Implementation verification confirms no additional workflow doc edits are required because lines 144-204 of `.agent/workflows/agentic-kb.md` already document the complete two-developer local GPU-backed publication, upload, download, and import workflow using the Dropbox backend (`Daedalus_KB`) from task-605 and the embedding-contract tuple (`contract_id`, `embedding_model`, `embedding_dimension`) from task-611. The documented workflow is internally consistent with both upstream task decisions and covers all task-606 scope items: publish/fetch helper constraints, consumption path, expected validation shape, and snapshot publication policy.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-606-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-606-impl-review.md` (to be created when implementation begins)
