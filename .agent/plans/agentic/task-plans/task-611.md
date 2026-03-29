# Task Plan: task-611 Define canonical embedding contract and republish policy

- Task ID: `task-611`
- Title: `Define canonical embedding contract and republish policy`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-601`, `task-602`, and `task-608` already established the manifest contract plus real mismatch enforcement in `snapshot import`, `status`, and `sync changed`, but the operator-facing workflow still needs one canonical v1 policy that explains how shared-baseline users discover and apply that contract.
- `.agent/workflows/agentic-kb.md` already contains a `Canonical Embedding Contract` section, so this task is not greenfield documentation. The remaining gap is to reconcile that section with the tracker requirement: one single supported v1 contract, explicit discovery guidance, explicit republish triggers from `develop`, and explicit mismatch signals across import, status, and sync flows.
- This task is now the smallest remaining Phase 6 documentation step that unblocks later team-sharing and rollout work such as `task-606`, `task-904`, and `task-906` without dragging in private shared-storage selection or helper-command design.

## Scope

- Update the canonical KB workflow doc so it names the single supported v1 shared-baseline embedding contract in operator-facing terms.
- Document the truthful v1 discovery path operators use to understand the canonical contract policy and evaluate a specific shared-baseline artifact before importing or extending it.
- Document when canonical snapshots must be republished from `develop` after intentional embedding-contract changes.
- Make the already-shipped mismatch handling explicit and consistent across `snapshot import`, `status`, and `sync changed`.
- Keep the workflow truthful about what is already implemented versus what remains later rollout work.

## Non-Goals

- Do not change snapshot, status, or sync implementation behavior; `task-608` already shipped the enforcement behavior this task documents.
- Do not select or document the private shared-storage backend; that remains `task-605` and follow-on publication workflow work.
- Do not add helper publish/fetch commands, publication automation, or ownership/fallback SOP material beyond dependency notes.
- Do not broaden this task into Project token guidance, manual full Project refresh docs, or other unrelated workflow cleanup.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-601` defined the manifest contract shape.
  - `task-602` implemented snapshot export/import sidecar behavior.
  - `task-608` enforced exact embedding-contract compatibility in import, `status`, and `sync changed`.
  - `task-803` updated the main KB workflow doc that this task will revise.
- Direct downstream tasks clarified by this work:
  - `task-606` local baseline publish/download workflow docs.
  - `task-904` canonical baseline ownership and fallback SOP.
  - `task-906` team-ready publication and handoff validation.
- Required planning context reviewed:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md`
  - `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md`

## Files Expected To Change

- `.agent/workflows/agentic-kb.md` - tighten the existing canonical embedding contract and snapshot publication sections so they document discovery, republish triggers from `develop`, and the precise operator-visible mismatch behavior.
- `.agent/plans/agentic/task-plans/task-611.md` - keep this canonical plan current through implementation and final outcome.
- `.agent/plans/agentic/research/task-611-canonical-embedding-contract-policy.md` - capture the durable repo-truth constraints behind the final discovery wording.
- `.agent/plans/agentic/task-plans/task-611-plan-review.md` - append-only planning review transcript.

## Implementation Approach

- Treat `.agent/workflows/agentic-kb.md` as the only product-doc target for this task unless implementation reveals a narrow truthfulness gap elsewhere.
- Preserve the existing `Canonical Embedding Contract` section, but rewrite it into an explicit v1 policy instead of a partial summary.
- State that the shared team baseline supports one canonical contract at a time and that the contract is the exact tuple already enforced by the tooling: `contract_id`, `embedding_model`, and `embedding_dimension`.
- Make operator discovery concrete without inventing new storage, commands, or registries:
  - the workflow is the current operator-facing policy source of truth for the v1 canonical-contract shape and republish rule
  - implementation may point to a checked-in operator-visible tuple source only if it first verifies the repo already exposes all three enforced fields there today
  - otherwise the workflow should direct operators to the published snapshot manifest for a specific shared artifact and to `status --json` / `embedding_compatibility` for post-import compatibility confirmation
  - operators should not treat ad hoc local `OLLAMA_EMBED_MODEL` overrides as shared-baseline contract changes or as a canonical-contract registry
- Document explicit republish triggers from `develop`:
  - any intentional change to the canonical contract tuple requires a fresh canonical snapshot rebuild and republish from `develop` before further team handoff
  - snapshots built before that change are no longer valid shared baselines for import-then-sync continuation under the new contract
  - ordinary repo/content refreshes do not redefine the contract by themselves
- Make mismatch handling wording concrete and aligned with shipped behavior:
  - `snapshot import` fails before restore for incompatible or legacy manifests
  - `status` keeps top-level readiness separate and reports compatibility in `embedding_compatibility`, including legacy or unavailable metadata states
  - `sync changed` refuses to extend an imported baseline whose latest imported manifest is incompatible, legacy, or malformed
- Keep the workflow honest about later work boundaries: artifact discovery, storage bootstrap, retention, outage recovery, helper commands, and ownership/fallback policy remain future tasks.
- Keep scope minimal. Prefer editing the existing sections and nearby team-sharing/publication text rather than adding new workflow subsections unless needed for clarity.

## Acceptance Criteria

- `.agent/workflows/agentic-kb.md` clearly states that v1 supports one canonical shared-baseline embedding contract at a time.
- The workflow defines that canonical contract using the exact enforced runtime fields: `contract_id`, `embedding_model`, and `embedding_dimension`.
- The workflow tells operators the truthful v1 discovery path before import or sync without implying unsupported helper tooling, storage assumptions, or an operator-visible canonical tuple registry that does not exist.
- The workflow states when canonical snapshots must be republished from `develop`: whenever the canonical embedding contract changes intentionally.
- The workflow distinguishes contract changes from ordinary content refreshes so the republish rule is not ambiguous.
- The workflow documents the already-shipped mismatch handling across `snapshot import`, `status`, and `sync changed` accurately and consistently with `task-608` research.
- The workflow does not widen into private shared-storage selection, helper commands, or ownership/fallback SOP decisions that belong to later tasks.

## Verification Plan

- Review the edited workflow against the tracker description and PRD requirements for canonical contract, discovery, republish policy, and mismatch surfacing.
- Cross-check the final wording against the durable behavior recorded in `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md` so the doc does not overstate or misstate implementation.
- Confirm the final workflow still matches the current shipped command surface and status semantics already documented elsewhere in the same file.
- No code execution is required for planning. Implementation verification should be documentation review only unless a truthfulness check reveals a shipped-behavior mismatch that needs broader repo updates.

## Risks / Open Questions

- The workflow already states the shared-baseline policy, but the exact discovery wording must stay truthful to current repo reality and not imply operators can derive one checked-in canonical tuple unless implementation first verifies such an operator-visible source already exists.
- The task must document republish policy precisely enough to be actionable without drifting into `task-904` ownership/fallback SOP content.
- The retired `task-603` research captured an obsolete GitHub Actions path; this plan must use it only as historical context and must not reintroduce Actions as a supported publication channel.

## Required Docs / Tracking / Research Updates

- Update `.agent/workflows/agentic-kb.md` during implementation.
- Keep this canonical task plan current with implementation notes, final verification notes, and final outcome when the task is completed.
- Record the durable discovery-policy research in `.agent/plans/agentic/research/task-611-canonical-embedding-contract-policy.md`.
- Append all planning-review decisions to `.agent/plans/agentic/task-plans/task-611-plan-review.md`.
- Do not create an implementation review log during planning.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` later only when implementation is complete and task metadata changes are being recorded.

## Current Outcome

- Implementation completed with a documentation-only change set. `.agent/workflows/agentic-kb.md` now states the canonical v1 embedding contract using the exact enforced tuple, documents truthful operator discovery via published manifests plus `status --json`, defines republish triggers from `develop`, and aligns mismatch handling wording with shipped `snapshot import`, `status`, and `sync changed` behavior.

## Final Outcome

- Completed as a narrow workflow-documentation update plus a task-611 research note and implementation review log. The shipped workflow now documents the approved canonical-contract policy and republish rule without adding runtime behavior, storage-backend selection, helper commands, or rollout-scope changes.

## Verification Outcome

- Documentation review completed against the approved plan, `.agent/workflows/agentic-kb.md`, `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md`, `agentic/config/snapshot-manifest.schema.json`, and `agentic/config/snapshot-manifest.example.json`.
- Verified that the final workflow wording matches shipped behavior for `snapshot import`, `status --json`, and `sync changed`, and stays truthful about v1 discovery by relying on published manifests rather than claiming a checked-in canonical tuple registry.
- Durable task-611 research note: `.agent/plans/agentic/research/task-611-canonical-embedding-contract-policy.md`

## Review Outcome

- Planning review log: `.agent/plans/agentic/task-plans/task-611-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-611-impl-review.md`

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-611-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-611-impl-review.md`

## Required User Inputs

- None.

## Required Manual Test Steps

- None before implementation.

## What Evidence Is Needed Back From The User

- None.

## Whether Implementation Can Proceed Before That User Interaction

- Yes. The task is autonomous and can proceed without user interaction after planning critique.
