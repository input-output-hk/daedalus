# Task Plan: task-705 Add local publish and fetch helper commands

- Task ID: `task-705`
- Title: `Add local publish and fetch helper commands`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `interactive_validation`
- Required User Inputs: `A real locally accessible Dropbox-synced path for the shared folder Daedalus_KB is required for authoritative manual validation; implementation can be designed before that path is supplied.`
- Required Manual Test Steps: `Configure the helper-command shared-folder path, run the publish helper from a trusted develop checkout, confirm the sibling snapshot pair is copied into the local Dropbox-synced folder, run the fetch helper back into agentic/snapshots/, import into a disposable KB, then verify status --json and the deterministic BM25 proof query.`
- Evidence Needed Back From User: `Command output or equivalent confirmation showing the publish helper copied one .dump/.manifest.json pair to the configured Dropbox-synced folder, the fetch helper restored the same pair locally, and post-import status/search validation succeeded.`
- Implementation Can Proceed Before User Interaction: `Yes`

## Why This Task Was Chosen Now

- `task-605` already selected Dropbox shared-folder storage and documented the truthful minimum handoff contract, but both `.agent/workflows/agentic-kb.md` and `agentic/README.md` still say publish/fetch helpers are later rollout work.
- The repo already has the core runtime pieces this task should wrap: helper-style `package.json` scripts from `task-105`, `sync all` from `task-701`, snapshot export/import from `task-602`, disposable-target safety from `task-612`, and embedding-compatibility enforcement from `task-608` and `task-611`.
- This is now the smallest remaining Phase 7 step that can reduce error-prone manual copy/paste work without inventing a new Dropbox integration or changing the documented shared-storage boundary.

## Current Build State

- Planning review is approved in `.agent/plans/agentic/task-plans/task-705-plan-review.md`.
- `package.json` now exposes local KB helper entrypoints for publish and fetch, backed by small repo-level shell wrappers in `scripts/`.
- `.agent/workflows/agentic-kb.md` and `agentic/README.md` now document the shipped local-path helper contract, including the required `AGENTIC_KB_SHARED_DIR` configuration and the explicit fetch-basename boundary.
- Dropbox still remains a shared-folder contract only: there is no repo-owned Dropbox API integration, account bootstrap helper, remote listing flow, or richer artifact registry.
- Manual validation is complete against a real locally accessible Dropbox-synced `Daedalus_KB` path. The validated basename was `agentic-kb-20260330`, publish copied both sibling files into the shared folder, fetch restored both files locally, disposable import succeeded, and `status --json` reported `ok: true` with compatible embedding state.

## Scope

- Add the smallest truthful local helper surface for canonical baseline publication and consumption.
- Wrap the existing Compose and `agentic-kb` commands rather than introducing new snapshot or storage behavior.
- Cover the local filesystem handoff boundary only: a developer machine with a locally accessible Dropbox-synced `Daedalus_KB` folder.
- Keep the `.dump` and sibling `.manifest.json` pair together during publish and fetch operations.
- Update the operator docs so they describe the helper-command contract, its required local path configuration, and its validation boundary accurately.

## Non-Goals

- Do not add Dropbox API usage, OAuth/token setup, account provisioning, or any repo-owned remote storage integration.
- Do not redefine artifact discovery into a richer canonical registry or hidden automation contract that task-605 did not approve.
- Do not change snapshot format, import safety rules, embedding-contract policy, or `sync changed` behavior.
- Do not absorb the full publication workflow docs from `task-606` or the automated publication smoke coverage from `task-706`.
- Do not hide the required external Dropbox-path/manual validation checkpoint inside an autonomous helper loop.

## Relevant Dependencies

- Completed upstream work:
- `task-105` added the existing `package.json` helper-wrapper pattern.
- `task-602` implemented snapshot export/import for the portable sibling pair.
- `task-605` selected Dropbox shared-folder storage and documented the truthful minimum backend contract for `Daedalus_KB`.
- `task-608` enforced embedding-contract compatibility for import, status, and post-import sync safety.
- `task-611` documented the canonical embedding-contract discovery and republish policy.
- `task-612` enforced disposable-target import safety.
- `task-701` shipped `sync all`, which the publish helper can wrap rather than reimplement.
- Current repo context reviewed:
- `.agent/plans/agentic/research/task-605-dropbox-shared-backend.md`
- `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md`
- `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md`
- `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md`
- `.agent/plans/agentic/research/task-611-canonical-embedding-contract-policy.md`
- `.agent/plans/agentic/knowledge-base-platform-prd.md`
- `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- `.agent/workflows/agentic-kb.md`
- `agentic/README.md`
- `package.json`

## Files Expected To Change

- `package.json` - add the user-facing helper command entrypoints.
- `scripts/agentic-kb-*.sh` or one similarly small helper-script location - only if `package.json` inline commands would become too fragile for path validation and sibling-pair copying.
- `.agent/workflows/agentic-kb.md` - replace the current "later rollout work" gap with the shipped helper-command contract while keeping the external Dropbox boundary explicit.
- `agentic/README.md` - document the same helper entrypoints and required local path configuration at the developer entrypoint level.
- `.agent/plans/agentic/task-plans/task-705.md` - canonical task plan and final task record.
- `.agent/plans/agentic/task-plans/task-705-plan-review.md` - append-only planning review log.
- `.agent/plans/agentic/task-plans/task-705-impl-review.md` - append-only implementation review log created when implementation starts.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update `task-705` status metadata when implementation is complete.

## Implementation Approach

- Keep the task classification `interactive_validation`. The helper commands themselves can be implemented autonomously, but authoritative acceptance still crosses an external boundary: a real locally accessible Dropbox-synced `Daedalus_KB` path and manual validation that the helper commands work against it.
- Prefer a very small wrapper contract over new abstractions:
- one publish helper that runs the existing local baseline build/export path and then copies the resulting sibling pair into the configured local Dropbox-synced folder
- one fetch helper that copies a chosen shared sibling pair from that local Dropbox-synced folder back into `agentic/snapshots/`
- optionally one import convenience wrapper only if it materially reduces repetition without obscuring the existing `snapshot import ... --yes` safety acknowledgment
- Require the shared-folder location through an explicit local path input, preferably one env var or one required helper argument. Fail clearly when the path is missing, does not exist, is not the expected directory, or is not readable/writable enough for the requested operation.
- Keep Dropbox semantics narrow and local-file based. The helpers should assume Dropbox desktop sync or an equivalent local access path already exists; they should not attempt remote authentication, remote listing APIs, or backend discovery beyond local filesystem checks.
- Preserve the sibling-pair contract. Publish and fetch helpers must operate on both the `.dump` and its sibling `.manifest.json` together and must fail if only one side of the pair is present.
- Preserve the current artifact-selection truth boundary from task-605. If the fetch helper supports zero-argument "latest" behavior, it must document exactly how that local choice is made and must not imply a richer canonical registry than the repo currently owns. A safer minimal implementation is to require an explicit basename or manifest/dump path within the synced folder.
- Reuse the existing import validation rather than duplicating it. After fetch, rely on `snapshot import` plus `status --json` and the deterministic BM25 proof for correctness.
- Keep docs honest about rollout boundaries: helper commands reduce manual local command chaining and local file copying only. They do not remove the need for a writable Dropbox-shared folder, manual validation, or later broader rollout tasks.

## Acceptance Criteria

- `package.json` exposes developer-friendly local helper commands for publish and fetch work around the existing KB CLI/Compose contract.
- The helper implementation uses a locally accessible Dropbox-synced folder path and does not add Dropbox API integration or account/bootstrap automation.
- The publish path wraps the existing baseline build/export flow and copies one complete `.dump` plus sibling `.manifest.json` pair into the configured shared folder.
- The fetch path copies one complete sibling pair from the configured shared folder back into `agentic/snapshots/` and does not allow silent single-file drift.
- The implementation fails clearly when the configured shared-folder path is absent, inaccessible, or missing one side of the expected snapshot pair.
- The workflow docs and `agentic/README.md` no longer describe helper commands as future work and instead document the shipped helper contract truthfully.
- The docs remain explicit that Dropbox setup itself is still external/manual and that authoritative task acceptance requires manual validation against a real synced `Daedalus_KB` path.
- The final workflow remains consistent with the existing embedding-contract, disposable-import, and deterministic post-import validation contracts.

## Verification Plan

- Static verification during implementation:
- review `package.json` and any helper script for path validation, sibling-pair handling, and minimal command chaining around the existing Compose commands
- re-read `.agent/workflows/agentic-kb.md` and `agentic/README.md` so they match the final helper entrypoints exactly
- Authoritative manual validation before final task completion:
- configure the helper-command shared-folder path to a real locally accessible Dropbox-synced `Daedalus_KB` directory
- run the publish helper from a trusted `develop` checkout and confirm the helper exports and copies a complete sibling pair into that directory
- run the fetch helper to restore that same pair into local `agentic/snapshots/`
- import the fetched snapshot into a disposable KB with `snapshot import ... --yes`
- run `status --json` and the deterministic BM25 documents-only proof query
- record the exact basename used and whether the imported manifest reported compatible embedding-contract metadata
- Keep broader multi-developer handoff proof separate. This task only needs to validate the local helper commands against the real shared-folder boundary; `task-706`, `task-903`, `task-905`, and `task-906` still own the larger rollout workflow.

## Risks / Open Questions

- The main critique point should be artifact selection truthfulness. The current repo contract still treats artifact discovery as manual, so the critiquer should stress-test whether the plan's fetch-helper boundary stays honest and does not invent an unsupported "latest" registry.
- The helper interface needs to keep path configuration minimal without guessing the user's Dropbox mount layout. A single explicit path input is likely enough, but the critiquer should verify that the plan does not accidentally depend on one machine-specific directory convention.
- Local validation depends on an external Dropbox-synced folder path that repo state cannot prove. The task should not be marked complete until that manual boundary is exercised and evidence is recorded.
- If helper commands grow beyond simple command wrapping and file copying, the task may start bleeding into `task-606` workflow design or `task-706` smoke coverage and should be pushed back.

## Required Docs / Tracking / Research Updates

- Maintain this canonical task plan at `.agent/plans/agentic/task-plans/task-705.md`.
- Append planning-review history to `.agent/plans/agentic/task-plans/task-705-plan-review.md`.
- Create `.agent/plans/agentic/task-plans/task-705-impl-review.md` when implementation begins.
- Update `.agent/workflows/agentic-kb.md` and `agentic/README.md` when the helper-command contract lands.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when implementation completes.
- Record durable helper-command findings in `.agent/plans/agentic/research/task-705-local-publish-fetch-helpers.md`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-705-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-705-impl-review.md`

## Current Outcome

- Completed.
- The repo now ships publish/fetch wrapper commands that keep the `.dump` plus sibling `.manifest.json` pair together and fail clearly when the configured shared path or one side of the pair is missing.
- Documentation now reflects the shipped helper contract truthfully while keeping the external Dropbox path and explicit fetch-basename boundary visible.
- Manual validation succeeded against a real Dropbox-synced `Daedalus_KB` path using basename `agentic-kb-20260330`.

## Final Outcome

- Completed after manual validation on 2026-03-30.
- Real-path validation evidence:
- `AGENTIC_KB_SHARED_DIR` was set to a real Dropbox-synced `Daedalus_KB` directory and `yarn agentic:kb:publish -- agentic-kb-20260330` completed successfully.
- The shared folder contained both `agentic-kb-20260330.dump` and `agentic-kb-20260330.manifest.json` after publish.
- `yarn agentic:kb:fetch -- agentic-kb-20260330` restored both sibling files into local `agentic/snapshots/`.
- A fresh-stack `snapshot import agentic/snapshots/agentic-kb-20260330.dump --yes` succeeded.
- `status --json` reported `"ok": true` and `embedding_compatibility.state = "compatible"`.
- The BM25 validation query returned hits including `.agent/workflows/agentic-kb.md`, but the first hit was `.agent/plans/agentic/task-plans/task-605.md`. Per implementation review approval, this is recorded as a non-blocking residual validation-contract issue outside task-705 helper scope.

## Review Outcome

- Planning review is approved in `.agent/plans/agentic/task-plans/task-705-plan-review.md`.
- Implementation review log `.agent/plans/agentic/task-plans/task-705-impl-review.md` records the shipped helper-command implementation, the user handoff, and the approved code review after manual validation evidence was supplied.

## Planning Status Rationale

- Planning status is `approved` because the canonical plan was reviewed and approved in `.agent/plans/agentic/task-plans/task-705-plan-review.md`.
- Build status is `completed` because the repo changes were implemented, the required manual validation against a real Dropbox-synced `Daedalus_KB` path was completed and recorded, and the implementation review concluded with `Decision: approved`.
