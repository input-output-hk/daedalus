# Task task-100: Add Shared Partial Sync Types And Status Contracts

## Task

- Task ID: `task-100`
- Title: `Add shared partial sync types and status contracts`

## Why This Task Now

- `task-002` locked the staged-only cutover rule and `task-004` locked boundary-dependent failure eligibility.
- `task-100` is the first implementation-facing task in phase 1 because later IPC, coordinator, service, and renderer work need one shared cross-process contract before they can add partial-sync behavior safely.

## Interaction Mode

- `autonomous`

## Scope

- Add shared TypeScript contracts for diagnostics-launched Mithril partial sync status, status updates, error metadata, and backend-owned failure actions.
- Resolve the file-layout decision for these contracts in a way that keeps bootstrap-only and partial-sync-only invariants easy to audit.
- Reuse existing generic Mithril shapes only where they are genuinely cross-flow concepts.
- Keep the result ready for `task-101` IPC channel definitions and later main/renderer/store work.

## Non-Goals

- Adding partial sync IPC channels in `source/common/ipc/api.ts`; that belongs to `task-101`.
- Implementing coordinator, service, startup recovery, or renderer behavior.
- Refactoring unrelated chain-storage types out of `mithril-bootstrap.types.ts`.
- Unifying bootstrap and partial sync into one generic Mithril state machine.
- Reopening the staged-only restore rule, range derivation, or failure-boundary rules already locked by `task-002`, `task-003`, and `task-004`.

## Relevant Dependencies

- Required completed dependency:
  - `task-002`
- Cross-checked completed design inputs that informed this plan:
  - `task-004`
- This task provides the contract base for:
  - `task-101`
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-201`
  - `task-204`
  - `task-300`
  - `task-303`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `understand`

## Live Repo Findings

- `source/common/types/mithril-bootstrap.types.ts` is the existing shared Mithril contract file, but it already mixes bootstrap-specific states with unrelated shared chain-storage contracts.
- That file currently exports bootstrap-only state names like `decision`, `unpacking`, and `starting-node`, plus bootstrap error stages like `download`, `verify`, `convert`, and `node-start`.
- `source/common/ipc/api.ts` currently consumes `MithrilBootstrapStatusUpdate` directly for bootstrap IPC status transport, so overloading that type for partial sync would immediately blur separate flows before `task-101` even adds dedicated channels.
- `source/main/mithril/mithrilErrors.ts` and `source/main/mithril/MithrilBootstrapService.ts` rely on bootstrap-specific error-stage semantics, which are narrower than the boundary-driven failure-action contract partial sync needs.
- `source/renderer/app/stores/MithrilBootstrapStore.ts` assumes bootstrap decision-cycle and working-status groupings that do not match partial sync states like `confirming`, `stopping-node`, `installing`, and `wipe-and-full-sync` recovery eligibility.
- `source/main/utils/chainStorageCoordinator.ts`, `chainStorageManager.ts`, and `chainStorageManagerShared.ts` import `ChainStorageConfig` and `ChainStorageValidation` from `mithril-bootstrap.types.ts`, so task-100 should not opportunistically relocate those chain-storage contracts while introducing partial sync types.
- Historical Mithril notes already treated shared contract growth carefully: prior bootstrap work added fields like `filesDownloaded/filesTotal` and `error.stage` without collapsing bootstrap completion and node handoff into one state, which is the same separation pressure task-100 now faces for bootstrap versus partial sync.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-100.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-100-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-100-impl-review.md`
- `source/common/types/mithril-partial-sync.types.ts`
- `source/common/types/mithril-bootstrap.types.ts`

## Preferred Implementation Approach

1. Add a dedicated shared file for partial-sync-specific contracts.
   - Create `source/common/types/mithril-partial-sync.types.ts`.
   - Keep partial sync statuses, status updates, failure actions, and error/action-eligibility contracts there.
   - Do not broaden `MithrilBootstrapStatus` or `MithrilBootstrapStatusUpdate` to cover diagnostics-launched partial sync.

2. Keep reuse narrow and explicit.
   - Reuse `MithrilProgressItem` and `MithrilProgressItemState` only if implementation truly needs the same progress-step shape in both flows.
   - If that reuse is needed, either leave the shared progress types in `mithril-bootstrap.types.ts` for now or extract only those generic progress-item types into the new partial-sync file and import them back into bootstrap.
   - Do not move `ChainStorageConfig` or `ChainStorageValidation` in this task unless compilation pressure makes a tiny follow-up extraction unavoidable.

3. Encode only backend-owned partial-sync lifecycle states in the shared contract.
    - Preferred `MithrilPartialSyncStatus` values:
      - `idle`
      - `stopping-node`
      - `preparing`
      - `downloading`
     - `verifying`
     - `converting`
     - `installing`
     - `finalizing`
     - `starting-node`
     - `completed`
     - `failed`
     - `cancelled`
    - Keep confirmation modal state renderer-local because background work starts only after explicit user confirmation.
    - Keep these distinct from bootstrap-only `decision` and `unpacking` so later code cannot accidentally treat the flows as one lifecycle.

4. Make backend-owned safety decisions visible in the contract layer.
    - Add `MithrilPartialSyncFailureAction` with:
      - `retry`
      - `restart-normal`
      - `wipe-and-full-sync`
    - Add an `allowedRecoveryActions` field on every `MithrilPartialSyncStatusUpdate`.
    - Require emitters to send `allowedRecoveryActions: []` whenever no recovery action is currently safe so merge-based stores cannot retain stale actions across later updates.
    - The backend should remain the source of truth for which actions are enabled after failure or interruption.

5. Keep error contracts specific enough for later recovery and support flows.
   - Add a partial-sync error shape that can carry `message`, optional `code`, optional `logPath`, and a partial-sync-specific stage or phase indicator.
   - Prefer stage names aligned with the real partial-sync lifecycle instead of reusing bootstrap's narrower `download|verify|convert|node-start` union unchanged.
   - Ensure the status update can carry `error`, `elapsedSeconds`, transfer counts, ancillary byte counts, and progress items without renderer-side invention.

6. Add only the minimal shared helper predicates that downstream tasks are likely to need immediately.
    - Add `isMithrilPartialSyncTerminalStatus`.
    - Add `isMithrilPartialSyncBlockingNodeStart` so main-process lifecycle gating can reuse one shared grouping instead of re-deriving it ad hoc.
    - Add `isMithrilPartialSyncRestoreCompleteStatus` if the partial-sync UI reuses bootstrap-style progress or terminal rendering that depends on a shared completion grouping.
    - Do not clone bootstrap helpers beyond those concrete groupings.

## Preferred Contract Shape

- `MithrilPartialSyncStatus`
- `MithrilPartialSyncFailureAction`
- `MithrilPartialSyncErrorStage` or equivalent phase union tied to partial-sync lifecycle
- `MithrilPartialSyncError`
- `MithrilPartialSyncStatusUpdate`
  - `status`
  - `allowedRecoveryActions`
  - `error`
  - `filesDownloaded`
  - `filesTotal`
  - `ancillaryBytesDownloaded`
  - `ancillaryBytesTotal`
  - `progressItems`
  - `elapsedSeconds`
  - `logPath` if not nested entirely under `error`
  - backend-owned allowed recovery actions, always present and empty when none are allowed

## Acceptance Criteria

- Shared types compile cleanly.
- Partial sync contracts live in a dedicated shared type file instead of broadening bootstrap-only unions.
- Bootstrap and partial sync invariants remain understandable from the type layer.
- The shared status/update contract is sufficient for confirmation, progress, cancellation, failure, and boundary-dependent recovery-action rendering.
- The contract exposes backend-owned allowed recovery actions so the renderer does not infer safety heuristically.
- The task does not opportunistically refactor unrelated chain-storage contracts.

## Verification Plan

- Run `yarn compile`.
- Run targeted Jest verification if the task introduces or updates shared-type helpers with unit coverage; otherwise document why compile-only verification was sufficient for this contract-only change.
- Verify the new contracts align with the locked PRD states and failure-action vocabulary.
- Verify `task-101` can define dedicated partial-sync IPC channels without overloading `MithrilBootstrapStatusUpdate`.
- Verify no existing bootstrap import is forced to accept partial-sync-only states.
- Verify chain-storage imports continue to compile without accidental relocation.

## Risks And Open Questions

- The exact partial-sync error-stage union still needs a small implementation decision: either lifecycle-stage names only or a separate preflight/install/recovery vocabulary. The contract should stay narrow and match actual emitted states.
- If both bootstrap and partial sync need the same progress-item shape, a tiny shared extraction may be justified, but this task should avoid a broader "generic Mithril types" refactor.
- The PRD still contains the open question about file placement; this plan resolves it in favor of a dedicated partial-sync file because the existing bootstrap file already carries mixed concerns.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-100` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` once review is approved.
- Add a durable research note recording the dedicated-file decision, required `allowedRecoveryActions` clearing semantics, and the `completed` vs `starting-node` node-start-gating distinction.

## Review Logs

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-100-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-100-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Completed 2026-05-19 after planning approval, two implementation iterations, and approved code review.
- Added `source/common/types/mithril-partial-sync.types.ts` as the dedicated shared partial-sync contract file.
- Kept bootstrap-specific unions in `source/common/types/mithril-bootstrap.types.ts` unchanged to avoid blurring startup bootstrap and diagnostics partial-sync invariants.
- Defined `MithrilPartialSyncStatusUpdate.allowedRecoveryActions` as a required field on every update so merge-based stores can clear stale recovery affordances with `[]` when nothing is allowed.
- Added the minimal shared helpers needed for downstream lifecycle grouping: terminal status, restore-complete status, and node-start-blocking status.
- Corrected the node-start-blocking helper during implementation review so `completed` is treated as terminal success rather than an in-progress blocking state.

## Verification Outcome

- `yarn compile` was attempted but is currently blocked by unrelated pre-existing Trezor typing failures in `source/main/ipc/getHardwareWalletChannel.ts` and `source/main/trezor/manifest.ts`.
- Targeted verification passed with `./node_modules/.bin/tsc --noEmit --pretty false --skipLibCheck --target es2020 --module commonjs source/common/types/mithril-bootstrap.types.ts source/common/types/mithril-partial-sync.types.ts`.
- No user interaction or manual validation was required for this contract-only task.

## Self-Review

- Scope-creep check: kept IPC, service, coordinator, renderer, and startup recovery implementation out of task-100.
- Workflow freshness check: the plan stays consistent with the current IPC and test workflows and with the locked PRD decisions from tasks `002` through `004`.
- Manifests/tests/docs check: no unrelated manifest or doc churn was added; only the canonical task doc was created.
- Consistency check: the proposed dedicated partial-sync file matches the live repo seam that bootstrap contracts are already specific and widely consumed, while chain-storage contracts still remain untouched.
