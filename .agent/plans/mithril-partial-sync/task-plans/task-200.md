# Task task-200: Create MithrilPartialSyncService Skeleton And Latest-Snapshot Resolution

## Task

- Task ID: `task-200`
- Title: `Create MithrilPartialSyncService skeleton and latest-snapshot resolution`

## Why This Task Now

- `task-002`, `task-004`, `task-102`, and `task-103` are complete, so the restore strategy, failure boundaries, coordinator seam, and node-lifecycle suppression seam are already locked.
- `task-200` is the next unblocked task on the remaining critical path.
- The repo now has dedicated partial-sync shared types and IPC channels, but no real partial-sync service behind them, so later backend tasks would otherwise have nowhere truthful to add latest-snapshot lookup, status emission, or process ownership.

## Interaction Mode

- `autonomous`
- Required user/manual checkpoints: none for planning or implementation of this task.

## Scope

- Add a distinct `MithrilPartialSyncService` backend entrypoint under `source/main/mithril/` rather than widening `MithrilBootstrapService` with mixed invariants.
- Give that service ownership of:
  - partial-sync status emission
  - active child-process tracking
  - partial-sync log handling
  - latest-snapshot metadata resolution
  - high-level lifecycle orchestration skeleton only
- Wire the service into the existing coordinator partial-sync handler seam from `task-102`.
- Replace the current start and cancel IPC stubs with real delegation to the coordinator and service.
- Keep restart-normal and wipe-and-full-sync IPC actions explicitly unimplemented until `task-204`.

## Non-Goals

- Implementing immutable range derivation, local immutable inspection, or latest-drift retry logic beyond the minimal latest-snapshot lookup needed here; that belongs to `task-201`.
- Implementing the real partial download, verification, LSM conversion, staged cutover, or live install logic; that belongs to `task-202` and `task-203`.
- Implementing backend recovery branching for retry, restart-normal, or wipe-and-full-sync; that belongs to `task-204`.
- Adding renderer store, diagnostics UI, confirmation modal, or overlay work; that belongs to phase 3.
- Generalizing bootstrap and partial-sync services into one shared runtime abstraction unless the live code forces one very small helper extraction.

## Relevant Dependencies

- Required completed dependencies:
  - `task-002`
  - `task-004`
  - `task-102`
  - `task-103`
- Already-completed supporting prerequisites used directly by this plan:
  - `task-100`
  - `task-101`
- This task directly unblocks:
  - `task-201`
  - `task-202`
  - `task-204`
  - backend portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/05-task-102-coordinator-seam-notes.md`
- `.agent/plans/mithril-partial-sync/research/06-task-103-node-lifecycle-seam-notes.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `understand` skill guidance was consulted, then all planning claims were verified against live repo files before inclusion.

## Live Repo Findings Verified For Planning

- `source/main/utils/chainStorageCoordinator.ts` already provides the correct narrow handoff for this task: `setPartialSyncHandlers(...)`, `startPartialSync(...)`, `cancelPartialSync()`, and `PartialSyncPreflightContext` containing `layoutResult` and `mithrilWorkDir`.
- `source/main/ipc/mithrilPartialSyncChannel.ts` currently has only cached status plumbing plus explicit "not implemented" action handlers, so task-200 should attach the real service here instead of widening unrelated startup code.
- `source/main/cardano/setup.ts` and `source/main/index.ts` already consume the coordinator-owned partial-sync activity boundary from `task-103`, so task-200 does not need a second active-state source.
- `source/main/mithril/MithrilBootstrapService.ts` already proves the reusable patterns worth copying selectively: status emitter, child-process tracking, Mithril CLI wrappers, JSON parsing, and backend-owned snapshot metadata helper structure.
- `source/main/mithril/mithrilCommandRunner.ts` currently hardcodes `mithril-bootstrap.log` in `openLogStream()`, so task-200 needs a minimal log-path seam or a partial-sync-specific wrapper to avoid mixing bootstrap and partial-sync logs.
- `source/common/types/mithril-partial-sync.types.ts` already defines partial-sync lifecycle and error contracts, but it does not include a snapshot field, so latest-snapshot metadata can stay backend-internal for now unless implementation proves a minimal shared-contract addition is necessary.
- `source/common/ipc/api.ts` already exposes dedicated start, status, cancel, restart-normal, and wipe-and-full-sync partial-sync channels, so no new IPC contract surface is required for task-200.
- `source/main/mithril/MithrilBootstrapService.ts` already contains backend-owned snapshot lookup helpers and also shows that snapshot metadata lookup can fail without crashing startup flow, so task-200 should use a live-supported latest-resolution path with explicit failure handling instead of assuming one exact CLI subcommand is already proven.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-200.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/mithrilCommandRunner.ts` if a minimal log-path seam is needed
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `source/main/utils/chainStorageCoordinator.ts` only if a very small registration or getter refinement is required by implementation

## Implementation Approach

1. Add a separate partial-sync service class.
   - Create `source/main/mithril/MithrilPartialSyncService.ts` instead of folding partial sync into `MithrilBootstrapService`.
   - Mirror only the small patterns that help auditability: default status object, `EventEmitter`-based status updates, current-process tracking, log-path helper, and safe JSON parsing.
   - Keep bootstrap and partial sync separately understandable even if a tiny helper extraction becomes worthwhile.

2. Keep latest-snapshot resolution small, backend-owned, and live-supported.
   - Resolve latest snapshot metadata inside the service using a backend-owned lookup path that task-200 verifies against the live implementation and focused tests, rather than assuming one specific Mithril CLI subcommand is already proven by prior research.
   - Accept either a direct latest-metadata lookup or a small fallback sequence such as listing snapshots and selecting the newest item, as long as the chosen path is supported by the shipped runtime behavior and kept fully in the backend.
   - Reuse existing bootstrap-era lookup helpers only where they remain truthful for partial sync and keep fallback behavior explicit when metadata lookup fails.
   - Treat the resolved latest snapshot as internal service state for now; do not widen the shared partial-sync status contract just to surface snapshot metadata before later tasks need it.
   - If no live-supported latest lookup path succeeds, surface a bounded `preparing`-stage failure with partial-sync log path and no downloader side effects.

3. Implement only the high-level lifecycle skeleton for now.
   - On start, move through the smallest truthful lifecycle for this task: initialize runtime state, emit `preparing`, resolve latest snapshot metadata, and stop at a backend-owned skeleton boundary ready for `task-201` to continue.
   - Do not add fake range derivation or placeholder install steps that would blur later safety boundaries.
   - If the service needs a temporary terminal outcome before later tasks exist, prefer an explicit backend-only not-yet-implemented failure over a misleading success path.

4. Isolate partial-sync log ownership.
   - Avoid writing partial-sync output into `mithril-bootstrap.log`.
   - Prefer the smallest truthful change: either parameterize the existing Mithril command-runner log path or add a thin partial-sync-specific wrapper that keeps command spawning behavior unchanged while targeting a distinct log file such as `mithril-partial-sync.log`.
   - Keep this change narrow so bootstrap behavior and existing bootstrap tests stay stable.

5. Wire the service through the existing coordinator and IPC seams.
   - Register the new service with `chainStorageCoordinator.setPartialSyncHandlers(...)` rather than adding a second orchestration entrypoint.
   - Replace `rejectUntilImplemented` for partial-sync start and cancel with coordinator-backed behavior.
   - Keep status requests and push updates sourced from the service status cache and emitter.
   - Leave restart-normal and wipe-and-full-sync handlers explicitly unimplemented and clearly labeled for `task-204`.
   - For node-state input to coordinator start, reuse the smallest existing main-process source of truth rather than inventing new infrastructure in this task.

6. Add focused tests around the new seam.
   - Add service tests for latest-snapshot lookup, status transitions, cancellation behavior at skeleton scope, and distinct log-path reporting.
   - Update IPC tests to verify coordinator/service delegation replaces the current not-implemented start/cancel stubs while restart-normal and wipe-full-sync remain intentionally rejected.
   - Only extend coordinator tests if implementation materially changes coordinator behavior beyond the existing seam.

## Acceptance Criteria

- A distinct `MithrilPartialSyncService` entrypoint exists under `source/main/mithril/`.
- The partial-sync backend can resolve latest snapshot metadata internally without renderer-supplied snapshot input, using a live-supported backend lookup path.
- The service emits truthful partial-sync lifecycle status updates and owns its current-process/runtime state.
- Partial-sync start and cancel IPC requests delegate through the coordinator/service seam instead of always rejecting as unimplemented.
- Restart-normal and wipe-and-full-sync actions remain explicitly unimplemented until `task-204` rather than receiving premature placeholder behavior.
- Partial-sync logging is kept separate from bootstrap logging.
- Bootstrap and partial-sync code paths remain separately understandable after the change.
- Focused tests cover the new service skeleton and IPC delegation closely enough to support later backend tasks.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts` only if task-200 changes coordinator behavior materially
- Verify these behaviors directly:
  - latest snapshot metadata is resolved from the backend service without renderer input, using a live-supported lookup path rather than an unverified CLI assumption
  - the service handles latest-metadata lookup failure gracefully and terminates with a bounded `preparing`-stage error instead of silently guessing or proceeding
  - partial-sync status moves through the planned skeleton lifecycle and reports failures with the partial-sync log path
  - start and cancel IPC actions no longer reject with the generic not-implemented error
  - restart-normal and wipe-and-full-sync still reject until `task-204`
  - bootstrap logging and bootstrap status behavior are unchanged
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` is still blocked by unrelated pre-existing failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The main implementation risk is accidental scope creep into range derivation or live install behavior; this plan keeps task-200 at the service skeleton boundary only.
- The current command runner hardcodes the bootstrap log filename, so task-200 must avoid a broad runner refactor while still isolating partial-sync logs.
- The current partial-sync shared status type does not carry snapshot metadata. That is acceptable for task-200 if the service keeps latest resolution internal, but later tasks may need to revisit this deliberately rather than by accident.
- The exact latest-resolution mechanism should stay implementation-neutral in planning until task-200 verifies one live-supported path in code and tests.
- The cleanest node-state source for partial-sync start is still slightly awkward because the existing exported provider lives in the bootstrap IPC module. Reusing that existing main-process source is acceptable here if it avoids unnecessary new infrastructure.

## Required Docs, Tracking, And Research Updates

- Create this canonical plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-200.md`.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-200` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- Add a short research note only if implementation lands a durable service, logging, or latest-resolution seam that later tasks must preserve.
- No `.agent/system/` documentation update is expected unless implementation changes durable IPC or Mithril runtime structure more than this narrow service skeleton.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-200-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-200-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Approved Plan

- Add a dedicated `MithrilPartialSyncService` and keep it separate from bootstrap.
- Limit task-200 to status emission, current-process ownership, latest-snapshot lookup, log isolation, and high-level lifecycle wiring.
- Reuse the coordinator partial-sync seam from `task-102` and the coordinator-owned active boundary from `task-103` instead of inventing new orchestration state.
- Implement real start/cancel delegation in partial-sync IPC now, but defer recovery actions to `task-204`.
- Avoid widening contracts or infrastructure unless a small log-path or node-state seam is truly required by the live code.

## Final Outcome

- Added `source/main/mithril/MithrilPartialSyncService.ts` as the distinct partial-sync service skeleton required by this task.
- The service now owns partial-sync status emission, current-process tracking, a dedicated `mithril-partial-sync.log` path, and backend-owned latest snapshot metadata resolution.
- Latest resolution is implemented with a live-supported fallback sequence: try `show latest` first, then fall back to snapshot-list metadata sorted by `createdAt` when direct lookup fails.
- The service stops at an intentional `PARTIAL_SYNC_NOT_READY` preparing-stage failure after latest resolution, preserving the approved task boundary before range derivation and restore/install work from later tasks.
- Partial-sync IPC start and cancel now delegate through `ChainStorageCoordinator` and the new service instead of always rejecting as unimplemented.
- Restart-normal and wipe-and-full-sync remain explicitly unimplemented, as planned, for `task-204`.
- Focused Jest verification passed for the service, IPC wiring, and log-file seam.
- Final review outcome: approved on the first code-review iteration; see the latest entry in `.agent/plans/mithril-partial-sync/task-plans/task-200-impl-review.md`.

## Verification Outcome

- Passed: `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/mithril/mithrilCommandRunner.spec.ts`
- Repo-wide `yarn compile` was not run for this task because focused Jest coverage was the truthful verification planned here and prior tasks already documented unrelated repo-wide compile blockers.

## Self-Review

- Scope-creep check: keeps range derivation, staged restore, LSM conversion, cutover, recovery branching, and renderer work out of `task-200`.
- Workflow-text check: test and doc-update workflow guidance is reflected accurately and only where it materially affects this plan.
- Missing-tests/docs/research check: includes focused service and IPC tests, review-log paths, and a conditional research-note update if implementation lands a durable seam.
- Consistency check: matches the current repo reality that the coordinator seam and active-state suppression already exist, the IPC actions are still stubbed, and the command runner still hardcodes the bootstrap log path.
