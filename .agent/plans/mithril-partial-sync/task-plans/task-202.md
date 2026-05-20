# Task task-202: Implement Partial Restore Download And Verification Flow

## Task

- Task ID: `task-202`
- Title: `Implement partial restore download and verification flow`

## Why This Task Now

- `task-001` proved the shipped Mithril CLI supports partial restore with `--start`, `--end`, `--include-ancillary`, and staged `--allow-override`, while also proving ancillary-enabled restore still requires later LSM conversion.
- `task-201` now derives the certified immutable range, validates concrete local preflight conditions, and prepares a Daedalus-owned staging root outside the managed chain subtree.
- `task-202` is the next unblocked backend task on the critical path and is the smallest truthful step that turns the current `PARTIAL_SYNC_NOT_READY` skeleton into a real staged restore plus verification flow without crossing into live cutover semantics reserved for `task-203`.
- The canonical task plan doc for this task did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this planning pass also fills that tracking gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Manual test steps required before implementation can proceed: none.
- Evidence expected back from the user during implementation: none.
- Implementation can proceed before user interaction: yes.

## Scope

- Extend `source/main/mithril/MithrilPartialSyncService.ts` from the current task-201 preflight boundary into real staged partial restore execution.
- Issue the validated Mithril partial restore command against the prepared staging parent using the derived immutable range and latest snapshot selection handled in the backend.
- Parse Mithril JSON stdout progress truthfully enough to drive partial-sync `downloading` and `verifying` status updates through the existing shared status contract.
- Surface staged restore failures with accurate stage attribution, log path, and bounded backend-owned recovery action availability for the pre-cutover boundary.
- Add focused Jest coverage for download invocation, progress/status mapping, verification-stage reporting, and failure handling at pre-cutover scope.

## Non-Goals

- Implementing LSM conversion or installing staged data into the live managed chain target; that belongs to `task-203`.
- Implementing durable cutover markers, unsafe-install startup recovery, retry/restart-normal/wipe-full-sync action handlers, or boundary-B/C branching; that belongs to `task-204`.
- Adding or changing IPC contracts, renderer stores, diagnostics UI, or frontend overlay behavior unless a very small internal compatibility fix becomes unavoidable.
- Broadly refactoring bootstrap and partial-sync orchestration into one service.
- Inventing renderer-driven thresholds, snapshot-selection UI, or alternative range derivation paths.

## Relevant Dependencies

- Required completed dependencies:
  - `task-001`
  - `task-201`
- Supporting completed prerequisites used directly by this plan:
  - `task-002`
  - `task-003`
  - `task-004`
  - `task-100`
  - `task-101`
  - `task-102`
  - `task-103`
  - `task-200`
- This task directly unblocks:
  - `task-203`
  - `task-204`
  - backend portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/07-task-200-service-skeleton-notes.md`
- `.agent/plans/mithril-partial-sync/research/08-task-201-range-and-staging-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was consulted first, then important planning claims were verified against live repo files before inclusion.

## Live Repo Findings Verified For Planning

- `source/main/mithril/MithrilPartialSyncService.ts` already owns partial-sync runtime state, status emission, process tracking, latest snapshot lookup, local immutable-range derivation, and staging-root preparation, but `start(...)` still terminates in `preparing` with `PARTIAL_SYNC_NOT_READY` before any download begins.
- The same service already caches `_currentProcess`, `_progressItems`, `_startedAt`, `_latestSnapshot`, and `_activeWorkDir`, so task-202 should reuse those fields instead of introducing another long-running execution boundary.
- `source/main/mithril/mithrilProgress.ts` already parses Mithril JSON lines for step announcements, `Files` progress, `Ancillary` byte progress, and elapsed time, but today it is generic and not yet wired into partial sync status updates.
- `source/common/types/mithril-partial-sync.types.ts` already exposes the status and telemetry fields task-202 needs: `filesDownloaded`, `filesTotal`, `ancillaryBytesDownloaded`, `ancillaryBytesTotal`, `elapsedSeconds`, `progressItems`, `error`, and `allowedRecoveryActions`.
- `source/main/ipc/mithrilPartialSyncChannel.ts` already broadcasts the service status cache and delegates start and cancel through `ChainStorageCoordinator`, so task-202 does not need IPC surface changes for download/verification telemetry.
- `source/main/utils/chainStorageCoordinator.ts` already enforces node-stopped and `isRecoveryFallback` guards before the service starts, which means task-202 can stay focused on staged restore execution and must preserve pre-cutover safety semantics rather than duplicating those checks.
- The validation spike locked two execution constraints that task-202 must honor: Mithril downloads into `<download-dir>/db`, and `--allow-override` is only safe evidence for Daedalus-controlled staging directories, not live chain targets.
- Historical bootstrap research confirms Mithril emits seven backend steps where steps 1-3 are download-oriented and steps 4-7 are verification-oriented, with no per-step percentage updates during verification. Task-202 therefore needs truthful stage transitions rather than fake verification percentages.
- Ancillary-enabled partial restore still yields `clean/`, `immutable/`, `ledger/`, and `protocolMagicId` under staged `db/` and still requires later conversion before node handoff, so task-202 should stop after verified staged restore and not imply restart readiness.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-202.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/mithrilProgress.ts` only if a very small parse helper or shape refinement is needed to keep status mapping truthful
- `.agent/plans/mithril-partial-sync/research/09-task-202-download-and-verification-notes.md`

## Implementation Approach

1. Extend the existing service flow instead of creating a second execution path.
   - Continue using `start(context)` as the single partial-sync orchestrator.
   - Keep latest metadata resolution, local immutable-range derivation, staging prep, command issuance, and status updates adjacent in one service flow so snapshot drift remains a bounded backend concern.
   - Preserve coordinator-owned active-state and node-restart suppression behavior from `task-102` and `task-103`.

2. Issue the staged partial restore command with the spike-validated shape.
   - Reuse the task-201 prepared staging parent as the `--download-dir` parent, not the live chain path.
   - Invoke `mithril-client --json cardano-db download latest --download-dir <staging-parent> --start <local+1> --end <latestCertified> --include-ancillary --allow-override` through the existing command-runner seam.
   - Keep snapshot selection backend-owned by continuing to use `latest` at command time instead of widening contracts with digest input.
   - Re-resolve latest snapshot metadata immediately before command issuance, or fail with a bounded retriable preflight error if the re-resolved latest immutable number no longer matches the prepared range assumptions, so latest-snapshot drift is handled explicitly instead of silently reusing stale metadata.

3. Map Mithril stdout into truthful partial-sync status transitions.
   - Wire `parseMithrilProgressUpdate` into the partial-sync command `onStdout` callback.
   - Treat step announcements for steps 1-3 as `downloading` and steps 4-7 as `verifying`.
   - Forward file counts, ancillary byte counts, and elapsed seconds when present.
   - Reuse `progressItems` conservatively: mark `preparing` complete once command execution starts, add `downloading` and `verifying` items as the CLI actually enters those phases, and avoid synthetic percentage completion for silent verification steps.
   - Do not emit `converting`, `installing`, `finalizing`, `starting-node`, or `completed` from task-202.

4. Keep failure attribution precise and pre-cutover-safe.
   - Fail command-launch and step-1..3 issues as `downloading` unless the live line parsing proves the CLI has already entered verification.
   - Fail digest/certificate/signature problems and non-zero exits after verification starts as `verifying`.
   - Surface the partial-sync log path on all failures.
   - Because task-202 remains entirely before live cutover, keep allowed recovery actions within Boundary A only: `retry`, `restart-normal`, and `wipe-and-full-sync` on terminal failure, while cancellation remains allowed through the existing pre-cutover service boundary.
   - Keep cleanup limited to staged-path and process state owned by this task; do not touch the live managed chain target.

5. Validate staged output only to the degree required before task-203.
   - Confirm Mithril produced the expected staged `db/` root before reporting download/verification success to later steps.
   - Ensure the service retains or exposes the staged `db` path internally for task-203 rather than forcing later code to guess where Mithril wrote output.
   - Stop after verified staged restore is present and hand off with a backend-owned boundary ready for conversion/cutover work.
   - If a minimal staged-layout sanity check is needed here, keep it narrow to presence of the expected `db/` root and required top-level entries already proven by the spike, without duplicating the full cutover allowlist validation reserved for `task-203`.

6. Preserve truthful lifecycle semantics at the end of task-202.
   - Replace the current `PARTIAL_SYNC_NOT_READY` stop point with a new bounded post-verification boundary rather than a misleading success signal.
   - The cleanest truthful outcome is to stop after `verifying` completes and fail with an explicit not-yet-implemented conversion/install boundary until `task-203` lands, unless implementation can carry a narrow internal handoff without widening renderer semantics.
   - Do not imply that the partial sync is finished or restart-safe after download verification alone.

7. Add focused tests for command shape, progress mapping, and failure boundaries.
   - Extend `MithrilPartialSyncService.spec.ts` with command-argument verification for staged partial restore.
   - Cover step-based transition from `preparing` to `downloading` to `verifying` using representative Mithril JSON lines.
   - Cover ancillary and file telemetry propagation into partial-sync status.
   - Cover latest-drift rejection, staged-root validation failure, command non-zero exit before verification, and command non-zero exit after verification begins.
   - Keep tests focused on service behavior; do not expand into renderer or IPC tests unless task-202 materially changes those seams.

## Acceptance Criteria

- `MithrilPartialSyncService` issues the partial restore command against the Daedalus-owned staging parent, never against the live managed chain path.
- The command matches the spike-validated partial restore shape, including `--start`, `--end`, `--include-ancillary`, and staged `--allow-override` usage.
- Latest-snapshot drift is handled explicitly at command time rather than by silently reusing stale prepared metadata.
- Partial-sync status transitions from `preparing` into truthful `downloading` and `verifying` phases based on Mithril CLI output.
- `filesDownloaded`, `filesTotal`, `ancillaryBytesDownloaded`, `ancillaryBytesTotal`, and `elapsedSeconds` are forwarded through the shared partial-sync status contract when the CLI emits them.
- Verification failures surface as staged partial-sync errors and do not proceed to conversion or live install.
- Terminal failure after task-202 still represents Boundary A only and exposes only pre-cutover-safe recovery actions.
- Task-202 leaves live managed chain data untouched and does not implement conversion, cutover, restart, or unsafe-install recovery behavior prematurely.
- Focused Jest coverage exists for command execution shape, progress/status mapping, latest-drift handling, and staged verification failure paths.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/mithril/mithrilProgress.ts`-related behavior through service tests, or a dedicated parser spec only if a new parser branch is added
- Verify these behaviors directly:
  - partial restore command uses the staged download parent, derived immutable range, `latest`, `--include-ancillary`, and `--allow-override`
  - live managed chain path is never used as the Mithril `--download-dir`
  - latest snapshot drift is rejected or re-derived safely before command execution
  - Mithril step announcements drive `downloading` then `verifying` without fake verification percentage synthesis
  - file-count and ancillary-byte telemetry propagate into `MithrilPartialSyncStatusUpdate`
  - staged output path exists under the prepared `db/` root after a simulated successful restore
  - pre-verification failures surface as `downloading`-stage errors with the partial-sync log path
  - post-verification failures surface as `verifying`-stage errors with the partial-sync log path
  - task-202 still stops short of conversion/cutover and does not emit misleading terminal success
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` remains blocked by unrelated pre-existing failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The largest implementation risk is stale-latest drift between task-201 preflight and task-202 command issuance. The service must re-check or bound that drift explicitly instead of reusing a stale prepared range.
- Verification-stage progress is inherently coarse because Mithril emits step announcements but no percentage updates for steps 4-7. The UI contract should therefore receive truthful stage changes and retained telemetry, not invented verification percentages.
- Partial-sync cancellation currently remains a broad pre-cutover service action. Task-202 should avoid introducing ambiguous mid-verification cleanup semantics that later conflict with task-204 boundary rules.
- A too-eager staged-layout validator here could overlap with task-203's locked allowlist cutover validation. Task-202 should validate only the minimal staged output assumptions needed to continue safely.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-202.md`.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-202` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- Add a short research note capturing the shipped staged-download command shape, latest-drift guard, post-verification handoff boundary, and mixed file plus ancillary telemetry retention rule for later tasks.
- No `.agent/system/` documentation update is expected unless implementation changes durable Mithril runtime structure beyond this narrow backend task.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-202-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-202-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- `task-202` is implemented and review-approved.
- `MithrilPartialSyncService.start()` now performs a real staged `mithril-client --json cardano-db download latest` partial restore into the Daedalus-owned staging parent, using the derived immutable range and re-resolving latest snapshot metadata immediately before command launch.
- Mithril JSON progress is now mapped into truthful partial-sync `downloading` and `verifying` status updates without inventing verification percentages, and mixed file plus ancillary telemetry is retained across progress lines instead of being cleared by ancillary-only updates.
- The service now validates the staged `db/` output root and required top-level entries (`clean`, `immutable`, `ledger`, `protocolMagicId`) before stopping at a deliberate post-verification `PARTIAL_SYNC_CUTOVER_NOT_READY` boundary for task-203.
- Terminal failures remain pre-cutover only and expose Boundary A recovery actions (`retry`, `restart-normal`, `wipe-and-full-sync`) without touching the live managed chain target.

## Verification Outcome

- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` passed.
- `yarn compile` was attempted and remains blocked by unrelated pre-existing TypeScript failures in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`.
- The implementation review log reached approval on the latest `Code Review` entry after one narrow telemetry-retention fix.

## Docs And Research Outcome

- Planning review log preserved at `.agent/plans/mithril-partial-sync/task-plans/task-202-plan-review.md`.
- Implementation review log preserved at `.agent/plans/mithril-partial-sync/task-plans/task-202-impl-review.md`.
- Durable task findings recorded in `.agent/plans/mithril-partial-sync/research/09-task-202-download-and-verification-notes.md`.
- Task tracker updated to mark `task-202` completed.

## Self-Review

- Scope-creep check: the plan is limited to staged restore execution, progress mapping, and verification telemetry, and explicitly defers conversion, cutover, and recovery-action implementation to tasks 203 and 204.
- Workflow-text check: only `test.md` and `update-doc.md` were consulted because this task does not truthfully require IPC or frontend workflow guidance.
- Missing-manifests/tests/docs check: the canonical task plan doc is created, review-log paths are defined, and focused Jest verification is included.
- Consistency check: the plan matches the live repo state where the service already owns preflight/staging state, IPC already carries status updates, and the current stop point is still a pre-download `PARTIAL_SYNC_NOT_READY` boundary.
