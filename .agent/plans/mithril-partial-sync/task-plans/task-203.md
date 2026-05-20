# Task task-203: Implement LSM Conversion And Validated Staged Cutover

## Task

- Task ID: `task-203`
- Title: `Implement LSM conversion and validated staged cutover`

## Why This Task Now

- `task-202` now performs a real staged partial restore and verification flow, but it still stops at the deliberate `PARTIAL_SYNC_CUTOVER_NOT_READY` boundary before any conversion or live install work begins.
- The validation spike, PRD, and completed design-hardening tasks already locked the live-data rule for the next step: staged restore only, then empty the managed chain target and reinstall only `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId` from validated staged output.
- The plan review surfaced one blocking truthfulness gap: live cutover cannot be implemented safely in `task-203` unless the same task also adds the minimum persisted partial-sync operation marker and startup-owned block/recovery seam for Boundary B/C1 interruptions, because the current repo has no persisted partial-sync startup recovery path.
- `task-203` remains the next unblocked backend task on the critical path, but its smallest truthful scope now includes that minimal persisted interruption boundary, while broader recovery-action branching and diagnostics-launched retry or restart behavior remain reserved for `task-204`.
- The canonical task plan doc for this task did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this planning pass also fills that tracking gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Manual test steps required before implementation can proceed: none.
- Evidence expected back from the user during implementation: none.
- Implementation can proceed before user interaction: yes.

## Scope

- Extend `source/main/mithril/MithrilPartialSyncService.ts` past the current post-verification handoff to perform LSM conversion on staged ancillary ledger state and then install only validated staged entries into the managed chain target.
- Add or extract the smallest reusable converter helper needed so partial sync can reuse the proven bootstrap conversion behavior without duplicating fragile filesystem steps.
- Add a dedicated chain-storage install seam for validated staged cutover that enforces the locked allowlist and rejects unexpected staged top-level entries instead of reusing the broader current `installSnapshot()` behavior unchanged.
- Add the minimum persisted partial-sync operation marker needed to distinguish Boundary A from Boundary B/C1 interruption states across app restarts.
- Add the smallest startup-owned block/recovery seam needed so startup can detect an interrupted unsafe partial-sync install, block normal node start, and expose at least wipe-and-full-sync recovery plus existing non-recovery affordances without depending on diagnostics UI or a running node.
- Keep task-203 focused on staged-to-live conversion, cutover semantics, and the minimum persisted startup truthfulness seam, with focused Jest coverage for converter invocation, allowlist validation, interruption-state detection, startup blocking, wipe-only recovery exposure, and cutover failure handling.

## Non-Goals

- Implementing full retry, restart-normal, and diagnostics-launched wipe-and-full-sync branching for all failure boundaries; that belongs to `task-204`.
- Adding renderer, IPC, or shared-contract changes unless a very small internal compatibility adjustment becomes unavoidable.
- Reworking the existing empty-chain bootstrap flow beyond the smallest shared conversion-helper extraction that keeps partial sync and bootstrap consistent.
- Introducing a generic chain-storage merge model that preserves or combines live and staged top-level entries.
- Adding rollout kill-switch wiring unless cutover code cannot remain truthful without a very small existing-config hook.
- Building polished startup recovery UX beyond the minimum startup-owned block/recovery surface required by the PRD's Boundary B/C1 rules.

## Relevant Dependencies

- Required completed dependencies:
  - `task-001`
  - `task-201`
  - `task-202`
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
  - `task-204`
  - backend portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/08-task-201-range-and-staging-notes.md`
- `.agent/plans/mithril-partial-sync/research/09-task-202-download-and-verification-notes.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was consulted first, then important planning claims were verified against live repo files before inclusion.

## Live Repo Findings Verified For Planning

- `source/main/mithril/MithrilPartialSyncService.ts` currently performs latest resolution, preflight, staged restore, and minimal staged-output validation, then intentionally fails at `PARTIAL_SYNC_CUTOVER_NOT_READY` with error stage `converting`.
- The same partial-sync service already tracks `_stagedDbPath`, `_latestSnapshot`, `_activeWorkDir`, `_currentProcess`, `_progressItems`, and status emission, but all of that state is currently in-memory and cleared in `finally`, so task-203 cannot cross the first live mutation boundary truthfully without adding a persisted marker.
- `source/main/mithril/MithrilBootstrapService.ts` already contains the only repo-proven LSM conversion logic. `_convertSnapshot()` identifies the newest numeric `ledger/<slot>` entry, temporarily moves the in-memory snapshot out of `ledger/`, runs `snapshot-converter --input-mem ... --output-lsm-snapshot ... --output-lsm-database ... --config <network config>`, removes any preexisting `lsm/`, and deletes the temporary input regardless of success.
- `source/main/utils/chainStorageManagerLayout.ts:524-568` shows current `installSnapshot()` is too broad for task-203: it empties the managed target, then moves every top-level entry from `dbDirectory` into the managed chain path. That behavior would copy unexpected staged entries through instead of enforcing the locked allowlist.
- `source/main/utils/chainStorageManagerShared.ts` currently defines `LEGACY_MANAGED_CHAIN_ENTRIES` that include `volatile` and omit `lsm` and `protocolMagicId`, so task-203 should not treat that legacy migration set as the live cutover allowlist.
- `source/common/types/mithril-partial-sync.types.ts` already includes `converting`, `installing`, and `finalizing` statuses, so task-203 can emit truthful backend progress without widening shared contracts.
- `source/main/utils/handleDiskSpace.ts` currently has startup-owned recovery logic only for `mithril-bootstrap.lock`; it wipes bootstrap artifacts and re-prompts for bootstrap, but it has no partial-sync marker detection, no startup-owned partial-sync recovery surface, and no mechanism to block normal node start on interrupted Boundary B/C1 partial-sync installs.
- `source/main/cardano/setup.ts` currently suppresses generic crash restart only while partial sync is active in memory through the coordinator-backed provider. That protects live mutation only while the same process remains alive and does nothing for crash, quit, or power-loss recovery after restart.
- `source/main/ipc/mithrilPartialSyncChannel.ts` still rejects restart-normal and wipe-and-full-sync requests as not implemented. That is acceptable for task-203 only if the startup-owned unsafe-install recovery seam does not depend on those diagnostics IPC handlers to remain safe after interruption.
- Existing specs already lock the current seams that task-203 must evolve carefully:
  - `MithrilPartialSyncService.spec.ts` locks the task-202 post-verification stop point and staged-output validation.
  - `MithrilBootstrapService.spec.ts` locks converter argument shape and no-ledger-slot failure behavior.
  - `MithrilBootstrapService.install.spec.ts` currently asserts bootstrap delegates snapshot installation to `ChainStorageManager.installSnapshot(...)`.
  - `ChainStorageManager.spec.ts` already covers `emptyManagedContents(...)` behavior and is the right place for new allowlist-cutover tests if the manager surface changes.
  - `handleDiskSpace.spec.ts` already covers startup gating behavior and is the right place for new interrupted-partial-sync startup blocking and recovery tests.
- `ChainStorageCoordinator` already serializes partial sync with bootstrap and other chain-storage mutations and enforces stopped-node plus non-recovery-fallback preconditions, so task-203 should keep using that seam rather than inventing a second startup or mutation coordinator.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-203.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/MithrilBootstrapService.ts` only if a narrow shared conversion helper extraction is the smallest safe option
- `source/main/mithril/MithrilBootstrapService.spec.ts` only if shared conversion extraction changes the bootstrap seam materially
- `source/main/mithril/MithrilBootstrapService.install.spec.ts` if the chain-storage install API used by bootstrap or partial sync changes
- `source/main/ipc/mithrilPartialSyncChannel.ts` only if the minimum startup-owned recovery seam needs a narrow status/helper export
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts` only if that helper/export changes materially
- `source/main/utils/chainStorageManager.ts`
- `source/main/utils/chainStorageManagerLayout.ts`
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`
- `source/main/utils/chainStorageManager.spec.ts`
- `.agent/plans/mithril-partial-sync/research/10-task-203-conversion-and-cutover-notes.md`
- `.agent/system/architecture.md` only if implementation lands a durable partial-sync startup-recovery runtime path that should be preserved for later tasks

## Implementation Approach

1. Extend the existing partial-sync service instead of introducing a second cutover orchestrator.
   - Keep `MithrilPartialSyncService.start(...)` as the single diagnostics-launched backend flow.
   - Replace the current `PARTIAL_SYNC_CUTOVER_NOT_READY` handoff with real conversion, marker updates, and install stages.
   - Preserve coordinator-owned mutation serialization and active-state semantics from `task-102` and `task-103`.

2. Reuse the proven bootstrap conversion path with the smallest safe extraction.
   - Prefer a narrow shared helper inside `source/main/mithril/` for the staged `db/` conversion steps rather than duplicating bootstrap's slot-selection and filesystem choreography inside partial sync.
   - Keep the conversion inputs aligned with the live bootstrap behavior already locked by tests:
     - read the newest numeric ledger slot directory
     - move the in-memory snapshot out of `ledger/<slot>` temporarily
     - remove any preexisting staged `lsm/`
     - run `snapshot-converter` with the resolved network config path
     - always clean the temporary input path afterward
   - Fail conversion truthfully as `converting` and stop before any live cutover if staged ledger conversion does not complete.

3. Add explicit staged-output validation for the locked live cutover allowlist.
   - Validate the staged `db/` top-level entries after conversion and before any live mutation.
   - Require exactly the allowed live-install entries: `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`.
   - Reject any unexpected staged top-level entry, including `volatile`, rather than copying it through.
   - Keep this allowlist local to the new cutover seam instead of reusing legacy migration entry sets.

4. Implement a dedicated validated staged cutover path in chain-storage helpers.
   - Do not route partial sync through the existing broad `installSnapshot()` helper unchanged.
   - Add a separate manager/layout API for validated staged installation, or a narrowly parameterized helper that still makes the fixed allowlist explicit at the call site.
   - Resolve the live managed chain target via existing managed-layout seams, empty its top-level contents, and reinstall only the validated staged allowlist entries.
   - Preserve the stable managed chain root itself while removing all live top-level contents, which intentionally discards live `volatile/` so cardano-node recreates it after restart.
   - Clean up the staged `db/` directory after successful move/cutover so superseded staged artifacts do not linger.

5. Add the minimum persisted partial-sync operation marker required for live cutover truthfulness.
   - Introduce a durable marker under the Daedalus-owned state/logs area, modeled as narrowly as possible after bootstrap's lock-file pattern but specific to partial sync and rich enough to distinguish at least:
     - pre-cutover staging only
     - Boundary B or live cutover in progress
     - Boundary C1 or staged DB installed before first proven node-start handoff
     - Boundary C2 only if task-203 truly reaches and proves that boundary, which this plan does not assume
   - Update the marker immediately before the first live managed-target mutation and again after validated staged DB installation so startup can distinguish wipe-only-recovery states from untouched-DB states after interruption.
   - Keep the marker backend-owned and filesystem-derived; do not depend on renderer cache or in-memory service state.

6. Add the smallest startup-owned block/recovery seam for interrupted Boundary B/C1 states.
   - Extend startup gating in `handleDiskSpace.ts` to detect the persisted partial-sync marker before normal node start proceeds.
   - If startup finds an interrupted Boundary B or C1 partial-sync state, block normal node start and expose a minimal startup-owned recovery path that at least allows wipe-and-full-sync plus any already-existing non-recovery affordances such as quit or log access.
   - Keep this startup-owned recovery surface intentionally minimal and separate from the later diagnostics-launched recovery flow. It exists only so interrupted unsafe installs are not misclassified as normal startup.
   - If task-203 also defines a safe interrupted Boundary A cleanup path through the same marker, keep that behavior narrow: staged cleanup, marker clear, and normal startup on the untouched DB.

7. Keep cutover semantics explicit and auditable in code and tests.
   - Add narrow comments at the cutover seam that explain why unexpected staged entries are rejected and why live `volatile/` is discarded.
   - Make the fixed allowlist visible in tests and implementation instead of deriving it from directory listings or migration helpers.
   - Make the persisted marker state transitions explicit in tests and implementation so later tasks do not silently weaken the Boundary B/C1 startup block.

8. Keep task-204's broader recovery branching and diagnostics UX work explicitly out of scope while making the post-task terminal boundary explicit.
   - Task-203 should carry the service through `converting`, `installing`, and any minimal `finalizing` cleanup needed for a validated staged cutover plus the startup-owned interruption block.
   - Task-203 must not emit `completed` or `starting-node` unless it also truthfully implements the first post-install node-start handoff semantics for that boundary, which this plan does not require.
   - The expected post-task terminal state is therefore explicit: successful task-203 implementation may still end in a non-`completed` post-cutover backend boundary if first-start handoff and boundary-dependent recovery branching remain task-204 work.
   - It should not broaden into diagnostics-launched retry/restart-normal action handling or the full final boundary-dependent recovery matrix.

9. Add focused tests around conversion reuse, validated cutover, and interruption semantics.
   - Extend `MithrilPartialSyncService.spec.ts` to cover successful conversion plus marker-aware validated cutover invocation, staged-entry rejection, and install-stage failures.
   - Add or extend chain-storage tests to lock the fixed allowlist behavior, including rejection of unexpected staged entries and discarding live `volatile/`.
   - Add startup-gating tests in `handleDiskSpace.spec.ts` for interrupted Boundary B/C1 detection, blocked normal start, and minimal wipe-and-full-sync recovery exposure.
   - Only touch bootstrap tests as needed if a shared converter helper changes that seam.

## Acceptance Criteria

- Staged ancillary ledger state is converted to the Daedalus-compatible LSM layout before any live install step begins.
- Partial sync does not duplicate fragile converter behavior unnecessarily; it reuses or extracts the existing proven conversion path in a narrow, auditable way.
- Live managed chain target mutation is constrained to the locked staged-only cutover rule: empty the managed target and reinstall only `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId` from validated staged output.
- Any unexpected staged top-level entry, including `volatile`, is treated as a validation failure and blocks install.
- Live `volatile/` is not preserved or merged across cutover.
- The implementation does not rely on the current broad `installSnapshot()` behavior that would move every top-level staged entry into the managed target.
- A persisted partial-sync operation marker exists before the first live mutation boundary and is sufficient for startup to distinguish interrupted Boundary B/C1 states from pre-cutover staging states.
- If startup encounters an interrupted Boundary B or C1 state, normal node start is blocked and the app exposes a minimal startup-owned wipe-and-full-sync recovery path without depending on diagnostics UI.
- Partial-sync status advances truthfully through `converting`, `installing`, and any narrow post-cutover boundary used by task-203, and does not imply restart-safe `completed` or `starting-node` semantics before those boundaries are actually implemented.
- Focused Jest coverage exists for conversion behavior, allowlist validation, live-target cutover semantics, interruption-state detection, startup blocking, and install-failure paths.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageManager.spec.ts`
  - `source/main/utils/handleDiskSpace.spec.ts`
  - `source/main/mithril/MithrilBootstrapService.spec.ts` only if a shared conversion helper extraction changes bootstrap's converter seam
  - `source/main/mithril/MithrilBootstrapService.install.spec.ts` only if the chain-storage install API under bootstrap coverage changes materially
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts` only if the minimum startup-owned recovery seam requires a narrow helper/export there
- Verify these behaviors directly:
  - the staged partial-sync flow no longer stops at `PARTIAL_SYNC_CUTOVER_NOT_READY` immediately after verification
  - partial sync invokes the converter against staged `db/` output using the newest numeric ledger slot and produces staged `lsm/`
  - preexisting staged `lsm/` is removed before conversion so `snapshot-converter` cannot prompt or reuse stale output
  - successful cutover empties the managed target contents and installs only `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`
  - a staged `volatile/` or any other unexpected top-level entry is rejected before live install begins
  - existing live `volatile/` is discarded during successful cutover rather than preserved or merged
  - the persisted marker transitions before the first live mutation boundary and after validated staged install are durable enough for startup classification
  - startup detects interrupted Boundary B/C1 partial-sync states, blocks normal node start, and does not silently fall through to ordinary startup on the possibly unsafe DB
  - the minimum startup-owned recovery path exposes wipe-and-full-sync handling for interrupted unsafe installs without depending on diagnostics-launched recovery actions
  - install failure after successful staged validation surfaces as an install-stage error and does not silently widen recovery behavior beyond task-203 scope
  - task-203 does not emit `completed` or `starting-node` unless those semantics are genuinely implemented
  - bootstrap conversion behavior remains intact if a shared helper extraction is used
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` remains blocked by unrelated pre-existing failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The largest code risk is accidentally reusing the current broad `installSnapshot()` helper, which would move every staged top-level entry into the managed chain target and violate the locked allowlist rule.
- Extracting bootstrap's converter logic must stay narrow. A broad shared Mithril abstraction would be scope creep for task-203.
- Live cutover crosses the PRD's Boundary B/C safety line, so the main task-203 design risk is under-building the persisted marker/startup gate and accidentally allowing interrupted unsafe installs to resume as normal startup.
- The current legacy managed-entry constants are not the right source of truth for partial-sync cutover. Reusing them would silently re-allow `volatile` and omit `lsm` and `protocolMagicId`.
- The startup-owned recovery surface must remain minimal. If implementation starts pulling diagnostics UX, restart-normal branching, or full recovery-action policy into task-203, that would be scope creep and should stay deferred to `task-204`.
- If task-203 can safely complete conversion, validated cutover, and startup-owned interruption blocking but still cannot truthfully own automatic restart, the service should stop at a narrow post-cutover boundary rather than claiming full completion.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-203.md`.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-203` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- Add a short research note capturing the selected conversion-reuse seam, fixed allowlist cutover enforcement, persisted marker state model, and startup-owned interrupted-install blocking rule that later tasks must preserve.
- Update `.agent/system/architecture.md` if implementation lands the planned durable partial-sync startup-recovery path, because that changes runtime Mithril behavior rather than only an internal helper seam.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-203-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-203-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- `task-203` completed with an approved implementation review.
- The backend now performs staged partial-sync LSM conversion, fixed-allowlist validated cutover into the managed chain target, and startup-owned interrupted-cutover blocking through a durable partial-sync marker.
- Boundary B (`cutover-in-progress`) remains wipe-only on startup interruption.
- Boundary C1 (`installed-awaiting-node-start`) now gets one truthful first-start proof attempt, and generic fallback normal startup is suppressed if that proof fails.
- Successful first-start proof currently clears the persisted marker instead of writing a separate `node-start-verified` state.
- Focused Jest verification passed for the touched Mithril, chain-storage, and startup files.
- Repo-wide `yarn compile` remains blocked by unrelated pre-existing issues outside task-203 scope.

## Final Verification

- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/chainStorageManager.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/mithril/MithrilBootstrapService.spec.ts source/main/mithril/MithrilBootstrapService.install.spec.ts`
- `yarn compile` attempted; failed due to pre-existing unrelated issues in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

## Self-Review

- Scope-creep check: the plan now widens only by the critique-required minimum persisted marker and startup-owned block/recovery seam needed to make live cutover truthful, while still deferring broader diagnostics recovery branching and post-install node-handoff semantics to `task-204`.
- Workflow-text check: `ipc.md` was not added because live-file verification still does not force shared-contract work; the startup-owned recovery seam can remain main-process-local unless implementation proves otherwise.
- Missing-tests/docs check: the revised plan now includes interruption verification and architecture/research updates for the new durable runtime behavior.
- Consistency check: the plan matches the live repo state where partial sync currently stops at `PARTIAL_SYNC_CUTOVER_NOT_READY`, bootstrap owns the only proven converter logic, current chain-storage `installSnapshot()` behavior is broader than the locked allowlist semantics, and startup currently has no persisted partial-sync unsafe-install recovery path.
