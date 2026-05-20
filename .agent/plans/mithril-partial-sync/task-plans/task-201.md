# Task task-201: Implement Range Calculation, Staging Directory Preparation, And Preflight Checks

## Task

- Task ID: `task-201`
- Title: `Implement range calculation, staging directory preparation, and preflight checks`

## Why This Task Now

- `task-003` locked the backend-only immutable range derivation and fail-closed preflight rules.
- `task-200` added the dedicated `MithrilPartialSyncService` and bounded latest-snapshot lookup, but it still stops at `PARTIAL_SYNC_NOT_READY` before any range or staging work.
- `task-201` is the next unblocked pending task on the critical path and is the smallest truthful step that unblocks real restore work in `task-202` without touching live cutover semantics from `task-203`.
- The canonical task plan doc for this task did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this task also fixes that planning artifact gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Manual test steps required before implementation can proceed: none.
- Evidence expected back from the user during implementation: none.
- Implementation can proceed before user interaction: yes.

## Scope

- Extend `source/main/mithril/MithrilPartialSyncService.ts` in place from the task-200 skeleton.
- Derive the local immutable position from the resolved managed chain target passed through `PartialSyncPreflightContext`.
- Derive the intended partial immutable range from local managed-chain state plus latest Mithril snapshot metadata, without renderer input.
- Prepare a Daedalus-owned staging area for later partial restore work while keeping it separate from live install behavior.
- Add precise fail-closed preflight errors for unsupported local layout and missing required local inputs before any destructive step occurs.
- Add focused Jest coverage for the new range, staging, and preflight behavior.

## Non-Goals

- Running `mithril-client cardano-db download` or parsing live restore progress; that belongs to `task-202`.
- Performing LSM conversion, fixed-allowlist staged install, or any live managed-target cutover; that belongs to `task-203`.
- Implementing retry, restart-normal, wipe-and-full-sync, or boundary-dependent cleanup logic; that belongs to `task-204`.
- Changing renderer, IPC contracts, or shared partial-sync status types unless a very small internal-only helper proves impossible without it.
- Broad chain-storage refactors or a second filesystem validation model outside the existing managed-layout seams.

## Relevant Dependencies

- Required completed dependencies:
  - `task-003`
  - `task-200`
- Supporting completed prerequisites used directly by this plan:
  - `task-001`
  - `task-002`
  - `task-102`
  - `task-103`
- This task directly unblocks:
  - `task-202`
  - `task-203`
  - backend portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/05-task-102-coordinator-seam-notes.md`
- `.agent/plans/mithril-partial-sync/research/06-task-103-node-lifecycle-seam-notes.md`
- `.agent/plans/mithril-partial-sync/research/07-task-200-service-skeleton-notes.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was consulted first, then important planning claims were verified against live repo files before this plan was written.

## Live Repo Findings Verified For Planning

- `source/main/mithril/MithrilPartialSyncService.ts` already owns the correct service boundary for this task and currently stops in `preparing` after `resolveLatestSnapshotMetadata()` with `PARTIAL_SYNC_NOT_READY`.
- `source/main/utils/chainStorageCoordinator.ts` already enforces the stopped-node and `isRecoveryFallback` guards before the service runs, and passes `layoutResult.managedChainPath` plus `mithrilWorkDir` through `PartialSyncPreflightContext`.
- `source/main/utils/chainStoragePathResolver.ts` currently resolves `resolveMithrilWorkDir()` to the active managed chain location itself, not to a separate scratch directory. That means task-201 must prepare a dedicated Daedalus-owned staging subtree and must not treat the workdir root as a safe download target.
- `source/main/utils/chainStorageManagerLayout.ts` and `source/main/utils/chainStorageManagerShared.ts` already define the durable managed-layout and source-of-truth rules: `stateDir/chain` remains authoritative, broken custom storage may fall back to default for startup, and that fallback is unsafe for partial sync unless rejected earlier.
- The live repo does not yet contain a reusable immutable-file parser or partial-sync staging helper, so task-201 should add only the smallest helpers needed for immutable filename parsing and stage-path preparation.
- `source/main/mithril/MithrilPartialSyncService.spec.ts` currently covers latest-resolution fallback and the task-200 skeleton boundary, so it is the right place to extend focused coverage for range derivation and preflight errors.
- The current shared type `MithrilSnapshotItem` does not expose latest certified immutable position, so task-201 should prefer service-local normalization or helper parsing over broad cross-process contract changes.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-201.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/utils/chainStorageManager.ts` only if a very small helper is needed for truthful managed-path or workdir access
- `source/main/utils/chainStorageManagerLayout.ts` only if a very small layout helper extraction materially reduces duplicated path checks
- `source/main/utils/chainStorageManager.spec.ts` only if manager/layout helpers change materially

## Implementation Approach

1. Keep task-201 inside the existing service boundary.
   - Extend `MithrilPartialSyncService.start(...)` immediately after latest snapshot resolution instead of creating another backend entrypoint.
   - Preserve the existing coordinator-owned active-state and stopped-node semantics from `task-102` and `task-103`.

2. Add narrow, concrete preflight helpers only.
   - Read the managed chain root from `context.layoutResult.managedChainPath`.
   - Verify concrete local requirements only:
     - managed chain root is readable as a directory
     - `immutable/` exists and is a readable directory
     - `protocolMagicId` exists and is a readable file
     - at least one parseable immutable filename exists
   - Fail closed with precise `preparing`-stage errors when those checks fail.
   - Do not add speculative corruption heuristics or reuse renderer sync state.

3. Derive local immutable position from filenames, not from renderer or volatile state.
   - Implement a small service-local immutable filename parser that extracts the file number from Cardano immutable filenames by reading the leading numeric stem before the first `.` and ignoring unparseable entries.
   - Select the highest parseable immutable file number from the resolved managed chain `immutable/` directory.
   - Treat "no parseable immutable file number" as a hard preflight blocker.
   - Keep this helper narrow and test-driven instead of promoting it into a broad chain-health validator.

4. Derive the requested range from latest certified immutable metadata.
   - Extend latest snapshot handling just enough for backend range calculation by extracting the latest certified immutable number from snapshot metadata inside the service.
   - Keep that metadata backend-local rather than widening renderer-visible contracts.
   - Support both existing latest-metadata resolution paths: direct `snapshot show latest --json` output and the `snapshot list --json` fallback item shape.
   - Request `start = localImmutable + 1` and `end = latestCertifiedImmutable`.
   - If `localImmutable >= latestCertifiedImmutable`, fail early with a bounded preflight error because there is no certified missing immutable range.
   - Preserve the already-locked drift rule by keeping latest resolution and range derivation adjacent in the same service flow so `task-202` can re-resolve immediately before command issuance instead of reusing stale renderer or cached input.

5. Prepare staging in a Daedalus-owned location outside the managed chain subtree.
   - Because the current `mithrilWorkDir` resolves to the active managed chain location, do not download into `context.mithrilWorkDir` directly and do not place the scratch path anywhere under `context.layoutResult.managedChainPath`.
   - Prepare a dedicated partial-sync staging root in a Daedalus-owned location that is fully separate from the live managed chain subtree, and reject any computed staging path that resolves inside the managed chain path.
   - Ensure preparation is idempotent and removes stale partial-sync scratch artifacts from prior bounded pre-cutover attempts before later download work begins.
   - Make the prepared stage paths explicit enough that `task-202` can use them without guessing where `mithril-client` should write its `db/` child.

6. Preserve the current truthful terminal boundary until task-202 extends runtime behavior.
   - Keep task-201 inside the existing `start()` flow and do not introduce a new success or handoff status.
   - After local preflight, range derivation, and staging preparation complete, stop at the same bounded pre-download not-ready boundary from task-200 rather than implying that download has started.
   - Keep any new internal fields backend-owned until later tasks prove the renderer needs them, and defer runtime-boundary or lifecycle-semantic changes to `task-202`.

7. Add focused tests around the new contract.
   - Cover default and custom managed-chain paths through the service context rather than broad integration setup.
   - Cover missing `immutable/`, unreadable or wrong-kind `protocolMagicId`, no parseable immutable filenames, and no-missing-range rejection.
   - Cover immutable-position extraction from both latest-metadata paths: `show latest` and the snapshot-list fallback.
   - Cover staging preparation and stale staging cleanup so later tasks do not accidentally target the live chain root or reuse partial artifacts.

## Acceptance Criteria

- `MithrilPartialSyncService` can derive the intended partial immutable range without renderer input.
- The local immutable position is derived from the highest parseable immutable filename in the resolved managed-chain `immutable/` directory.
- Partial sync fails early when the managed chain root lacks a readable `immutable/` directory, a readable `protocolMagicId`, or any parseable immutable filename.
- Partial sync fails early when there is no certified missing immutable range.
- Latest certified immutable position used for range calculation is resolved in the backend and kept separate from renderer contracts.
- Latest certified immutable extraction works for both `snapshot show latest --json` and the snapshot-list fallback payload shape.
- A dedicated partial-sync staging directory structure is prepared before any live install step, is cleaned of stale partial artifacts first, and resolves outside `layoutResult.managedChainPath` for both default and custom storage.
- Task-201 preserves the current bounded pre-download `PARTIAL_SYNC_NOT_READY` boundary after preflight work completes and does not introduce new runtime status or lifecycle semantics.
- The implementation remains within preflight and staging scope and does not introduce restore, conversion, cutover, or recovery-action behavior prematurely.
- Focused Jest coverage exists for the new range, staging, and fail-closed preflight behavior.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageManager.spec.ts` only if manager or layout helpers change materially
- Verify these behaviors directly:
   - local immutable position comes from parseable `immutable/` filenames, not renderer data
   - latest certified immutable extraction is covered for both `snapshot show latest --json` and snapshot-list fallback metadata shapes
   - `protocolMagicId` and `immutable/` wrong-kind or missing states fail with precise preflight errors
   - no parseable immutable filenames produce a bounded failure
   - `localImmutable >= latestCertifiedImmutable` rejects without staging a restore command
   - staging preparation removes stale partial-sync scratch artifacts from prior attempts before reusing the staging location
   - staging preparation creates a dedicated partial-sync scratch location outside `layoutResult.managedChainPath` and does not treat the live managed chain root as the restore destination for either default or custom storage
   - current latest-resolution fallback behavior from task-200 remains intact
   - task-201 still terminates at the existing truthful pre-download `PARTIAL_SYNC_NOT_READY` boundary rather than introducing a new runtime handoff status
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` is still blocked by unrelated pre-existing failures, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The biggest live-code risk is staging-path drift: the current coordinator workdir seam resolves to the managed chain location, so task-201 must force the scratch path outside `layoutResult.managedChainPath` instead of merely avoiding the root entry set.
- The exact latest-snapshot field carrying immutable file number may vary across `show latest` and `list` payload shapes, so normalization should stay backend-local and be tested against both live-supported shapes.
- Immutable filename parsing must stay narrow. Over-broad parsing would quietly reintroduce heuristic corruption detection that `task-003` explicitly rejected.
- Task-201 should avoid storing derived range state in shared contracts or changing lifecycle semantics unless later implementation proves it necessary; premature contract widening would blur the backend-only safety rule and undercut the task-103 active-state boundary.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-201.md`.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-201` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- Add a short research note only if implementation lands a durable staging-path or immutable-range seam that later tasks must preserve.
- No `.agent/system/` documentation update is expected unless implementation changes durable chain-storage or Mithril runtime structure beyond this narrow backend task.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-201-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-201-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Files Changed

- `.agent/plans/mithril-partial-sync/task-plans/task-201.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-201-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-201-impl-review.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/08-task-201-range-and-staging-notes.md`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`

## Final Outcome

- Final result: approved and completed.
- Task-201 now derives the local immutable position from the managed-chain `immutable/` directory, extracts the latest certified immutable number from backend-local Mithril metadata normalization, prepares a Daedalus-owned staging root outside the managed chain subtree, and preserves the bounded pre-download `PARTIAL_SYNC_NOT_READY` terminal boundary for task-202.
- Reviewer-required follow-up fixes landed in the same task: unreadable `immutable/` and `protocolMagicId` now fail through bounded preparing-stage errors, and the spec now covers staging-path rejection inside `managedChainPath`.
- No renderer, IPC, or shared-contract widening was needed.

## Final Verification

- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`
- Latest implementation review result: approved in `.agent/plans/mithril-partial-sync/task-plans/task-201-impl-review.md`

## Research Outcome

- Durable findings recorded in `.agent/plans/mithril-partial-sync/research/08-task-201-range-and-staging-notes.md`

## Self-Review

- Scope-creep check: the plan stays within range derivation, concrete preflight validation, and staging prep, and explicitly defers restore, conversion, cutover, and recovery branching to later tasks.
- Workflow-text check: only `test.md` and `update-doc.md` were consulted because this task does not truthfully require IPC or frontend workflow guidance.
- Missing-manifests/tests/docs check: the canonical task plan doc now exists, review-log paths are defined, and focused Jest coverage is included in the verification plan.
- Consistency check: the plan matches the live repo constraint that `resolveMithrilWorkDir()` currently resolves to the managed chain path itself, so staging must resolve outside `layoutResult.managedChainPath` and task-201 must preserve the current pre-download runtime boundary until `task-202` extends it.
