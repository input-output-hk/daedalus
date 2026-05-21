# Task task-400: Add Automated Tests Across Backend, IPC, And Renderer

## Task

- Task ID: `task-400`
- Title: `Add automated tests across backend, IPC, and renderer`

## Why This Task Now

- `task-204`, `task-303`, and `task-304` are complete, so the Mithril partial-sync backend, IPC, diagnostics CTA, confirmation step, overlay, i18n, and Storybook surfaces now exist and can be locked down with focused automated coverage.
- Live-file verification shows this task is not greenfield test creation across the whole feature. Most critical seams already have focused Jest coverage in place.
- The smallest truthful plan is therefore:
  - audit the current coverage against `task-400` acceptance,
  - keep existing passing specs as the primary body of evidence,
  - add only the missing focused integration assertions needed to close acceptance gaps.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual validation before completion: none for this task beyond developer-run automated verification.
- Manual supported-network QA remains later `task-401` work.

## Scope

- Treat existing Mithril partial-sync Jest coverage as first-class implementation evidence instead of rewriting or duplicating it.
- Verify and, where necessary, extend automated coverage across:
  - main-process partial-sync orchestration,
  - IPC request and status handler wiring,
  - startup recovery and crash-restart suppression seams,
  - renderer store lifecycle and recovery-action state,
  - diagnostics CTA and confirmation gating,
  - diagnostics-to-overlay transition behavior,
  - app-shell overlay wiring for recovery and completed-dismiss actions.
- Add only the minimum new tests needed to make `task-400` acceptance truthful.
- Record the resulting coverage audit and targeted additions in a repo-local task research note during implementation.

## Non-Goals

- Do not add broad E2E coverage unless a focused Jest seam proves insufficient.
- Do not refactor production code just to make tests more abstract if a small local test seam is enough.
- Do not duplicate existing backend service, coordinator, IPC, store, or overlay assertions already covered by current specs.
- Do not reopen product behavior, IPC contracts, renderer flow ownership, or Storybook scope already settled by earlier tasks.
- Do not write the planning or implementation review-log files during planning.

## Dependencies

- Required completed dependencies:
  - `task-204`
  - `task-303`
  - `task-304`
- Supporting completed work used directly by this plan:
  - `task-100`
  - `task-101`
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-201`
  - `task-202`
  - `task-203`
  - `task-300`
  - `task-301`
  - `task-302`
- This task directly supports:
  - `task-401`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
- `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/test.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was loaded for the required repository-understanding pass, and all material claims below were verified against live files.

## Existing Task-400 Artifacts

- No existing `task-400` canonical plan, planning review log, or implementation review log was present under `.agent/plans/mithril-partial-sync/task-plans/` at planning time.

## Current Task Artifacts

- Canonical task plan: `.agent/plans/mithril-partial-sync/task-plans/task-400.md`
- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-400-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-400-impl-review.md`
- Research note: `.agent/plans/mithril-partial-sync/research/17-task-400-automated-test-coverage-notes.md`

## Live Repo Findings Verified For Planning

- Main-process service coverage already exists in `source/main/mithril/MithrilPartialSyncService.spec.ts` for:
  - latest snapshot metadata resolution,
  - local immutable preflight and range derivation,
  - latest-snapshot drift rejection,
  - staged download and verification status mapping,
  - validated conversion and staged cutover allowlist flow,
  - cancellation cleanup,
  - boundary-dependent recovery-action state including restart-normal and wipe-and-full-sync handling.
- Coordinator coverage already exists in `source/main/utils/chainStorageCoordinator.spec.ts` for:
  - explicit partial-sync preflight context,
  - serialization against bootstrap and chain-storage mutations,
  - node-stopped enforcement,
  - recovery-fallback rejection,
  - cancel and recovery delegation.
- IPC coverage already exists in `source/main/ipc/mithrilPartialSyncChannel.spec.ts` for:
  - idempotent request registration,
  - status caching and broadcast behavior,
  - start, cancel, restart-normal, and wipe-and-full-sync delegation,
  - active-state provider use.
- Startup and backend lifecycle coverage already exists in:
  - `source/main/utils/handleDiskSpace.spec.ts` for interrupted partial-sync startup gating and recovery-sensitive startup behavior,
  - `source/main/cardano/setup.spec.ts` for crash-restart suppression while partial sync is active.
- `source/main/utils/handleDiskSpace.spec.ts` still contains one brittle source-string regression assertion for the `installed-awaiting-node-start` fallback-suppression rule instead of a direct behavioral proof, matching the follow-up debt recorded in `.agent/plans/mithril-partial-sync/research/10-task-203-conversion-and-cutover-notes.md`.
- Renderer store coverage already exists in `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` for:
  - cached status sync,
  - backend-owned recovery-action replacement,
  - optimistic `stopping-node` start seed,
  - polling lifecycle,
  - teardown safety,
  - completed-overlay dismissal,
  - shared-channel single-flight behavior.
- Renderer component coverage already exists in:
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` for recommendation copy, CTA disabled states, confirmation-first start flow, and placeholder-free runtime copy,
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` for progress copy, backend-owned recovery buttons, completed dismissal, and locale placeholder checks.
- The current container coverage in `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts` is only a helper-level assertion for `shouldCloseDiagnosticsForPartialSyncOverlay(...)`; it does not yet verify the live injected dialog/action wiring.
- No `App.tsx` partial-sync-specific integration spec exists today, even though `source/renderer/app/App.tsx` is the global owner that mounts `MithrilPartialSyncOverlay` and forwards recovery callbacks and `dismissCompletedOverlay`.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-400.md`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts` or `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.tsx`
- `source/renderer/app/App.spec.tsx`
- `source/main/utils/handleDiskSpace.spec.ts`
- only if the coverage audit proves a real acceptance gap remains in other already-tested main-process seams:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/cardano/setup.spec.ts`
- implementation-phase tracking update expected:
  - `.agent/plans/mithril-partial-sync/research/17-task-400-automated-test-coverage-notes.md`
- post-implementation status update expected:
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`

## Implementation Approach

1. Start with a coverage audit against `task-400` acceptance instead of assuming all listed areas still need new tests.
   - Treat the existing main-process, IPC, store, diagnostics, and overlay specs as the base implementation.
   - Map each acceptance bullet to live spec evidence before adding anything.

2. Fill the renderer integration gap at the diagnostics dialog boundary.
   - Extend `DaedalusDiagnosticsDialog` coverage from the current pure helper assertion into one focused injected-container test.
   - Verify the dialog closes only when status transitions into an overlay-backed backend state, not from the optimistic `stopping-node` seed alone.
   - Keep the test seam small by mocking injected `stores` and `actions` rather than recreating the whole app shell first.

3. Add a committed focused app-shell test for the global overlay owner.
   - Create `source/renderer/app/App.spec.tsx` as the direct live seam for the global partial-sync overlay wiring that is currently unproven elsewhere.
   - Cover that `App.tsx` mounts `MithrilPartialSyncOverlay` from store state and forwards:
      - `cancelPartialSync`
      - `startPartialSync` for retry
      - `restartNormally`
      - `wipeAndFullSync`
      - `dismissCompletedOverlay`
   - Also verify the overlay stays unmounted when `mithrilPartialSync.shouldShowOverlay` is false so the app-shell ownership boundary is explicit.

4. Replace the known brittle startup string assertion with direct behavioral coverage.
   - Update `source/main/utils/handleDiskSpace.spec.ts` to remove the current `handleDiskSpace.toString()` regression assertion for `installed-awaiting-node-start`.
   - Replace it with a behavioral test that proves startup suppresses normal-start fallback after a failed first-start proof in the `installed-awaiting-node-start` marker state.
   - The replacement test should assert runtime outcomes, not implementation text. At minimum it should prove the startup path does not fall through to ordinary `cardanoNode.start()` fallback behavior when the marker is in the unsafe C1 path and the first-start proof fails.

5. Keep any additional backend or IPC additions conditional and minimal beyond the committed startup replacement above.
   - Only add further backend or IPC assertions if the audit shows an acceptance branch is implied by earlier work but not directly locked by a test.
   - The likely remaining additions, if any, should be explicit acceptance-mapping assertions rather than new production behavior.

6. Preserve the established manual-only diagnostics rule in renderer coverage.
   - Ensure any new renderer integration test keeps the confirmation-first start boundary and does not accidentally assert an automatic partial-sync trigger path.

7. Capture the task-400 coverage result as documentation, not just code.
   - Add a short research note during implementation that records which acceptance areas were already satisfied by existing specs and which focused tests were added by task-400.
   - This keeps later `task-401` planning honest and avoids repeating the same audit.

## Acceptance Criteria

- Main-process, IPC, and renderer automated coverage exists for the critical Mithril partial-sync paths identified in `task-400`.
- The accepted backend safety rules remain covered by direct tests, including at least one failure-mode assertion for each recovery option branch across the existing-plus-new spec set:
  - `retry`
  - `restart-normal`
  - `wipe-and-full-sync`
- Automated coverage replaces the brittle `handleDiskSpace.spec.ts` source-string assertion with direct behavioral proof for startup-owned `installed-awaiting-node-start` fallback suppression.
- Automated coverage explicitly verifies that diagnostics-launched partial sync remains manual and confirmation-first.
- Automated coverage explicitly verifies the diagnostics dialog closes only when partial sync reaches an overlay-backed backend status.
- Automated coverage explicitly verifies `App.tsx` global overlay wiring for partial-sync visibility and callback forwarding.
- The task completes with deterministic Jest coverage; no new E2E dependency is required unless a focused unit or integration seam proves insufficient.

## Verification Plan

- Run the focused Jest suites that already form the task-400 evidence set:
  - `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/utils/chainStorageCoordinator.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/cardano/setup.spec.ts source/renderer/app/stores/MithrilPartialSyncStore.spec.ts source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts source/renderer/app/App.spec.tsx`
- If implementation widens any main-process spec to close an acceptance gap, keep verification targeted to the touched files rather than running unrelated full suites.
- Record any truthful verification caveats, including unrelated pre-existing `yarn compile` failures outside this task, in the task-400 research note and implementation review.

## Risks And Open Questions

- The main scope-creep risk is rewriting or duplicating already-good tests instead of filling only the remaining acceptance gaps.
- Renderer integration tests around old React, MobX injection, and modal ownership can become brittle quickly; keep them narrow and ownership-focused.
- `App.tsx` is a wide integration surface, but this plan now commits to that seam because it is the real global overlay owner and the current repo has no equivalent direct proof elsewhere.
- The main backend gap is now explicit rather than implied: the `installed-awaiting-node-start` startup fallback rule must be proven behaviorally, not by source text.

## Final Outcome

- Outcome: completed and approved.
- Final implementation stayed within the approved minimal scope:
  - replaced the brittle startup source-string assertion with a behavioral test in `source/main/utils/handleDiskSpace.spec.ts`,
  - expanded `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts` to cover live close behavior,
  - added `source/renderer/app/App.spec.tsx` for global overlay ownership and callback forwarding,
  - repaired the existing `source/main/mithril/MithrilPartialSyncService.spec.ts` log-file assertion to use the stable command-runner seam.
- Final verification:
  - `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/utils/chainStorageCoordinator.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/cardano/setup.spec.ts source/renderer/app/stores/MithrilPartialSyncStore.spec.ts source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts source/renderer/app/App.spec.tsx`
  - Result: passed (10 suites, 121 tests).
- Review result:
  - Planning review approved via the latest entry in `.agent/plans/mithril-partial-sync/task-plans/task-400-plan-review.md`.
  - Implementation review approved via the latest entry in `.agent/plans/mithril-partial-sync/task-plans/task-400-impl-review.md`.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-400.md`.
- Preserve the expected review-log paths, but do not write the review-log files during planning.
- During implementation, add `.agent/plans/mithril-partial-sync/research/17-task-400-automated-test-coverage-notes.md` summarizing the acceptance-to-spec mapping and any new tests added.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- No `.agent/system/` documentation update is expected unless implementation unexpectedly introduces a durable new testing seam or workflow requirement.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-400-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-400-impl-review.md`

## Planning Self-Review

- Scope creep check: kept to coverage audit plus the smallest likely missing renderer integration seams.
- Critique closure check: interaction mode now uses an allowed label, the brittle `handleDiskSpace` source-string assertion is explicitly slated for behavioral replacement, and `App.tsx` overlay wiring is now a committed test seam rather than an optional branch.
- Stale workflow text check: verification references current Jest-focused workflow, not generic full-suite claims.
- Missing manifests/tests/docs check: called out the likely touched specs, required research note, and later task tracking update.
- Plan consistency check: acceptance, implementation approach, and verification all align with the live repo state that most backend and component coverage already exists.

## Planning Status

- `approved`

## Build Status

- `completed`
