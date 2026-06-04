Planner: Iteration 1
Timestamp: 2026-05-21T15:47:53Z

Task: `task-303` | `Integrate progress and error overlay reuse`

Summary
- Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-303.md`.
- Selected interaction mode: `autonomous`.
- Planning status: `approved`.
- Build status: `not started`.

Docs/Workflows/Skills Consulted
- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
- `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/design-progress-view-composition.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill loaded; key findings verified against live files
- `i18n-messaging` skill loaded because partial-sync-specific overlay copy is likely required

Key Verified Repo Findings
- `App.tsx` has no diagnostics-launched partial-sync overlay owner yet.
- `DaedalusDiagnostics.tsx` now confirms and starts partial sync, but does not hand off to a global overlay.
- `DaedalusDiagnosticsDialog.tsx` is still the live `ReactModal` owner, so task-303 must avoid leaving it open behind a new overlay.
- `MithrilPartialSyncStore.ts` already has the required renderer truth and backend-owned recovery gating.
- `MithrilProgressView.tsx` and `MithrilStepIndicator.tsx` are close to reusable with small status widening.
- `MithrilErrorView.tsx` is bootstrap-specific and needs a small generalization or thin partial-sync wrapper.
- Existing empty-chain bootstrap flow can remain isolated if task-303 uses a thin partial-sync wrapper instead of routing through the full bootstrap shell.

Plan
- Add a minimal app-level partial-sync overlay mount in `source/renderer/app/App.tsx`.
- Close diagnostics when partial sync becomes active to avoid stacked modal ownership.
- Reuse existing Mithril progress UI with small type/status widening, mapping partial-sync `installing` into visible `Finalizing`.
- Reuse error UI through a minimal presentational generalization or a thin partial-sync terminal wrapper.
- Keep backend-owned `allowedRecoveryActions` as the only source of truth for retry/restart/wipe button rendering.
- Keep success mounted through `starting-node`, then clear naturally when status returns to `idle`.
- Add focused renderer tests for diagnostics-to-overlay handoff, progress reuse mapping, and terminal recovery-action rendering.
- Run `yarn i18n:manage` only if new overlay strings are introduced.

Expected Files
- `.agent/plans/mithril-partial-sync/task-plans/task-303.md`
- `source/renderer/app/App.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx`
- one thin partial-sync overlay wrapper under the Mithril loading component area
- focused renderer specs
- i18n artifacts if new strings are added
- `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md`

Risks/Open Questions
- Main risk is over-generalizing bootstrap infrastructure instead of keeping a thin partial-sync wrapper.
- Another risk is rendering recovery actions from status names instead of backend `allowedRecoveryActions`.
- Error copy reuse must not leak bootstrap-specific wording like `Sync from genesis` into diagnostics partial sync.

Outcome
- Canonical plan doc added and aligned with the PRD, task graph, prior renderer task notes, required workflows, and live renderer files.
- No review log written in planning, per instruction.
- Self-review passed for scope control, workflow freshness, docs/tests coverage, and plan consistency.

Critiquer: Iteration 1
Timestamp: 2026-05-21T15:52:14Z

Blocking findings:
- The success teardown contract in the plan is wrong. `task-303.md` says the overlay should clear when partial sync returns to `idle` and ties the global owner to `stores.mithrilPartialSync.isActive` (`.agent/plans/mithril-partial-sync/task-plans/task-303.md:148-151`, `188-191`, `220`). Live code does not return to `idle` on success. The startup-owned handoff emits `starting-node`, waits 6 seconds, then emits `completed` and stops there (`source/main/utils/handleDiskSpace.ts:286-315`). The only `idle` resets in `MithrilPartialSyncService` are recovery paths such as `restartNormal`, `wipeAndFullSync`, and final wipe cleanup (`source/main/mithril/MithrilPartialSyncService.ts:372-396`, `675-690`). If task-303 is implemented as planned, an app-level overlay mounted from `isActive` will stay up indefinitely after a successful partial sync. The plan needs a truthful success-clear contract before approval.
- The diagnostics-close handoff is under-specified and unsafe for start/preflight rejection. The plan proposes closing `DaedalusDiagnosticsDialog` when partial sync becomes active (`.agent/plans/mithril-partial-sync/task-plans/task-303.md:153-157`). But `MithrilPartialSyncStore.startPartialSync()` marks the renderer active immediately by setting `status: stopping-node` before the start IPC resolves (`source/renderer/app/stores/MithrilPartialSyncStore.ts:221-245`). The corresponding main-process start request is long-running and can still reject before any backend status is emitted, including launcher kill-switch rejection and preflight/layout failures (`source/main/ipc/mithrilPartialSyncChannel.ts:137-142`, `source/main/utils/chainStorageCoordinator.ts:223-266`). In that case the store falls back to `idle` on the post-request sync, so a naive "close on active" handoff can dismiss diagnostics and leave no persistent error surface. The plan needs an explicit same-owner fallback for immediate start rejection before it is safe to approve.

Non-blocking observations:
- The timer reuse seam is missing from the plan. `MithrilProgressView` currently derives elapsed time from `bootstrapStartedAt` (`source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx:17-27`, `71-88`), while partial sync exposes `elapsedSeconds` instead (`source/common/types/mithril-partial-sync.types.ts:39-50`, `source/renderer/app/stores/MithrilPartialSyncStore.ts:46-53`). This is still solvable inside task-303, but the implementation notes should name the approach.
- The plan’s “small generalization or thin wrapper” direction for `MithrilErrorView` is still right, but live code is fully bootstrap-specific today: prop names and button labels are hard-coded to bootstrap recovery (`source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx:17-22`, `124-136`). A thin partial-sync wrapper remains the lower-risk simplification.

Approval bar:
- Update the plan so overlay unmount/success behavior matches the live backend status lifecycle.
- Specify how diagnostics remains the truthful owner for immediate start rejection, or specify an equivalent handoff that still preserves user-visible failure feedback before backend progress begins.
- No broader IPC or architecture rewrite is required once those two seams are corrected.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-21T15:54:43Z

Task: `task-303` | `Integrate progress and error overlay reuse`

Summary
- Re-read the canonical task plan, the planning review log, and the live renderer/main-process lifecycle files cited by critique.
- Revised `.agent/plans/mithril-partial-sync/task-plans/task-303.md` to fix both blockers.
- Interaction mode remains `autonomous`.
- Planning status remains `approved`.
- Build status remains `not started`.

Blocker Fixes Applied
- Corrected the success teardown contract to match live backend behavior:
  - partial sync success remains mounted through `starting-node` and terminal `completed`
  - the plan no longer assumes a success reset to `idle`
  - completed overlay dismissal is now an explicit renderer action rather than a fake backend lifecycle assumption
- Corrected the diagnostics-to-overlay handoff:
  - the plan no longer closes diagnostics on the store’s optimistic local `stopping-node` seed
  - diagnostics remains the owner until backend-confirmed progress or terminal display state exists
  - immediate start/preflight rejection now truthfully falls back into the existing diagnostics owner instead of risking a dismissed dialog with no persistent error surface

Live Findings Re-Verified
- `MithrilPartialSyncStore.startPartialSync()` seeds `status: stopping-node` before the start IPC resolves, so `isActive` alone is not a safe overlay handoff trigger.
- `chainStorageCoordinator.startPartialSync()` can still reject during launcher gating and preflight/layout checks before backend progress begins.
- `handleDiskSpace.ts` drives successful post-cutover handoff as `starting-node` followed by terminal `completed`, not `idle`.
- `MithrilPartialSyncService._resetToIdleStatus()` is used for recovery/cleanup paths, not for the successful startup-owned completion handoff.
- `MithrilProgressView.tsx` still uses `bootstrapStartedAt`, while partial sync currently exposes `elapsedSeconds`, so the plan now names a small timer-seeding seam explicitly.

Plan Changes
- App-level overlay mounting is now tied to backend-confirmed display statuses instead of raw `mithrilPartialSync.isActive`.
- Diagnostics close handoff is now planned in `DaedalusDiagnosticsDialog.tsx` only after confirmed backend progress/terminal display state arrives.
- Success acceptance criteria now require the overlay to remain mounted through terminal `completed`.
- Verification now requires tests for:
  - start rejection preserving diagnostics ownership
  - overlay mounting only after backend-confirmed display states
  - completed-state dismissal returning the user to the normal app shell
  - truthful timer reuse from partial-sync timing data

Outcome
- The canonical task plan now matches the critique, the live store start path, and the startup-owned partial-sync success lifecycle.
- The plan remains intentionally small:
  - thin app-level overlay owner
  - thin partial-sync wrapper around reused Mithril views
  - no broader IPC or architecture rewrite
- Follow-up implementation should preserve both corrected contracts:
  - no early diagnostics dismissal on optimistic start
  - no assumed success reset to `idle` after `completed`

