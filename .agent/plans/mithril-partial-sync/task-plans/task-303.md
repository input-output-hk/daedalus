# Task task-303: Integrate Progress And Error Overlay Reuse

## Task

- Task ID: `task-303`
- Title: `Integrate progress and error overlay reuse`

## Why This Task Now

- `task-300`, `task-301`, and `task-302` are complete, so the diagnostics CTA, confirmation boundary, and dedicated `stores.mithrilPartialSync` seam already exist.
- `task-204` is also complete, so the backend now exposes the real recovery-action truth through `allowedRecoveryActions`; the renderer no longer needs to guess which terminal actions are safe.
- Live-file verification shows the current renderer gap is now isolated to post-confirmation UX:
  - the diagnostics dialog can start partial sync,
  - the store can track active and terminal status,
  - but `App.tsx` still mounts no diagnostics-launched Mithril overlay,
  - and the existing reusable Mithril views remain bootstrap-biased in types, copy, and action wiring.
- This makes `task-303` the next smallest truthful slice: reuse the existing Mithril overlay internals where they already fit, add only the minimum generalization needed for diagnostics-launched partial sync, and keep bootstrap-only selection and startup decision concepts out of the reused path.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual validation before implementation can proceed: none.
- Required evidence back from the user during implementation: none.
- Manual QA remains later `task-401` work.

## Scope

- Mount a diagnostics-launched Mithril partial sync overlay outside the startup loading route.
- Reuse the existing Mithril progress presentation where it is already truthful:
  - overlay backdrop and card framing,
  - progress view layout,
  - step indicator,
  - elapsed-timer behavior,
  - starting-node handoff block.
- Reuse the existing Mithril error presentation where practical, but adapt it for partial-sync-only titles, hints, and recovery buttons.
- Keep bootstrap-only UI out of the partial sync flow:
  - no storage picker,
  - no snapshot selector,
  - no bootstrap accept or decline copy.
- Wire the overlay to the existing `MithrilPartialSyncStore` actions:
  - `cancelPartialSync`
  - `restartNormally`
  - `wipeAndFullSync`
  - retry through the existing `startPartialSync` path only when the backend still allows `retry`.
- Ensure recovery actions are rendered only from backend-owned `allowedRecoveryActions`.
- Ensure successful completion follows the live backend lifecycle truthfully:
  - keep the overlay mounted through terminal `completed`
  - provide an explicit renderer dismissal path from that completed state back to the normal app shell.
- Keep diagnostics as the owner until backend-confirmed progress or terminal status exists; do not dismiss diagnostics on the store's optimistic local `stopping-node` seed alone.
- Add focused renderer tests for the overlay handoff, reused progress mapping, and backend-owned recovery-action rendering.

## Non-Goals

- Do not merge bootstrap and partial sync into one shared store.
- Do not add new IPC channels or change shared partial-sync contracts unless a small type widening is unavoidable for renderer reuse.
- Do not generalize the entire Mithril bootstrap shell into a new framework for hypothetical future flows.
- Do not change the startup-owned empty-chain bootstrap flow or its routing.
- Do not rework diagnostics confirmation ownership beyond the minimum handoff needed to avoid stacked modal owners.
- Do not add Storybook stories, theme follow-through, or manual QA scope here; those remain later `task-304` and `task-401` work.
- Do not let renderer logic infer recovery safety from status names instead of backend `allowedRecoveryActions`.

## Dependencies

- Required completed dependencies:
  - `task-204`
  - `task-300`
  - `task-302`
- Supporting completed prerequisites used directly by this plan:
  - `task-100`
  - `task-101`
  - `task-103`
  - `task-301`
- This task directly supports:
  - `task-304`
  - renderer verification in `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
- `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/design-progress-view-composition.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/frontend.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` skill guidance was loaded for the required non-trivial renderer understanding pass, and every material plan claim was then verified against live files.
- `i18n-messaging` guidance materially affects this plan because partial-sync-specific overlay and recovery copy is likely required even with component reuse.

## Live Repo Findings Verified For Planning

- `source/renderer/app/App.tsx` still mounts only the standard dialogs and global overlays. There is no diagnostics-launched Mithril partial sync overlay owner yet.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` now owns confirmation locally and calls `onStartMithrilPartialSync()` on confirm, but it does not hand off to any global overlay or close the diagnostics dialog afterward.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` remains the actual `ReactModal` owner, so leaving it open while a new global overlay appears would create an avoidable stacked-owner state.
- `MithrilPartialSyncStore.startPartialSync()` immediately seeds `status: 'stopping-node'` before the start IPC resolves, so `isActive` becomes true even when the main-process start request may still reject on launcher kill-switch or preflight checks.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` already exposes the exact renderer truth this task needs:
  - `status`
  - `isActive`
  - `isWorking`
  - `isTerminal`
  - `progressItems`
  - transfer telemetry fields
  - backend-owned recovery-action booleans and action methods.
- Live backend success does not reset partial sync to `idle`. `handleDiskSpace.ts` emits `starting-node`, waits the 6-second handoff delay, clears the marker, then emits terminal `completed`. Service-owned `idle` resets are for recovery and cleanup flows, not the success handoff.
- `source/common/types/mithril-partial-sync.types.ts` already reuses `MithrilProgressItem` from bootstrap shared types, which means the backend progress-item model is intentionally aligned with the existing waterfall UI.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` and `MithrilStepIndicator.tsx` are mostly presentational and are close to reusable, but they are still typed around bootstrap statuses and currently do not know about partial-sync-only `installing`.
- `MithrilProgressView.tsx` currently derives elapsed time from `bootstrapStartedAt`, while partial sync surfaces `elapsedSeconds`; task-303 therefore needs a small timer-seeding seam instead of assuming the bootstrap prop shape carries over directly.
- `MithrilStepIndicator.tsx` already uses the same 3-step visible model the PRD wants for partial sync reuse, so the smallest truthful change is to widen status handling rather than fork the whole stepper.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx` is the main bootstrap-specific blocker:
  - its error stage copy is bootstrap-named,
  - its action buttons are hard-coded to `Wipe chain & retry` and `Sync from genesis`,
  - it therefore needs either a small presentational generalization or a thin partial-sync wrapper that injects the correct labels and actions.
- Existing focused specs already cover the reused bootstrap views, so targeted extension is practical:
  - `MithrilProgressView.spec.tsx`
  - `MithrilBootstrap.spec.tsx`
  - `DaedalusDiagnostics.spec.tsx`
- No `.understand-anything/knowledge-graph.json` is currently present in the repo, so this planning pass relied on loaded skill guidance plus live-file verification rather than a prebuilt graph artifact.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-303.md`
- `source/renderer/app/App.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx`
- one thin partial-sync overlay wrapper adjacent to the existing Mithril loading components, likely under `source/renderer/app/components/loading/mithril-bootstrap/`
- focused renderer specs, likely including:
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx`
  - one new partial-sync overlay spec
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` or a small dialog/container spec if the diagnostics-close handoff is container-owned
- likely i18n artifacts if new partial-sync-specific overlay copy is added:
  - `translations/messages.json`
  - `source/renderer/app/i18n/locales/defaultMessages.json`
  - `source/renderer/app/i18n/locales/en-US.json`
  - `source/renderer/app/i18n/locales/ja-JP.json`
- `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md`

## Files Changed

- `.agent/plans/mithril-partial-sync/task-plans/task-303.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-303-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-303-impl-review.md`
- `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `source/renderer/app/App.tsx`
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
- `translations/messages.json`
- `source/renderer/app/i18n/locales/defaultMessages.json`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`

## Implementation Approach

1. Add one thin global overlay owner in `App.tsx`.
   - Mount the partial-sync overlay from a truthful visibility seam, not raw `isActive`.
   - The smallest truthful owner is a computed condition that includes backend-confirmed working and terminal display states such as `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, `starting-node`, `failed`, `cancelled`, and `completed`.
   - Exclude the store's optimistic pre-request `stopping-node` seed from the global app-level overlay owner so immediate start rejection does not tear diagnostics down prematurely.
   - Keep the owner minimal and declarative so the app shell only decides whether to mount the overlay, not how the overlay interprets backend states.
   - This satisfies the PRD requirement that progress or error UI can appear outside the startup-only loading route.

2. Avoid stacked modal ownership during handoff.
   - Keep `DaedalusDiagnosticsDialog` open through the optimistic `stopping-node` start seed.
   - Close diagnostics only once backend-confirmed partial-sync progress or terminal state arrives, so preflight and launcher-gate rejection can fall back into the existing diagnostics owner instead of leaving no persistent surface.
   - Keep the task-302 confirmation contract otherwise intact; do not move confirmation ownership into the store or `App.tsx`.
   - The smallest truthful place for this handoff remains `DaedalusDiagnosticsDialog.tsx`, but it should observe a backend-confirmed display status list rather than `mithrilPartialSync.isActive` alone.
   - If start rejects before any backend-confirmed status arrives, diagnostics should remain open and continue to own the error or ready-state feedback on the next status refresh.

3. Reuse the existing progress view by widening it slightly instead of forking it.
   - Extend `MithrilProgressView` to accept either bootstrap or partial-sync working statuses, plus small copy overrides for title, subtitle, cancel label, and starting-node handoff text.
   - Add the smallest timer seam needed for partial sync, likely by letting the view accept either `bootstrapStartedAt` or an already-derived `elapsedSeconds` seed for non-bootstrap callers.
   - Keep the timer and overall layout unchanged.
   - Keep the cancel button as the only active-progress action.
   - Do not pull bootstrap decision or selection props into the partial-sync path.

4. Reuse the existing step indicator with the smallest status widening.
   - Teach `MithrilStepIndicator` to accept partial-sync status values in addition to bootstrap values.
   - Map partial-sync `installing` into the existing visible `Finalizing` step.
   - Keep the shared 3-step visible model: `Preparing`, `Downloading`, `Finalizing`.
   - Reuse existing `progressItems` as-is, since partial sync already shares that payload shape.
   - Do not create a second waterfall implementation.

5. Reuse the error UI through a thin presentational generalization.
   - Convert `MithrilErrorView` from bootstrap-hard-coded actions into a view that can render injected actions and variant-appropriate title or hint mapping.
   - Keep bootstrap behavior unchanged by passing its existing wipe-and-retry and decline actions from `MithrilBootstrap.tsx`.
   - Add a partial-sync wrapper that translates backend terminal state into the correct button set:
     - render `Retry Mithril partial sync` only when `canRetry`
     - render `Restart normally` only when `canRestartNormally`
     - render `Wipe chain data and do full Mithril sync` only when `canWipeAndFullSync`
   - Treat `failed` and `cancelled` as terminal recovery surfaces driven by the same backend action availability rules instead of inventing renderer-only cancel semantics.

6. Keep partial-sync-specific orchestration in one thin wrapper component.
   - Add a wrapper such as `MithrilPartialSyncOverlay` adjacent to the reused Mithril loading components.
   - That wrapper should do only three jobs:
     - choose progress vs terminal view from `stores.mithrilPartialSync.status`
     - pass partial-sync-specific copy and handlers into the reused presentational pieces
     - reuse the existing overlay backdrop or card styling so the new UI remains visually consistent.
   - Do not route partial sync through the full `MithrilBootstrap` shell, because that shell still owns bootstrap-only decision and storage states.

7. Preserve the success handoff to normal sync UX.
   - Keep the overlay mounted through `starting-node` so the user sees the same explicit handoff block that bootstrap already uses.
   - Keep the overlay mounted for terminal `completed` as well, because that is the live success state emitted after the startup-owned 6-second node-start proof handoff.
   - Provide one explicit renderer dismissal path from `completed` back to the normal app shell, most likely a lightweight close or continue action owned by the partial-sync overlay wrapper rather than a backend status reset.
   - Do not invent a fake post-success `idle` transition in the renderer.
   - Do not add renderer-owned timers or extra completion transitions beyond what the backend and existing progress view already expose.

8. Keep the copy truthful and minimal.
   - Reuse bootstrap strings only when they are still accurate for diagnostics partial sync.
   - Add new partial-sync-specific labels where bootstrap wording would be misleading, especially for:
     - overlay title
     - retry or restart action labels
     - wipe-and-full-sync wording
     - error-stage hints if they mention bootstrap or genesis sync specifically.
   - Run `yarn i18n:manage` if new strings are added, and polish locale outputs so no new runtime `!!!` placeholders ship in the touched overlay copy.

9. Bias tests toward the real ownership boundaries.
   - Add one focused spec for the partial-sync overlay wrapper or app-shell mounting seam.
   - Extend reused-view specs only where status widening or action generalization needs direct protection.
   - Keep tests centered on backend-owned recovery action rendering and the diagnostics-to-overlay handoff, not on speculative shared abstractions.

## Acceptance Criteria

- After confirmation, diagnostics-launched Mithril partial sync shows a global overlay outside the startup loading route.
- The diagnostics dialog remains the owner during the optimistic start request window and is not dismissed on a start attempt that rejects before backend-confirmed progress begins.
- Once backend-confirmed progress or terminal display state exists, the diagnostics dialog is not left open behind the partial-sync overlay.
- The partial-sync overlay does not show bootstrap-only storage or snapshot-selection UI.
- Active partial-sync states reuse the Mithril progress presentation with truthful partial-sync status mapping, including `installing` inside the visible `Finalizing` step.
- The active progress surface exposes a cancel action only during the working phase where the backend still accepts cancellation.
- Terminal partial-sync states render recovery actions only from backend-owned `allowedRecoveryActions`.
- Terminal recovery actions can expose:
  - retry partial sync
  - restart normally
  - wipe chain data and perform full Mithril sync
  only when the backend has allowed them.
- On success, the overlay remains through `starting-node` and terminal `completed`, matching the live backend lifecycle.
- The renderer provides an explicit way to dismiss the completed overlay and return to the normal app shell without inventing a fake backend `idle` success transition.
- The implementation preserves the existing empty-chain Mithril bootstrap flow without behavior regression.
- Focused renderer coverage exists for the diagnostics-to-overlay handoff, progress reuse mapping, and terminal recovery-action rendering.

## Verification Plan

- Run focused Jest coverage for the touched renderer seams, likely including:
  - the new partial-sync overlay wrapper spec
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` and-or a focused diagnostics-dialog handoff spec if the close behavior is container-owned.
- Verify directly that:
  - partial sync overlay mounts only from backend-confirmed display states, not the optimistic `stopping-node` seed alone
  - diagnostics stays open when a start attempt rejects before backend-confirmed progress begins
  - diagnostics closes once backend-confirmed partial-sync progress or terminal display state arrives
  - the reused progress view renders for partial-sync working states
  - partial-sync `installing` maps to the visible `Finalizing` step
  - partial-sync timer reuse is seeded truthfully from partial-sync `elapsedSeconds` or equivalent derived data
  - cancel is wired only for the active-progress surface
  - failed and cancelled terminal states render only the backend-allowed recovery actions
  - retry routes back through `startPartialSync()` only when the backend allows retry
  - restart-normal and wipe-full-sync call the store methods exactly once when present
  - success and `starting-node` handoff keep the overlay mounted through terminal `completed`
  - dismissing the completed overlay returns the user to the normal app shell without mutating backend status semantics
  - bootstrap tests still pass after any view generalization.
- Run `yarn i18n:manage` if any new partial-sync-specific messages are added.
- If repo-wide `yarn compile` remains blocked by unrelated existing issues, record that explicitly and rely on focused renderer verification plus diff inspection.

## Risks And Open Questions

- The main scope-creep risk is over-generalizing the bootstrap shell instead of adding one thin partial-sync wrapper plus small presentational widenings.
- A second risk is leaving diagnostics open behind the new overlay, which would create modal-owner confusion and make focus behavior harder to reason about.
- A second risk is dismissing diagnostics too early from the store's optimistic `stopping-node` seed, which would hide immediate start or preflight rejection feedback.
- A third risk is accidentally letting renderer code infer safe terminal actions from visible status text instead of `allowedRecoveryActions`.
- Existing bootstrap error copy is too bootstrap-specific, so the implementation must be careful not to reuse misleading labels such as `Sync from genesis` for diagnostics partial sync.
- Partial sync terminal `cancelled` and `completed` behaviors should be treated as backend-owned terminal surfaces. The renderer should not auto-dismiss them by assuming a later `idle` success reset that does not exist.
- Open implementation choice to keep honest during coding:
  - if `MithrilErrorView` can be generalized with a tiny `actions` array and variant-aware copy mapping, prefer that;
  - if that generalization starts to distort bootstrap behavior, keep a smaller shared layout seam and a dedicated partial-sync terminal wrapper instead.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-303.md`.
- Preserve review-log paths for planning and implementation review, but do not write those review logs during planning.
- Add `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md` after implementation to capture the approved app-shell handoff, presentational reuse boundary, and any i18n nuance carried into `task-304`.
- That post-implementation note should explicitly record the backend-confirmed handoff rule for start rejection and the terminal `completed` dismissal contract so later renderer tasks do not regress them.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- No `.agent/system/architecture.md` update is expected unless the implementation introduces a broader durable renderer overlay seam beyond the minimal `App.tsx` mount and diagnostics-close handoff planned here.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-303-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-303-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Added a thin diagnostics-launched partial-sync overlay owner at the app shell that mounts only for backend-confirmed display statuses and never for the optimistic local `stopping-node` seed.
- Kept `DaedalusDiagnosticsDialog` as the modal owner until a backend-confirmed overlay state arrives, preserving truthful fallback behavior when start or preflight rejection occurs before progress begins.
- Reused the Mithril progress card, timer, and step indicator with minimal widening for partial-sync statuses, including `installing` mapped into visible `Finalizing` and direct `elapsedSeconds` support.
- Reused the Mithril error presentation through injected copy and backend-owned action rendering, so retry, restart-normal, and wipe-full-sync buttons appear only when `allowedRecoveryActions` permits them.
- Kept success mounted through `starting-node` and terminal `completed`, then dismissed it with an explicit renderer-owned continue action rather than inventing a fake backend `idle` success reset.
- Avoided misleading transfer telemetry by disabling the bootstrap byte-progress footer for the partial-sync path, because current partial-sync download telemetry is file-count based rather than byte-total based.
- Polished the new runtime locale entries in `en-US` and `ja-JP` so the shipped overlay copy does not contain `!!!` placeholders.

## Verification Outcome

- `yarn i18n:manage`
- `yarn test:jest "source/renderer/app/stores/MithrilPartialSyncStore.spec.ts" "source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx" "source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx" "source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts"`
- Focused Jest passed after the final review fixes, including coverage for overlay gating, completed dismissal, diagnostics-close handoff helper, placeholder-free locale strings, and suppression of the bootstrap byte-progress footer on the partial-sync path.

## Self-Review

- Scope-creep check: the plan keeps Storybook, theme work, shared-store fusion, IPC contract redesign, and bootstrap-flow rewrites out of `task-303`.
- Workflow freshness check: frontend, test, doc-update, understand, and i18n guidance were applied only where they materially affect this renderer overlay reuse task.
- Missing-tests/docs check: the plan explicitly requires focused overlay and reuse tests, review-log paths, and a post-implementation research note instead of leaving those follow-ups implicit.
- Consistency check: the revised plan now matches the PRD, the locked task graph, the critique findings, the live store start-seed behavior, and the startup-owned success lifecycle that ends at terminal `completed` rather than a success reset to `idle`.
