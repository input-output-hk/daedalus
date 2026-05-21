Implementation: Iteration 1
Timestamp: 2026-05-21T16:03:07Z

Changes made
- Added a thin global partial-sync overlay owner in `source/renderer/app/App.tsx` wired to `stores.mithrilPartialSync.shouldShowOverlay`.
- Added `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` to reuse existing Mithril progress and error presentation for diagnostics-launched partial sync.
- Extended `MithrilPartialSyncStore` with backend-confirmed overlay visibility semantics and explicit completed-overlay dismissal, while excluding the optimistic `stopping-node` seed from overlay ownership.
- Updated `DaedalusDiagnosticsDialog.tsx` to close only when partial sync transitions into a backend-confirmed overlay-backed status, and extracted a pure helper for that handoff rule.
- Widened `MithrilProgressView.tsx` and `MithrilStepIndicator.tsx` for partial-sync status reuse, including `installing` -> visible `Finalizing` mapping and elapsed-seconds input support.
- Generalized `MithrilErrorView.tsx` to accept injected title, hint, and action definitions so bootstrap behavior stays intact while partial sync gets backend-driven recovery buttons.
- Added partial-sync-specific copy in `MithrilBootstrap.messages.ts` and synced i18n artifacts with `yarn i18n:manage`.
- Added focused specs for overlay status gating and dismiss behavior, progress view prop changes, diagnostics close handoff helper, and store-level overlay visibility semantics.

Files touched
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

Verification run
- `yarn i18n:manage`
- `yarn test:jest "source/renderer/app/stores/MithrilPartialSyncStore.spec.ts" "source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx" "source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx" "source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts"`

Deviations from approved plan
- Kept diagnostics-close coverage at a pure helper seam instead of a mounted container test because the injected decorated class is brittle to instantiate directly in this repo’s test setup. The behavior under test is unchanged.
- Reused the existing bootstrap SCSS wrapper classes for the partial-sync overlay instead of adding a new stylesheet, which kept the solution smaller than the plan’s file estimate.

User interaction required
- none

Outcome
- Renderer now shows a diagnostics-launched Mithril partial-sync overlay only after backend-confirmed display states, preserves diagnostics ownership through immediate start rejection, reuses Mithril progress and error surfaces with partial-sync-specific copy and recovery actions, and keeps success visible through terminal `completed` until explicit dismissal.

Implementation: Iteration 2
Timestamp: 2026-05-21T16:07:07Z

Changes made
- Removed misleading bootstrap transfer-bar reuse from the partial-sync path by adding an explicit `showDownloadProgressBar` seam and disabling it in `MithrilPartialSyncOverlay`, so partial sync no longer treats file counts as byte totals.
- Added focused overlay coverage proving the bootstrap combined byte-progress footer is not rendered for partial sync file-count telemetry.
- Replaced all newly added `loading.mithrilPartialSync.*` runtime locale placeholders with polished `en-US` copy and translated `ja-JP` copy.
- Kept the diagnostics-close ownership helper and the rest of the overlay wiring unchanged.

Files touched
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`

Verification run
- `yarn test:jest "source/renderer/app/stores/MithrilPartialSyncStore.spec.ts" "source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx" "source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx" "source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx" "source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts"`

Deviations from approved plan
- none beyond the already recorded helper-level diagnostics ownership test seam.

User interaction required
- none

Outcome
- Partial-sync progress reuse is now truthful for the available telemetry model, and the newly introduced runtime strings are no longer shipping as placeholder values.

Code Review: Iteration 2
Timestamp: 2026-05-21T16:08:31Z

Blocking findings:
- None.

Non-blocking observations:
- Prior blocker 1 is resolved. `MithrilPartialSyncOverlay.tsx` now disables bootstrap download-bar reuse via `showDownloadProgressBar={false}`, so the partial-sync overlay no longer renders misleading byte-based transfer telemetry from file-count inputs. The new spec in `MithrilPartialSyncOverlay.spec.tsx` locks that behavior.
- Prior blocker 2 is resolved. The new `loading.mithrilPartialSync.*` runtime entries in `en-US.json` and `ja-JP.json` are now polished and no longer ship as `!!!` placeholders, with focused test coverage for that invariant.
- I did not find a new material regression in the touched renderer ownership, overlay gating, backend-owned recovery-action rendering, or completed-dismiss flow.
- The planned follow-up note `.agent/plans/mithril-partial-sync/research/15-task-303-overlay-reuse-notes.md` still appears to be missing, so the handoff and completed-dismiss contract is not yet captured in the research/docs trail.

Approval bar:
- None for code approval on `task-303`.
- Add the missing research note when the plan workspace is next updated so later tasks have the intended renderer handoff contract recorded.

Decision: approved

