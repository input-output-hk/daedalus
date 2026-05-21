# Task-400 Automated Test Coverage Notes

- Purpose: record the acceptance-to-spec coverage audit and the focused additions landed by `task-400`.
- Date: 2026-05-21

## Coverage Audit Result

- `task-400` was a gap-fill task, not a broad new test build-out. Most meaningful Mithril partial-sync coverage already existed before implementation in these suites:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/cardano/setup.spec.ts`
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

## Focused Additions Landed In Task-400

- `source/main/utils/handleDiskSpace.spec.ts`
  - Replaced the brittle `handleDiskSpace.toString()` assertion with a behavioral test for the `installed-awaiting-node-start` marker path.
  - The new test proves a failed startup proof attempt emits partial-sync failure state with wipe-only recovery and does not fall through to the normal startup fallback path.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts`
  - Added live container rerender coverage for the diagnostics close handoff.
  - The new assertions keep diagnostics open through the optimistic `stopping-node` seed and close only when the backend transitions into an overlay-backed status such as `preparing` or `failed`.
- `source/renderer/app/App.spec.tsx`
  - Added the previously missing app-shell seam for partial-sync overlay ownership.
  - The new spec proves `App.tsx` mounts the overlay only when `mithrilPartialSync.shouldShowOverlay` is true and forwards cancel, retry, restart-normal, wipe-and-full-sync, and completed-dismiss callbacks from the store.
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - Repaired the existing distinct-log-file assertion so it uses the current `runCommand` mock seam truthfully instead of a non-redefinable export spy.

## Verification Evidence

- Focused Jest evidence passed:
  - `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/utils/chainStorageCoordinator.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/cardano/setup.spec.ts source/renderer/app/stores/MithrilPartialSyncStore.spec.ts source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts source/renderer/app/App.spec.tsx`
- Result: passed on 2026-05-21 with 10 suites / 121 tests.

## Durable Findings

- The remaining task-400 gaps were integration seams, not missing production behavior.
- Renderer ownership around diagnostics handoff and app-shell overlay mounting is easiest to keep truthful with narrow container/root harnesses rather than widening production abstractions.
- Startup-owned recovery rules are safer to lock with behavioral tests than source-shape assertions because the latter hide regressions behind refactors.
