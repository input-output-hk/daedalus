# Mithril Snapshot UX Refinement

## Overview

- Decompose monolithic `MithrilBootstrap.tsx` into focused sub-components following the `SyncingConnecting` pattern
- Add multi-step visual stepper modeled on `SyncingProgress`, then follow it with truthful post-download restore telemetry instead of a hidden 90% plateau
- Surface download metadata (bytes transferred, throughput, timing)
- Implement stage-specific error screens
- Allow custom chain storage location via directory picker with symlink-based redirection
- Migrate hardcoded colors to CSS theme variables
- Extend backend types to carry `filesDownloaded`, `filesTotal`, and error `stage` through IPC
- Full Storybook and E2E test coverage

## Requirements

### Backend
- [x] Extend shared types with `filesDownloaded`, `filesTotal` on status updates and `stage` on errors
- [x] Update progress pipeline to preserve raw file counts and annotate errors with failure stage
- [x] Add MobX computed properties for `bytesDownloaded` and `throughputBps`
- [ ] Refine the Mithril service/status model for the planned `preparing -> downloading -> verifying -> finalizing` UX without surfacing Mithril JSON message strings as UI copy
  - Keep the download-phase progress bar scoped to the file download work and scale it from 0 to 100 inside `downloading`
  - Transition into `verifying` when the exposed file download work completes
  - Investigate honest `_installSnapshot()` / `fs.move()` progress exposure for final local install work; fall back to activity-only `finalizing` if measured progress is not reliable

### Custom Chain Storage Location
- [x] Add `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` and `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL` IPC channels
- [x] Implement `ChainStorageManager` with:
  - [x] Directory validation (writable, sufficient space, not inside stateDir)
  - [x] Symlink creation (`{stateDir}/chain` -> user-selected directory) during `setDirectory`
  - [x] Symlink removal for reset-to-default flow
  - [x] Config persistence to `{stateDir}/chain-storage-config.json`
  - [x] Data migration between directories (`migrateData` with cross-device fallback)
- [x] Integrate with `handleDiskSpace.ts` to check space on resolved symlink target
- [x] Pass custom directory as `workDir` to `MithrilBootstrapService` so downloads go directly to target
- [x] Add `ChainStorageConfig` and `ChainStorageValidation` shared types
- [x] Add validate-only chain storage IPC for non-destructive picker feedback
- [x] Add rollback safeguards in `setDirectory` for symlink-switch failures
- [x] Verify symlink integrity on app startup; warn if broken

### Component Decomposition
- [x] Decompose `MithrilBootstrap.tsx` into 6+ sub-components
- [x] Implement multi-step visual stepper with spinner/checkmark/pending states
- [x] Surface download metadata in progress view
- [x] Stage-specific error screens with contextual titles, hints, and actions
- [x] Extract snapshot selector and details grid into reusable components
- [x] Expose post-download local install work as an explicit progress step so the 90% plateau matches real backend work
  - Download step covers Mithril transfer plus the client's built-in certificate/signature verification
  - Post-download local work centers on `_installSnapshot()` moving/copying the restored DB into `chain` or the resolved custom chain target
  - Transfer stats stop once network download ends; local-processing copy replaces them until follow-up task-024a collapses that work into the single visible finalizing phase
- [x] Collapse the visible Mithril progress model to `preparing -> downloading -> finalizing`
  - Keep Mithril verification inside the download phase and retain `verify` only for error staging
  - Treat `_installSnapshot()` db-to-chain movement plus cleanup/handoff as the single visible finalizing phase
  - Keep the Mithril stepper visible through the end of post-download work instead of switching straight to node sync
- [x] Rename install-related post-download state/copy to `unpacking` where internal sub-phases are still needed
  - Keep the visible UX mapped to finalizing even if service/message details still distinguish post-download work internally
  - Replace install wording in post-download progress/detail copy so it matches snapshot materialization more closely
- [x] Derive localized step labels from `status` in the renderer; do not transport user-facing copy in `currentStep`
- [ ] Promote the planned visible Mithril progress model to `preparing -> downloading -> verifying -> finalizing`
  - Keep `Downloading` as the label for the determinate transfer phase and reserve the progress bar for that step only
  - Transition into `Verifying` once the exposed file download completes; keep that step indeterminate unless Mithril later exposes truthful verification progress
  - Reword Daedalus-side post-download `unpacking` to `installing` where it describes local DB placement work
- [ ] Investigate `_installSnapshot()` / `fs.move()` progress exposure for finalizing
  - Prefer honest measured install progress only when it can be derived without inventing percentages for same-device moves, renames, or symlink-target handoff
  - If explicit install telemetry is not reliable, keep `Finalizing` minimal and indeterminate like `Verifying`
- [ ] Confirm the extracted i18n messages are appropriate for the exposed `Preparing`, `Downloading`, `Verifying`, and `Finalizing` steps
  - Do not route Mithril JSON step messages into the renderer as user-facing copy
- [ ] Add a11y attributes (`role="progressbar"`, `aria-live`, `aria-label`, keyboard nav)
- [x] `BlockDataStorageLocationPicker` as first screen in bootstrap flow (before decision view)
  - Shows current path, available space, validation feedback
  - Uses the latest available snapshot size as the storage estimate when that metadata is already loaded; otherwise falls back to a generic large-space warning
  - Directory picker via `SHOW_OPEN_DIALOG_CHANNEL`
  - "Reset to default" and "Continue" actions
  - Disabled during active bootstrap
 - [x] Allow returning from the Mithril decision view to the block-data storage picker to choose a better location before continuing

### i18n (follow i18n-messaging skill)
- [x] Extract all messages into `MithrilBootstrap.messages.ts`
- [x] Add ~14 new messages for errors, download metadata, steps, and storage picker
- [x] Run `yarn i18n:manage` to sync translations

### Theming (follow theme-management skill)
- [ ] Migrate hardcoded RGBA values to `--theme-mithril-*` CSS variables
- [ ] Add variables to `createTheme` with light/dark values
- [ ] Run `yarn themes:check:createTheme` and `yarn themes:update`
- [ ] Run `yarn typedef:sass` after SCSS changes

### Storybook (follow storybook-creation skill)
- [ ] Create `storybook/stories/loading/` domain with 16 stories
- [ ] Register in `storybook/stories/index.ts`

### E2E Tests (follow e2e-test-creation skill)
- [ ] Create `tests/mithril/` domain with Gherkin features and step definitions
- [ ] Cover decision, progress, error, and storage location flows

## Technical Design

### Components Affected

**Shared Types:**
- `source/common/types/mithril-bootstrap.types.ts` - Add file counts to status, stage to errors, chain storage types
- Follow-up Phase 6 extends Mithril status payloads again to support a visible `verifying` phase and any honest local-install telemetry that can be measured during finalizing without depending on Mithril JSON message strings for UI copy

**Main Process:**
- `source/main/mithril/mithrilProgress.ts` - Preserve raw file counts
- `source/main/mithril/MithrilBootstrapService.ts` - Pass file counts, annotate error stages
- `source/main/utils/handleDiskSpace.ts` - Annotate node-start stage, resolve symlinks for disk checks
- `source/main/utils/chainStorageManager.ts` **(new)** - Symlink management, validation, migration
- `source/main/ipc/chainStorageChannel.ts` **(new)** - Chain storage IPC handlers

**Renderer - Store:**
- `source/renderer/app/stores/MithrilBootstrapStore.ts` - Download metadata observables/computeds, chain storage actions

**Renderer - Components** (new sub-component directory):
- `MithrilBootstrap.tsx` - Root: overlay + view delegation (slimmed down)
- `BlockDataStorageLocationPicker.tsx` **(new)** - Preliminary blockchain-data storage screen
- `MithrilDecisionView.tsx` - Snapshot selector + accept/decline
- `MithrilProgressView.tsx` - Stepper + progress bar + metadata + cancel
- `MithrilErrorView.tsx` - Stage-specific error screens
- `MithrilStepIndicator.tsx` - Reusable step list
- `MithrilSnapshotSelector.tsx` - Snapshot dropdown
- `MithrilSnapshotDetails.tsx` - Metadata grid
- `MithrilBootstrap.messages.ts` - All i18n messages

**Renderer - Container:**
- `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` - Pass new observables

### Target Structure
```
components/loading/mithril-bootstrap/
├── MithrilBootstrap.tsx          # Root: overlay + view delegation
├── MithrilBootstrap.scss
├── BlockDataStorageLocationPicker.tsx
├── BlockDataStorageLocationPicker.scss
├── MithrilDecisionView.tsx
├── MithrilDecisionView.scss
├── MithrilProgressView.tsx
├── MithrilProgressView.scss
├── MithrilErrorView.tsx
├── MithrilErrorView.scss
├── MithrilStepIndicator.tsx
├── MithrilStepIndicator.scss
├── MithrilSnapshotSelector.tsx
├── MithrilSnapshotSelector.scss
├── MithrilSnapshotDetails.tsx
├── MithrilSnapshotDetails.scss
├── MithrilBootstrap.messages.ts
└── index.ts
```

### Store Changes

`MithrilBootstrapStore` additions:
- `@observable filesDownloaded, filesTotal` - raw file counts from progress
- `@computed bytesDownloaded` - derives from filesDownloaded/filesTotal * snapshot size
- `@computed throughputBps` - derives bytesDownloaded / elapsed seconds
- `@observable customChainPath` - user-selected chain directory (null = default)
- `@observable chainStorageValidation` - validation result for selected directory
- `@observable isChainStorageLoading` - true during validation/migration
- `@observable storageLocationConfirmed` - controls view routing (storage picker -> decision)
- `@action setChainStorageDirectory(path)` - sends to main, updates observables
- `@action resetChainStorageDirectory()` - removes symlink, reverts to default
- `@action confirmStorageLocation()` - advances past storage picker
- `@action loadChainStorageConfig()` - called on store init

### IPC Changes

- **Progress data:** No new channels - `filesDownloaded`, `filesTotal`, error `stage` flow through existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL`
- **Chain storage:** Two new channels:
  - `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` - renderer sends path (or null to reset), main validates/creates symlink, returns `ChainStorageValidation`
  - `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL` - renderer queries current config, main returns `ChainStorageConfig`

### Step Indicator Design

Phase 6 follow-up target:
- `preparing` - local preflight work before file transfer is underway
- `downloading` - the exposed Mithril file transfer phase only; show the determinate progress bar here and scale it from 0 to 100 within the step
- `verifying` - begins once the exposed file download completes; use localized verifying copy with no determinate progress bar
- `finalizing` - Daedalus local install and cleanup/handoff work after Mithril exits; rename local `unpacking` wording to `installing` where that is the user-visible concept
- Active step: spinner SVG + progress percentage only when the step has truthful determinate progress
- Completed step: checkmark SVG
- Pending step: dimmed circle
- Download step keeps estimated bytes, throughput, and elapsed/remaining timing metadata
- Verifying and finalizing replace transfer stats with localized activity copy unless explicit install telemetry proves reliable enough to expose
- Do not use Mithril JSON step messages as labels; keep renderer copy fully localized

### Post-download Telemetry Strategy

- Downloading should remain the only determinate progress step. Keep the existing bytes display as an estimate derived from exposed file progress and snapshot size.
- Transition from `downloading` into `verifying` when the exposed file download work completes; keep verifying indeterminate unless Mithril later exposes a truthful verification metric.
- Do not use Mithril JSON `message` strings as renderer copy. If service-side phase detection consumes Mithril JSON, rely on structured signals and keep UI text localized.
- Investigate `_installSnapshot()` and cleanup/handoff instrumentation so finalizing can expose honest local install progress where possible.
- If `fs.move()` progress cannot be exposed honestly across the relevant move/copy paths, keep finalizing minimal and activity-only rather than fabricating percentages.
- Destination-size polling remains fallback-only and should not be the default UX contract.
- Renderer fallback should remain explicit state copy plus elapsed time whenever the backend can only emit activity state temporarily, but the target outcome is real local restore telemetry rather than estimated progress.

### Error View Design

| Stage | Title | Hint |
|-------|-------|------|
| `download` | Snapshot download failed | Check network connectivity |
| `verify` | Snapshot verification failed | Integrity check failed |
| `convert` | Ledger conversion failed | Try again |
| `node-start` | Node failed to start | Consider wiping data |
| (generic) | Bootstrap failed | Fallback for unknown stage |

- Collapsible `error.code` details, clickable `error.logPath` link
- Actions: "Wipe chain & retry" (primary) + "Sync from genesis" (secondary)

### ChainStorageManager Design

Manages symlink-based redirection of `{stateDir}/chain` to a user-chosen directory.

| Method | Description |
|--------|-------------|
| `validate(targetDir)` | Check writable, sufficient space, not inside stateDir |
| `setDirectory(targetDir)` | Validate, migrate data, create symlink, persist config |
| `resetToDefault()` | Copy data back, remove symlink, remove config |
| `migrateData(from, to)` | fs.move() with cross-device fallback |
| `verifySymlink()` | Check target exists on startup |
| `getConfig()` | Read chain-storage-config.json |

**Symlink strategy:**
- Linux/macOS: `fs.symlink(target, chainPath, 'dir')` (POSIX symlink)
- Windows: `fs.symlink(target, chainPath, 'junction')` (NTFS junction, no admin required)

**Config file** (`chain-storage-config.json`):
```json
{ "customPath": "/mnt/external/daedalus-chain", "setAt": "2026-03-05T12:00:00Z" }
```

**Safety:**
- Never delete source until move confirmed complete
- Atomic rename where possible; copy+delete fallback for cross-device
- Restore original on migration failure
- Disable picker during active bootstrap/sync

### Theme Variable Migration

| Hardcoded Value | New Variable |
|-----------------|-------------|
| Backdrop color | `var(--theme-loading-background-color)` |
| Card bg `rgba(15,24,34,0.96)` | `var(--theme-mithril-card-background)` |
| Text colors | `var(--theme-mithril-text-color)`, `var(--theme-mithril-text-secondary)` |
| Button colors | `var(--theme-mithril-button-primary-background)`, etc. |

## Implementation Steps

### Phase 1: Backend Type & Pipeline Extensions
1. Extend shared types with file counts and error stage
2. Update mithrilProgress parser and MithrilBootstrapService
3. Annotate node-start failures in handleDiskSpace
4. Update backend unit tests

### Phase 2: Store & Container Updates
5. Add download metadata observables/computeds to MithrilBootstrapStore
6. Update MithrilBootstrapPage container to pass progress metadata props
7. Update MithrilBootstrapPage container to pass chain storage props after store actions exist

### Phase 3: Custom Chain Storage (Backend)
8. Add ChainStorageConfig and ChainStorageValidation types
9. Add chain storage IPC channels
10. Implement ChainStorageManager (validate + config)
11. Implement ChainStorageManager (setDirectory + migrateData)
12. Implement ChainStorageManager (resetToDefault + verifySymlink)
13. Integrate with disk space checks and download path
14. Add chain storage store actions
15. Add startup symlink verification
16. Unit tests for ChainStorageManager

### Phase 4: i18n (follow i18n-messaging skill)
17. Extract i18n messages file with existing + new messages
18. Run `yarn i18n:manage`

### Phase 5: Component Decomposition
19. MithrilStepIndicator - step logic and rendering
20. MithrilStepIndicator - download metadata and SCSS
21. MithrilSnapshotSelector
22. MithrilSnapshotDetails
23. MithrilErrorView
24. MithrilProgressView
24a. Collapse visible Mithril progress to preparing/downloading/finalizing
24b. Rename install-related post-download state/copy to unpacking while keeping finalizing as the visible step
24c. Remove `currentStep` display transport and derive step labels from status in the renderer
25. MithrilDecisionView
26. BlockDataStorageLocationPicker - layout and path display
27. BlockDataStorageLocationPicker - picker and validation
27a. Return from decision view to BlockDataStorageLocationPicker
28. Slim down root MithrilBootstrap.tsx

### Phase 6: Verifying & Finalizing UX Follow-up
29. Refine Mithril service/status boundaries for `preparing -> downloading -> verifying -> finalizing`
30. Wire the 4-step progress UI so only `downloading` shows a 0-100 progress bar with estimated transfer metadata, and confirm extracted i18n copy remains appropriate for the exposed steps
31. Investigate honest `_installSnapshot()` / `fs.move()` progress exposure for finalizing and add focused backend/renderer coverage for both measured-progress and activity-only fallback behavior

### Phase 7: Theming (follow theme-management skill)
32. Migrate SCSS to theme variables
33. Add variables to createTheme
34. Run theme validation and update
35. Regenerate SCSS type definitions

### Phase 8: Accessibility & Storybook (follow storybook-creation skill)
36. Add a11y attributes
37. Storybook stories - Decision and Storage states
38. Storybook stories - Progress and Error states
39. Register stories in barrel file

### Phase 9: E2E Tests (follow e2e-test-creation skill)
40. Create E2E feature file
41. Write step definitions

### Phase 10: Verification
42. Run full suite: compile, lint, prettier, themes, i18n, jest, storybook, typedef:sass

## Testing Strategy

### Automated
- Unit tests for MithrilBootstrapService (file counts, error stages) and ChainStorageManager
- `yarn compile` / `yarn lint` / `yarn prettier:check`
- `yarn themes:check:createTheme` / `yarn i18n:manage`

### E2E Scenarios

| Scenario | Description |
|---|---|
| Decision screen displays | Snapshot selector and details render |
| Accept bootstrap | Transition to preparing/downloading |
| Decline bootstrap | Alternative sync path |
| Progress stages advance | Step indicator stays on downloading through late verification copy, then transitions into finalizing with truthful local restore telemetry instead of a hidden 90% stall |
| Error screen displays | Stage-specific messaging |
| Cancel during progress | Return to decision or sync |
| Storage location change | Select external directory, verify symlink |
| Storage location reset | Remove symlink, show default path |
| Storage insufficient space | Show validation error |
| Storage during active sync | Picker is disabled |

E2E coverage should follow the repo's backend-integrated pattern: drive the real app flow in Electron and use browser-context helpers only for targeted setup/assertion support rather than directly forcing Mithril store state.

### Storybook States

| Story | State |
|---|---|
| Decision - Loading | `isFetchingSnapshots: true`, empty snapshots |
| Decision - With Snapshots | 3 snapshots, "latest" selected |
| Decision - Specific Snapshot | Specific digest selected, details visible |
| Progress - Preparing | `progress: 5` |
| Progress - Downloading (early) | `progress: 15`, partial bytes |
| Progress - Downloading (mid) | `progress: 55`, bytes + throughput |
| Progress - Downloading (late verification) | `status: downloading`, high progress, verification copy, transfer metadata may stop updating |
| Progress - Finalizing (active restore) | local restore telemetry active |
| Progress - Finalizing (late) | cleanup/handoff |
| Error - Download Failed | `error.stage: 'download'` |
| Error - Verify Failed | `error.stage: 'verify'` |
| Error - Convert Failed | `error.stage: 'convert'` |
| Error - Node Start Failed | `error.stage: 'node-start'` |
| Storage - Default Path | `customChainPath: null` |
| Storage - Custom Path | Custom path + available space |
| Storage - Validation Error | `isValid: false, error: 'Insufficient space'` |
| Storage - Migrating | `isChainStorageLoading: true` |

### Manual QA
Walk through full bootstrap flow in dev mode (`yarn dev`):
- Decision screen with formatted snapshot options
- Progress screen with stepper transitions and download metadata
- Error screen with stage-appropriate messaging
- Cancel and decline paths
- Storage location picker with space display and validation
- Custom directory selection, symlink creation, reset to default
- Disabled picker during active bootstrap
- App restart with custom path verifies symlink

## Design Decisions

1. **Decomposition pattern:** Follows `SyncingConnecting` precedent (5 sub-components) for complex loading screens
2. **Throughput calculation:** Computed in MobX store rather than backend to avoid timer logic in service layer
3. **Step animations only:** No CSS transitions between views, only spinner/checkmark per step
4. **Theme variables:** Registered in `createTheme`, propagated via `yarn themes:update`
5. **No new IPC for progress:** Extended type payloads on existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL`
6. **Storybook API:** Uses `storiesOf()` per Daedalus convention (not CSF)
7. **E2E coverage:** Follow the repo's backend-integrated E2E pattern; drive the real app flow and use browser-context helpers only for test harness setup or assertions, not to force Mithril UI state transitions.
8. **Symlink strategy:** POSIX symlinks on Linux/macOS, NTFS junctions on Windows (no admin needed)
9. **Config location:** `chain-storage-config.json` in stateDir (co-located with chain data, survives reinstalls)
10. **Picker disabled during sync:** Prevents filesystem race conditions
11. **Storage picker timing:** Show the storage location picker first, before the snapshot decision view, including on the initial Mithril flow.
12. **Throughput display:** Use a rolling-average transfer rate in the progress UX.
13. **Visible phase model:** Use a 3-step Mithril progress UX: `preparing`, `downloading`, and `finalizing`. Verification stays inside the download phase, while `_installSnapshot()` and cleanup/handoff are grouped under finalizing.
14. **Post-download naming:** Where service-level or message-level sub-phases are still needed after download, prefer `unpacking` over `installing`; renderer still maps that work into the visible finalizing step.
15. **Step labels:** Renderer components derive localized labels from `status`; do not transport user-facing step copy in `currentStep`.
16. **Post-download telemetry:** Preferred fix is explicit backend instrumentation of `_installSnapshot()` and cleanup/handoff so finalizing exposes real local restore telemetry; do not fake verification percentages or rely on destination-size polling unless instrumentation is blocked.
17. **Snapshot digest copy:** Defer copy-to-clipboard support to a follow-up iteration.

## Rollout Plan
- UX refinement of existing functionality; no feature flags needed
- Deploy alongside existing Mithril bootstrap feature
- Light-theme differentiation deferred to a future PR

## Open Questions
- Once explicit local restore telemetry lands, should finalizing surface bytes, files, or both in the metadata row?
- External drive disconnection while running: immediate error, periodic health check, or passive failure?
- Theme createTheme structure: add `mithril` section key in theme output, or through createTheme params?

## References
- Parent feature plan: [bootstrap-cardano-node.md](bootstrap-cardano-node.md)
- Backend/client notes: [research/mithril-bootstrap-client-notes.md](research/mithril-bootstrap-client-notes.md)
- Frontend UX notes: [research/mithril-bootstrap-ux-notes.md](research/mithril-bootstrap-ux-notes.md)
- `SyncingConnecting` pattern: `source/renderer/app/components/loading/syncing-connecting/`
- `SyncingProgress` stepper: `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`

---

**Status:** In Progress
**Date:** 2026-03-03  
**Author:** david-profrontsolutions
**Notes:**
- [2026-03-06] Extended shared Mithril status/error types for file counts and stage metadata.
- [2026-03-06] Updated parser and bootstrap service to emit file counters and staged errors.
- [2026-03-06] Added `node-start` stage annotation for post-bootstrap startup failures.
- [2026-03-06] Expanded backend unit coverage for file-count and stage propagation.
- [2026-03-09] Added store-level `filesDownloaded/filesTotal` observables and derived `bytesDownloaded/throughputBps` metadata for the upcoming progress UX.
- [2026-03-09] Tightened Mithril store status assignment to honor explicit `undefined` resets from the backend so progress metadata does not linger across state transitions.
- [2026-03-09] Split container wiring so progress metadata can land now while chain storage props wait for the later custom storage store actions.
- [2026-03-09] Added shared `ChainStorageConfig` and `ChainStorageValidation` types for upcoming chain storage IPC and store integration.
- [2026-03-09] Added shared chain storage IPC contracts and wired main/renderer channel wrappers with request handlers for set/get directory flow.
- [2026-03-09] Implemented `ChainStorageManager.validate()` and `getConfig()` with writable/space/stateDir checks and manager-backed chain storage IPC request handling.
- [2026-03-09] Implemented `ChainStorageManager.setDirectory()`, `migrateData()`, `resetToDefault()`, and `verifySymlink()` with symlink-aware migration flows and config lifecycle handling.
- [2026-03-09] Added `ChainStorageManager` unit tests for set/reset/migrate/verify flows and recorded follow-up tasks for validate-only IPC and symlink-switch rollback hardening.
- [2026-03-09] Added `VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL` and main/renderer channel bindings so UI can validate candidate directories without applying side effects.
- [2026-03-09] Integrated chain storage path resolution into disk-space polling and Mithril bootstrap IPC startup/cancel flow so custom storage locations are used consistently.
- [2026-03-09] Updated `_installSnapshot()` to compare resolved real paths and preserve chain symlinks while installing snapshot data into external storage targets.
- [2026-03-09] Added transactional rollback safeguards in `setDirectory()` to restore prior chain path/config state when symlink switch fails after migration.
- [2026-03-09] Added low-priority follow-up task to clean legacy lint debt in `handleDiskSpace.ts` (`ts-ignore`, `any`, and naming shadowing) without behavioral changes.
- [2026-03-09] Extended `MithrilBootstrapStore` with chain storage state/actions (`customChainPath`, `chainStorageValidation`, `isChainStorageLoading`, `storageLocationConfirmed`) and config bootstrap loading during store setup.
- [2026-03-09] Added renderer-store unit tests covering chain storage config loading, set/reset validation handling, and storage confirmation state transitions.
- [2026-03-09] Wired chain storage store state/actions through `MithrilBootstrapPage` into the existing `MithrilBootstrap` component contract to unblock storage picker UI decomposition tasks.
- [2026-03-09] Added startup chain storage symlink verification in main IPC initialization and surfaced invalid custom-path warnings through `MithrilBootstrapStore` validation state.
- [2026-03-09] Completed targeted `handleDiskSpace.ts` lint cleanup for this phase (removed `@ts-ignore` sites, fixed typed disk report fields, and standardized logger call signatures).
- [2026-03-09] Added low-priority follow-up task to cover startup chain-storage IPC verification logging with dedicated unit tests.
- [2026-03-09] Completed startup IPC verification test coverage in `chainStorageChannel.spec.ts` for invalid and exception verification branches.
- [2026-03-11] Extracted Mithril bootstrap copy into a shared messages module and added staged-error, stepper, download-metadata, and storage-picker strings for the component split.
- [2026-03-11] Ran `yarn i18n:manage` to sync Mithril bootstrap message descriptors into the extracted catalogs and locale files.
- [2026-03-11] Added a reusable `MithrilStepIndicator` component with status-derived step states, connector styling, and compact download metadata formatting for later composition in `MithrilProgressView`.
- [2026-03-11] Review follow-up: `MithrilStepIndicator` is implemented at the component level, but the current `MithrilBootstrap.renderProgress()` path still renders the legacy `ProgressBarLarge`-only view until tasks 021 and 024 land.
- [2026-03-11] Extracted `MithrilSnapshotSelector` with dedicated styling and option labels formatted as truncated digest plus localized date and snapshot size, then wired the root bootstrap decision view to consume it.
- [2026-03-11] Extracted `MithrilSnapshotDetails` with a dedicated metadata card, truncated digest display, and shared snapshot-formatting helpers so selector and details views stay aligned on date and size formatting.
- [2026-03-11] Extracted `MithrilErrorView` with stage-to-copy mapping, collapsible diagnostic details, file-URL log-path linking, and root/container wiring so failed Mithril states now render through a focused error component.
- [2026-03-11] Extracted `MithrilProgressView` with the step indicator, explicit downloaded/rate/timing metadata, and cancel action, then routed `MithrilBootstrap.renderProgress()` through it so the new progress UX is now user-visible.
- [2026-03-11] Extracted `MithrilDecisionView` to own the decision-screen header, selector/details composition, and accept/decline actions, then routed the root decision branch through it in preparation for the later storage-picker-first flow.
- [2026-03-11] Completed task-023a: added `BlockDataStorageLocationPicker`, routed the Mithril decision flow through a storage-first screen, and surfaced default/custom chain-path plus available-space metadata from store config so the layout can render before picker-side validation work lands.
- [2026-03-11] Completed task-023b: wired the block-data storage picker to `SHOW_OPEN_DIALOG_CHANNEL`, added inline validation/reset/loading feedback, and patched reset/default metadata so the picker stays coherent when switching back from custom chain storage.
- [2026-03-12] Renamed the planning references for the generic blockchain-data picker to `BlockDataStorageLocationPicker` and updated the documented target file names accordingly.
- [2026-03-12] Updated the storage picker recommendation plan so it uses the latest available snapshot size when loaded and otherwise falls back to a generic large-space warning; added a follow-up task for returning from the Mithril decision view to the storage picker.
- [2026-03-12] Completed task-023c by adding a decision-screen action that returns users to `BlockDataStorageLocationPicker`, backed by an explicit store action and focused renderer/store test coverage.
- [2026-03-12] Completed task-024 by slimming `MithrilBootstrap.tsx` down to a single overlay shell with one delegated content branch, moving the progress heading into `MithrilProgressView`, and switching the container to the mithril-bootstrap barrel export.
- [2026-03-12] Completed task-024a by collapsing the visible Mithril progress flow to Preparing, Downloading, and Finalizing, with post-download install/conversion work emitted as finalizing while the overlay stays up through local handoff.
- [2026-03-12] Completed task-024b by renaming the remaining post-download install wording to unpacking in Mithril service/status copy, progress detail messaging, and extracted translations while keeping the visible UX on the finalizing step.
- [2026-03-12] Completed task-024c by removing `currentStep` from Mithril status updates, emitting `unpacking`/`converting` as internal progress statuses from main, and deriving localized progress headings/details from `status` in the renderer with focused service/store/component coverage.
- [2026-03-13] Follow-up refinement: flattened the progress stepper into a horizontal row, removed redundant status-box metadata, switched the late-download copy to verification/unpacking language, kept the overlay mounted through completed handoff, and fixed accepted-bootstrap polling so post-download work no longer snaps back to the storage picker mid-bootstrap.
- [2026-03-13] Added a dedicated Phase 6 follow-up plan for the remaining 90% plateau gap: keep verification as state-only copy inside `downloading`, and prefer explicit backend instrumentation of post-download local restore work over destination-size polling so finalizing can expose real telemetry.
- [2026-03-11] Captured a Phase 5 cleanup follow-up for the 90% plateau: Mithril verification already happens inside `cardano-db download`, while the long post-download work is `_installSnapshot()` moving/copying DB files into `chain`, so the visible UX should collapse to a single finalizing phase after download.
- [2026-03-11] Completed task-021a: Mithril bootstrap now transitions from `downloading` into explicit `installing` and `finalizing` phases, clears stale transfer metrics after network download, and shows local-processing copy instead of frozen bandwidth stats during post-download work; follow-up task-024a will collapse that post-download work into one visible finalizing phase before node-sync handoff.
- [2026-03-11] Added follow-up tasks 024a/024b: finish the 3-step visible model (`preparing -> downloading -> finalizing`) and remove `currentStep` as display transport so renderer labels come from status.
