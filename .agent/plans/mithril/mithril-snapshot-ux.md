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

## Task Tracking

Task tracking is split by context to reduce agent context overhead. Load only the file relevant to your current work:

| File | Phases | Tasks | Status |
|------|--------|-------|--------|
| [backend tasks](mithril-snapshot-ux-tasks-backend.json) | 1–3: Types, Store, Chain Storage | 21 | All completed |
| [component & UX tasks](mithril-snapshot-ux-tasks-component-ux.json) | 4–6: i18n, Decomposition, Waterfall | 26 | 1 pending |
| [polish & testing tasks](mithril-snapshot-ux-tasks-polish-testing.json) | 7–10: Theming, A11y, Storybook, E2E, Verification | 11 | All pending |

For detailed implementation history, see the [changelog](mithril-snapshot-ux-changelog.md).

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
- [x] Implement 3-step vertical waterfall UX for `preparing -> downloading -> finalizing`
  - The waterfall step indicator and parent progress-view composition are implemented and wired through `MithrilProgressView`
  - Preparing is the initial mithril-snapshot call; actual download waits until the step process renders
  - All mithril-client steps 1-7 are exposed as individual waterfall sub-items under `Downloading` so users follow along
  - Two progress bars during Downloading: primary for snapshot files (files_downloaded/files_total with bytes estimate and remaining time), secondary for ancillary files (bytes_downloaded/bytes_total with remaining time); both persist at 100% once complete
  - Finalizing covers only post-mithril-client work: snapshot conversion (only if conversion runs) and DB install/move + cleanup
  - Vertical waterfall layout with spinner (active), checkmark (completed), or red X (error) icons next to each step/sub-item
  - Auto-scroll to latest active waterfall item
  - Fast sub-second steps show as completed checkmarks immediately
- [x] Keep user cancel on the cancelled path instead of falling through to failed when mithril-client is intentionally stopped
- [x] Preserve the last snapshot-files progress across step-only waterfall updates so the files bar stays at 100% through verification sub-steps
- [x] Clear ancillary transfer metrics when leaving Downloading and when a new bootstrap run starts
- [x] Ensure the single elapsed timer survives renderer resync and wipe-and-retry by reseeding from backend elapsedSeconds
- [x] Extend mithril progress parser for `label`, `step_num`, `bytes_downloaded`/`bytes_total` fields
  - Distinguish Files vs Ancillary progress by `label` field; graceful degradation if `label` absent
- [x] Split download tracking in service: separate Files and Ancillary streams, waterfall item accumulation
- [x] Add single elapsed timer for the whole 3-step process (not per-step), displayed throughout
- [x] Add 3-second completion delay with spinning circle showing "cardano-node is starting up to complete sync" before yielding
- [x] Add ~15 new i18n messages for waterfall step labels, ancillary progress, and node-starting delay
  - Do not route Mithril JSON step messages into the renderer as user-facing copy; map step_num to localized labels
- [ ] Show conversion waterfall item in Finalizing only if snapshot conversion actually runs
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
- `source/main/mithril/MithrilBootstrapService.ts` - Core bootstrap orchestrator (~700 lines after refactor)
- `source/main/mithril/mithrilProgress.ts` - Progress line parser (label, step_num, bytes fields)
- `source/main/mithril/mithrilNetworkConfig.ts` - Network config helpers (extracted from service)
- `source/main/mithril/mithrilCommandRunner.ts` - CLI subprocess runner (extracted from service)
- `source/main/mithril/mithrilErrors.ts` - Error builder utilities (extracted from service)
- `source/main/utils/handleDiskSpace.ts` - Annotate node-start stage, resolve symlinks for disk checks
- `source/main/utils/chainStorageManager.ts` - Symlink management, data migration, config persistence
- `source/main/utils/chainStorageValidation.ts` - Chain storage validation logic (extracted from manager)
- `source/main/utils/chainStoragePathResolver.ts` - Chain storage path resolution (extracted from manager)
- `source/main/ipc/chainStorageChannel.ts` - Chain storage IPC handlers

**Renderer - Store:**
- `source/renderer/app/stores/MithrilBootstrapStore.ts` - Download metadata observables/computeds, chain storage actions

**Renderer - Components** (`components/loading/mithril-bootstrap/`):
- `MithrilBootstrap.tsx` - Root: overlay + view delegation (slimmed down)
- `BlockDataStorageLocationPicker.tsx` - Preliminary blockchain-data storage screen
- `MithrilDecisionView.tsx` - Snapshot selector + accept/decline
- `MithrilProgressView.tsx` - Header + elapsed timer + scrollable waterfall + completion handoff + cancel
- `MithrilErrorView.tsx` - Stage-specific error screens
- `MithrilStepIndicator.tsx` - Reusable step list
- `MithrilSnapshotSelector.tsx` - Snapshot dropdown
- `MithrilSnapshotDetails.tsx` - Metadata grid
- `MithrilBootstrap.messages.ts` - All i18n messages
- `snapshotFormatting.ts` - Snapshot display formatting (digest, date, size)
- `storagePickerUtils.ts` - Storage path/validation formatting helpers

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
├── snapshotFormatting.ts         # Snapshot display formatting
├── storagePickerUtils.ts         # Storage path/validation formatting
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

3-step vertical waterfall model:
- `preparing` — initial mithril-snapshot call to kick off the process; the actual download does not start until the step process renders
- `downloading` — all mithril-client work (steps 1-7); each step exposed as an individual waterfall sub-item so users can follow along:
  - Step 1: Checking local disk info
  - Step 2: Verifying certificate chain
  - Step 3: Downloading snapshot data (primary progress bar: snapshot files with bytes estimate + remaining time)
  - Ancillary data download (secondary progress bar: ancillary bytes + remaining time)
  - Step 4: Verifying downloaded digests
  - Step 5: Verifying cardano database
  - Step 6: Computing snapshot message
  - Step 7: Verifying snapshot signature
- `finalizing` — post-mithril-client Daedalus-local work only:
  - Snapshot conversion (waterfall item shown only if conversion is enabled/runs)
  - Moving snapshot to blockchain storage (`_installSnapshot()`)
  - Cleaning up bootstrap files

**Layout:**
- Vertical waterfall with left icon column: spinner (active), checkmark (completed), red X (error), grey circle (pending)
- Vertical connector line between steps (green for completed, grey for pending)
- Each top-level step has a sub-content area for waterfall sub-items and progress bars
- Progress bars persist at 100% once their phase completes — never unmounted
- CSS transitions for smooth waterfall item appearance (opacity 0→1, translateY)
- Auto-scroll to latest active item; scrollable container if content exceeds viewport

**Progress bars:**
- Primary (Downloading sub-content): snapshot files progress — files_downloaded/files_total with estimated bytes conversion and remaining time
- Secondary (Downloading sub-content): ancillary data progress — bytes_downloaded/bytes_total with remaining time
- Both bars show inline within the Downloading waterfall step

**Timer:**
- Single elapsed timer for the whole 3-step process (H:MM:SS or M:SS), displayed at top of progress view from Preparing through completion

**Completion delay:**
- 3-second delay with spinning circle after all steps complete
- Shows "cardano-node is starting up to complete sync" message
- Cancel button disabled during delay

**Icons:**
- Reuse existing error/close SVG tinted red for error state
- Fast sub-second steps show as completed checkmarks immediately (no minimum display time)
- Do not use Mithril JSON step messages as labels; keep renderer copy fully localized via step_num → i18n mapping

### Progress Telemetry Strategy

- Downloading has two determinate progress streams: snapshot files (estimated bytes from file count × snapshot size) and ancillary data (actual bytes_downloaded/bytes_total from mithril-client)
- All mithril-client steps 1-7 are tracked as waterfall items with spinner → checkmark transitions
- Do not use Mithril JSON `message` strings as renderer copy; map `step_num` to localized step labels
- Graceful degradation: if mithril-client doesn't emit `label` field, fall back to single-stream behavior and hide the ancillary progress bar
- Finalizing covers only post-mithril-client work: conversion (if enabled), `_installSnapshot()` DB move, and cleanup — each as a waterfall item
- Keep finalizing waterfall items activity-only (spinner → checkmark) rather than fabricating percentages for same-device moves/renames
- Single overall elapsed timer tracked from `bootstrapStartedAt` via 1-second interval in the store; cleared on completed/failed/cancelled
- **Cancel is a terminal state:** keep user cancel on the cancelled path instead of falling through to failed when mithril-client is intentionally killed
- **Files progress is sticky:** preserve the last snapshot-files progress payload across step-only waterfall announcements during downloading so the files bar stays truthful through late verification steps
- **Ancillary metrics are download-only:** clear ancillary transfer stats when leaving Downloading or starting a fresh run so finalizing and failed states do not reuse stale download statistics
- **Timer survives resync and retry:** seed elapsed timer from backend `elapsedSeconds` when syncing into an in-progress bootstrap so the timer survives renderer refresh/resubscribe; reset timer when a new bootstrap starts from failed/completed so wipe-and-retry does not inherit the previous run

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

| Method / Module | Description |
|--------|----------|
| `setDirectory(targetDir)` | Validate, migrate data, create symlink, persist config |
| `resetToDefault()` | Copy data back, remove symlink, remove config |
| `migrateData(from, to)` | fs.move() with cross-device fallback |
| `verifySymlink()` | Check target exists on startup |
| `getConfig()` | Read chain-storage-config.json |
| `chainStorageValidation.ts` | Extracted: writable, space, stateDir checks (pure functions) |
| `chainStoragePathResolver.ts` | Extracted: chain path and symlink target resolution |

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

### Phase 6: 3-Step Vertical Waterfall UX
29. Extend mithril progress parser for label, step_num, and ancillary bytes
30. Split download tracking and emit waterfall items in service (steps 1-7 under downloading)
31. Extend store and container for waterfall and dual progress
31a. Harden service cancellation and progress-state retention
31b. Reseed elapsed timer across resync and retry
31c. Add waterfall and ancillary i18n messages
31d. Vertical waterfall MithrilStepIndicator with two inline progress bars
31e. Recompose MithrilProgressView with waterfall, single elapsed timer, and 3-second completion delay
31f. Backend and renderer integration tests

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
| Progress - Downloading (dual bars active) | `status: downloading`, step-3 active, both snapshot files + ancillary progress bars at mid-progress |
| Progress - Downloading (verification steps) | `status: downloading`, steps 4-7 visible as waterfall sub-items cascading, both bars at 100% |
| Progress - Finalizing (install active) | `status: finalizing`, DB install waterfall item active with spinner |
| Progress - Completion delay | `status: completed`, 3-second delay with spinner and node-starting message |
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
3. **Waterfall transitions:** CSS opacity/translateY transitions for smooth waterfall item appearance; spinner/checkmark/red-X per step and sub-item
4. **Theme variables:** Registered in `createTheme`, propagated via `yarn themes:update`
5. **No new IPC for progress:** Extended type payloads on existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL`
6. **Storybook API:** Uses `storiesOf()` per Daedalus convention (not CSF)
7. **E2E coverage:** Follow the repo's backend-integrated E2E pattern; drive the real app flow and use browser-context helpers only for test harness setup or assertions, not to force Mithril UI state transitions.
8. **Symlink strategy:** POSIX symlinks on Linux/macOS, NTFS junctions on Windows (no admin needed)
9. **Config location:** `chain-storage-config.json` in stateDir (co-located with chain data, survives reinstalls)
10. **Picker disabled during sync:** Prevents filesystem race conditions
11. **Storage picker timing:** Show the storage location picker first, before the snapshot decision view, including on the initial Mithril flow.
12. **Throughput display:** Use a rolling-average transfer rate in the progress UX.
13. **Visible phase model:** 3-step vertical waterfall UX: `preparing` (initial mithril-snapshot call), `downloading` (all mithril-client steps 1-7 with two progress bars), and `finalizing` (conversion if enabled + DB install + cleanup). All mithril-client work stays under downloading as individual waterfall sub-items.
14. **Post-download naming:** Where service-level or message-level sub-phases are still needed after download, prefer `unpacking` over `installing`; renderer still maps that work into the visible finalizing step.
15. **Service extraction:** `MithrilBootstrapService` delegates network config, CLI execution, and error building to extracted modules (`mithrilNetworkConfig`, `mithrilCommandRunner`, `mithrilErrors`), keeping the service focused on orchestration. `ChainStorageManager` similarly delegates validation and path resolution to `chainStorageValidation` and `chainStoragePathResolver`.
16. **Utility co-location:** `storagePickerUtils.ts` and `snapshotFormatting.ts` live in the component folder (not `renderer/app/utils/`) because they depend on component-local i18n messages and are only consumed within `mithril-bootstrap/`.
17. **Step labels:** Renderer components derive localized labels from `status`; do not transport user-facing step copy in `currentStep`.
18. **Post-download telemetry:** Preferred fix is explicit backend instrumentation of `_installSnapshot()` and cleanup/handoff so finalizing exposes real local restore telemetry; do not fake verification percentages or rely on destination-size polling unless instrumentation is blocked.
19. **Snapshot digest copy:** Defer copy-to-clipboard support to a follow-up iteration.
20. **Vertical waterfall layout:** Replace horizontal step indicator with vertical waterfall where each step flows downward with sub-content areas for progress bars and detailed sub-items.
21. **Dual progress bars:** Snapshot files (primary, estimated bytes from file count) and ancillary data (secondary, actual bytes from mithril-client) rendered inline within the Downloading waterfall step. Both persist at 100%.
22. **Single elapsed timer:** One timer for the whole 3-step process tracked in MobX store, not per-step timers.
23. **Completion delay:** 3-second pause with spinner after Finalizing completes, showing node-starting message, before yielding to parent for cardano-node sync handoff.

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
**Notes:** See [changelog](mithril-snapshot-ux-changelog.md) for detailed implementation history.
