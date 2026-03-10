# Mithril Snapshot UX Refinement

## Overview

- Decompose monolithic `MithrilBootstrap.tsx` into focused sub-components following the `SyncingConnecting` pattern
- Add multi-step visual stepper (Preparing -> Downloading -> Verifying -> Finalizing) modeled on `SyncingProgress`
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
- [ ] Decompose `MithrilBootstrap.tsx` into 6+ sub-components
- [ ] Implement multi-step visual stepper with spinner/checkmark/pending states
- [ ] Surface download metadata in progress view
- [ ] Stage-specific error screens with contextual titles, hints, and actions
- [ ] Extract snapshot selector and details grid into reusable components
- [ ] Add a11y attributes (`role="progressbar"`, `aria-live`, `aria-label`, keyboard nav)
- [ ] `MithrilStorageLocationPicker` as first screen in bootstrap flow (before decision view)
  - Shows current path, available space, validation feedback
  - Directory picker via `SHOW_OPEN_DIALOG_CHANNEL`
  - "Reset to default" and "Continue" actions
  - Disabled during active bootstrap

### i18n (follow i18n-messaging skill)
- [ ] Extract all messages into `MithrilBootstrap.messages.ts`
- [ ] Add ~14 new messages for errors, download metadata, steps, and storage picker
- [ ] Run `yarn i18n:manage` to sync translations

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
- `MithrilStorageLocationPicker.tsx` **(new)** - Preliminary storage screen
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
├── MithrilStorageLocationPicker.tsx
├── MithrilStorageLocationPicker.scss
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

4 steps derived from `status` prop: `preparing`, `downloading`, `verifying`, `finalizing`
- Active step: spinner SVG + progress percentage
- Completed step: checkmark SVG
- Pending step: dimmed circle
- Download step additionally shows bytes (e.g., "2.1 GB / 8.4 GB") and throughput ("12.3 MB/s")

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
25. MithrilDecisionView
26. MithrilStorageLocationPicker - layout and path display
27. MithrilStorageLocationPicker - picker and validation
28. Slim down root MithrilBootstrap.tsx

### Phase 6: Theming (follow theme-management skill)
29. Migrate SCSS to theme variables
30. Add variables to createTheme
31. Run theme validation and update
32. Regenerate SCSS type definitions

### Phase 7: Accessibility & Storybook (follow storybook-creation skill)
33. Add a11y attributes
34. Storybook stories - Decision and Storage states
35. Storybook stories - Progress and Error states
36. Register stories in barrel file

### Phase 8: E2E Tests (follow e2e-test-creation skill)
37. Create E2E feature file
38. Write step definitions

### Phase 9: Verification
39. Run full suite: compile, lint, prettier, themes, i18n, jest, storybook, typedef:sass

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
| Progress stages advance | Step indicator transitions through all stages |
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
| Progress - Verifying | `progress: 92.5` |
| Progress - Finalizing | `progress: 97.5` |
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
13. **Snapshot digest copy:** Defer copy-to-clipboard support to a follow-up iteration.

## Rollout Plan
- UX refinement of existing functionality; no feature flags needed
- Deploy alongside existing Mithril bootstrap feature
- Light-theme differentiation deferred to a future PR

## Open Questions
- Migration progress: progress bar (requires monitoring fs.move) or spinner with estimate?
- External drive disconnection while running: immediate error, periodic health check, or passive failure?
- Theme createTheme structure: add `mithril` section key in theme output, or through createTheme params?

## References
- Parent feature plan: [bootstrap-cardano-node.md](bootstrap-cardano-node.md)
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
