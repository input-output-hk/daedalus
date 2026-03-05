# Mithril Snapshot UX Refinement

## Overview
Decompose the monolithic `MithrilBootstrap.tsx` into focused sub-components following the `SyncingConnecting` pattern. Add a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing) modeled after `SyncingProgress`. Surface rich download metadata (bytes transferred, throughput, timing). Implement stage-specific error screens. Migrate all hardcoded dark RGBA values to CSS theme variables. Create full Storybook coverage for all states. Extend backend types and status updates to carry `filesDownloaded`, `filesTotal`, and error `stage` through IPC. Allow users to choose a custom storage location for the chain/db directory (e.g., external drives) via a directory picker with symlink-based redirection.

## Requirements

### Backend
- [ ] Extend shared types with `filesDownloaded`, `filesTotal` on status updates and `stage` on errors.
- [ ] Update backend progress pipeline to preserve raw file counts and annotate errors with failure stage.
- [ ] Add MobX computed properties for `bytesDownloaded` and `throughputBps`.

### Custom Chain Storage Location
- [ ] Add new IPC channel `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` for renderer→main communication of user-selected directory.
- [ ] Add new IPC channel `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL` for renderer to query the current custom chain directory (or `null` for default).
- [ ] Implement `ChainStorageManager` in `source/main/utils/chainStorageManager.ts` to handle:
  - Validating selected directory (exists, writable, sufficient disk space).
  - Creating a symlink from `{stateDir}/chain` → user-selected directory.
  - Removing/replacing symlink when user resets to default location.
  - Persisting the custom path to `{stateDir}/chain-storage-config.json`.
  - Migrating existing chain data to the new location when the directory changes.
- [ ] Integrate `ChainStorageManager` with `handleDiskSpace.ts` so disk space checks target the resolved (symlink target) directory.
- [ ] Update `MithrilBootstrapService` to use the custom storage location as its `_workDir` so that snapshot downloads (`mithril-client cardano-db download`) write directly to the chosen directory — avoids requiring double the disk space for download + move.
- [ ] Add shared types: `ChainStorageConfig` (`{ customPath: string | null; isSymlinked: boolean; resolvedPath: string }`) and `ChainStorageValidation` (`{ isValid: boolean; availableSpace: number; isWritable: boolean; error?: string }`).
- [ ] Validate directory on selection: check writable, check available space ≥ `DISK_SPACE_REQUIRED`, reject paths inside `stateDir` (would create circular symlinks).
- [ ] On app startup, verify existing symlink integrity (target still exists and is accessible); if broken, warn user and offer to reset to default.

### Component Decomposition
- [ ] Decompose `MithrilBootstrap.tsx` (424 lines) into 6+ focused sub-components.
- [ ] Implement a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing).
- [ ] Surface download metadata (bytes transferred, throughput, timing) in the progress view.
- [ ] Implement stage-specific error screens with contextual titles, hints, and actions.
- [ ] Extract snapshot selector into a reusable component with truncated digest labels.
- [ ] Extract snapshot details grid into a dedicated component.
- [ ] Add accessibility attributes (`role="progressbar"`, `aria-live`, `aria-label`, keyboard nav).
- [ ] Implement `MithrilStorageLocationPicker` component presented **before** the snapshot/sync decision screen to let users choose an alternative chain storage directory.
  - The storage location picker is the **first step** in the bootstrap flow — the user must confirm or change the storage location before being shown the snapshot selector or sync options.
  - Directory picker button triggers `SHOW_OPEN_DIALOG_CHANNEL` with `properties: ['openDirectory']`.
  - Display current storage path (default or custom), available disk space on the target drive, and a "Reset to default" action.
  - Show validation feedback: insufficient space warning, permission errors, broken symlink alerts.
  - Integrated as a preliminary screen in the `MithrilBootstrap` root component, shown before `MithrilDecisionView`.

### i18n (per i18n-messaging skill)
- [ ] Extract all i18n messages into a dedicated `MithrilBootstrap.messages.ts` file.
- [ ] Add ~8 new messages for stage-specific errors, download metadata labels, and step indicators.
- [ ] Add ~6 new messages for storage location picker: directory label, change button, reset button, space available, validation errors (insufficient space, not writable, broken symlink).
- [ ] Follow message ID convention: `mithril.bootstrap.<messageKey>` (dot-separated, hierarchical).
- [ ] All `defaultMessage` values must be prefixed with `!!!` (Daedalus convention).
- [ ] Include `description` field on every message for translator context.
- [ ] Run `yarn i18n:manage` (extract + check) to sync translations and validate.

### Theming (per theme-management skill)
- [ ] Migrate all hardcoded RGBA/color values to CSS theme variables (`--theme-mithril-*`).
- [ ] Add new `--theme-mithril-*` variables to `createTheme` in `source/renderer/app/themes/utils/constants.ts` with `light` and `dark` values.
- [ ] Run `yarn themes:check:createTheme` to validate theme consistency.
- [ ] Run `yarn themes:update` to propagate variables to ALL theme files (not just cardano.ts and light-blue.ts).
- [ ] Regenerate SCSS type definitions after all SCSS changes (`yarn typedef:sass`).

### Storybook (per storybook-creation skill)
- [ ] Create `storybook/stories/loading/` domain directory (new domain).
- [ ] Create 12 stories using `storiesOf()` API (NOT Component Story Format/CSF).
- [ ] Use story naming convention: `storiesOf('Loading / Mithril Bootstrap', module)`.
- [ ] Register stories in `storybook/stories/index.ts` barrel file.
- [ ] Use appropriate decorator stack (`StoryDecorator` for theme, `withKnobs` for interactive props).

### E2E Tests (per e2e-test-creation skill)
- [ ] Create new `tests/mithril/` domain directory with `e2e/features/` and `e2e/steps/` subdirectories.
- [ ] Write `mithril-bootstrap.feature` Gherkin file with `@e2e` tag covering decision, progress, and error flows.
- [ ] Write step definitions in `mithril-bootstrap.ts` using Cucumber.js conventions.
- [ ] Use CSS class selectors matching SCSS module classes for element interaction.

## Technical Design

### Components Affected

- **Shared Types**
  - `source/common/types/mithril-bootstrap.types.ts`: Add `filesDownloaded`, `filesTotal` to status updates; add `stage` to error type.
- **Main Process**
  - `source/main/mithril/mithrilProgress.ts`: Extend `MithrilProgressUpdate` and `parseMithrilProgressUpdate` to preserve raw file counts.
  - `source/main/mithril/MithrilBootstrapService.ts`: Pass `filesDownloaded`/`filesTotal` through `_updateStatus()` and annotate errors with `stage`.
  - `source/main/utils/handleDiskSpace.ts`: Annotate node-start failure errors with `stage: 'node-start'`. Update disk space checks to target resolved symlink directory.
  - `source/main/utils/chainStorageManager.ts` **(new)**: Manages custom chain directory symlinks, validation, migration, and config persistence.
  - `source/main/ipc/chainStorageChannels.ts` **(new)**: IPC handlers for `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` and `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL`.
- **Renderer — Store**
  - `source/renderer/app/stores/MithrilBootstrapStore.ts`: Add observables and computed properties for download metadata. Add `customChainPath`, `chainStorageValidation` observables and `setChainStorageDirectory`, `resetChainStorageDirectory`, `loadChainStorageConfig` actions.
- **Renderer — Components** (new sub-component directory)
  - `MithrilBootstrap.tsx` — Root: overlay + backdrop + view delegation (slimmed down).
  - `MithrilDecisionView.tsx` — Decision screen (title, description, selector, details, buttons). Shown after storage location is confirmed.
  - `MithrilProgressView.tsx` — Progress screen (stepper + bar + meta + cancel).
  - `MithrilErrorView.tsx` — Error screen (stage-specific title/hint/actions).
  - `MithrilStepIndicator.tsx` — Reusable step list (spinner/check per step).
  - `MithrilSnapshotSelector.tsx` — Snapshot dropdown with truncated digest labels.
  - `MithrilSnapshotDetails.tsx` — Metadata grid (digest, date, size, node version).
  - `MithrilStorageLocationPicker.tsx` **(new)** — Preliminary screen shown before decision view: directory picker with space display, validation feedback, "Continue" button.
  - `MithrilBootstrap.messages.ts` — All i18n messages (extracted + new).
- **Renderer — Container**
  - `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`: Pass new store observables through to components.
- **Themes** (use theme-management skill tooling)
  - `source/renderer/app/themes/utils/constants.ts`: Add `--theme-mithril-*` variables to `createTheme` with `light` and `dark` values.
  - All theme files (`cardano.ts`, `light-blue.ts`, `dark-cardano.ts`, etc.): Auto-updated via `yarn themes:update` — do NOT manually edit.
- **Storybook** (per storybook-creation skill)
  - `storybook/stories/loading/MithrilBootstrap.stories.tsx`: 12 stories using `storiesOf()` API.
  - `storybook/stories/index.ts`: Register new `loading/` domain import.
- **E2E Tests** (per e2e-test-creation skill)
  - `tests/mithril/e2e/features/mithril-bootstrap.feature`: Gherkin scenarios for bootstrap flow.
  - `tests/mithril/e2e/steps/mithril-bootstrap.ts`: Step definitions with CSS selectors.

### Store Changes
- `MithrilBootstrapStore` additions:
  - `@observable filesDownloaded: number | undefined`
  - `@observable filesTotal: number | undefined`
  - `@computed get bytesDownloaded()` — derives `(filesDownloaded / filesTotal) * (snapshot?.size ?? 0)`
  - `@computed get throughputBps()` — derives `bytesDownloaded / elapsedSeconds` when both values are available
  - `@observable customChainPath: string | null` — user-selected chain storage directory (null = default)
  - `@observable chainStorageValidation: ChainStorageValidation | null` — validation result for selected directory
  - `@observable isChainStorageLoading: boolean` — true while validating/migrating
  - `@observable storageLocationConfirmed: boolean` — false until user clicks "Continue" on storage picker; controls view routing in root component (storage picker → decision → progress/error)
  - `@action setChainStorageDirectory(path: string)` — sends path to main via `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL`, updates observables with result
  - `@action resetChainStorageDirectory()` — sends `null` to main to remove symlink and revert to default
  - `@action confirmStorageLocation()` — sets `storageLocationConfirmed = true` to advance past the storage picker to the decision view
  - `@action loadChainStorageConfig()` — called on store init, fetches current config via `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL`

### IPC Changes
- **No new IPC channels for progress data.** All new data (`filesDownloaded`, `filesTotal`, error `stage`) flows through the existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL` via extended type payloads.
- **Two new IPC channels for chain storage location:**
  - `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL`: Renderer sends a selected directory path (or `null` to reset). Main process validates, creates/removes symlink, migrates data, and returns `ChainStorageValidation`.
  - `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL`: Renderer requests current chain storage config. Main process returns `ChainStorageConfig`.

### Component Decomposition Pattern
Follows the `SyncingConnecting` precedent (5 sub-components) rather than keeping a monolith — matches codebase conventions for complex loading screens.

**Target structure:**
```
components/loading/mithril-bootstrap/
├── MithrilBootstrap.tsx          # Root: overlay + backdrop + view delegation
├── MithrilBootstrap.scss         # Overlay/backdrop/card layout only
├── MithrilDecisionView.tsx       # Decision screen
├── MithrilDecisionView.scss
├── MithrilProgressView.tsx       # Progress screen (stepper + bar + meta + cancel)
├── MithrilProgressView.scss
├── MithrilErrorView.tsx          # Error screen (stage-specific)
├── MithrilErrorView.scss
├── MithrilStepIndicator.tsx      # Reusable step list (spinner/check per step)
├── MithrilStepIndicator.scss
├── MithrilSnapshotSelector.tsx   # Snapshot dropdown
├── MithrilSnapshotSelector.scss
├── MithrilSnapshotDetails.tsx    # Metadata grid
├── MithrilSnapshotDetails.scss
├── MithrilStorageLocationPicker.tsx  # Directory picker + space display
├── MithrilStorageLocationPicker.scss
├── MithrilBootstrap.messages.ts  # All i18n messages
└── index.ts                      # Re-export
```

### Step Indicator Design
Modeled on `SyncingProgress.tsx`:

- **Steps defined:** `preparing`, `downloading`, `verifying`, `finalizing` — derived from the `status` prop.
- **Per-step rendering:** Spinner SVG (CSS `@keyframes rotate`) when active; checkmark SVG when completed; dimmed circle when pending.
- **Active step** shows the overall `progress` percentage; download step additionally shows bytes (e.g., "2.1 GB / 8.4 GB") and throughput ("12.3 MB/s").
- **Props:** `currentStatus`, `progress`, `bytesDownloaded?`, `bytesTotal?`, `throughputBps?`.

### Error View Design
Stage-specific messaging:

| Stage | Title | Hint |
|-------|-------|------|
| `download` | Snapshot download failed | Check network connectivity |
| `verify` | Snapshot verification failed | Integrity check failed |
| `convert` | Ledger conversion failed | Try again |
| `node-start` | Node failed to start | Consider wiping data |
| (generic) | Bootstrap failed | Fallback for unknown stage |

- Display `error.code` in a collapsible details section when present.
- Show `error.logPath` as a clickable "View logs" link (triggers IPC to open file).
- Actions: "Wipe chain & retry" (primary) + "Sync from genesis" (secondary).

### Custom Chain Storage Location Design

Allows users to redirect the `{stateDir}/chain` directory to an alternative location (e.g., external drive, secondary partition) using filesystem symlinks. The canonical `chain` path inside `stateDir` becomes a symlink pointing to the user-chosen directory.

**Architecture:**

```
Main Process                           Filesystem
┌─────────────────────────┐
│ ChainStorageManager     │            {stateDir}/chain → /mnt/external/daedalus-chain  (symlink)
│  - validate(dir)        │            {stateDir}/chain-storage-config.json  (persisted setting)
│  - setDirectory(dir)    │
│  - resetToDefault()     │
│  - migrateData(from,to) │
│  - verifySymlink()      │
└─────────────────────────┘
```

**`ChainStorageManager` (`source/main/utils/chainStorageManager.ts`):**

| Method | Description |
|--------|-------------|
| `validate(targetDir)` | Check directory exists, is writable, has `≥ DISK_SPACE_REQUIRED` free space, is not inside `stateDir` (prevents circular symlinks). Returns `ChainStorageValidation`. |
| `setDirectory(targetDir)` | (1) Validate target. (2) If current `chain` is a real directory, migrate contents to `targetDir`. (3) Remove `chain` dir/symlink. (4) Create symlink `chain → targetDir`. (5) Persist to `chain-storage-config.json`. |
| `resetToDefault()` | (1) If `chain` is a symlink, copy contents back to a real `chain` directory. (2) Remove symlink. (3) Remove config file. |
| `migrateData(from, to)` | Move chain data between directories using `fs.move()` with progress logging. Called during both `setDirectory` and `resetToDefault`. |
| `verifySymlink()` | Called on app startup. If `chain` is a symlink, verify the target exists and is accessible. If broken, return a diagnostic that the UI surfaces as a warning. |
| `getConfig()` | Read `chain-storage-config.json` and return `ChainStorageConfig`. |

**Config file (`chain-storage-config.json`):**
```json
{
  "customPath": "/mnt/external/daedalus-chain",
  "setAt": "2026-03-05T12:00:00Z"
}
```

**Symlink strategy (cross-platform):**
- **Linux/macOS:** `fs.symlink(targetDir, chainPath, 'dir')` (standard POSIX symlink).
- **Windows:** `fs.symlink(targetDir, chainPath, 'junction')` (NTFS junction — does not require admin privileges unlike directory symlinks on Windows).
- Use `fs.lstat()` to detect existing symlinks vs real directories before operations.

**Integration with disk space checks:**
- `handleDiskSpace.ts` currently calls `checkDiskSpace(stateDirectoryPath)`. When a custom chain directory is set, the disk space check must target the resolved symlink destination (the external drive) using `fs.realpath()` on the chain directory.
- The `MithrilBootstrapService` constructor accepts a `workDir` parameter defaulting to `stateDirectoryPath`. When a custom chain directory is set, `handleDiskSpace.ts` must pass the resolved custom directory as `workDir` so mithril-client downloads directly to the target location — symlinks are transparent to `fs.move()` and `fs.pathExists()`, and this avoids needing double the disk space for download artifacts. `_installSnapshot()` should resolve the real path before comparing `dbDirectory === chainDir`.

**Storage Location Picker Component (`MithrilStorageLocationPicker.tsx`):**
- Presented as a **preliminary screen before the decision view** — the user sees the storage picker first, confirms or changes the location, then proceeds to the snapshot/sync decision screen. This ensures the download target is resolved before any sync method is chosen.
- Shows current storage path: `"Chain data: /home/user/.local/share/Daedalus/mainnet/chain"` (default) or `"Chain data: /mnt/external/daedalus-chain"` (custom).
- **"Change location" button** opens native directory picker via `SHOW_OPEN_DIALOG_CHANNEL` with `properties: ['openDirectory', 'createDirectory']`.
- After selection, displays validation result: available space formatted with `prettysize()`, or error message (insufficient space, not writable, etc.).
- **"Reset to default" link** (only shown when custom path is active) — triggers `resetChainStorageDirectory()` store action.
- **"Continue" button** proceeds to the decision view once a valid storage location is confirmed.
- While migrating data, shows a spinner with `"Moving chain data..."` message. UI is non-interactive during migration to prevent race conditions.

**Props:** `customChainPath`, `chainStorageValidation`, `isChainStorageLoading`, `onSelectDirectory()`, `onResetToDefault()`, `onContinue()`, `resolvedChainPath`, `availableSpace`.

**Download location behavior:**
- When a custom chain directory is set, `MithrilBootstrapService._workDir` is set to the custom path so that `mithril-client cardano-db download` writes snapshot data directly into the target directory. This avoids requiring double the disk space (download to default + move to custom).
- The symlink from `{stateDir}/chain` → custom directory is created **before** the download begins, so cardano-node can find the chain data at the canonical path after bootstrap completes.
- `_installSnapshot()` becomes a no-op when `_workDir` already points to the custom location (the `dbDirectory === chainDir` resolved-path check catches this).

**Safety considerations:**
- Never delete the source directory until the move is confirmed complete.
- Use atomic rename where possible; fall back to copy+delete for cross-device moves.
- If migration fails mid-way, restore the original directory and remove the incomplete target.
- Prevent setting chain directory while bootstrap/sync is in progress (disable picker during `preparing`/`downloading`/`verifying`/`converting` statuses).

### Theme Variable Migration (per theme-management skill)

Replace hardcoded values:

| Hardcoded Value | New Variable |
|-----------------|-------------|
| Backdrop color | `var(--theme-loading-background-color)` |
| Card background `rgba(15,24,34,0.96)` | `var(--theme-mithril-card-background)` |
| Text colors | `var(--theme-mithril-text-color)`, `var(--theme-mithril-text-secondary)` |
| Button colors | `var(--theme-mithril-button-primary-background)`, etc. |

**Workflow (do NOT manually edit theme output files):**
1. Add new `--theme-mithril-*` variables to `createTheme` in `source/renderer/app/themes/utils/constants.ts` with both `light` and `dark` values.
2. Run `yarn themes:check:createTheme` to validate consistency.
3. Run `yarn themes:update` to propagate to ALL theme files automatically.
4. Variables follow naming pattern: `--theme-mithril-[element]-[property]`.
5. Run `yarn prettier:check` and `yarn compile` to verify.

### Storybook Conventions (per storybook-creation skill)

- **API**: Use `storiesOf('Loading / Mithril Bootstrap', module)` — NOT CSF exports.
- **Location**: `storybook/stories/loading/MithrilBootstrap.stories.tsx` (new `loading/` domain).
- **Registration**: Add `import './loading/MithrilBootstrap.stories'` to `storybook/stories/index.ts`.
- **Decorators**: Use `StoryDecorator` (theme provider) + `withKnobs` for interactive props.
- **Mock data**: Props-based — no MobX stores needed (components receive data via props).
- **States coverage**: default, loading, empty, error + all 12 Mithril-specific states.

### i18n Conventions (per i18n-messaging skill)

- **File**: `MithrilBootstrap.messages.ts` using `defineMessages()` from react-intl.
- **ID format**: `mithril.bootstrap.<messageKey>` (dot-separated, hierarchical namespace).
- **`defaultMessage` prefix**: All values must start with `!!!` (Daedalus convention).
- **`description` field**: Required on every message for translator context.
- **Placeholders**: Use `{variableName}` syntax, document in `description`.
- **Extraction**: Run `yarn i18n:manage` (combines extract + check) — do NOT run `i18n:extract` separately.
- **Locale files updated**: `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, and `translations/messages.json`.

### E2E Test Conventions (per e2e-test-creation skill)

- **Domain**: New `tests/mithril/` directory since no existing domain covers Mithril.
- **Structure**:
  ```
  tests/mithril/
  └── e2e/
      ├── features/
      │   └── mithril-bootstrap.feature
      └── steps/
          └── mithril-bootstrap.ts
  ```
- **Tags**: `@e2e` required on all feature files.
- **Framework**: Cucumber.js + Spectron + Chai assertions.
- **Selectors**: Use CSS class selectors matching SCSS module output (e.g., `.MithrilBootstrap_component`).
- **Context**: Access `daedalus.stores.mithrilBootstrap` via `this.client.execute()` to set/verify state.
- **Step definitions**: Use `async function` (not arrow functions), group selectors in `SELECTORS` const.

## Implementation Steps

### Phase 1: Backend Type & Pipeline Extensions
1. **Extend shared types** — Add `filesDownloaded`, `filesTotal` to `MithrilBootstrapStatusUpdate` and `stage` to `MithrilBootstrapError` in `source/common/types/mithril-bootstrap.types.ts`.
2. **Update backend progress pipeline** — Extend `mithrilProgress.ts` parser and `MithrilBootstrapService.ts` to preserve file counts and annotate errors with stage. Update `handleDiskSpace.ts` for `node-start` stage.
3. **Update unit tests** — Extend `MithrilBootstrapService.spec.ts` to verify `filesDownloaded`/`filesTotal` preservation and `stage` annotation on errors.

### Phase 2: Store & Container Updates
4. **Update MobX store** — Add `filesDownloaded`, `filesTotal` observables and `bytesDownloaded`, `throughputBps` computed properties to `MithrilBootstrapStore.ts`.
5. **Update MithrilBootstrapPage container** — Pass new store observables through to components.

### Phase 2b: Custom Chain Storage Location (Backend)
5b. **Add chain storage shared types** — Add `ChainStorageConfig` and `ChainStorageValidation` types to `source/common/types/mithril-bootstrap.types.ts`.
5c. **Add chain storage IPC channels** — Define `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` and `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL` in `source/common/ipc/api.ts`. Create main-side handlers in `source/main/ipc/chainStorageChannels.ts` and renderer-side clients in `source/renderer/app/ipc/chainStorageChannels.ts`.
5d. **Implement ChainStorageManager** — Create `source/main/utils/chainStorageManager.ts` with `validate()`, `setDirectory()`, `resetToDefault()`, `migrateData()`, `verifySymlink()`, and `getConfig()` methods. Use `fs.symlink()` (POSIX) / `fs.symlink(..., 'junction')` (Windows).
5e. **Integrate with disk space checks and download path** — Update `handleDiskSpace.ts` to resolve symlinks via `fs.realpath()` before calling `checkDiskSpace()`. Pass custom directory as `workDir` to `MithrilBootstrapService` constructor so downloads go directly to the target location. Update `MithrilBootstrapService._installSnapshot()` to use resolved paths.
5f. **Add chain storage store actions** — Add `customChainPath`, `chainStorageValidation`, `isChainStorageLoading` observables and `setChainStorageDirectory()`, `resetChainStorageDirectory()`, `loadChainStorageConfig()` actions to `MithrilBootstrapStore.ts`.
5g. **Add startup symlink verification** — Call `ChainStorageManager.verifySymlink()` during app init. If broken symlink detected, surface warning through `MithrilBootstrapStore`.
5h. **Unit tests for ChainStorageManager** — Test validate, setDirectory, resetToDefault, migrateData, verifySymlink with mocked fs. Test cross-device move fallback.

### Phase 3: i18n Message Extraction (use i18n-messaging skill)
6. **Extract i18n messages** — Create `MithrilBootstrap.messages.ts` with all existing messages + ~8 new stage-specific error messages. Follow conventions: `mithril.bootstrap.*` IDs, `!!!` prefix on `defaultMessage`, `description` on every message.
7. **Run i18n manage** — Run `yarn i18n:manage` to extract and validate translations across en-US and ja-JP locales.

### Phase 4: Component Decomposition
8. **Implement MithrilStepIndicator** — Multi-step visual stepper with spinner/check/pending states and download metadata display.
9. **Implement MithrilSnapshotSelector** — Extract and improve snapshot dropdown with truncated digests and formatted size labels.
10. **Implement MithrilSnapshotDetails** — Extract metadata grid into dedicated component.
11. **Implement MithrilErrorView** — Stage-specific error screens with contextual messaging and actions.
12. **Implement MithrilProgressView** — Compose stepper + progress bar + timing metadata.
13. **Implement MithrilDecisionView** — Extract decision screen with selector, details, and accept/decline buttons. Shown **after** storage location is confirmed.
13b. **Implement MithrilStorageLocationPicker** — Preliminary screen shown **before** the decision view. Directory picker component with current path display, available space indicator, validation feedback, "Change location" button (uses `SHOW_OPEN_DIALOG_CHANNEL`), "Reset to default" link, and "Continue" button to proceed to decision. Disable picker during active bootstrap/sync.
14. **Slim down root MithrilBootstrap.tsx** — Reduce to overlay chrome + view delegation. Route through storage picker screen → decision screen → progress/error screens based on store state.

### Phase 5: Theming & Styling (use theme-management skill)
15. **Migrate SCSS to theme variables** — Replace all hardcoded colors with `--theme-mithril-*` CSS custom properties.
16. **Add theme variables to createTheme** — Add `--theme-mithril-*` variables to `source/renderer/app/themes/utils/constants.ts` with `light` and `dark` values. Do NOT manually edit theme output files.
17. **Run theme tooling** — Run `yarn themes:check:createTheme` to validate, then `yarn themes:update` to propagate to all theme files.
18. **Regenerate SCSS type definitions** — Run `yarn typedef:sass`.

### Phase 6: Accessibility & Storybook (use storybook-creation skill)
19. **Add accessibility attributes** — `role="progressbar"`, `aria-live`, `aria-label`, keyboard nav.
20. **Create Storybook stories** — 12 stories using `storiesOf('Loading / Mithril Bootstrap', module)` API with `StoryDecorator` + `withKnobs`. Cover all component states.
21. **Register stories** — Create `storybook/stories/loading/` domain directory. Add `import './loading/MithrilBootstrap.stories'` to `storybook/stories/index.ts`.

### Phase 7: E2E Tests (use e2e-test-creation skill)
22. **Create E2E feature file** — Create `tests/mithril/e2e/features/mithril-bootstrap.feature` with `@e2e` tag. Cover decision screen, progress flow, and error states.
23. **Write step definitions** — Create `tests/mithril/e2e/steps/mithril-bootstrap.ts` with CSS selectors matching SCSS module classes, async step functions, and Chai assertions.

### Phase 8: Verification
24. **Run full verification** — `yarn compile`, `yarn lint`, `yarn prettier:check`, `yarn themes:check:createTheme`, `yarn i18n:manage`, `yarn test:jest`, `yarn storybook`.

## Testing Strategy

### Automated
- **Unit tests:** Extend `MithrilBootstrapService.spec.ts` to verify `filesDownloaded`/`filesTotal` preservation and error `stage` propagation.
- **TypeScript:** `yarn compile` — no new errors.
- **Lint:** `yarn lint` — clean.
- **Prettier:** `yarn prettier:check` — clean.
- **Theme validation:** `yarn themes:check:createTheme` — all `--theme-mithril-*` variables consistent across all themes.
- **i18n validation:** `yarn i18n:manage` — all messages extracted and validated for en-US and ja-JP.

### E2E Tests (Cucumber BDD)
New `tests/mithril/` domain with Gherkin scenarios:

| Scenario | Description |
|---|---|
| Decision screen displays | Verify snapshot selector and details render on decision status |
| User accepts bootstrap | Click accept, verify transition to preparing/downloading |
| User declines bootstrap | Click decline, verify alternative sync path |
| Progress stages advance | Verify step indicator transitions through preparing → downloading → verifying → finalizing |
| Error screen displays | Force error state, verify stage-specific messaging |
| Cancel during progress | Click cancel during download, verify return to decision or sync |
| Storage location change | Select external directory, verify symlink created and path displayed |
| Storage location reset | Reset to default, verify symlink removed and default path displayed |
| Storage location insufficient space | Select directory with insufficient space, verify error displayed |
| Storage location during active sync | Verify picker is disabled during bootstrap progress |

**Note:** E2E tests set MithrilBootstrap store state directly via `this.client.execute(() => daedalus.stores.mithrilBootstrap)` since a real Mithril bootstrap requires a running cardano-node backend.

### Storybook
All 12 stories render correctly across all states; verify theme variable inheritance:

| Story | State |
|---|---|
| Decision — Loading | `status: 'decision'`, `isFetchingSnapshots: true`, empty snapshots |
| Decision — With Snapshots | `status: 'decision'`, 3 snapshots, "latest" selected |
| Decision — Specific Snapshot | `status: 'decision'`, specific digest selected, details visible |
| Progress — Preparing | `status: 'preparing'`, `progress: 5` |
| Progress — Downloading (early) | `status: 'downloading'`, `progress: 15`, partial bytes |
| Progress — Downloading (mid) | `status: 'downloading'`, `progress: 55`, bytes + throughput + timing |
| Progress — Verifying | `status: 'verifying'`, `progress: 92.5` |
| Progress — Finalizing | `status: 'converting'`, `progress: 97.5` |
| Error — Download Failed | `status: 'failed'`, `error.stage: 'download'` |
| Error — Verify Failed | `status: 'failed'`, `error.stage: 'verify'` |
| Error — Convert Failed | `status: 'failed'`, `error.stage: 'convert'` |
| Error — Node Start Failed | `status: 'failed'`, `error.stage: 'node-start'` |
| Storage — Default Path | `customChainPath: null`, default resolved path shown |
| Storage — Custom Path | `customChainPath: '/mnt/external/daedalus-chain'`, custom path + available space shown |
| Storage — Validation Error | `chainStorageValidation: { isValid: false, error: 'Insufficient space' }` |
| Storage — Migrating | `isChainStorageLoading: true`, spinner + "Moving chain data..." |

### Manual QA
Walk through the full Mithril bootstrap flow in dev mode (`yarn dev`), verifying:
- Decision screen renders with formatted snapshot options (truncated digests).
- Progress screen shows step stepper with spinner/checkmark transitions.
- Download step shows bytes, throughput, and timing.
- Error screen shows stage-appropriate messaging.
- Cancel and decline paths work correctly.
- Storage location picker displays current path and available space.
- Selecting a custom directory creates symlink and shows updated path/space.
- Resetting to default removes symlink and shows default path.
- Storage picker is disabled during active bootstrap progress.
- Restarting app with custom path shows the custom path and verifies symlink.

## Design Decisions

1. **Decomposition pattern:** Follows `SyncingConnecting` precedent (5 sub-components) rather than keeping a monolith — matches codebase conventions for complex loading screens.
2. **Throughput calculation:** Computed in the MobX store (`@computed`) from `filesDownloaded / filesTotal * snapshotSize / elapsedSeconds` rather than in the backend — avoids adding timer logic to the service layer.
3. **Step animations only (no view transitions):** Spinner/checkmark icons animate per step, but no CSS transitions between decision/progress/error views.
4. **Theme variable registration:** New `--theme-mithril-*` variables added to `createTheme` in `constants.ts` and propagated to ALL theme files via `yarn themes:update` — ensures consistency across all theme variants.
5. **No new IPC channels:** All new data (`filesDownloaded`, `filesTotal`, error `stage`) flows through the existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL` — just extended type payloads.
6. **Storybook API:** Uses `storiesOf()` (legacy API) per Daedalus convention — NOT Component Story Format (CSF).
7. **E2E test mocking:** E2E tests manipulate store state directly via `daedalus.stores` since real Mithril bootstrap requires a running cardano-node backend.
8. **Symlink strategy for chain storage:** Use POSIX symlinks on Linux/macOS and NTFS junctions on Windows (no admin privileges required). Symlinks are transparent to `fs.move()`, `fs.pathExists()`, and `checkDiskSpace()` after `fs.realpath()` resolution.
9. **Chain storage config persistence:** Custom path stored in `{stateDir}/chain-storage-config.json` rather than Electron `electron-store` or app settings — keeps it co-located with chain data and survives app reinstalls.
10. **Disable picker during sync:** Storage location picker is disabled when bootstrap status is `preparing`, `downloading`, `verifying`, or `converting` to prevent filesystem race conditions.

## Rollout Plan
- This is a UX refinement of existing functionality; no feature flags needed.
- Deploy alongside the existing Mithril bootstrap feature.
- Light-theme differentiation deferred to a future PR.

## Open Questions
- Should the copy-to-clipboard button on snapshot digest be included now or flagged for a future iteration?
- Should throughput display use a rolling average or instantaneous rate?
- **E2E test approach:** Should E2E tests mock the Mithril backend entirely (set store state directly via `daedalus.stores.mithrilBootstrap`) since a real bootstrap requires cardano-node, or should they be tagged `@skip` for CI and only run manually against a real backend?
- **Chain storage migration UX:** Should data migration show a progress bar (requires monitoring `fs.move()` progress), or is a spinner with estimated time sufficient?
- **Chain storage on first launch:** Should the storage location picker appear on first launch (before any sync), or only after the user has an existing chain directory?
- **External drive disconnection:** If the user disconnects an external drive while Daedalus is running, how should the app respond? Immediate error screen, periodic health check, or passive failure when the next disk operation fails?
- **Theme createTheme structure:** The theme-management skill shows a flat key format in `constants.ts`, but the actual codebase uses sectioned objects in theme output files (e.g., `aboutWindow: { ... }`). Should the new mithril variables be added as a `mithril` section key in the theme output, or through the `createTheme` params? (Needs verification of how `themes:check:createTheme` and `themes:update` handle new sections.)

## References
- Parent feature plan: [bootstrap-cardano-node.md](bootstrap-cardano-node.md)
- `SyncingConnecting` component pattern: `source/renderer/app/components/loading/syncing-connecting/`
- `SyncingProgress` stepper reference: `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`

---

**Status:** 🚧 In Progress  
**Date:** 2026-03-03  
**Author:** david-profrontsolutions
