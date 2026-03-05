# Mithril Snapshot UX Refinement

## Overview
Decompose the monolithic `MithrilBootstrap.tsx` into focused sub-components following the `SyncingConnecting` pattern. Add a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing) modeled after `SyncingProgress`. Surface rich download metadata (bytes transferred, throughput, timing). Implement stage-specific error screens. Migrate all hardcoded dark RGBA values to CSS theme variables. Create full Storybook coverage for all states. Extend backend types and status updates to carry `filesDownloaded`, `filesTotal`, and error `stage` through IPC.

## Requirements

### Backend
- [ ] Extend shared types with `filesDownloaded`, `filesTotal` on status updates and `stage` on errors.
- [ ] Update backend progress pipeline to preserve raw file counts and annotate errors with failure stage.
- [ ] Add MobX computed properties for `bytesDownloaded` and `throughputBps`.

### Component Decomposition
- [ ] Decompose `MithrilBootstrap.tsx` (424 lines) into 6+ focused sub-components.
- [ ] Implement a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing).
- [ ] Surface download metadata (bytes transferred, throughput, timing) in the progress view.
- [ ] Implement stage-specific error screens with contextual titles, hints, and actions.
- [ ] Extract snapshot selector into a reusable component with truncated digest labels.
- [ ] Extract snapshot details grid into a dedicated component.
- [ ] Add accessibility attributes (`role="progressbar"`, `aria-live`, `aria-label`, keyboard nav).

### i18n (per i18n-messaging skill)
- [ ] Extract all i18n messages into a dedicated `MithrilBootstrap.messages.ts` file.
- [ ] Add ~8 new messages for stage-specific errors, download metadata labels, and step indicators.
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
  - `source/main/utils/handleDiskSpace.ts`: Annotate node-start failure errors with `stage: 'node-start'`.
- **Renderer — Store**
  - `source/renderer/app/stores/MithrilBootstrapStore.ts`: Add observables and computed properties for download metadata.
- **Renderer — Components** (new sub-component directory)
  - `MithrilBootstrap.tsx` — Root: overlay + backdrop + view delegation (slimmed down).
  - `MithrilDecisionView.tsx` — Decision screen (title, description, selector, details, buttons).
  - `MithrilProgressView.tsx` — Progress screen (stepper + bar + meta + cancel).
  - `MithrilErrorView.tsx` — Error screen (stage-specific title/hint/actions).
  - `MithrilStepIndicator.tsx` — Reusable step list (spinner/check per step).
  - `MithrilSnapshotSelector.tsx` — Snapshot dropdown with truncated digest labels.
  - `MithrilSnapshotDetails.tsx` — Metadata grid (digest, date, size, node version).
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

### IPC Changes
- **No new IPC channels.** All new data (`filesDownloaded`, `filesTotal`, error `stage`) flows through the existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL` via extended type payloads.

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

### Phase 3: i18n Message Extraction (use i18n-messaging skill)
6. **Extract i18n messages** — Create `MithrilBootstrap.messages.ts` with all existing messages + ~8 new stage-specific error messages. Follow conventions: `mithril.bootstrap.*` IDs, `!!!` prefix on `defaultMessage`, `description` on every message.
7. **Run i18n manage** — Run `yarn i18n:manage` to extract and validate translations across en-US and ja-JP locales.

### Phase 4: Component Decomposition
8. **Implement MithrilStepIndicator** — Multi-step visual stepper with spinner/check/pending states and download metadata display.
9. **Implement MithrilSnapshotSelector** — Extract and improve snapshot dropdown with truncated digests and formatted size labels.
10. **Implement MithrilSnapshotDetails** — Extract metadata grid into dedicated component.
11. **Implement MithrilErrorView** — Stage-specific error screens with contextual messaging and actions.
12. **Implement MithrilProgressView** — Compose stepper + progress bar + timing metadata.
13. **Implement MithrilDecisionView** — Extract decision screen with selector, details, and buttons.
14. **Slim down root MithrilBootstrap.tsx** — Reduce to overlay chrome + view delegation.

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

### Manual QA
Walk through the full Mithril bootstrap flow in dev mode (`yarn dev`), verifying:
- Decision screen renders with formatted snapshot options (truncated digests).
- Progress screen shows step stepper with spinner/checkmark transitions.
- Download step shows bytes, throughput, and timing.
- Error screen shows stage-appropriate messaging.
- Cancel and decline paths work correctly.

## Design Decisions

1. **Decomposition pattern:** Follows `SyncingConnecting` precedent (5 sub-components) rather than keeping a monolith — matches codebase conventions for complex loading screens.
2. **Throughput calculation:** Computed in the MobX store (`@computed`) from `filesDownloaded / filesTotal * snapshotSize / elapsedSeconds` rather than in the backend — avoids adding timer logic to the service layer.
3. **Step animations only (no view transitions):** Spinner/checkmark icons animate per step, but no CSS transitions between decision/progress/error views.
4. **Theme variable registration:** New `--theme-mithril-*` variables added to `createTheme` in `constants.ts` and propagated to ALL theme files via `yarn themes:update` — ensures consistency across all theme variants.
5. **No new IPC channels:** All new data (`filesDownloaded`, `filesTotal`, error `stage`) flows through the existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL` — just extended type payloads.
6. **Storybook API:** Uses `storiesOf()` (legacy API) per Daedalus convention — NOT Component Story Format (CSF).
7. **E2E test mocking:** E2E tests manipulate store state directly via `daedalus.stores` since real Mithril bootstrap requires a running cardano-node backend.

## Rollout Plan
- This is a UX refinement of existing functionality; no feature flags needed.
- Deploy alongside the existing Mithril bootstrap feature.
- Light-theme differentiation deferred to a future PR.

## Open Questions
- Should the copy-to-clipboard button on snapshot digest be included now or flagged for a future iteration?
- Should throughput display use a rolling average or instantaneous rate?
- **E2E test approach:** Should E2E tests mock the Mithril backend entirely (set store state directly via `daedalus.stores.mithrilBootstrap`) since a real bootstrap requires cardano-node, or should they be tagged `@skip` for CI and only run manually against a real backend?
- **Theme createTheme structure:** The theme-management skill shows a flat key format in `constants.ts`, but the actual codebase uses sectioned objects in theme output files (e.g., `aboutWindow: { ... }`). Should the new mithril variables be added as a `mithril` section key in the theme output, or through the `createTheme` params? (Needs verification of how `themes:check:createTheme` and `themes:update` handle new sections.)

## References
- Parent feature plan: [bootstrap-cardano-node.md](bootstrap-cardano-node.md)
- `SyncingConnecting` component pattern: `source/renderer/app/components/loading/syncing-connecting/`
- `SyncingProgress` stepper reference: `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`

---

**Status:** 🚧 In Progress  
**Date:** 2026-03-03  
**Author:** david-profrontsolutions
