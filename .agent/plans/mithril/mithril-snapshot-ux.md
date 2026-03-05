# Mithril Snapshot UX Refinement

## Overview
Decompose the monolithic `MithrilBootstrap.tsx` into focused sub-components following the `SyncingConnecting` pattern. Add a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing) modeled after `SyncingProgress`. Surface rich download metadata (bytes transferred, throughput, timing). Implement stage-specific error screens. Migrate all hardcoded dark RGBA values to CSS theme variables. Create full Storybook coverage for all states. Extend backend types and status updates to carry `filesDownloaded`, `filesTotal`, and error `stage` through IPC.

## Requirements
- [ ] Extend shared types with `filesDownloaded`, `filesTotal` on status updates and `stage` on errors.
- [ ] Update backend progress pipeline to preserve raw file counts and annotate errors with failure stage.
- [ ] Add MobX computed properties for `bytesDownloaded` and `throughputBps`.
- [ ] Decompose `MithrilBootstrap.tsx` (424 lines) into 6+ focused sub-components.
- [ ] Implement a multi-step visual stepper (Preparing → Downloading → Verifying → Finalizing).
- [ ] Surface download metadata (bytes transferred, throughput, timing) in the progress view.
- [ ] Implement stage-specific error screens with contextual titles, hints, and actions.
- [ ] Extract snapshot selector into a reusable component with truncated digest labels.
- [ ] Extract snapshot details grid into a dedicated component.
- [ ] Migrate all hardcoded RGBA/color values to CSS theme variables (`--theme-mithril-*`).
- [ ] Register new theme variables in both `cardano.ts` (dark) and `light-blue.ts` (light) themes.
- [ ] Extract all i18n messages into a dedicated messages file with ~8 new messages.
- [ ] Create Storybook stories for all 12 component states.
- [ ] Add accessibility attributes (`role="progressbar"`, `aria-live`, `aria-label`, keyboard nav).
- [ ] Regenerate SCSS type definitions after all SCSS changes.

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
- **Themes**
  - `source/renderer/app/themes/daedalus/cardano.ts`: Register `--theme-mithril-*` CSS custom properties (dark values).
  - `source/renderer/app/themes/daedalus/light-blue.ts`: Register `--theme-mithril-*` CSS custom properties (matching dark values as defaults).
- **Storybook**
  - `storybook/stories/loading/MithrilBootstrap.stories.tsx`: 12 stories covering all states.

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

### Theme Variable Migration
Replace hardcoded values:

| Hardcoded Value | New Variable |
|-----------------|-------------|
| Backdrop color | `var(--theme-loading-background-color)` |
| Card background `rgba(15,24,34,0.96)` | `var(--theme-mithril-card-background)` |
| Text colors | `var(--theme-mithril-text-color)`, `var(--theme-mithril-text-secondary)` |
| Button colors | `var(--theme-mithril-button-primary-background)`, etc. |

Register all `--theme-mithril-*` properties in both theme files with identical dark values as defaults.

## Implementation Steps

1. **Extend shared types** — Add `filesDownloaded`, `filesTotal` to `MithrilBootstrapStatusUpdate` and `stage` to `MithrilBootstrapError` in `source/common/types/mithril-bootstrap.types.ts`.
2. **Update backend progress pipeline** — Extend `mithrilProgress.ts` parser and `MithrilBootstrapService.ts` to preserve file counts and annotate errors with stage. Update `handleDiskSpace.ts` for `node-start` stage.
3. **Update unit tests** — Extend `MithrilBootstrapService.spec.ts` to verify `filesDownloaded`/`filesTotal` preservation and `stage` annotation on errors.
4. **Update MobX store** — Add `filesDownloaded`, `filesTotal` observables and `bytesDownloaded`, `throughputBps` computed properties to `MithrilBootstrapStore.ts`.
5. **Extract i18n messages** — Create `MithrilBootstrap.messages.ts` with all existing messages + ~8 new stage-specific error messages.
6. **Decompose MithrilBootstrap** — Create sub-component directory with `MithrilDecisionView`, `MithrilProgressView`, `MithrilErrorView`, `MithrilStepIndicator`, `MithrilSnapshotSelector`, `MithrilSnapshotDetails`.
7. **Implement MithrilStepIndicator** — Multi-step visual stepper with spinner/check/pending states and download metadata display.
8. **Implement MithrilSnapshotSelector** — Extract and improve snapshot dropdown with truncated digests and formatted size labels.
9. **Implement MithrilSnapshotDetails** — Extract metadata grid into dedicated component.
10. **Implement MithrilErrorView** — Stage-specific error screens with contextual messaging and actions.
11. **Implement MithrilProgressView** — Compose stepper + progress bar + timing metadata.
12. **Slim down root MithrilBootstrap.tsx** — Reduce to overlay chrome + view delegation.
13. **Update MithrilBootstrapPage container** — Pass new store observables through to components.
14. **Migrate SCSS to theme variables** — Replace all hardcoded colors with `--theme-mithril-*` variables.
15. **Register theme variables** — Add `--theme-mithril-*` properties to `cardano.ts` and `light-blue.ts`.
16. **Add accessibility attributes** — `role="progressbar"`, `aria-live`, `aria-label`, keyboard nav.
17. **Create Storybook stories** — 12 stories covering all component states.
18. **Regenerate SCSS type definitions** — Run `yarn typedef:sass`.
19. **Run i18n extraction** — Run `yarn i18n:extract` and `yarn i18n:manage`.

## Testing Strategy

### Automated
- **Unit tests:** Extend `MithrilBootstrapService.spec.ts` to verify `filesDownloaded`/`filesTotal` preservation and error `stage` propagation.
- **TypeScript:** `yarn compile` — no new errors.
- **Lint:** `yarn lint` — clean.
- **Prettier:** `yarn prettier:check` — clean.

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
4. **Theme variable registration:** New `--theme-mithril-*` variables registered in both themes with identical dark values — establishes the extension point for future light-theme differentiation without requiring it now.
5. **No new IPC channels:** All new data (`filesDownloaded`, `filesTotal`, error `stage`) flows through the existing `MITHRIL_BOOTSTRAP_STATUS_CHANNEL` — just extended type payloads.

## Rollout Plan
- This is a UX refinement of existing functionality; no feature flags needed.
- Deploy alongside the existing Mithril bootstrap feature.
- Light-theme differentiation deferred to a future PR.

## Open Questions
- Should the copy-to-clipboard button on snapshot digest be included now or flagged for a future iteration?
- Should throughput display use a rolling average or instantaneous rate?

## References
- Parent feature plan: [bootstrap-cardano-node.md](bootstrap-cardano-node.md)
- `SyncingConnecting` component pattern: `source/renderer/app/components/loading/syncing-connecting/`
- `SyncingProgress` stepper reference: `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`

---

**Status:** 🚧 In Progress  
**Date:** 2026-03-03  
**Author:** david-profrontsolutions
