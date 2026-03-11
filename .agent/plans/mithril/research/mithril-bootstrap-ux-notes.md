# Mithril bootstrap UX notes

## Sources
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingStatus.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`

## Notes
- `SyncingProgress.tsx` is the structural model for Mithril progress sub-components: functional component, `contextTypes`, `intlShape`, `SVGInline`, CSS-module state classes.
- In the loading domain, new components should prefer `import classNames from 'classnames'`; `cx` exists in `SyncingProgress.tsx` but is the minority convention.
- `loading-spin` is already available as a global keyframe from `themes/mixins/loading-spinner.scss`; Mithril stepper SCSS can reference it directly with `:global { animation: loading-spin ... }`.
- `MithrilBootstrapStore.snapshot?.size` is the total size source for download metadata. `bytesDownloaded` and `throughputBps` are already derived in the store.
- `MithrilBootstrapStore` should call `syncStatus()` during setup so the renderer can recover cached Mithril decision/progress state if the initial broadcast was missed.
- Decision/progress UX depends on main IPC caching the current Mithril status so renderer status requests can recover after reload or onboarding transitions.
- `handleDiskSpace` originally gated Mithril decision emission on `_startupTries === 0`; removing that gate is what allows the decision overlay to appear while the node is stopped.
- The Mithril overlay was previously hidden while onboarding (`app.isSetupPage`) and when the node was stopped; decision/progress views need both gates removed so setup routes remain visible while Mithril UI is active.
- If the user declines Mithril, emit an `idle` status before normal sync starts so the Mithril overlay clears reliably.
- If Mithril UI strings log missing ids, run `yarn i18n:manage` to update `translations/messages.json`, `translations/en-US.json`, and `translations/ja-JP.json`.
- `MithrilBootstrapStore` should assign optional progress fields using property-presence checks (`'field' in update`) so explicit backend resets like `filesDownloaded: undefined` are not dropped.
- `MithrilBootstrapStore` owns chain-storage UI state/actions (`customChainPath`, validation/loading state, config preload, set/reset/confirm flows) and should remain the single source of truth for the storage picker.
- `MithrilBootstrapPage` is the contract boundary that forwards store state/actions into the Mithril UI components; decomposition tasks should keep new view components dumb and consume props from there.
- `MithrilBootstrapStore.loadChainStorageConfig()` should validate configured custom paths during setup so invalid storage targets surface immediately in renderer state.
- Renderer-store tests already cover chain-storage config loading, set/reset flows, and confirmation state transitions; extend those tests instead of introducing parallel fixtures.
- `MithrilStepIndicator` is currently implemented but not mounted anywhere. User-visible stepper/metadata changes do not land until `MithrilProgressView` and the root `MithrilBootstrap` delegation work are integrated.
- Keep PRD checklist items user-visible: component-level completion belongs in task notes, but progress-view completion should wait until the new progress view replaces `MithrilBootstrap.renderProgress()`.
- `downloadBytesLabel` and `downloadRateLabel` message descriptors exist but are not yet consumed by the compact stepper row; they remain available for future progress-view copy or a11y labels.
- Snapshot metadata UX uses `total_db_size_uncompressed` as the size source and should show digest, size, created timestamp, and node version. Created timestamps should prefer local time formatting with raw-string fallback when parsing fails.
- Manual QA for the full Mithril flow lives in `.agent/plans/mithril/bootstrap-cardano-node.md` under Testing Strategy.
- `.scss.d.ts` files are regenerated during `yarn compile`; do not hand-edit them unless tooling is unavailable.
