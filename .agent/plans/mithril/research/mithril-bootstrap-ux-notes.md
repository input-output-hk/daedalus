# Mithril bootstrap UX notes

## Sources
- `source/renderer/app/components/loading/mithril-bootstrap/`
- `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`
- `source/renderer/app/components/widgets/collapsible-section/CollapsibleSection.tsx`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`

## Component boundaries
- `BlockDataStorageLocationPicker` owns the preliminary blockchain-data location screen: current/default path summary, available-space card, picker/reset actions, and deferred apply-on-continue behavior.
- `MithrilDecisionView` owns the pre-bootstrap decision composition: title/description copy, selector, details card, and accept/decline actions.
- `MithrilSnapshotSelector` owns the snapshot dropdown row and formats concrete snapshot options as truncated digest plus localized created timestamp and formatted size.
- `MithrilSnapshotDetails` owns the metadata card and truncates the displayed digest while preserving the full digest in the hover title.
- `MithrilErrorView` owns failed-state copy mapping, collapsible diagnostics, and log-path linking.
- `MithrilProgressView` owns the mounted progress composition: step indicator, progress bar, explicit download/timing metadata, and cancel action.
- Root `MithrilBootstrap` still owns overlay chrome and top-level view routing around the extracted `BlockDataStorageLocationPicker` and the remaining Mithril views.

## Data and state flow
- `MithrilBootstrapStore` is the source of snapshot metadata, derived download metrics, staged error state, and chain-storage picker state.
- `MithrilBootstrapPage` is the contract boundary: it normalizes the `latest` sentinel, resolves the selected snapshot, and forwards store/app actions into the extracted Mithril view components.
- Store-to-view routing now threads `defaultChainPath`, `defaultChainStorageValidation`, and the latest snapshot size into `BlockDataStorageLocationPicker` so the storage screen can render a meaningful default branch before any directory mutation.
- Optional Mithril status fields should be assigned with property-presence checks (`'field' in update`) so explicit backend resets like `filesDownloaded: undefined` propagate correctly.
- `MithrilBootstrapStore.loadChainStorageConfig()` should validate persisted custom paths during setup so broken storage targets surface immediately in renderer state.
- `validateChainStorageDirectory()` is the probe-only action for picker previews; `setChainStorageDirectory()` / `resetChainStorageDirectory()` remain the commit-time mutations used only after the user presses `Continue`.
- `storageLocationConfirmed` is not a permanent one-way flag: the store clears it both when the decision view sends users back to the storage picker and when backend status re-enters a fresh decision-cycle state.

## UI behavior reminders
- `snapshotFormatting.ts` is the shared source for digest truncation plus localized date and size formatting; keep selector/details formatting aligned there.
- `formatTransferSize()` is the local wrapper for download-progress byte values because the shared `formattedBytesToSize()` helper renders `0` as `n/a`.
- `SyncingProgress.tsx` is the structural model for Mithril progress sub-components: functional component, `contextTypes`, `intlShape`, `SVGInline`, and CSS-module state classes.
- In the loading domain, new components should prefer `import classNames from 'classnames'`; `cx` exists in `SyncingProgress.tsx` but is the minority convention.
- `loading-spin` is already available as a global keyframe from `themes/mixins/loading-spinner.scss`; Mithril loading components can reference it directly with `:global { animation: loading-spin ... }`.
- The current 90% UX gap comes from backend phase mapping, not missing renderer math: `MithrilBootstrapService` maps Mithril CLI download progress into 10-90%, then `_installSnapshot()` does the heavy local file move/copy after status has already left `downloading`.
- Task 021a improved the plateau by adding explicit `installing`/`finalizing` phases and download-only transfer stats, but follow-up task-024a should collapse the visible UX to `preparing -> downloading -> finalizing`, with finalizing covering post-download install plus cleanup/handoff.
- Follow-up task-024b should rename install-related post-download state/copy to `unpacking` where internal sub-phases still exist, while the visible UX stays on finalizing.
- Follow-up task-024c should remove `currentStep` as display transport and let the renderer derive localized labels directly from `status`.
- Snapshot metadata UX uses `total_db_size_uncompressed` as the size source and should show digest, size, created timestamp, and node version. Created timestamps should prefer local time formatting with raw-string fallback when parsing fails.
- `BlockDataStorageLocationPicker` uses the latest snapshot size as the recommended-space hint when it is already available; otherwise it falls back to a generic high-capacity warning and still shows the current available-space reading.
- The picker treats the default chain path as a first-class selection, validates custom candidates locally after `SHOW_OPEN_DIALOG_CHANNEL`, and defers all on-disk changes until `Continue`; this keeps browsing/reset actions side-effect free.
- The decision view now includes a storage-context row with the selected blockchain-data location and a "Back to directory location" action so users can change storage without leaving the Mithril decision cycle.
- `MithrilErrorView` maps backend `error.stage` values onto the extracted Mithril error-title/hint messages and keeps raw `error.message`/`error.code` in a collapsible diagnostic section.
- Local `error.logPath` values should be converted to `file://` URLs with a renderer-safe helper before sending them through `stores.app.openExternalLink`; do not import Node `url.pathToFileURL()` in the browser bundle.
- Mithril loading components should follow the same image-import depth as `SyncingProgress.tsx`; from `components/loading/mithril-bootstrap/`, shared loading icons live under `../../../assets/images/`.
- If Mithril UI strings change, run `yarn i18n:manage` to update `translations/messages.json`, `translations/en-US.json`, and `translations/ja-JP.json`.
- `.scss.d.ts` files are regenerated during `yarn compile`; do not hand-edit them unless tooling is unavailable.

## Current gaps
- `BlockDataStorageLocationPicker` is the generic blockchain-data location picker; keep future copy and file references aligned to that name instead of Mithril-specific wording.
- Chain-storage UI state already lives in `MithrilBootstrapStore`; keep it there as the block-data storage picker evolves.

## QA pointer
- Manual QA for the full Mithril flow lives in `.agent/plans/mithril/bootstrap-cardano-node.md` under Testing Strategy.
