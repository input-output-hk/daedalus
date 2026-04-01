# Mithril bootstrap UX notes

## Sources
- `source/renderer/app/components/loading/mithril-bootstrap/`
- `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx`
- `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingProgress/SyncingProgress.tsx`
- `source/renderer/app/components/widgets/collapsible-section/CollapsibleSection.tsx`
- `storybook/stories/loading/MithrilBootstrap.stories.tsx`
- `storybook/stories/index.ts`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`

## Component boundaries
- `BlockDataStorageLocationPicker` owns the preliminary blockchain-data location screen: current/default path summary, available-space card, picker/reset actions, and deferred apply-on-continue behavior.
- `MithrilDecisionView` owns the pre-bootstrap decision composition: title/description copy, selector, details card, and accept/decline actions.
- `MithrilSnapshotSelector` owns the snapshot dropdown row and formats concrete snapshot options as truncated digest plus localized created timestamp and formatted size.
- `MithrilSnapshotDetails` owns the metadata card and truncates the displayed digest while preserving the full digest in the hover title.
- `MithrilErrorView` owns failed-state copy mapping, collapsible diagnostics, and log-path linking.
- `MithrilProgressView` owns the mounted progress composition: step indicator, progress bar, explicit download/timing metadata, cancel action, and the progress heading copy.
- Root `MithrilBootstrap` now owns only the overlay shell and a single active view selection; view-specific headings stay inside the delegated child components.

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
- The visible Mithril stepper is now a 3-step flow: `preparing -> downloading -> finalizing`; main emits internal `unpacking`/`converting` statuses for post-download local work, and the renderer maps those back into the visible finalizing step while the overlay stays mounted through cleanup and handoff.
- The post-restore Cardano startup handoff must not reuse `completed` as the long-lived UI state. `completed` is now the restore-complete milestone, and `starting-node` is the visible handoff state used for the node-start spinner/cancel-disable block.
- Cancellation-to-genesis is a separate backend recovery path from failure-to-genesis. If the user cancels Mithril mid-download and then chooses `Sync from genesis`, the backend must handle `cancelled -> decline` immediately; relying on the next disk-space poll leaves the renderer on the stopped-node loading screen even though genesis was selected.
- A previous bug let the generic Cardano crash-restart path fire during active Mithril bootstrap, which could restart Cardano outside the Mithril gate and strand the overlay on a fully checked-off waterfall. UX now relies on main suppressing generic restarts while Mithril is active and converting handoff instability into a Mithril `node-start` failure.
- Task-024h finding: waterfall sub-items are intentionally not pre-seeded in the renderer; they appear only as backend waterfall events arrive so the UI reflects actual Mithril step emission rather than a fabricated pending list.
- Preferred follow-up: keep late-download verification as copy inside `downloading`, then instrument `_installSnapshot()` and cleanup/handoff directly so finalizing can expose real local restore telemetry rather than guessed percentages.
- Keep the step indicator horizontally pinned across the full progress view, and let the current-status panel focus on stage copy rather than redundant progress/timing metadata.
- Post-download Mithril service/detail copy should use `unpacking` rather than `installing` wherever internal sub-phases are still exposed while the visible UX stays on finalizing.
- Task-024c completed: Mithril no longer transports `currentStep`; `MithrilProgressView` and `MithrilStepIndicator` derive localized headings/labels directly from `status`.
- Disk-space polling must honor an existing accepted Mithril decision before re-emitting `decision`, or the renderer can bounce from the progress state back to the storage picker during post-download work.
- Destination-size polling against snapshot size is documented only as a fallback option; it is not the preferred UX contract because same-device moves and cleanup phases make it too jumpy.
- Snapshot metadata UX uses `total_db_size_uncompressed` as the size source and should show digest, size, created timestamp, and node version. Created timestamps should prefer local time formatting with raw-string fallback when parsing fails.
- `BlockDataStorageLocationPicker` uses the latest snapshot size as the recommended-space hint when it is already available; otherwise it falls back to a generic high-capacity warning and still shows the current available-space reading.
- The picker treats the default chain path as a first-class selection, validates custom candidates locally after `SHOW_OPEN_DIALOG_CHANNEL`, and defers all on-disk changes until `Continue`; this keeps browsing/reset actions side-effect free.
- The decision view now includes a storage-context row with the selected blockchain-data location and a "Back to directory location" action so users can change storage without leaving the Mithril decision cycle.
- `MithrilErrorView` maps backend `error.stage` values onto the extracted Mithril error-title/hint messages and keeps raw `error.message`/`error.code` in a collapsible diagnostic section.
- Local `error.logPath` values should be converted with a renderer-safe helper before sending them through `stores.app.openExternalLink`; Windows drive-letter paths must normalize to `file:///...` URLs so they are not misread as URI schemes. Do not import Node `url.pathToFileURL()` in the browser bundle.
- Mithril loading components should follow the same image-import depth as `SyncingProgress.tsx`; from `components/loading/mithril-bootstrap/`, shared loading icons live under `../../../assets/images/`.
- If Mithril UI strings change, run `yarn i18n:manage` to update `translations/messages.json`, `translations/en-US.json`, and `translations/ja-JP.json`.
- `.scss.d.ts` files are regenerated during `yarn compile`; do not hand-edit them unless tooling is unavailable.

## Accessibility and Storybook reminders
- `MithrilBootstrap` is the accessibility shell for the overlay: dialog semantics belong at the root, while the active child view owns the visible heading referenced by `aria-labelledby`.
- `MithrilDecisionView`, `MithrilProgressView`, and `MithrilErrorView` currently rely on dialog labeling and local semantic structure rather than extra mount-time focus management; avoid adding heading autofocus unless there is a specific accessibility requirement and reviewed visual treatment.
- `MithrilSnapshotSelector` owns snapshot-picking semantics and should expose a grouped, labeled control even if the underlying select implementation does not provide a stable input id.
- `MithrilSnapshotDetails` is metadata, not layout-only chrome; semantic description-list markup is the durable contract for digest, size, time, and version rows.
- `BlockDataStorageLocationPicker` owns label association, validation messaging, and status feedback for blockchain-data directory selection; keep those accessibility relationships local to the picker.
- Preserve the existing `InlineProgressBar` contract: `role='progressbar'` with `aria-valuenow`, `aria-valuemin`, and `aria-valuemax` is already correct.
- `MithrilStepIndicator` should remain a semantic list/listitem structure with `aria-current` on the active step, but its region labels must come from `react-intl` message keys rather than hard-coded English strings.
- Decorative Mithril status SVGs should stay hidden from assistive technology with `aria-hidden='true'`.
- Live regions should stay scoped to meaningful state changes such as error messaging and chain-storage validation or apply feedback, not the entire waterfall step list.
- Mithril loading stories live in the loading domain, use the repo's legacy `storiesOf` registration pattern, and rely on `StoryDecorator` without extra provider scaffolding.
- Keep Mithril stories props-driven and visual: chain-storage browsing depends on IPC and should not be exercised directly inside Storybook.
- Maintain Mithril Storybook coverage for decision, storage, progress, and error states so loading regressions remain inspectable outside the full runtime flow.
- The centralized `accessibilityIds` pattern is the stable way to keep dialog labeling, heading ids, field descriptions, and validation relationships consistent across Mithril views.
- `react-polymorph` can be brittle under jsdom, so Mithril accessibility specs are more reliable when they assert semantic roles and attributes instead of deep component internals.

## Final verification insights
- Final verification confirmed the Mithril bootstrap flow and standalone chain-storage picker pass the automated Mithril-scope checks.
- Runtime log analysis on 2026-04-01 confirmed the cancel-path bug and its fix: logs showed `accept` during Mithril start, later `decline` with Mithril status `cancelled`, and no `cardano-node.start()` until explicit `cancelled -> decline` recovery was added in `handleDiskSpace.ts`.
- Follow-up PR review hardening on 2026-04-01 added an explicit in-flight guard around `handleMithrilCancelledDecline()` so overlapping polling and decision-listener triggers become no-ops while cancel-to-genesis recovery is already wiping/startup sequencing.
- Theme structure is correct in all 9 Daedalus runtime theme files, with `chainStorage` and `mithrilBootstrap` tokens at the expected nesting level.
- Jest passed with 37 suites and 295 tests, including `MithrilBootstrapService.spec.ts`, `MithrilBootstrapStore.spec.ts`, `MithrilBootstrap.spec.tsx`, `MithrilProgressView.spec.tsx`, `MithrilStepIndicator.spec.tsx`, `ChainStorageLocationPicker.spec.tsx`, `chainStorageChannel.spec.ts`, and `chainStorageManager.spec.ts`.
- Targeted follow-up Jest coverage now also exercises `mithrilCommandRunner.spec.ts` for installed-path/binary-name resolution branches and `handleDiskSpace.spec.ts` for idempotent cancel-to-decline recovery;
- Mithril loading and chain-storage Storybook stories compile successfully; the remaining `yarn storybook --smoke-test` failure is the pre-existing unrelated news utility import issue.
- i18n review confirmed about 62 new Mithril and chain-storage message keys are ready for Japanese translation.
- E2E coverage now includes the Mithril bootstrap feature file and step definitions, and the added test files are lint-clean.

## Long-term E2E insights
- Seeded Mithril E2E helpers should neutralize every async path started by `MithrilBootstrapStore.setup()`, not only `syncStatus()` and `loadSnapshots()`; `loadChainStorageConfig()` was a concrete missed path that could repopulate seeded state.
- Treat IPC listener guarding for the Mithril status broadcast channel as part of the seeded-test contract so asynchronous pushes cannot overwrite seeded renderer state mid-scenario.
- Keep teardown ordering strict: restore polling and original Mithril store methods before waiting for `networkStatus.isSynced`, or cleanup can deadlock with sync still disabled.
- Chain-storage E2E coverage still has a fidelity limit because the dialog stub returns a filesystem path that can behave like a file path rather than a true directory selection.
- CSS module selectors that follow the verified `[name]_[local]` convention remain acceptable durable E2E targets for Mithril views and chain-storage controls when accessibility IDs are not the active selector surface.

## Current gaps
- `BlockDataStorageLocationPicker` is the generic blockchain-data location picker; keep future copy and file references aligned to that name instead of Mithril-specific wording.
- Chain-storage UI state already lives in `MithrilBootstrapStore`; keep it there as the block-data storage picker evolves.
- Disk-space enforcement still depends on a static 4 GB backend threshold (`DISK_SPACE_REQUIRED`) while Mithril snapshots are much larger; align validation with snapshot-aware required space (for example latest `total_db_size_uncompressed` plus margin) so warnings and hard validation match real data size.
- If there is a mismatch between the `cardano-node` version used by Daedalus and the version the Mithril snapshot was created with, the blockchain will need to be verified, which can add significant time to the sync process. Investigate adding a warning UI for this scenario, and explore whether Mithril command-line conversions or snapshot version checks can be used to detect and mitigate this issue.

## QA pointer
- Manual QA for the full Mithril flow lives in `.agent/plans/mithril/bootstrap-cardano-node.md` under Testing Strategy.
