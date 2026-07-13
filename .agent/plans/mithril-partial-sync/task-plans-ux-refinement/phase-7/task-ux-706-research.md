# task-ux-706 — research notes (durable findings)

Appended at build close-out (2026-07-12) from the CAT A–G implementation wave; durable beyond this
wave:

- **`yarn i18n:manage` is a full two-way sync, not report-only.** It auto-adds every inline
  `defineMessages` descriptor it finds in source to `en-US.json`/`ja-JP.json` (with the `!!!`
  placeholder as the EN value) **and auto-deletes obsolete locale keys** whose descriptors are gone
  (CAT-F confirmed empirically when deleting `noCertifiedRange`). Consequence: an un-extracted
  inline descriptor (e.g. `mithrilSyncInterrupt` at `SyncingConnectingStatus.tsx`) re-appears in
  both locale JSONs on *every* run until the owning change lands — CATs A/B/C/D each had to
  re-remove it per seam S3. Regeneration also trues up unrelated pre-existing staleness in
  `defaultMessages.json`/`translations/messages.json` (deterministic tool output, keep it).
- **Sequencing lesson for single-owner i18n seams:** when one CAT owns a key end-to-end but other
  CATs run `i18n:manage` first, either land the owned key early in the wave or put the re-removal
  drill in every other CAT's doc. The S3 drill worked but cost four re-removals.
- **`npx` is broken in this environment** (npm fails with `Invalid property devEngines.node`), and
  worse, `npx prettier` resolves a *newer* prettier than the repo's 2.1.2 and reports false dirt —
  HEAD-drift classification must use the repo-local binary (`node_modules/.bin/prettier`). An early
  npx-based classification pass in CAT-A produced bogus results and had to be redone.
- **`CardanoNode._isDead` (`CardanoNode.ts:935-936`) confirms only IPC-disconnect + process-exit.**
  A re-queried node state can read `STOPPED` while ImmutableDB/ledger file handles are still
  releasing. Any consumer that needs the immutable directory actually free (e.g. spawning the
  `cardano-db download --allow-override` child) must wait on a lock-release signal, not on the
  state machine — this is why CAT-G's live `getNodeState` re-query is a window-narrowing
  placeholder and the tracked follow-up needs a bounded settle on lock release.
- **Chunk counts are never byte-formatted in the progress UI** (C3 verification): the immutable
  chunk figure renders only via `formatFileCount` (`MithrilStepIndicator.tsx:214-219`);
  `InlineProgressBar`'s downloaded/total byte-format fallback (`InlineProgressBar.tsx:25-27`) is
  unreachable for it because the single call site (`MithrilStepIndicator.tsx:682-687`) passes
  `details` only. No backend unit plumbing needed.
- **Per-surface button geometry can override the theme without touching it:** setting the
  `--rp-button-height/-width/-line-height` custom properties on the component's module rules
  deterministically beats the `documentElement` theme values (CAT-E recipe: `flex-wrap` on the
  actions row + `height/width: auto`, `max-width: 100%`, `white-space: normal`). Caveat:
  `width: auto` collapses short labels ("Try again" → ~110px), so pair it with a `min-width` floor
  (180px matches the existing prompt/diagnostics convention).
- **`isAtOrPastSnapshot` is an explicit constant-shape boolean** on the behind-ness probe result
  (true only on `gap <= 0`, explicit false elsewhere including the probe-failed catch). The
  renderer variant ladder selects on the flag only — probe-failed outranks it, and there is no
  epochs-absence inference; keep it that way when extending the ladder.
- **The drift re-resolve invalidates preflight-time snapshot validation:**
  `MithrilPartialSyncService.ts:249` re-assigns `_latestSnapshot` *after*
  `_assertSufficientDiskSpace` passed, so any later consumer of the snapshot size must re-validate
  through the shared `hasKnownSnapshotSize` predicate (`mithrilSnapshotMetadata.ts`) rather than
  trusting the preflight (seam S7; binds the Tier-3 size-plumbing follow-up).
