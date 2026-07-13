# task-ux-706 · CAT-C — Progress metrics: Ledger state relabel + unit sanity

Implements **DD-706-2** (label). Resolves JA review #6 and resolution #6. Copy/i18n only.
**DD-706-4's size plumbing (Extra #6 + the ancillary-only jitter) was downgraded 2026-07-10 to the
Tier-3 post-merge follow-up** (plan-review R-1, decision DD-706-9 option a) — see Step C2 for the
corrected root cause and the preserved recon.

## Step C1 — "Fast sync:" → "Ledger state:" / 「台帳状態:」 (label only)

The "Fast sync" figure is the Mithril **ancillary-files** byte size that unpacks into the node's ledger
state. It is a **byte size and stays one** — only the label text changes. The label is purely i18n-driven
(no hardcoded string in components — confirmed: no "Ledger state" or "Fast sync" literal in renderer code;
`InlineProgressBar` just prints the `details`/`label` props).

| Key | line | EN now → EN new | JA now → JA new |
| --- | --- | --- | --- |
| `loading.mithrilBootstrap.progress.combinedDetail` | 312 | `Snapshot files: {snapshotDownloaded} / {snapshotTotal} files \| Fast sync: {fastSyncDownloaded} / {fastSyncTotal}` → `Snapshot files: {snapshotDownloaded} / {snapshotTotal} files \| Ledger state: {fastSyncDownloaded} / {fastSyncTotal}` | `スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 \| 高速同期: {fastSyncDownloaded} / {fastSyncTotal}` → `スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 \| 台帳状態: {fastSyncDownloaded} / {fastSyncTotal}` |
| `loading.mithrilBootstrap.progress.combinedLabel` (consistency companion) | 313 | `Snapshot Files and Fast Sync` → `Snapshot Files and Ledger State` | `スナップショットファイルと高速同期` → `スナップショットファイルと台帳状態` |

- All `{placeholder}` tokens stay byte-identical — only the surrounding label words change.
- Source defaults: `MithrilBootstrap.messages.ts:243` (`progressCombinedDetail`) and `:236`
  (`progressCombinedLabel`); re-sync `defaultMessages.json` (combinedDetail ~2771, combinedLabel ~2766).
- The `{fastSyncTotal}` value is `formatTransferSize(ancillaryBytesTotal)` (MithrilStepIndicator.tsx:222)
  — a human byte size; do not touch that formatting.
- Spec updates (plan-review): `MithrilStepIndicator.spec.tsx:125,163,184,227,255,293,318` and
  `MithrilPartialSyncOverlay.spec.tsx:271` assert the old "fast sync" label strings and will fail after
  the relabel — update them. Do **not** touch `/fast sync with mithril/i` assertions
  (`MithrilProgressView.spec.tsx:116,170`, `MithrilBootstrap.spec.tsx`) — that's the feature *title*,
  which is CAT-A-out-of-scope and keeps its name.
- *Pending JA translator round-2 sign-off (❓#6) — implement, but flag the string for confirmation.*

## Step C2 — ~~Plumb the partial-sync snapshot size~~ DOWNGRADED to the Tier-3 follow-up (2026-07-10)

**❌ Re-planned per plan-review R-1 (decision DD-706-9, option a).** The planned fix — thread
`_latestSnapshot.size` into the partial-sync status so the byte-weighted branch activates — is unsound.
Three compounding problems, confirmed by both CAT-C validators against
`MithrilStepIndicator.tsx:143-197`:

1. **Wrong byte figure.** `_latestSnapshot.size` is the **whole-DB** snapshot size, not the
   partial-range delta (the service treats it that way: disk calc subtracts what's on disk,
   `MithrilPartialSyncService.ts:1066-1074`; partial sync downloads only `--start/--end` chunks,
   `:569-572`). In the weighted branch (`MithrilStepIndicator.tsx:183-188`),
   `snapshotWeight = size / (size + ancillaryBytesTotal) × 100` with a whole-snapshot numerator gives
   ≈97-99% on mainnet — at or above the 95% fallback it replaces. With `filesTotal = 2` the combined
   bar still swings ~47 pts per file (`snapshotPercent` itself swings 50). Jitter unchanged.
2. **Ancillary term pinned at 0.** The weighted branch consumes `ancillaryPercent`
   (`MithrilStepIndicator.tsx:146,163`), sourced from the `ancillaryProgress` prop (`:483` defaults it
   to 0). The bootstrap path computes and passes it (`MithrilBootstrapStore.ts:78-88` →
   `MithrilBootstrapPage.tsx:149`); the partial-sync path has no such field anywhere in the chain
   (store → `App.tsx:100-111` → `MithrilPartialSyncOverlay.tsx:212`).
3. **Misleading size line.** The "≈ {totalSize} total" context (`MithrilStepIndicator.tsx:225-233`)
   would render the **full** snapshot size (e.g. "≈ 150 GB total") on a run fetching a 2-chunk delta —
   the opposite of Extra #6's "catch-up size" intent.

**What the follow-up must do instead** (corrected root cause — the fallback weights are *not* the
problem): derive a **ranged-delta** byte size (no such field exists today — `MithrilSnapshotItem.size`
is the only size field, `mithril-bootstrap.types.ts:49-55`; e.g. derive from the download child's
reported totals), thread an `ancillaryProgress` percent for the partial-sync path mirroring the
bootstrap store's computation, and give the "≈ total" line a partial-sync-specific message or value.
It must import `hasKnownSnapshotSize` from `mithrilSnapshotMetadata.ts` (seam S7 — the drift re-resolve
at `MithrilPartialSyncService.ts:249` re-assigns `_latestSnapshot` after F4's preflight check passed,
so the plumbing cannot rely on that check alone).

**Preserved recon (mechanically verified, valid for the follow-up):** the type seam is
`MithrilPartialSyncTransferProgress` (`mithril-partial-sync.types.ts:61-67`); store sites
`MithrilPartialSyncStore.ts:44-48, 180-184`; the emit path is unfiltered (`{...this._status}` at
`MithrilPartialSyncService.ts:1167` → IPC → store `_updateStatus`, `MithrilPartialSyncStore.ts:149`,
no field whitelisting); `MithrilProgressView` already declares/forwards
`snapshotSizeBytes` (`:25`, `:200`), so only App.tsx + overlay + store + type need edits. The transfer
payload is reset to `{}` at `MithrilPartialSyncService.ts:187, 345, 401, 457, 798` — derive any size
field inside `_updateStatus` from `this._latestSnapshot?.size` (survives resets); a set-once field does
not. Runtime unknown: whether `ancillaryBytesTotal` (`:611-613`) arrives concurrently with the chunk
counts.

## Step C3 — Immutable-chunk unit sanity (no new pipeline — DD-706-5)

Resolution #6 wants immutable chunks shown as **files**, not a converted MB/GB. The `combinedDetail` snapshot
side already shows a file count (`formatFileCount(filesTotal)`, MithrilStepIndicator.tsx:214-219). Confirm
no other user-facing surface presents the immutable chunk count as a byte size; the only byte figures are
the ancillary "Ledger state" pair and the whole-snapshot "≈ {totalSize} total" context, both legitimate.
The conversion-factor→file-count backend transition noted in the resolutions is **out of scope** here (no new
metric pipeline) — flag it as a separate backend follow-up if any converted MB/GB immutable figure is
found.

## Acceptance

- Progress detail reads "Ledger state:" / 「台帳状態:」; bar title reads "Snapshot Files and Ledger State"
  / 「スナップショットファイルと台帳状態」; placeholders intact.
- The old-label assertions in `MithrilStepIndicator.spec.tsx` (`:125,163,184,227,255,293,318`) and
  `MithrilPartialSyncOverlay.spec.tsx:271` are updated and green; the `/fast sync with mithril/i`
  feature-title assertions are untouched.
- The Tier-3 follow-up (size line + jitter, DD-706-4 downgrade) is logged in the master doc's tier
  list with the corrected root cause — no code change for C2 lands in this wave.
- i18n-messaging validation clean; `yarn compile` + lint clean.
