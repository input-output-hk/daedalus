# task-ux-401 — Honest file-count download progress in both partial sync and bootstrap

- **Phase:** phase-4 (Renderer Progress And Error Overlay Honesty)
- **Implements:** PRD **D4** (download semantic, file-count based). Closes research-19 **gap #4** (overlay mislabels file COUNTS as BYTES) and the **download portion of gap #5** (the non-download "looks stalled" portion of gap #5 is task-ux-402, not this task).
- **Planning status:** `approved`
- **Build status:** `completed`
- **Interaction mode:** `autonomous`
- **Single commit subject:** `feat(mithril): task-ux-401 drive Mithril download progress from honest file counts in both flows`

---

## Why now

phase-4 makes the in-flight Mithril overlays honest. The download readout is the first dishonest surface a user sees during both flows:

- **Bootstrap** renders a *synthetic* byte readout — `bytesDownloaded = (filesDownloaded / filesTotal) * snapshot.size` — labeled "Snapshot files", i.e. interpolated bytes pretending to be files (assumes uniform file size).
- **Partial sync** feeds raw integer file counts into the shared component's **byte-named props** (`bytesDownloaded` / `snapshotSize`), so the counts ride a byte-semantic path.

D4 mandates a single, truthful, file-count semantic shared by both flows, with real `snapshot.size` shown only as **static** "≈ N GB total" context (never a moving bar), and real ancillary bytes kept byte-based. This task lands that. task-ux-402 (depends on this) then adds the active-spinner / ticking-elapsed / reassurance affordances on top of the same shared components.

---

## Interaction mode + justification

**`autonomous`.** D4 specifies the file-count semantic and the exact example readout ("Restoring verified data — 142 / 980 files"); the static "≈ N GB total" context is mechanically derived from real `snapshot.size`. The change reuses the **already-approved** combined bar label ("Snapshot Files and Fast Sync") and the locked "verified … chain data" subtitle wording (#4) verbatim — it only adds a "files" unit to the detail line, swaps synthetic values for true counts, and adds one optional static-size sentence. There is **no benefit-framing / behind-ness copy judgment** here (that was task-ux-304, which was `interactive_decision`). No genuine user-blocking decision exists, so `needsUserDecision = false`. If the implementer believes the new "files" unit or the "≈ N GB total" sentence needs copy sign-off, that is **not** a blocker — both are mechanical per D4 — proceed.

---

## Scope

- Make the download bar **fill** and **running readout** file-count based in **both** flows, driven by `filesDownloaded / filesTotal`.
- Remove the bootstrap synthetic-byte getter; feed the store's already-present raw `filesDownloaded` / `filesTotal` instead.
- Rename the shared component's byte-named download props to file-count names so counts no longer ride byte-named props.
- Show real `snapshot.size` (bootstrap only) as a **static** "≈ N GB total" context segment; partial sync has no real total size on its `transferProgress`, so it shows file counts only.
- Keep real **ancillary** bytes byte-formatted where already shown (the ancillary stream carries TRUE bytes).
- Update the shared specs and re-verify the bootstrap success/progress path.

### Non-goals (do NOT do here)

- **No** active-spinner, ticking-elapsed, indeterminate indicator, reassurance copy, or `stopping-node` frame — that is **task-ux-402** (depends on this task). Do not touch `.activeCircle`, `MithrilStepIndicator.spec.tsx:59-60`'s `.iconSpinner` assertion, `bootstrapStartedAt`, or `elapsedSeconds` wiring.
- **No** error-copy map / cancelled-vs-failed copy — that is **task-ux-403**.
- **No** recovery-action / finalize-channel / hideAction changes — that is **task-ux-404**.
- **No** new IPC, no backend change, **no** synthetic throughput / remaining-time / overall-% (locked invariant #18 = prompt boundary #8).
- **No** snapshot-selection or storage-location picker.
- **No** rename of the combined bar accessible label `progressCombinedLabel` ("Snapshot Files and Fast Sync") — keep it; the existing specs assert on it.

---

## Dependencies

- JSON `dependencies: []` — unblocked. (task-ux-402/403/404 depend on this one; 402 builds directly on the shared components this task touches.)

---

## Research / docs / workflows / skills consulted

- PRD `mithril-partial-sync-ux-refinement-prd.md` **D4** (lines 185-254), the gap-coverage row "#4 file-count semantic, both flows" (line 948), and the components-impact notes (lines 895-908).
- research-19 `19-ux-refinement-state-and-gaps.md` **gap #4** (line 131) and **gap #5** (line 132); §5 locked invariant **#18** (no synthetic throughput/remaining-time/overall-% over IPC, line 223).
- prompt `prompt-ux-refinement.md` — locked boundaries **#8** (no synthetic figures) and **#11** (do not regress empty-chain bootstrap); convergence ("smallest truthful change", reuse seams).
- Format/rigor reference only: `task-plans-ux-refinement/phase-3/task-ux-304.md` (live-seam table, mechanical steps, env workarounds).
- Workflows: `.agent/workflows/frontend.md`, `.agent/workflows/test.md`. Skill: `i18n-messaging` (the `!!!` default convention + `yarn i18n:extract`).

---

## Files expected to change (exact paths)

Production:
1. `source/renderer/app/stores/MithrilBootstrapStore.ts` — remove synthetic `bytesDownloaded` getter.
2. `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` — feed raw counts + real size into the renamed props.
3. `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx` — rename props, thread `snapshotSizeBytes`.
4. `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` — rename props, thread `snapshotSizeBytes`.
5. `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` — rename props, file-count detail + percent, static size context.
6. `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — map `transferProgress` file counts to the renamed props.
7. `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` — update `progressCombinedDetail`; add `progressSnapshotSizeContext`.

i18n (regenerated/edited):
8. `source/renderer/app/i18n/locales/en-US.json`
9. `source/renderer/app/i18n/locales/ja-JP.json`
10. `source/renderer/app/i18n/locales/defaultMessages.json` (via `yarn i18n:extract`; include only if it changes)
11. `translations/messages.json` (via `yarn i18n:extract`)

Tests:
12. `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx` — rename props; update the detail-text assertion; add a file-count + static-size test.
13. `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx` — rename the `bytesDownloaded`/`snapshotSize` test props to the new names.

(No change expected to `MithrilPartialSyncOverlay.spec.tsx` or `MithrilBootstrap.spec.tsx` — re-run them as regression; see Verification.)

---

## Live-seam verification table (verified against the working tree 2026-06-26)

| Anchor (JSON / PRD) | What it should be | Live finding | Status |
|---|---|---|---|
| `MithrilPartialSyncOverlay.tsx:87-88` | counts routed into byte props | L87 `bytesDownloaded={transferProgress?.filesDownloaded}`, L88 `snapshotSize={transferProgress?.filesTotal}` | CONFIRMED |
| `MithrilBootstrapStore.ts:79-97` | synthetic byte getter | `@computed get bytesDownloaded()` spans **L78-99**; the `(filesDownloaded/filesTotal)*snapshot.size` math is **L96-98**. Raw `@observable filesDownloaded`/`filesTotal` already exist (L49-51). | CONFIRMED (getter is L78-99, not 79-97 — minor anchor drift) |
| `MithrilStepIndicator.tsx:196-198` | combined download line render | `formatCombinedProgressDetails` returns `intl.formatMessage(messages.progressCombinedDetail, { snapshotDownloaded: formatSnapshotCount(bytesDownloaded), snapshotTotal: formatSnapshotCount(snapshotSize), fastSync… })` at **L196-202** | CONFIRMED |
| `formatSnapshotCount` / "byte formatter" seam | the byte formatter that renders counts | **L193-194**: `formatSnapshotCount = (v) => … formattedNumber(v)`. It is `formattedNumber` (thousands-separated **plain number**), NOT a byte/GB formatter. | DRIFT (documented): the JSON/gap-#4 phrase "byte formatter" is imprecise; PRD's 2026-06-19 factual correction (lines 199-203) already records that these render as plain numbers via `formattedNumber`. The real defect is **synthetic values + byte-named props**, not a GB format. |
| (consumer chain, not in JSON) | where bootstrap's real data enters | `MithrilBootstrapPage.tsx:144-145,150` → `MithrilBootstrap.tsx:23-24,109-110,193-194` → `MithrilProgressView.tsx:21-22,67-68,133-134` → `MithrilStepIndicator.tsx:29-30,131-132,182-183,436-437` | CONFIRMED — all carry `bytesDownloaded`/`snapshotSize` to rename |
| ancillary bytes (keep byte-based) | `formatTransferSize` (GB) | `MithrilStepIndicator.tsx:199-201` uses `formatTransferSize(ancillaryBytes…)`; helper in `snapshotFormatting.ts:38-44` | CONFIRMED — leave unchanged |
| static "≈ N GB total" source | real `snapshot.size` | bootstrap store `snapshot.size`; helper `formatSnapshotSize` (`snapshotFormatting.ts:32-36`, returns e.g. "9.7 GB"). Partial sync `MithrilPartialSyncTransferProgress` (`mithril-partial-sync.types.ts:39-44`) has **no** size field. | CONFIRMED — bootstrap shows it, partial sync omits it |
| combined detail catalogs | EN/JA keys | `en-US.json:314`, `ja-JP.json:314` (`…progress.combinedDetail`); label at `:315` | CONFIRMED |

**Net design consequence of the drift:** renaming `bytesDownloaded`→`filesDownloaded` and `snapshotSize`→`filesTotal` and computing the snapshot percent directly from files is **mathematically identical** to today's bootstrap percent, because today `bytesDownloaded = (filesDownloaded/filesTotal)*snapshot.size` and `snapshotSize = snapshot.size`, so `bytesDownloaded/snapshotSize === filesDownloaded/filesTotal`. The combined-bar weighting keeps using **real bytes** via the new `snapshotSizeBytes` prop. Therefore bootstrap's bar percentages do not regress (proven against the existing spec expectations below).

---

## Locked invariants this change MUST NOT break (inline)

- **#8 / #18 — no synthetic figures.** Do NOT introduce throughput, remaining-time, or overall-% over IPC. The whole point of this task is to DELETE the synthetic `bytesDownloaded` byte readout. The download bar fill stays a fraction of real file counts (and, for the combined bar, the pre-existing real-byte weighting); never a fabricated byte total. Never route raw mithril-client JSON into UI copy.
- **#11 — do not regress the empty-chain bootstrap flow.** This edits the SHARED `MithrilBootstrapStore` / `MithrilBootstrap` / `MithrilProgressView` / `MithrilStepIndicator`. Re-verify the bootstrap progress AND success path: `MithrilBootstrap.spec.tsx`, `MithrilStepIndicator.spec.tsx`, `MithrilProgressView.spec.tsx` must pass; the bootstrap combined-bar percentages (54% / 100% test cases) must be unchanged.
- **#4 — preserve the locked "verified … chain data" wording.** Do NOT alter `partialSyncProgressSubtitle` ("Daedalus is restoring verified Mithril chain data …"). The download label keeps the approved combined label.
- **Real ancillary bytes stay byte-based.** Only the synthetic main-snapshot byte readout becomes file counts; `formatTransferSize(ancillary…)` is untouched.
- **Renderer computes no threshold** and reads only backend-provided counts. No new IPC channel/field.

---

## Implementation approach — ordered, mechanical steps

### Step 1 — Remove the synthetic byte getter (`MithrilBootstrapStore.ts`)

Delete the entire `@computed get bytesDownloaded(): number | undefined { … }` block (lines **78-99**, from the `@computed` decorator on L78 through the closing `}` on L99). Leave `@observable filesDownloaded` / `@observable filesTotal` (L49-51) and the `ancillaryProgress` getter (L101-111) intact. Nothing else references `this.bytesDownloaded` after Step 2.

### Step 2 — Feed raw counts + real size into the renamed props (`MithrilBootstrapPage.tsx`)

In the `progressProps` object (lines **143-151**), replace:
```ts
bytesDownloaded: mithrilBootstrap.bytesDownloaded,
snapshotSize: mithrilBootstrap.snapshot?.size,
```
with:
```ts
filesDownloaded: mithrilBootstrap.filesDownloaded,
filesTotal: mithrilBootstrap.filesTotal,
snapshotSizeBytes: mithrilBootstrap.snapshot?.size,
```
(Leave the `ancillary*`, `progressItems`, `bootstrapStartedAt` keys unchanged.)

### Step 3 — Rename props through the bootstrap wrapper (`MithrilBootstrap.tsx`)

- In `interface Props` (lines 22-24 region): replace `bytesDownloaded?: number;` / `snapshotSize?: number;` with `filesDownloaded?: number;` / `filesTotal?: number;` and add `snapshotSizeBytes?: number;`.
- In the destructure (lines 109-110): replace `bytesDownloaded,` / `snapshotSize,` with `filesDownloaded,` / `filesTotal,` / `snapshotSizeBytes,`.
- In the `<MithrilProgressView …>` element (lines 193-194): replace `bytesDownloaded={bytesDownloaded}` / `snapshotSize={snapshotSize}` with `filesDownloaded={filesDownloaded}` / `filesTotal={filesTotal}` and add `snapshotSizeBytes={snapshotSizeBytes}`.

### Step 4 — Rename props through the progress view (`MithrilProgressView.tsx`)

- `interface Props` (lines 21-22): `bytesDownloaded?: number;` / `snapshotSize?: number;` → `filesDownloaded?: number;` / `filesTotal?: number;`; add `snapshotSizeBytes?: number;`.
- Destructure (lines 67-68): rename accordingly; add `snapshotSizeBytes,`.
- `<MithrilStepIndicator …>` element (lines 133-134): `bytesDownloaded={bytesDownloaded}` / `snapshotSize={snapshotSize}` → `filesDownloaded={filesDownloaded}` / `filesTotal={filesTotal}`; add `snapshotSizeBytes={snapshotSizeBytes}`.
- Do NOT touch the elapsed-timer block (lines 84-127) — that is task-ux-402's territory.

### Step 5 — File-count semantics in `MithrilStepIndicator.tsx`

5a. **Props type** (lines 29-30) + **destructure** (lines 436-437): rename `bytesDownloaded?: number;`→`filesDownloaded?: number;`, `snapshotSize?: number;`→`filesTotal?: number;`; add `snapshotSizeBytes?: number;` to both. In the destructure default block (lines 433-442) add `snapshotSizeBytes,`.

5b. **Snapshot percent** (lines 450-453): rename to read from files:
```ts
const snapshotPercent =
  typeof filesTotal === 'number' && filesTotal > 0
    ? ((filesDownloaded ?? 0) / filesTotal) * 100
    : 0;
```

5c. **`deriveCombinedDownloadPercent`** (lines 127-178): rename the two params `bytesDownloaded`→`filesDownloaded`, `snapshotSize`→`filesTotal`, and ADD a `snapshotSizeBytes?: number` param. Inside:
- Completion check (line 149): `isTransferComplete(filesDownloaded, filesTotal)` (file completion — unchanged logic, renamed args).
- Byte-weighting branch (lines 158-171): drive the weights from **`snapshotSizeBytes`** (real bytes), NOT `filesTotal`. Replace the three `snapshotSize` references with `snapshotSizeBytes`:
  ```ts
  if (
    typeof snapshotSizeBytes === 'number' &&
    snapshotSizeBytes > 0 &&
    typeof ancillaryBytesTotal === 'number' &&
    ancillaryBytesTotal > 0
  ) {
    const totalBytes = snapshotSizeBytes + ancillaryBytesTotal;
    const snapshotWeight = (snapshotSizeBytes / totalBytes) * 100;
    const ancillaryWeight = (ancillaryBytesTotal / totalBytes) * 100;
    return (normalizedSnapshotPercent / 100) * snapshotWeight
         + (normalizedAncillaryPercent / 100) * ancillaryWeight;
  }
  ```
  When `snapshotSizeBytes` is absent (partial sync) it falls through to the existing `FALLBACK_SNAPSHOT_WEIGHT`/`FALLBACK_FAST_SYNC_WEIGHT` branch — which is more correct than today (partial sync previously mis-fed file counts as bytes here).
- Update the call site (lines 456-464): pass `filesDownloaded`, `filesTotal`, `snapshotSizeBytes`, and keep ancillary args.

5d. **`formatCombinedProgressDetails`** (lines 180-203): rename params `bytesDownloaded`→`filesDownloaded`, `snapshotSize`→`filesTotal`; add `intl` already present, add `snapshotSizeBytes?: number`. Rename the inner helper `formatSnapshotCount`→`formatFileCount` (keep its `formattedNumber`-based body verbatim). Build the base string, then append the static size context when a real total exists:
```ts
const base = intl.formatMessage(messages.progressCombinedDetail, {
  snapshotDownloaded: formatFileCount(filesDownloaded),
  snapshotTotal: formatFileCount(filesTotal),
  fastSyncDownloaded: formatTransferSize(ancillaryBytesDownloaded) ?? '—',
  fastSyncTotal: formatTransferSize(ancillaryBytesTotal) ?? '—',
});
const totalSize = formatSnapshotSize(snapshotSizeBytes); // null if not a real positive size
return totalSize
  ? `${base} · ${intl.formatMessage(messages.progressSnapshotSizeContext, { totalSize })}`
  : base;
```
Add `formatSnapshotSize` to the existing import from `./snapshotFormatting` (line 17 currently imports `formatTransferSize`). Update the call site (lines 465-471) to pass `snapshotSizeBytes`.

5e. **`showBars` gate** (lines 585-594): the byte-presence guard at lines 591-594 currently checks `typeof bytesDownloaded === 'number' || typeof snapshotSize === 'number' || …`. Rename those two to `typeof filesDownloaded === 'number' || typeof filesTotal === 'number' ||` (keep the ancillary checks). No behavioral change.

### Step 6 — Map partial-sync file counts to the renamed props (`MithrilPartialSyncOverlay.tsx`)

In the `<MithrilProgressView …>` element (lines 87-88) replace:
```tsx
bytesDownloaded={transferProgress?.filesDownloaded}
snapshotSize={transferProgress?.filesTotal}
```
with:
```tsx
filesDownloaded={transferProgress?.filesDownloaded}
filesTotal={transferProgress?.filesTotal}
```
Do NOT add `snapshotSizeBytes` here (partial sync has no real total size — file counts only, per D4 open dependent). Leave the ancillary props (lines 89-92), `elapsedSeconds`, `showDownloadProgressBar`, etc. unchanged (those belong to task-ux-402).

### Step 7 — Messages (`MithrilBootstrap.messages.ts`)

7a. Update `progressCombinedDetail` (lines 234-240) — add the "files" unit after the counts (keep the same interpolation key names to minimize extraction churn):
```ts
defaultMessage:
  '!!!Snapshot files: {snapshotDownloaded} / {snapshotTotal} files | Fast sync: {fastSyncDownloaded} / {fastSyncTotal}',
```

7b. Add a NEW message immediately after it:
```ts
progressSnapshotSizeContext: {
  id: 'loading.mithrilBootstrap.progress.snapshotSizeContext',
  defaultMessage: '!!!≈ {totalSize} total',
  description:
    'Static, real-size context appended to the Mithril download readout (derived from snapshot.size); not a moving bar.',
},
```

### Step 8 — i18n catalogs

8a. Run `yarn i18n:extract` to regenerate `translations/messages.json` (and `defaultMessages.json` if it changes) with the changed `combinedDetail` and the new `snapshotSizeContext` id.

8b. Edit `en-US.json` line 314 to the first-class EN (drop `!!!`):
```
"loading.mithrilBootstrap.progress.combinedDetail": "Snapshot files: {snapshotDownloaded} / {snapshotTotal} files | Fast sync: {fastSyncDownloaded} / {fastSyncTotal}",
```
Add the EN key for the new message:
```
"loading.mithrilBootstrap.progress.snapshotSizeContext": "≈ {totalSize} total",
```

8c. Edit `ja-JP.json` line 314 to first-class JA (add the file counter 個 after the count):
```
"loading.mithrilBootstrap.progress.combinedDetail": "スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 | 高速同期: {fastSyncDownloaded} / {fastSyncTotal}",
```
Add the JA key for the new message (約 = "approximately"):
```
"loading.mithrilBootstrap.progress.snapshotSizeContext": "約 {totalSize}（合計）",
```
Provide first-class JA (no `!!!`); the holistic JA review is task-ux-601. Use the `i18n-messaging` skill conventions.

### Step 9 — Specs

9a. `MithrilStepIndicator.spec.tsx`:
- In `TestProps` (lines 12-19) rename `bytesDownloaded` → `filesDownloaded`, `snapshotSize` → `filesTotal`; add `snapshotSizeBytes?: number;`.
- In every `renderComponent('downloading', { … bytesDownloaded: X, snapshotSize: Y … })` call, rename the keys to `filesDownloaded`/`filesTotal` and **also pass `snapshotSizeBytes: Y`** (the old `snapshotSize` value) wherever ancillary bytes are present so the combined-percent weighting is preserved. Specifically the percent-bearing tests at lines 124-146 (expects 54%), 169-191 (100%), 193-228 (100%) must keep `snapshotSizeBytes` equal to the prior `snapshotSize` to keep the asserted percentages. The completion test 169-191 sets ancillary 100/100 → still 100%.
- Update the detail-text assertion at lines 162-166 from
  `/snapshot files: 901 \/ 25,400 \| fast sync: 24\.0 kb \/ 195\.0 kb/i`
  to
  `/snapshot files: 901 \/ 25,400 files \| fast sync: 24\.0 kb \/ 195\.0 kb/i`
  (this test has no `snapshotSizeBytes`, so no static-size suffix is appended — assertion stays a substring match).
- ADD one test: render `downloading` with `filesDownloaded: 142, filesTotal: 980, snapshotSizeBytes: 9_700_000_000` and an active `step-3` → assert the detail shows `142 / 980 files` AND contains the static `≈ 9.7 GB total` (testCases: file counts not bytes; static total only when real size present). (Use `formattedBytesToSize(9.7e9)`'s actual output — assert with a regex like `/≈ .* total/i` plus `/142 \/ 980 files/i` to avoid coupling to the exact GB string.)

9b. `MithrilProgressView.spec.tsx`: in the `renderComponent` prop type + the `<MithrilProgressView>` element (lines 11-34), rename the `bytesDownloaded`/`snapshotSize` test props to `filesDownloaded`/`filesTotal` (these tests assert the header/timer, not the detail text, so no assertion text changes).

9c. No edits expected to `MithrilPartialSyncOverlay.spec.tsx` — its assertion `/snapshot files: .*fast sync:/i` (line ~105) still matches the new "… files |" detail. Re-run it as regression.

---

## Acceptance criteria (VERBATIM from the tasks JSON)

- Download bar/readout is file-count based and truthful in both flows.
- Bootstrap synthetic-byte getter is removed; partial sync no longer routes counts through a byte formatter.
- Any total-size figure is static context derived from real snapshot.size only.
- No regression to the bootstrap success path.

### testCases (VERBATIM from the tasks JSON)

- Partial sync shows file counts, not counts-through-a-byte-formatter
- Bootstrap shows file counts, not synthetic bytes
- Static total-size context renders only when a real size is available
- No synthetic throughput/remaining-time/overall-% is introduced
- Bootstrap success path unchanged

---

## Verification plan (exact commands, from repo root `/workspaces/mithril-partial-sync-ux`)

```bash
cd /workspaces/mithril-partial-sync-ux
yarn i18n:extract            # regenerate translations/messages.json (+ defaultMessages.json if changed)
grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json
                            # → no NEW !!! for combinedDetail / snapshotSizeContext
yarn compile                # tsc --noEmit must pass (allow up to 600s)
yarn lint                   # ESLint clean on touched files
yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx
yarn prettier:check         # (or prettier --check on the touched files)
grep -rn "bytesDownloaded" source/renderer/app/stores/MithrilBootstrapStore.ts
                            # → no match (synthetic getter gone)
```

> **KNOWN ENV WORKAROUND (recorded by task-ux-301/303/304):** `yarn compile`'s `typedef:sass` precompile hook and jest's css-modules transform crash under **Node v24 dart-sass** ("Invalid or unexpected token"). If hit:
> - Run the authoritative TS gate directly: `node_modules/.bin/tsc --noEmit -p .` (allow up to 600s).
> - Run jest with `.scss` mapped to `identity-obj-proxy` via a **CLI-only** override, leaving committed config unchanged, e.g.:
>   `node_modules/.bin/jest --config '{"moduleNameMapper":{"\\.(scss|sass|css)$":"identity-obj-proxy"},"preset":"...existing..."}' <specs>` — or the equivalent `--moduleNameMapper` sidecar the prior tasks used (a gitignored sidecar config; do NOT stage it).
> This is a pre-existing, environment-wide defect, NOT a regression from this task. No new stories are added (task-ux-502 owns Storybook), so storybook is not run here.

### Tests to add/update, mapped to JSON testCases

- **Bootstrap shows file counts / success unchanged** → `MithrilStepIndicator.spec.tsx` (renamed props + preserved 54%/100% percent tests + updated detail-text assertion) and `MithrilBootstrap.spec.tsx` (re-run unchanged → bootstrap-regression coverage for #11).
- **Static total-size only when real size present** → new `MithrilStepIndicator.spec.tsx` test (with `snapshotSizeBytes` → `≈ … total`; without → no suffix, proven by the existing 901/25,400 test).
- **Partial sync shows file counts** → `MithrilPartialSyncOverlay.spec.tsx` (re-run; `/snapshot files: .*fast sync:/i` still green) — no `snapshotSizeBytes`, so no static suffix.
- **No synthetic figures** → the `grep` for `bytesDownloaded` in the store returns nothing; reviewer confirms no throughput/ETA/overall-% added.
- **No bootstrap regression** → `MithrilProgressView.spec.tsx` header/timer tests pass.

---

## Risks / open questions

- **Combined-bar percentage regression (bootstrap).** Mitigated by feeding the **real-byte** `snapshotSizeBytes` into the weighting and computing the snapshot percent from files (algebraically identical to today). The existing 54%/100% spec expectations are the guard — keep them green by passing `snapshotSizeBytes` equal to the prior `snapshotSize` value in those tests.
- **Partial sync has no real total size.** Confirmed: `MithrilPartialSyncTransferProgress` carries no size field. Per D4's open dependent, partial sync shows **file counts only** (no `snapshotSizeBytes`). This is intentional, not a gap.
- **JA quality.** First-class JA is provided; the holistic JA pass is task-ux-601. The 個 counter / 約…（合計） phrasing is a reasonable first-class rendering, not a placeholder.
- **`formatSnapshotSize` thresholds.** `formatSnapshotSize` returns `null` for non-positive sizes, so the static suffix only appears for a real positive `snapshot.size` — satisfies "static context derived from real snapshot.size only".

---

## Required doc / research updates

- No `.agent/system/api-endpoints.md` change (no IPC/contract change).
- At completion: fill the "Final outcome" below, set the JSON task `status: completed` (+ `completedAt`), and record durable findings in `task-ux-401-research.md` (esp. the algebraic-equivalence argument and the `formatSnapshotCount` "byte formatter" misnomer correction).

## Review-log paths

- Planning review: `task-plans-ux-refinement/phase-4/task-ux-401-plan-review.md`
- Implementation review: `task-plans-ux-refinement/phase-4/task-ux-401-impl-review.md`
- Research note: `task-plans-ux-refinement/phase-4/task-ux-401-research.md`

## Final outcome

**Status:** shipped. Code review **approved** at **2026-06-26T10:17:56Z** (impl committed 2026-06-26T10:12:53Z). Single renderer-only commit; no backend/IPC change.

### What shipped

D4 download semantic is now truthful in BOTH flows. The download bar fill and running readout are file-count based (`filesDownloaded / filesTotal`); the synthetic bootstrap byte readout is deleted; partial sync no longer routes raw file counts through byte-named props. Real `snapshot.size` appears only as a **static** "≈ N GB total" context segment (bootstrap only — partial sync has no real total size and shows file counts only). Real **ancillary** bytes stay byte-formatted (`formatTransferSize`). The combined-bar weighting now uses the REAL `snapshot.size` via a new `snapshotSizeBytes` prop, so bootstrap bar percentages are algebraically unchanged.

### Files changed (16)

Production:
- `source/renderer/app/stores/MithrilBootstrapStore.ts` — deleted the synthetic `@computed get bytesDownloaded()` getter (the `(filesDownloaded/filesTotal)*snapshot.size` interpolation); raw observables + `ancillaryProgress` left intact.
- `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` — `progressProps` now feeds `filesDownloaded` / `filesTotal` / `snapshotSizeBytes: mithrilBootstrap.snapshot?.size`.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx` — props renamed + `snapshotSizeBytes` threaded.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` — props renamed + `snapshotSizeBytes` threaded; elapsed-timer block untouched (task-ux-402).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` — props renamed; `snapshotPercent` now `filesDownloaded/filesTotal`; `deriveCombinedDownloadPercent` byte-weighting driven by `snapshotSizeBytes` (falls through to FALLBACK weights when absent, e.g. partial sync); `formatSnapshotCount`→`formatFileCount`; appends static "≈ N GB total" only when `formatSnapshotSize(snapshotSizeBytes)` returns a real positive size; `showBars` guard renamed; ancillary `formatTransferSize(...)` left byte-based.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — `transferProgress` file counts mapped to `filesDownloaded` / `filesTotal` (no `snapshotSizeBytes`).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` — `progressCombinedDetail` gains the "files" unit; new `progressSnapshotSizeContext` (`'!!!≈ {totalSize} total'`).

i18n:
- `source/renderer/app/i18n/locales/en-US.json` — `combinedDetail` ("… files") + new `snapshotSizeContext` ("≈ {totalSize} total"); no `!!!`.
- `source/renderer/app/i18n/locales/ja-JP.json` — `combinedDetail` ("… 個") + new `snapshotSizeContext` ("約 {totalSize}（合計）"); no `!!!`.
- `translations/messages.json` — regenerated via `yarn i18n:extract`. (`defaultMessages.json` is NOT the extract target and is unchanged — stale but harmless; see notes.)

Tests + storybook ripple:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx` — props renamed; `snapshotSizeBytes` passed equal to prior `snapshotSize` in percent-bearing tests; detail assertion updated to `/snapshot files: 901 \/ 25,400 files \| fast sync: …/i`; new test (`filesDownloaded:142, filesTotal:980, snapshotSizeBytes:9_700_000_000`) asserts `142 / 980 files` AND `≈ … total`.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx` — test props renamed.
- `source/renderer/app/stores/MithrilBootstrapStore.spec.ts` — removed two getter-only tests + one stale `bytesDownloaded` assertion (ripple beyond the plan's file list).
- `storybook/stories/loading/_support/mithrilHarness.tsx`, `storybook/stories/loading/mithril/MithrilProgressView.stories.tsx`, `storybook/stories/loading/mithril/MithrilBootstrap.stories.tsx` — mechanical prop renames + local `SNAPSHOT_FILES_TOTAL` constant (required for the TS gate to compile; storybook is otherwise task-ux-502).

### REAL verification results (reproduced by code review, not trusted from implementer)

- `node_modules/.bin/tsc --noEmit -p .` → **EXIT 0** (authoritative TS gate, used directly per the documented Node v24 dart-sass defect on `yarn compile`'s `typedef:sass` hook).
- `node_modules/.bin/jest … MithrilStepIndicator/MithrilProgressView/MithrilBootstrap/MithrilPartialSyncOverlay/MithrilBootstrapStore specs` → **5 suites / 55 tests PASS**, incl. the bootstrap success + 54%/100% combined-bar regression tests and the new file-count + static-size test. (Code review used the `identity-obj-proxy` CLI override; implementer's run did not need it — committed config left unchanged either way.)
- `node_modules/.bin/eslint` on the 13 touched src/spec/storybook files → **EXIT 0** (only pre-existing mobx-decorator warnings, confirmed present on HEAD via `git stash`).
- `yarn i18n:extract` → Done; `translations/messages.json` updated; `defaultMessages.json` unchanged.
- `grep '!!!'` on runtime en-US/ja-JP for combinedDetail/snapshotSizeContext → **no matches** (runtime catalogs clean).
- `node_modules/.bin/prettier --check` on touched files → **EXIT 0** ("All matched files use Prettier code style!", after `--write`; prettier 2.1.2 non-idempotency noted below).
- grep proofs: store has no `bytesDownloaded`; shared components have no `snapshotSize`/`bytesDownloaded` props; `filesDownloaded`/`filesTotal` threaded through; ancillary still `formatTransferSize(ancillaryBytes…)`; locked subtitle "restoring verified Mithril chain data" intact.

### Locked invariants upheld

- **#8 / #18 (no synthetic figures):** synthetic byte getter DELETED; snapshot bar fill is a true file-count fraction; combined-bar weighting uses REAL `snapshot.size` bytes (never a fabricated total). No throughput / remaining-time / overall-% added; no raw mithril-client JSON in copy. Verified by store grep + reading the bar math.
- **#11 (no empty-chain bootstrap regression):** SHARED components' bootstrap progress AND success paths re-run (not assumed) — `MithrilBootstrap.spec.tsx`, `MithrilStepIndicator.spec.tsx`, `MithrilProgressView.spec.tsx`, `MithrilBootstrapStore.spec.ts` all PASS (55 tests). The 54%/100% combined-bar percentages are unchanged: bootstrap percent is algebraically identical (today `bytesDownloaded/snapshotSize === filesDownloaded/filesTotal`) and byte weighting is preserved by passing `snapshotSizeBytes` equal to the prior `snapshotSize` in percent-bearing tests. Starting-node handoff + completed-state tests pass (success path intact).
- **#4 (locked wording):** `partialSyncProgressSubtitle` ("Daedalus is restoring verified Mithril chain data…") untouched; combined-bar accessible label `progressCombinedLabel` ("Snapshot Files and Fast Sync") unchanged.
- **Real ancillary bytes stay byte-based**; renderer computes no threshold; no new IPC. The `source/main/mithril/*.ts` `bytesDownloaded` references are pre-existing backend (real ancillary byte stream), untouched by this renderer-only diff.

### Code-review decision

**approved** — 2026-06-26T10:17:56Z. No correctness, invariant, coverage, or contract-drift problems found. Non-blocking nits only (see research note): stale `defaultMessages.json`, an unrelated unstaged `.gitignore` line, and the accepted storybook/store-spec ripple.
