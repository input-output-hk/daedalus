# task-ux-401 — Planning review log (append-only)

Append-only chronological transcript. Each entry: speaker label, ISO-8601 UTC `Timestamp:`, outcome.
Critiquer/Code-Review entries end with `Decision: approved` or `Decision: requires_changes`.

---

## Planner:
Timestamp: 2026-06-26T09:47:21Z

**What I planned.** Authored the canonical `task-ux-401.md` for PRD D4's download-honesty semantic (gap #4 + the download portion of gap #5). The smallest-truthful-change design:
1. Delete the bootstrap synthetic byte getter `bytesDownloaded` (`MithrilBootstrapStore.ts:78-99`); feed the already-present raw `@observable filesDownloaded`/`filesTotal`.
2. Rename the shared download props `bytesDownloaded`→`filesDownloaded`, `snapshotSize`→`filesTotal` across the consumer chain (`MithrilBootstrapPage` → `MithrilBootstrap` → `MithrilProgressView` → `MithrilStepIndicator`) so counts no longer ride byte-named props; add a NEW `snapshotSizeBytes` prop carrying real `snapshot.size`.
3. Compute the snapshot percent from files; keep the combined-bar **byte weighting** on the real `snapshotSizeBytes` so bootstrap percentages are algebraically unchanged (proven: today `bytesDownloaded/snapshotSize === filesDownloaded/filesTotal`).
4. Label the detail readout "files"; append a static "≈ N GB total" segment only when a real size exists (bootstrap); partial sync omits it (no size on `transferProgress`).
5. Keep real ancillary bytes byte-formatted; preserve the locked "verified … chain data" subtitle and the existing combined bar label.
6. Update `MithrilStepIndicator.spec.tsx` / `MithrilProgressView.spec.tsx`; re-run `MithrilBootstrap.spec.tsx` + `MithrilPartialSyncOverlay.spec.tsx` as bootstrap-regression / partial-sync coverage.

**Anchor drifts found.**
- `MithrilBootstrapStore.ts` synthetic getter is at **L78-99** (JSON said 79-97); the `*(filesDownloaded/filesTotal)*snapshot.size` math is L96-98. Minor, documented.
- The "byte formatter" (`formatSnapshotCount`, `MithrilStepIndicator.tsx:193-194`) is actually `formattedNumber` (thousands-separated plain number), NOT a GB/byte formatter. The PRD's 2026-06-19 factual correction already records this; the true defect is synthetic values + byte-named props. Documented as a drift so the implementer does not hunt for a GB formatter.
- `MithrilPartialSyncOverlay.tsx:87-88` confirmed exactly (`transferProgress?.filesDownloaded`/`filesTotal` into `bytesDownloaded`/`snapshotSize`).
- `MithrilStepIndicator.tsx:196-198` combined-detail render confirmed (L196-202).
- Found the un-anchored real-data consumer chain (`MithrilBootstrapPage.tsx:144-145,150` etc.) and carried it into the plan so the rename is complete.

**Interaction-mode decision.** `autonomous`. D4 specifies the file-count semantic + example readout, and the static "≈ N GB total" is mechanically derived from real `snapshot.size`. The change reuses already-approved labels and preserves locked wording #4 — no benefit-framing / behind-ness copy judgment (that was 304). No genuine user-blocking decision exists; `needsUserDecision = false`, `userBlockingQuestion = ""`.

**Outcome.** Canonical plan written to the small-model bar (exact files, line anchors, prop/getter/message names, mechanical steps, exact verification commands incl. the Node v24 dart-sass workarounds). Planning status set to `in_review`. Ready for the Critiquer subagent pass.

## Critiquer: 2026-06-26T09:55:16Z

**Scope of pass:** one broad pass over task-ux-401 plan vs PRD D4, research-19 gaps #4/#5 (download portion), JSON entry, locked boundaries #8/#11/#4, and the LIVE working tree at every cited anchor.

**Live-seam re-verification (all CONFIRMED against working tree):**
- `MithrilBootstrapStore.ts`: synthetic `@computed get bytesDownloaded()` spans L78-99 (math `(filesDownloaded/filesTotal)*snapshot.size`); raw `@observable filesDownloaded/filesTotal` present (L49-51); `ancillaryProgress` getter separate. Plan's documented drift (cited 79-97) is harmless — the delete-the-whole-getter instruction is unambiguous.
- `MithrilBootstrapPage.tsx:144-145` feeds `bytesDownloaded`/`snapshot?.size`. Consumer chain `MithrilBootstrap.tsx` → `MithrilProgressView.tsx` → `MithrilStepIndicator.tsx` all carry `bytesDownloaded`/`snapshotSize` to rename. CONFIRMED.
- `MithrilStepIndicator.tsx`: `deriveCombinedDownloadPercent` byte-weighting branch keys on `snapshotSize` (L150-171 region); `formatCombinedProgressDetails` uses `formatSnapshotCount` → `formattedNumber` (plain number, NOT GB); `snapshotPercent` derived from `bytesDownloaded/snapshotSize`; `showBars` guard references both. Import line 17 = `formatTransferSize` only; `formatSnapshotSize` exists in `snapshotFormatting.ts` and returns null for non-positive size. CONFIRMED — matches plan exactly.
- `MithrilPartialSyncOverlay.tsx:87-88` routes `transferProgress?.filesDownloaded/filesTotal` into `bytesDownloaded`/`snapshotSize` byte props. CONFIRMED (gap #4).
- i18n: `en-US.json:314`/`ja-JP.json:314` carry first-class `combinedDetail`; specs render via en-US translations (so the 8b en-US edit must precede running the StepIndicator/Overlay specs — verification ordering already does i18n→jest). CONFIRMED.

**Correctness checks I ran (algebra against the live specs):**
- 54% test (`filesDownloaded`=500,`filesTotal`=1000,`snapshotSizeBytes`=1000,anc 90/100): snapshotPercent 50%; weight on snapshotSizeBytes=1000 + ancBytes=100 → 0.5*90.9 + 0.9*9.09 = 53.6 → 54%. Unchanged. ✓
- Both 100% completion tests short-circuit via ancillary-complete / file-complete regardless of weighting. ✓
- 901/25,400 detail test asserts text only (no progressbar%), so it correctly omits `snapshotSizeBytes` and proves the "no static suffix without real size" path; regex updated to insert "files". ✓
- PartialSyncOverlay regex `/snapshot files: .*fast sync:/i` still matches the new "… / Y files | Fast sync:" string. ✓

**Coverage verdict:** D4 + gap #4 + download-portion of gap #5 fully covered. Synthetic-byte getter deleted; counts no longer ride byte-named props; real ancillary bytes (`formatTransferSize`) untouched; static "≈ N GB total" only from real `snapshot.size` via `formatSnapshotSize` (null-guarded); partial sync correctly omits the static size (no real total in `MithrilPartialSyncTransferProgress`). No synthetic throughput/remaining-time/overall-% introduced (#8/#18 honored). Bootstrap regression (#11) explicitly re-verified via `MithrilBootstrap.spec.tsx` + `MithrilProgressView.spec.tsx` + preserved 54%/100% percent assertions; algebraic-equivalence argument is sound. Locked wording #4 (`partialSyncProgressSubtitle`) and the approved combined label untouched. Non-goals (spinner/elapsed/error-copy/recovery) correctly fenced to 402/403/404.

**Blockers:** none.

**Nits (non-blocking):**
- Spec line anchors in Step 9a (e.g. "169-191", "193-228") drift a few lines from the live file; the small model locates the tests by their 54%/100% assertion text, so this is cosmetic.
- Worth one sentence that the StepIndicator/Overlay specs render against `en-US.json` (so the assertion changes depend on the 8b edit) — the verification command order already enforces i18n-before-jest, so no action required.

Decision: approved
