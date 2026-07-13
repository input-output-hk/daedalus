# task-ux-401 — Implementation & Code-Review log (append-only)

Append-only chronological transcript. Each entry: speaker label (`Implementation:` / `Code Review:`), ISO-8601 UTC `Timestamp:`, outcome. Code-Review entries end with `Decision: approved` or `Decision: requires_changes`. Entries are appended during/after build; none yet.

---

Implementation:
Timestamp: 2026-06-26T10:12:53Z

Executed the approved plan end to end. Download progress is now driven by honest file counts in both the bootstrap and partial-sync flows; the synthetic main-snapshot byte readout is removed.

Resolved changes (by step):
- Step 1 — `MithrilBootstrapStore.ts`: deleted the entire synthetic `@computed get bytesDownloaded()` getter (the `(filesDownloaded/filesTotal)*snapshot.size` interpolation). Raw `@observable filesDownloaded`/`filesTotal` and `ancillaryProgress` left intact.
- Step 2 — `MithrilBootstrapPage.tsx`: `progressProps` now feeds `filesDownloaded`, `filesTotal`, and `snapshotSizeBytes: mithrilBootstrap.snapshot?.size` (replacing `bytesDownloaded`/`snapshotSize`).
- Step 3 — `MithrilBootstrap.tsx`: renamed `Props` (`filesDownloaded`/`filesTotal` + new `snapshotSizeBytes`), destructure, and `<MithrilProgressView>` wiring.
- Step 4 — `MithrilProgressView.tsx`: same rename through `Props`/destructure/`<MithrilStepIndicator>`; elapsed-timer block untouched (task-ux-402 territory).
- Step 5 — `MithrilStepIndicator.tsx`: renamed props/destructure; `snapshotPercent` now `filesDownloaded/filesTotal`; `deriveCombinedDownloadPercent` renamed params + new `snapshotSizeBytes` param drives the real-byte combined-bar weighting (falls through to FALLBACK weights when absent, e.g. partial sync); `formatCombinedProgressDetails` renamed `formatSnapshotCount`→`formatFileCount` and appends the static `≈ N GB total` context only when `formatSnapshotSize(snapshotSizeBytes)` returns a real positive size; `showBars` byte-presence guard renamed to file props. Added `formatSnapshotSize` to the `./snapshotFormatting` import. Ancillary `formatTransferSize(...)` left byte-based.
- Step 6 — `MithrilPartialSyncOverlay.tsx`: `transferProgress` file counts now map to `filesDownloaded`/`filesTotal` (no `snapshotSizeBytes` — partial sync has no real total size, so file-counts only and no static suffix).
- Step 7 — `MithrilBootstrap.messages.ts`: `progressCombinedDetail` gains the "files" unit; added new `progressSnapshotSizeContext` (`'!!!≈ {totalSize} total'`).
- Step 8 — i18n: ran `yarn i18n:extract` (regenerated `translations/messages.json`; `defaultMessages.json` is NOT the extract target and is unchanged — verified). Hand-edited runtime `en-US.json` / `ja-JP.json`: updated `combinedDetail` (EN "… files", JA "… 個") and added `snapshotSizeContext` (EN "≈ {totalSize} total", JA "約 {totalSize}（合計）"). No `!!!` in runtime en-US/ja-JP values.
- Step 9 — specs: `MithrilStepIndicator.spec.tsx` props renamed (`filesDownloaded`/`filesTotal`, plus `snapshotSizeBytes` equal to the prior `snapshotSize` wherever ancillary bytes are present so the 54%/100% combined-percent expectations are preserved); detail-text assertion updated to `/snapshot files: 901 \/ 25,400 files \| fast sync: …/i`; added a new test rendering `filesDownloaded:142, filesTotal:980, snapshotSizeBytes:9_700_000_000` asserting `142 / 980 files` AND `≈ … total`. `MithrilProgressView.spec.tsx` test props renamed.

Files changed:
- source/renderer/app/stores/MithrilBootstrapStore.ts
- source/renderer/app/stores/MithrilBootstrapStore.spec.ts (ripple: removed two getter-only tests + one stale `bytesDownloaded` assertion)
- source/renderer/app/containers/loading/MithrilBootstrapPage.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts
- source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- translations/messages.json
- storybook/stories/loading/_support/mithrilHarness.tsx (ripple: rename props)
- storybook/stories/loading/mithril/MithrilProgressView.stories.tsx (ripple: rename props + local SNAPSHOT_FILES_TOTAL)
- storybook/stories/loading/mithril/MithrilBootstrap.stories.tsx (ripple: rename props + local SNAPSHOT_FILES_TOTAL)

REAL verification commands + results:
- `node_modules/.bin/tsc --noEmit -p .` → EXIT 0 (authoritative TS gate; used directly per the documented Node v24 dart-sass env defect on `yarn compile`'s `typedef:sass` hook). First run surfaced downstream consumers of the renamed/removed props (store spec + 3 storybook files) which the plan's live-seam table missed; fixed mechanically, second run clean.
- `node_modules/.bin/eslint <13 touched src/spec/storybook files> --ext .ts,.tsx` → EXIT 0 (warnings only; all pre-existing — confirmed the 6 mobx decorator "defined but never used" warnings in `MithrilBootstrapStore.ts` also occur on the HEAD version via `git stash`).
- `yarn i18n:extract` → Done; `translations/messages.json` updated (combinedDetail + new snapshotSizeContext), `defaultMessages.json` unchanged.
- `grep -n '!!!' …/en-US.json …/ja-JP.json | grep -i 'combinedDetail|snapshotSizeContext'` → no matches (runtime catalogs clean; `!!!` only in the messages.json source defaults, as expected).
- `node_modules/.bin/jest MithrilStepIndicator.spec MithrilProgressView.spec MithrilBootstrap.spec MithrilPartialSyncOverlay.spec MithrilBootstrapStore.spec --coverage=false` → 5 suites / 55 tests PASS (committed css-modules transform did NOT crash in this env, so the identity-obj-proxy CLI override was not needed; committed config left unchanged).
- `node_modules/.bin/prettier --check <all touched files>` → "All matched files use Prettier code style!" EXIT 0 (after `--write`; see deviation re: prettier 2.1.2 non-idempotency).
- grep proofs: store has no `bytesDownloaded`; shared components have no `snapshotSize`/`bytesDownloaded` props; `filesDownloaded`/`filesTotal` threaded through `MithrilStepIndicator.tsx`; ancillary still `formatTransferSize(ancillaryBytes…)`; locked subtitle "restoring verified Mithril chain data" intact.

Locked invariants upheld:
- #8 / #18 (no synthetic figures): the synthetic byte getter is DELETED; the snapshot bar fill is now a true fraction of real file counts, and the combined bar weighting uses the REAL `snapshot.size` bytes via the new `snapshotSizeBytes` prop (never a fabricated byte total). No throughput / remaining-time / overall-% added; no raw mithril-client JSON routed into copy. Verified by the store grep (no `bytesDownloaded`) and by reading the bar math.
- #11 (no empty-chain bootstrap regression): re-verified the SHARED components' bootstrap progress AND success paths — `MithrilBootstrap.spec.tsx`, `MithrilStepIndicator.spec.tsx`, `MithrilProgressView.spec.tsx`, and `MithrilBootstrapStore.spec.ts` all PASS (55 tests). The combined-bar 54% and 100% percentages are unchanged: bootstrap percent is algebraically identical (today `bytesDownloaded/snapshotSize === filesDownloaded/filesTotal`) and the byte weighting is preserved by passing `snapshotSizeBytes` equal to the prior `snapshotSize` in the percent-bearing tests. Starting-node handoff + completed-state tests in `MithrilProgressView.spec.tsx` pass (success path intact).
- #4 (locked wording): `partialSyncProgressSubtitle` "Daedalus is restoring verified Mithril chain data…" untouched; combined-bar accessible label `progressCombinedLabel` ("Snapshot Files and Fast Sync") unchanged.
- Real ancillary bytes stay byte-based; renderer computes no threshold; no new IPC.

Deviations:
1. Scope ripple beyond the plan's "Files expected to change": removing the getter / renaming the props broke three downstream consumers the plan's live-seam table did not list — `MithrilBootstrapStore.spec.ts` (two tests asserted only the deleted synthetic getter → removed; one stale `bytesDownloaded` assertion in the still-valid clearing test → removed) and the storybook files `mithrilHarness.tsx`, `MithrilProgressView.stories.tsx`, `MithrilBootstrap.stories.tsx` (mechanical prop renames; storybook is otherwise task-ux-502's domain, but the authoritative TS gate compiles it, so it had to compile). No new stories added; a local `SNAPSHOT_FILES_TOTAL` constant was introduced in two stories to derive demo file counts from the existing percent knob.
2. Prettier 2.1.2 non-idempotency: the committed (form-A) wrapping of a few lines NOT semantically touched (`STATUS_TO_STEP` type in MithrilStepIndicator.tsx; an `await` assignment in MithrilBootstrapStore.ts; `as unknown as` casts in the store spec; destructure-default lines in mithrilHarness.tsx) began failing `prettier --check` once the file content changed (getter/tests removed flips prettier's fixed point). Per the plan's allowed "--write then re-check", I accepted prettier's output for those lines so `prettier --check` is green. This is a pre-existing prettier defect, not a semantic change.

---

## Code Review: 2026-06-26T10:17:56Z

Independent broad pass over the working-tree diff vs the approved task-ux-401 plan. I reproduced every gate myself (did not trust implementer claims).

REPRODUCED verifications:
- `node_modules/.bin/tsc --noEmit -p .` → EXIT 0 (clean; authoritative gate per the documented Node v24 dart-sass `typedef:sass` defect).
- `node_modules/.bin/jest --moduleNameMapper '{"\\.(scss|sass|css)$":"identity-obj-proxy"}' MithrilStepIndicator.spec MithrilProgressView.spec MithrilBootstrap.spec MithrilPartialSyncOverlay.spec MithrilBootstrapStore.spec` → 5 suites / 55 tests PASS, incl. the bootstrap success + 54%/100% combined-bar percent regression tests (#11) and the new file-count + static-size test.
- `node_modules/.bin/prettier --check <11 touched files>` → "All matched files use Prettier code style!" EXIT 0 (the incidental reformatting in MithrilBootstrapStore.ts/spec, STATUS_TO_STEP, mithrilHarness is prettier-conformant, not stray churn).
- grep proofs: store has no `bytesDownloaded` getter; no leftover `snapshotSize`/`bytesDownloaded` props in the shared renderer components/container; `formatTransferSize(ancillaryBytes…)` untouched; partial-sync overlay spec `/snapshot files: .*fast sync:/i` still green.

Correctness vs plan (all CONFIRMED):
- D4 file-count semantic truthful in BOTH flows: snapshot percent now `filesDownloaded/filesTotal`; partial sync maps `transferProgress.files*` to the renamed `filesDownloaded`/`filesTotal` props (no byte-named path). Counts render via `formatFileCount` (`formattedNumber`), not a byte formatter.
- Synthetic byte getter actually REMOVED from MithrilBootstrapStore.ts (grep NONE); its two dedicated store-spec tests removed and the stale clearing-assertion dropped.
- Static "≈ {size} total" appended only when real `snapshot.size` present (bootstrap, via `snapshotSizeBytes`); omitted for partial sync (no real total) — matches D4. Confirmed by the new test (with size → suffix) and the existing 901/25,400 test (no size → no suffix).
- Real ancillary bytes stay byte-based (`formatTransferSize`), and the combined-bar weighting now uses the REAL `snapshotSizeBytes` (algebraically identical to prior bootstrap math → no percent regression).

Locked invariants: #8/#18 upheld (synthetic readout deleted; no throughput/remaining-time/overall-%; no raw mithril-client JSON in copy; bar fill = true file fraction + real-byte weight). #11 upheld (bootstrap progress + success paths re-run, not assumed; all green). #4 subtitle and `progressCombinedLabel` untouched. No new IPC/contract; renderer computes no threshold. The `source/main/mithril/*.ts` `bytesDownloaded` references are pre-existing backend (real ancillary byte stream), untouched by this renderer-only diff.

Non-blocking notes (nits):
- `defaultMessages.json` is stale (still lacks the "files" unit and the new `snapshotSizeContext` key). Harmless: `i18n:extract` only writes `translations/messages.json`, and `defaultMessages.json` is loaded by `require.context` solely as a locale literally named "defaultMessages" that is never selected at runtime (app uses en-US/ja-JP). Pre-existing legacy-artifact pattern; not introduced by this task.
- `.gitignore` carries an unrelated `+.devcontainer` line — UNSTAGED (` M`), pre-existing in the starting working-tree snapshot, nothing is staged. Confirmed it is not part of this task's change set.
- Storybook ripple (mithrilHarness + 2 stories) and the store-spec edits go beyond the plan's "Files expected to change" list, but were required for the TS gate to compile and are mechanical; acceptable.

No correctness, invariant, coverage, or contract-drift problems found.

Decision: approved
