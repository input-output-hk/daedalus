# task-ux-401 — Research note

Durable findings (decisions, gotchas, evidence, residual gaps) appended at completion.

Seed context (from planning):
- The bootstrap synthetic byte readout `bytesDownloaded = (filesDownloaded/filesTotal)*snapshot.size` makes `bytesDownloaded/snapshotSize` algebraically equal to `filesDownloaded/filesTotal`, so moving to a file-driven percent is a no-op for bootstrap bar percentages — keep the combined-bar weighting on real `snapshotSizeBytes`.
- `formatSnapshotCount` (`MithrilStepIndicator.tsx`) is `formattedNumber` (plain number), not a GB formatter — gap-#4's "byte formatter" wording is imprecise (PRD already corrected this 2026-06-19).
- `MithrilPartialSyncTransferProgress` carries no total-size field, so partial sync shows file counts only (D4 open dependent resolved: omit).

---

## Durable findings (completion — code review approved 2026-06-26T10:17:56Z)

### Verified anchors (confirmed live, 2026-06-26)
- The dishonest seams were exactly two and both renderer-only: (a) the synthetic `@computed get bytesDownloaded()` in `MithrilBootstrapStore.ts` (deleted), and (b) `MithrilPartialSyncOverlay.tsx` feeding `transferProgress.filesDownloaded/filesTotal` into the shared component's **byte-named** props. No backend/IPC was implicated.
- The file-count fix flows through one shared prop chain: `MithrilBootstrapPage.tsx` → `MithrilBootstrap.tsx` → `MithrilProgressView.tsx` → `MithrilStepIndicator.tsx`. Renaming `bytesDownloaded`→`filesDownloaded`, `snapshotSize`→`filesTotal` and adding `snapshotSizeBytes` had to be applied through all four to keep TS green.
- The combined-bar weighting (`deriveCombinedDownloadPercent`) must be driven by real bytes (`snapshotSizeBytes`), NOT by `filesTotal`. Passing `snapshotSizeBytes` equal to the prior `snapshotSize` preserves the asserted 54%/100% bootstrap percentages — the algebraic-equivalence argument held in practice (55 tests green).

### Drift recorded
- **Anchor drift:** the synthetic getter is `MithrilBootstrapStore.ts` **L78-99** (plan/JSON cited 79-97). Minor; the math is L96-98.
- **Semantic drift (corrected):** gap-#4's "byte formatter" is a misnomer — file counts already rendered via `formattedNumber` (plain thousands-separated number), not a GB/byte formatter. The real defects were (1) **synthetic** values and (2) **byte-named props** carrying counts, NOT a wrong number format. PRD's 2026-06-19 correction already records this; treat gap-#4 wording as imprecise in any downstream reference.

### Decisions / gotchas
- **File-count vs byte semantic:** the bar fill + running readout are file-based in both flows; real `snapshot.size` is allowed only as a **static** "≈ N GB total" suffix, appended solely when `formatSnapshotSize(snapshotSizeBytes)` returns a real positive size. Partial sync has no size field → no suffix. This is the load-bearing D4 distinction.
- **Real ancillary bytes handling:** the ancillary (fast-sync) stream carries TRUE bytes and stays byte-formatted via `formatTransferSize`. Only the synthetic *main-snapshot* byte readout became file counts. Do not "unify" ancillary into file counts in 402/502/503 — it is correctly byte-based.
- **Env-defect workarounds (still active, repo-wide, NOT task regressions):**
  - Node v24 dart-sass breaks `yarn compile`'s `typedef:sass` hook → use `node_modules/.bin/tsc --noEmit -p .` as the authoritative TS gate (allow up to 600s).
  - jest css-modules transform can crash under the same defect → map `\.(scss|sass|css)$` to `identity-obj-proxy` via a **CLI-only** override; do NOT stage a sidecar config. (Note: the implementer's run did NOT hit the jest crash this time; the committed config was sufficient. The override remains a safe fallback.)
  - prettier 2.1.2 is non-idempotent — once touched-file content changes, a few previously-committed wrappings (`STATUS_TO_STEP`, an `await` assignment, `as unknown as` casts, destructure-defaults) flip and fail `--check`. Accept `prettier --write` output for those lines; it is a tooling defect, not a semantic change.

### Residual gaps / follow-ups
- **`defaultMessages.json` is stale** (lacks the "files" unit + `snapshotSizeContext`). Harmless: `i18n:extract` only writes `translations/messages.json`; `defaultMessages.json` is a never-selected locale literal. Pre-existing legacy-artifact pattern. If a future i18n task (task-ux-601) wants it consistent, regenerate deliberately — out of scope here.
- **task-ux-402:** builds directly on these shared components (`MithrilStepIndicator`/`MithrilProgressView`). The elapsed-timer block (`MithrilProgressView.tsx`), `.activeCircle`/`.iconSpinner`, `bootstrapStartedAt`, `elapsedSeconds` were intentionally left untouched — they are 402's active-spinner / ticking-elapsed / reassurance territory. The `snapshotSizeBytes` plumbing and file-count props are now in place for 402 to layer on.
- **task-ux-502 (Storybook):** this task already had to do mechanical prop renames + a local `SNAPSHOT_FILES_TOTAL` constant in `mithrilHarness.tsx` and the two mithril stories to keep the TS gate compiling. No new stories were added — 502 still owns adding/expanding stories and should reconcile the demo file-count derivation.
- **task-ux-503:** no direct coupling surfaced here; the file-count semantic and real-ancillary-byte handling established by this task are the truthful baseline any 503 surface should preserve.
