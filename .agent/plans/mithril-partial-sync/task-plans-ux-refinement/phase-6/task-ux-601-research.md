# task-ux-601 — Research: Holistic EN+JA copy review, one canonical ja-JP rendering, glossary

> Phase 6 / PRD D10. Copy-only task. No IPC/backend/threshold/figure-logic changes.
> Grounding date: 2026-06-26. Line anchors confirmed by re-opening every file (see below).

## 1. Scope grounding (user decisions, 2026-06-26 sign-off — OVERRIDE older JSON "keep partial sync")

- **D-A (sync-% scope):** Remove sync-percentage from BOTH the confirmation-modal behind-ness AND the
  recommendation line. Behind-ness is EPOCHS-ONLY in confirmation + recommendation. The diagnostics
  TABLE row keeps its own raw `{syncPercentage}` figure (`getRow('syncPercentage', ...)` —
  `DaedalusDiagnostics.tsx:719`). Do NOT touch that row.
- **D-B (canonical name):** EN feature name = **"Mithril Sync"** (plain "Mithril" where it reads
  naturally in prose). JA canonical = **"Mithril同期"** (keep Latin "Mithril" brand token, append
  同期, DROP 部分). ONE canonical JA rendering across BOTH diagnostics + overlay namespaces. NEVER
  "partial sync" / "Mithril Partial Sync" / 部分同期 in any user-facing string.
- **D-C (full D13 reach):** Also rename diagnostics `buttonLabel`, `sectionLabel`, `buttonHintReady`.
  Goal: ZERO "partial sync" in any user-facing surface.

Vocabulary rules (durable): behind-ness in EPOCHS only (no sync-%, no immutable files, no slots).
Feature name user-facing = "Mithril Sync" vs "standard sync". "Mithril partial sync" is
internal/engineering language only.

Locks honoured: **#4** keep verified-data wording verbatim ("...can restore verified chain data to
help it catch up faster...") — only swap the name token. **#34** keep copy simple. **#18** never
route raw mithril-client JSON into copy. Do NOT re-derive the behind-ness epochs FIGURE logic
(owned by task-ux-304).

## 2. Current-state findings (line-anchored)

### A) `source/renderer/app/i18n/locales/en-US.json`
Diagnostics block (confirmed lines):
- 157 `mithrilPartialSyncButtonHintReady` = "Review what will happen before Daedalus starts Mithril partial sync."
- 158 `mithrilPartialSyncButtonLabel` = "Mithril Partial Sync"
- 159 `mithrilPartialSyncConfirmationBehind` = "Your node is about {epochs} epochs behind the blockchain tip. Mithril partial sync can restore verified chain data to help it catch up faster than waiting for normal sync."
- 160 `mithrilPartialSyncConfirmationBehindSyncContext` = "({syncPercentage}% synced)"  ← DELETE (D-A)
- 161 `...BehindUnknown` = "Your node is behind the latest verified snapshot." (clean, keep)
- 163 `mithrilPartialSyncConfirmationConfirm` = "Start Mithril partial sync"
- 164 `...Intro` = "...download and restore verified Mithril data." (clean, keep)
- 165 `...Recovery` = "...wiping chain data and running a full Mithril sync." (clean — "full Mithril sync" is a different concept; keep)
- 166-168 stepStop/stepDownload/stepRestart — 167 `stepRestart` has "...normal syncing resumes." (harmonize normal→standard)
- 169 `...ConfirmationSuccess` = "If Mithril partial sync succeeds, ... normal syncing will resume."
- 170 `...ConfirmationTitle` = "Before Mithril partial sync begins"
- 171 `mithrilPartialSyncRecommendation` = "If Cardano node catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster."
- 172 `mithrilPartialSyncRecommendationWithProgress` = "Cardano node is currently {syncPercentage}% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster."  ← carries the only recommendation sync-%
- 173 `mithrilPartialSyncSectionLabel` = "Mithril Partial Sync"

Proactive prompt block (174-182) — already epochs-only/clean (task-ux-302). Verified NO "partial
sync"; uses "Mithril" + "standard sync" + "Mithril Sync (fast)" / "Standard Sync (slow)". No change.

Overlay block (loading.mithrilPartialSync.*) — "partial sync" occurrences:
- 351 `completed.subtitle` = "Mithril partial sync completed successfully. Continue to return to the normal Daedalus app flow."
- 353 `error.cancelled.title` = "Mithril partial sync was cancelled"
- 358 `error.failed.hint` = "Use one of the available recovery actions to retry Mithril partial sync, restart normally, or wipe chain data and do a full Mithril sync."
- 359 `error.failed.title` = "Mithril partial sync failed"
- 366 `error.retry` = "Retry Mithril partial sync"
- **371 `progress.nodeStartingDetail`** = "Mithril partial sync has finished restoring chain data. Cardano node is starting so Daedalus can resume normal syncing."  ← **NOT in the task's listed anchors; found by grep. Must fix to hit ZERO "partial sync".**
- 376 `title` = "Mithril partial sync"

`grep -n "partial sync\|Partial Sync"` over en-US.json returns exactly: 157,158,159,163,169,170,171,172,173,351,353,358,359,366,371,376. All other overlay error keys (conversionFailed, downloadFailed, latestDrift, noCertifiedRange, stagedDbInvalid) already say "Mithril sync" / "Mithril snapshot" and are clean.

### B) `source/renderer/app/i18n/locales/ja-JP.json` (the 3-way mess)
- 157/159/163/169/170/171/172 literally embed the English "Mithril partial sync" inside JA prose.
- 158/173 say English "Mithril Partial Sync".
- Overlay 351/353/358/359/366/371/376 use "Mithril部分同期".
- 352/360 etc. already use "Mithril同期" (mixed).
- 347 `mithrilBootstrap.stepIndicatorLabel` already canonical "Mithril同期の進捗" — confirms 同期 is the established JA token.
- Proactive prompt (174-182) JA already uses "Mithril" + "標準同期" (standard sync) + "Mithril Sync（高速）" / "標準同期（低速）". Clean. **JA canonical contrast term for "standard sync" = 標準同期** (already in use line 175/183).
- 160 BehindSyncContext JA = "（{syncPercentage}% 同期済み）" ← DELETE.

### C) Generated catalogs
- `source/renderer/app/i18n/locales/defaultMessages.json` — has ids `...BehindSyncContext` (line 1873)
  and `...RecommendationWithProgress` (line 1923). GENERATED from component defineMessages.
- `translations/messages.json` — formatjs extract target.
- Regen flow: `i18n:extract` (formatjs extract → translations/messages.json) then `i18n:check`
  (`translations/translation-runner.ts` → react-intl-translations-manager, validates en-US/ja-JP and
  regenerates defaultMessages.json). Both deleted ids vanish once removed from the component
  `defineMessages`. Do NOT hand-edit the generated catalogs.

## 3. Component / prop-threading investigation

### Recommendation render site: `MithrilPartialSyncRecommendation.tsx`
- `defineMessages` has `recommendation` (no var) and `recommendationWithProgress` (with
  `{syncPercentage}`), plus `buttonLabel`, `buttonHintBlocked`, `buttonHintReady`.
- Render (lines 55-94): `recommendationMessage = isSynced ? messages.recommendation : messages.recommendationWithProgress;`
  then `intl.formatMessage(recommendationMessage, { syncPercentage: formattedSyncPercentage })`.
- **`{epochs}` is NOT available in this component** (props are `formattedSyncPercentage, isActionBlocked,
  isSynced, onShowConfirmation` only). Therefore the task's "reword epochs-only" is NOT possible here
  without threading a new behindByEpochs prop. **Smallest truthful change = drop the WithProgress
  variant and always use the base `recommendation`** (which is already epochs-agnostic and carries the
  lock-#4 verified-data wording). This is the recorded decision (see plan §3).
- Consequence: after collapse, `formattedSyncPercentage` is DEAD here (only consumer was the
  WithProgress `{syncPercentage}`); and `isSynced` is DEAD here (only consumer was the variant select).

### Confirmation: `MithrilPartialSyncConfirmation.tsx`
- `behindSyncContext` in defineMessages (lines 34-40) and its render `<p>` (lines 148-152) are the
  ONLY consumers of `formattedSyncPercentage` (Props line 93, destructure line 108, use line 150).
- Behind-ness primary line (141-147) uses `behindByEpochs` → `messages.behind` / `messages.behindUnknown`
  — epochs-only, unaffected by D-A. After removing the sync-context `<p>`, `formattedSyncPercentage`
  is DEAD here → remove prop.

### Section: `MithrilPartialSyncSection.tsx`
- Receives `formattedSyncPercentage` (Props 20) + `isSynced` (Props 23); threads both to Confirmation
  (131) and Recommendation (149/151). After both children drop them, both are DEAD in Section → remove
  from Props + stop threading.

### DaedalusDiagnostics.tsx (origin)
- `formattedSyncPercentage = formattedNumber(syncPercentage, 2)` (line 569). Used at **line 719**
  `getRow('syncPercentage', ...)` (the TABLE ROW — KEEP) AND line 722 (Section invocation — REMOVE).
- `isSynced` used at line 718 `getRow('synced', isSynced)` (KEEP) AND line 725 (Section — REMOVE).
- So `formattedSyncPercentage` and `isSynced` remain live in DaedalusDiagnostics for the table; only
  the Section-invocation props are removed. The TABLE-row sync-% is a different code path (D-A safe).

### Specs that assert the removed behaviour (MUST be updated — not in task anchors, found by grep)
- `MithrilPartialSyncConfirmation.spec.tsx`:
  - line 13/55/76 default `formattedSyncPercentage: '62.50'` fixture.
  - Test "renders the epochs behind-ness line plus the compact sync-% reference below it" (≈53-70)
    asserts old behind text ("...Mithril partial sync... normal sync.") AND `getByText('(62.50% synced)')`
    + DOM-ordering of the sync-context below the primary line. Must be rewritten: new behind text
    ("...Mithril Sync... standard sync."), DROP the sync-% assertions/ordering. Keep the
    `queryByText(/immutable/i)` null check.
  - Test "falls back to generic behind-ness copy ... but still shows sync-%" (≈72-84): DROP the
    `getByText('(62.50% synced)')` assertion; keep the behindUnknown + no-"undefined" checks.
- `MithrilPartialSyncSection.spec.tsx`: line 24 `formattedSyncPercentage`, line 27 `isSynced: false`
  in the default props object — remove if the props are dropped (or leave the fixture keys harmlessly;
  cleanest = remove).

### Storybook
- `storybook/stories/nodes/status/Diagnostics.stories.tsx`:
  - `recommendationBaseProps` (≈80-87) has `formattedSyncPercentage: '62.5'` + `isSynced: false` → remove.
  - 'Synced' story (≈128-130) overrides `isSynced` → remove that story (recommendation no longer varies
    by sync state) or drop the override.
  - `confirmationBaseProps` (≈89-95) has `formattedSyncPercentage: '62.5'` → remove.
  - `baseProps` (DaedalusDiagnostics) feeds the table via `syncPercentage`; no change.
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`: line 106 storybook-local
  fixture string "...retry partial sync..." and line 225 storiesOf TITLE "Loading / Mithril / Partial
  Sync Overlay" are dev-only (story names / mock copy), NOT shipped i18n. Optional tidy; not required
  for the ZERO-user-facing-"partial sync" goal. Flag to reviewer; default = leave story titles, but the
  line-106 mock string is rendered in a story — recommend updating to "...retry Mithril Sync..." for a
  truthful preview.

## 4. Harmonization decision: "normal" → "standard"
`grep` of "normal"/"standard" in Mithril copy (en-US): 159, 165, 167, 169, 175, 351, 358, 365, 371.
- The fork-choice contrast noun is "standard sync" (already used at 175 + proactive buttons; JA 標準同期).
- **Change to standard:** 159 ("...waiting for normal sync." → "...waiting for standard sync."),
  167 ("normal syncing resumes" → "standard syncing resumes"), 169 ("normal syncing will resume" →
  "standard syncing will resume"), 371 ("resume normal syncing" → "resume standard syncing"). Rationale:
  these all name the node's regular sync method, which is the "standard sync" the user weighs against
  Mithril Sync — one consistent vocabulary.
- **Leave "normal":** 351 "the normal Daedalus app flow" (UI flow, not a sync method); 165 "restarting
  normally" / 358 "restart normally" / 365 "Restart normally" (the named **restart-normal recovery
  action** idiom — restart the node normally on current data; a distinct concept). "full Mithril sync"
  (165/358/369) is the bootstrap full-sync, not "partial sync" — keep.
- JA mirror: 通常の同期 → 標準同期 at 159/167/169/371; leave 通常 at 351; leave 通常再起動 (restart
  normally) at 358.

## 5. Verification environment notes (Node v24)
- The deletion removes `.mithrilPartialSyncConfirmationBehindSyncContext` from
  `DaedalusDiagnostics.scss` (line 400-404). Because the `.scss` changes, the generated
  `DaedalusDiagnostics.scss.d.ts` (line 17 exports the class) goes stale and `tsc --noEmit` will fail
  until regenerated. Regen the touched file first:
  `node_modules/.bin/typed-scss-modules source/renderer/app/components/status/DaedalusDiagnostics.scss`
  then re-run `yarn compile`.
- Also adjust `.mithrilPartialSyncConfirmationBehind` bottom margin: it was `margin: 0 0 4px` only
  because the sync-context line followed it with `0 0 16px`. After deleting the sync-context line,
  bump the behind line to `margin: 0 0 16px` so spacing before the `<ol>` steps is preserved.
- `yarn i18n:manage` (= extract + check) is THE gate: 4 catalogs consistent, no `!!!` placeholders,
  deleted ids gone. Focused jest on scss-importing components needs the gitignored identity-obj-proxy
  scss sidecar `--config`; env quirk, not a regression.
