# task-ux-706 · CAT-B — JA consistency fixes + handoff-note shortcut

Resolves JA review #2, #3, #5 (resolutions #2, #3, #5) plus Extra #8 (handoff-note Ctrl/Cmd). Mostly
copy/i18n; B4 adds one small platform-aware render tweak. Same i18n mechanics as CAT-A (edit source
default + `ja-JP.json`, then re-sync `defaultMessages.json` via i18n-messaging). Lines are shared between
`en-US.json` and `ja-JP.json`.

## Step B1 — "Daedalus Diagnostics" as the full page name in all three locations (JA review #2)

Direction: use the full screen name "Daedalus Diagnostics" everywhere it is named (English in both
locales, not 「診断」).

- `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` (line 173) — **already** contains
  "Daedalus Diagnostics" in both EN and JA. **No change** beyond CAT-A's `Mithril同期`→`Mithril Sync`
  edit on the same line. Confirm it reads "…からMithril Syncを開始できます" (no spaces around the token
  — CAT-A's in-place rule keeps surrounding particles untouched) with the page name intact.
- `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel` (line 160):
  - EN: `Back to diagnostics` → `Back to Daedalus Diagnostics`
  - JA: `診断に戻る` → `Daedalus Diagnosticsに戻る`
- `menu.helpSupport.daedalusDiagnostics` (`source/main/locales/ja-JP.json:36` — **main-process**
  locale, added 2026-07-10 per DD-706-9): JA `Daedalus診断` → `Daedalus Diagnostics`. This is the Help
  menu item the handoff note points users at (`osx.ts:204` / `win-linux.ts:223` render this label);
  after the renderer edits, JA copy would otherwise name a menu entry that doesn't exist verbatim.
  EN (`en-US.json:36`) already reads "Daedalus Diagnostics" — JA-only, hand-edited (the main-process
  locales are outside the renderer i18n trio; no `defaultMessages.json` re-sync). Flag in the round-2
  table for translator confirmation.

The two renderer edits plus the menu label remove the last user-facing 「診断」 occurrences (the
original "only occurrence" recon claim missed the Help menu — plan-review CAT-B #1). Edit the
`mithrilPartialSyncConfirmationCancel` source default (in `MithrilPartialSyncConfirmation.tsx` message
defs) + re-sync.

## Step B2 — 「シャットダウン」 → 「停止」 (JA review #3)

JA-only; pairs "stop ↔ start" consistently (rows using 停止 elsewhere).

- `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` (line 178):
  - JA: `…お使いのCardanoノードをシャットダウンする必要があります。…`
      → `…お使いのCardanoノードを停止する必要があります。…`
  - EN unchanged ("shut down" stays). This line also carries CAT-A's `Mithril同期`→`Mithril Sync` edit —
    apply both in one edit.

## Step B3 — 「先端」 → 「最新ブロック」 for "blockchain tip" (JA review #5)

JA-only; 「先端」 is a literal calque, 「最新ブロック」 is the community-standard rendering. EN "blockchain
tip" is unchanged.

- `daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown` (line 169):
  - JA: `ノードはブロックチェーンの先端より遅れています。`
      → `ノードはブロックチェーンの最新ブロックより遅れています。`
- `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip` (line 164):
  - JA: `ノードはブロックチェーンの先端に近い状態です。…`
      → `ノードはブロックチェーンの最新ブロックに近い状態です。…`
  - This line also carries CAT-A (`Mithril同期`). Settled (CAT-D D1 reconciliation, 2026-07-09/10):
    CAT-D does **not** rewrite this string — `recommendationNearTip` stays scoped to
    `0 < gap < threshold` — so apply B3's 最新ブロック edit here.

## Step B4 — Handoff-note shortcut: platform-aware "(Ctrl + D)" vs "(Cmd + D)" (Extra #8)

The proactive-prompt handoff note hardcodes the wrong shortcut on macOS. Same line 173 as B1.

- Current (`mithrilProactivePromptHandoffNote`, en-US/ja-JP line 173): "…start the Mithril Sync from the
  Daedalus Diagnostics screen under the Help menu. **(Ctrl + D)**". Rendered in
  `SyncingConnectingMithrilPrompt.tsx:171-175` (message def `promptHandoffNote` `:40-46`).
- The Diagnostics menu accelerator is platform-specific: `Command+D` on macOS
  (`source/main/menus/osx.ts:205`) but `Ctrl+D` on Windows/Linux (`source/main/menus/win-linux.ts:224`).
  So "(Ctrl + D)" is wrong on macOS — it should read "(Cmd + D)" (or ⌘). There is **no** existing
  renderer-side substitution for this string today; it is a flat literal.

**Fix (implementer picks one; prefer the first):**
1. **Placeholder + platform value.** Change the string to end `…under the Help menu. ({shortcut})` and
   pass `shortcut` from the component using the precomputed boolean `global.environment.isMacOS`
   (`environment.types.ts:33`, populated via `checkIsMacOS` in `source/main/environment.ts`; renderer
   precedents: `crypto.ts:116`, `AppUpdateContainer.tsx:18`, `StakePoolsSettings.tsx:152`) — one hop
   simpler than wiring `checkIsMacOS(...)` directly — yielding `Cmd + D` on macOS and `Ctrl + D`
   elsewhere. One message, one placeholder; keeps EN/JA in sync (the JA string uses the same
   `{shortcut}` token). Do **not** translate the shortcut token itself. Feasibility confirmed: the same
   file already uses `intl.formatMessage(messages.promptBody, { epochs })`
   (`SyncingConnectingMithrilPrompt.tsx:164-166`), so `{shortcut}` values are directly supported.
2. **Two message variants** (`…HandoffNoteMac` / `…HandoffNote`) selected at render time. Only if the
   placeholder approach fights the ICU/`FormattedMessage` wiring. More strings to keep in sync — avoid
   unless #1 is impractical.

Keep the literal key `D` (both accelerators use `D`); only the modifier word changes (Ctrl ↔ Cmd). This
is a small code change in addition to the copy edit — reflected in the CAT-B type ("small code"). The
smoke-test cheat sheet already flags this (its macOS notes, "verify what actually opens Diagnostics on
macOS (try ⌘D)") — the Extra #7 cheat-sheet update should tick it off once fixed.

## Acceptance

- No user-facing 「診断」 or 「先端」 remains — including the main-process Help-menu label; no
  「シャットダウン」 remains in the process summary.
- "Daedalus Diagnostics" appears as the full page name in the handoff note, the confirmation cancel
  button, and the JA Help menu (flagged for round-2 confirmation).
- The handoff-note shortcut renders "(Cmd + D)" on macOS and "(Ctrl + D)" on Windows/Linux; the `{...}`
  placeholder (if used) stays byte-identical across EN/JA and is never translated.
- EN copy changes limited to the confirmation-cancel button and the handoff-note shortcut wording;
  everything else JA-only (incl. the main-process menu label). B4 also adds a small platform check in
  the prompt component (`global.environment.isMacOS`).
- i18n-messaging validation clean; `yarn compile` + lint clean.
