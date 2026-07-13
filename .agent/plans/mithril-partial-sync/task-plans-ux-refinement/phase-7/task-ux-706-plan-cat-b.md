# task-ux-706 · CAT-B — JA consistency fixes + handoff-note shortcut

Resolves JA review #2, #3, #5 (resolutions #2, #3, #5) plus Extra #8 (handoff-note Ctrl/Cmd). Mostly
copy/i18n; B4 adds one small platform-aware render tweak. Same i18n mechanics as CAT-A (edit source
default + `ja-JP.json`, then re-sync `defaultMessages.json` via i18n-messaging). Lines are shared between
`en-US.json` and `ja-JP.json`.

## Step B1 — full page name everywhere, matching how each locale's menu names it (JA review #2)

Direction: every reference to the Diagnostics page uses the full name **as the user's Help menu shows
it** — EN "Daedalus Diagnostics" (`en-US.json:36`), JA 「Daedalus診断」
(`menu.helpSupport.daedalusDiagnostics`, `source/main/locales/ja-JP.json:36`, rendered by `osx.ts:204`
/ `win-linux.ts:223`). The page itself renders no title, so the menu label is the only place the JA UI
names it — copy that references the page must match that label verbatim. The menu label itself does
**not** change (no main-process edit).

- `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` (line 173) — EN already reads
  "Daedalus Diagnostics screen"; JA-only edit:
  - JA: `…ヘルプメニューにあるDaedalus Diagnostics画面からMithril同期を開始できます。…`
      → `…ヘルプメニューにあるDaedalus診断画面からMithril同期を開始できます。…`
- `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel` (line 160):
  - EN: `Back to diagnostics` → `Back to Daedalus Diagnostics`
  - JA: `診断に戻る` → `Daedalus診断に戻る`

After these two edits, every JA reference to the page reads 「Daedalus診断」, matching the Help-menu
entry the handoff note points users at (the original "only occurrence" recon claim missed the Help menu
— plan-review CAT-B #1). Edit the `mithrilPartialSyncConfirmationCancel` source default (in
`MithrilPartialSyncConfirmation.tsx` message defs) + re-sync. Both revised JA strings go in the round-2
delta as normal entries.

## Step B2 — 「シャットダウン」 → 「停止」 (JA review #3)

JA-only; pairs "stop ↔ start" consistently (rows using 停止 elsewhere).

- `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` (line 178):
  - JA: `…お使いのCardanoノードをシャットダウンする必要があります。…`
      → `…お使いのCardanoノードを停止する必要があります。…`
  - EN unchanged ("shut down" stays).

## Step B3 — 「先端」 → 「最新ブロック」 for "blockchain tip" (JA review #5)

JA-only; 「先端」 is a literal calque, 「最新ブロック」 is the community-standard rendering. EN "blockchain
tip" is unchanged.

- `daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown` (line 169):
  - JA: `ノードはブロックチェーンの先端より遅れています。`
      → `ノードはブロックチェーンの最新ブロックより遅れています。`
- `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip` (line 164):
  - JA: `ノードはブロックチェーンの先端に近い状態です。…`
      → `ノードはブロックチェーンの最新ブロックに近い状態です。…`
  - Settled (CAT-D D1 reconciliation, 2026-07-09/10): CAT-D does **not** rewrite this string —
    `recommendationNearTip` stays scoped to `0 < gap < threshold` — so apply B3's 最新ブロック edit here.

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

- No bare user-facing 「診断」 (i.e. without the Daedalus prefix) or 「先端」 remains; no
  「シャットダウン」 remains in the process summary.
- The full page name appears in the handoff note and the confirmation cancel button, matching each
  locale's Help-menu label — EN "Daedalus Diagnostics", JA 「Daedalus診断」. The menu label itself is
  unchanged.
- The handoff-note shortcut renders "(Cmd + D)" on macOS and "(Ctrl + D)" on Windows/Linux; the `{...}`
  placeholder (if used) stays byte-identical across EN/JA and is never translated.
- EN copy changes limited to the confirmation-cancel button and the handoff-note shortcut wording;
  everything else JA-only. B4 also adds a small platform check in the prompt component
  (`global.environment.isMacOS`).
- i18n-messaging validation clean; `yarn compile` + lint clean.
