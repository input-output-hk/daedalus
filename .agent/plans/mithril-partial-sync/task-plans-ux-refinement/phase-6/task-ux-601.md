# task-ux-601 — Holistic EN+JA copy review, one canonical ja-JP rendering, and glossary

> Phase 6 / PRD D10. **COPY-ONLY** task: no IPC/backend/threshold/figure-logic changes.
> Status: pending → planned. Deps complete. Planner sign-off: see task-ux-601-plan-review.md.

## Objective
Make every user-facing Mithril string consistent across EN + JA: one canonical feature name, one
canonical JA rendering, epochs-only behind-ness (no sync-%), and ZERO "partial sync" anywhere a user
can read it. Ship an EN+JA glossary.

## The 3 user decisions (2026-06-26 sign-off — OVERRIDE older JSON "keep partial sync")
- **D-A** Remove sync-% from confirmation behind-ness AND recommendation. Behind-ness = epochs only in
  both. Diagnostics TABLE row keeps its own `{syncPercentage}` (`DaedalusDiagnostics.tsx:719`) — do not touch.
- **D-B** Canonical names: EN = **"Mithril Sync"** (plain "Mithril" in flowing prose). JA = **"Mithril同期"**
  (Latin "Mithril" + 同期, drop 部分). One JA rendering across diagnostics + overlay. Never "partial
  sync" / "Mithril Partial Sync" / 部分同期 user-facing.
- **D-C** Also rename diagnostics `buttonLabel`, `sectionLabel`, `buttonHintReady`. Goal: ZERO "partial
  sync" in any user-facing surface.

## Canonical names (use everywhere)
| Concept | EN | JA |
|---|---|---|
| Feature name (CTA/title/label) | **Mithril Sync** | **Mithril同期** |
| In flowing prose | Mithril | Mithril |
| Contrast: regular sync method | standard sync | 標準同期 |
| Bootstrap full sync (unchanged) | full Mithril sync | 完全なMithril同期 |

## Harmonization: "normal" → "standard"
Change the sync-method noun/gerund only: en-US 159/167/169/371 ("normal sync(ing)" → "standard
sync(ing)"; JA 通常の同期 → 標準同期). Leave: 351 "normal Daedalus app flow" (UI flow); the
restart-normal recovery-action idiom (165/358/365 "restart normally" / 通常再起動); "full Mithril sync".

## Harmonization: capitalization (EN only)
EN 352 (cancelled.hint) and 360 (latestDrift.hint) use lowercase "Mithril sync" for the feature flow →
capitalize to "Mithril Sync" for consistency with the canonical name. NOTE the deliberate exception:
"full Mithril sync" (158/165/369) keeps lowercase "sync" because it names the distinct bootstrap
concept, mirroring EN "Mithril Sync" (feature) vs "full Mithril sync" (bootstrap). JA needs no
capitalization change (同期 has no case); the JA equivalents at 352/360 already read "Mithril同期".

## Harmonization: JA embedded-English (blocking fix from plan-review)
JA 165 (`mithrilPartialSyncConfirmationRecovery`) embedded the English "完全なMithril sync", clashing
with JA 369's "完全なMithril同期". Harmonize 165 → "完全なMithril同期". EN 165 is untouched (keeps the
distinct "full Mithril sync"). This is a JA-only fix.

## Copy change table

### `source/renderer/app/i18n/locales/en-US.json`
| Key (id suffix) | Action | New EN value |
|---|---|---|
| `mithrilPartialSyncButtonHintReady` | reword | `Review what will happen before Daedalus starts Mithril Sync.` |
| `mithrilPartialSyncButtonLabel` | rename | `Mithril Sync` |
| `mithrilPartialSyncConfirmationBehind` | reword | `Your node is about {epochs} epochs behind the blockchain tip. Mithril Sync can restore verified chain data to help it catch up faster than waiting for standard sync.` |
| `mithrilPartialSyncConfirmationBehindSyncContext` | **delete** | — (remove key from all catalogs + component) |
| `mithrilPartialSyncConfirmationConfirm` | reword | `Start Mithril Sync` |
| `mithrilPartialSyncConfirmationStepRestart` | reword | `Daedalus restarts Cardano node automatically and standard syncing resumes.` |
| `mithrilPartialSyncConfirmationSuccess` | reword | `If Mithril Sync succeeds, Daedalus will restart Cardano node automatically and standard syncing will resume.` |
| `mithrilPartialSyncConfirmationTitle` | reword | `Before Mithril Sync begins` |
| `mithrilPartialSyncRecommendation` | reword | `If Cardano node catch-up is taking longer than you want, Mithril Sync can restore verified chain data to help it catch up faster.` |
| `mithrilPartialSyncRecommendationWithProgress` | **delete** | — (collapse to base recommendation; see component edits) |
| `mithrilPartialSyncSectionLabel` | rename | `Mithril Sync` |
| `loading.mithrilPartialSync.completed.subtitle` | reword | `Mithril Sync completed successfully. Continue to return to the normal Daedalus app flow.` |
| `loading.mithrilPartialSync.error.cancelled.hint` | reword (cap. harmonize) | `Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.` |
| `loading.mithrilPartialSync.error.cancelled.title` | reword | `Mithril Sync was cancelled` |
| `loading.mithrilPartialSync.error.failed.hint` | reword | `Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril sync.` |
| `loading.mithrilPartialSync.error.failed.title` | reword | `Mithril Sync failed` |
| `loading.mithrilPartialSync.error.latestDrift.hint` | reword (cap. harmonize) | `A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed.` |
| `loading.mithrilPartialSync.error.retry` | reword | `Retry Mithril Sync` |
| `loading.mithrilPartialSync.progress.nodeStartingDetail` | reword | `Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume standard syncing.` |
| `loading.mithrilPartialSync.title` | reword | `Mithril Sync` |

Verified-data wording (lock #4) is preserved verbatim in Behind + Recommendation — only the name token
and the "normal→standard" contrast token change.

### `source/renderer/app/i18n/locales/ja-JP.json` (same keys, first-class JA)
| Key (id suffix) | Action | New JA value |
|---|---|---|
| `mithrilPartialSyncButtonHintReady` | reword | `DaedalusがMithril同期を開始する前に、実行内容を確認してください。` |
| `mithrilPartialSyncButtonLabel` | rename | `Mithril同期` |
| `mithrilPartialSyncConfirmationBehind` | reword | `ノードはブロックチェーンの先端より約{epochs}エポック遅れています。Mithril同期で検証済みのチェーンデータを復元すれば、標準同期を待つよりも速く追いつけます。` |
| `mithrilPartialSyncConfirmationBehindSyncContext` | **delete** | — |
| `mithrilPartialSyncConfirmationConfirm` | reword | `Mithril同期を開始` |
| `mithrilPartialSyncConfirmationRecovery` | reword (JA-only embedded-EN fix) | `ここで選択する必要はありません。同期に失敗した場合は、進行状況画面で利用可能な復旧オプション（再試行、現在のデータでの通常再開、またはチェーンデータを消去して完全なMithril同期を実行など）が表示されます。` |
| `mithrilPartialSyncConfirmationStepRestart` | reword | `DaedalusはCardanoノードを自動的に再起動し、標準同期が再開します。` |
| `mithrilPartialSyncConfirmationSuccess` | reword | `Mithril同期が成功すると、DaedalusはCardanoノードを自動的に再起動し、標準同期を再開します。` |
| `mithrilPartialSyncConfirmationTitle` | reword | `Mithril同期を始める前に` |
| `mithrilPartialSyncRecommendation` | reword | `Cardanoノードの追いつきに時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元し、より速く追いつけます。` |
| `mithrilPartialSyncRecommendationWithProgress` | **delete** | — |
| `mithrilPartialSyncSectionLabel` | rename | `Mithril同期` |
| `loading.mithrilPartialSync.completed.subtitle` | reword | `Mithril同期が正常に完了しました。続行すると通常のDaedalus画面に戻ります。` |
| `loading.mithrilPartialSync.error.cancelled.title` | reword | `Mithril同期はキャンセルされました` |
| `loading.mithrilPartialSync.error.failed.hint` | reword | `利用可能な復旧操作を使って、Mithril同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。` |
| `loading.mithrilPartialSync.error.failed.title` | reword | `Mithril同期に失敗しました` |
| `loading.mithrilPartialSync.error.retry` | reword | `Mithril同期を再試行` |
| `loading.mithrilPartialSync.progress.nodeStartingDetail` | reword | `Mithril同期によるチェーンデータの復元が完了しました。Daedalusが標準同期を再開できるよう、Cardanoノードを起動しています。` |
| `loading.mithrilPartialSync.title` | reword | `Mithril同期` |

Proactive prompt block (174-182, both locales) already epochs-only + uses Mithril / standard sync /
標準同期 — verified clean, NO change. Overlay error keys conversionFailed/downloadFailed/latestDrift/
noCertifiedRange/stagedDbInvalid already say "Mithril sync"/"Mithril snapshot"/Mithril同期 — clean.

## recommendationWithProgress decision
`MithrilPartialSyncRecommendation.tsx` has NO `{epochs}` prop (only `formattedSyncPercentage,
isActionBlocked, isSynced, onShowConfirmation`). Per task rule "if epochs not available, drop the
WithProgress variant and fall back to the base recommendation." **Decision: COLLAPSE to base** — delete
the `recommendationWithProgress` message + key, always render `messages.recommendation` (already
epochs-agnostic and carrying the lock-#4 verified-data wording). This removes the only recommendation
sync-% per D-A and is the smallest truthful change.

## Component edits

### `MithrilPartialSyncConfirmation.tsx`
1. Delete the `behindSyncContext` entry from `defineMessages` (lines 34-40).
2. Delete the render `<p className={styles.mithrilPartialSyncConfirmationBehindSyncContext}>...</p>` block (lines 148-152).
3. Remove `formattedSyncPercentage: string;` from `Props` (line 93) and from the render destructure (line 108) — now dead.
4. Update remaining `defaultMessage` source strings (keep the `!!!` prefix) so the regenerated
   defaultMessages.json mirrors en-US: `title` → `!!!Before Mithril Sync begins`; `success` →
   `!!!If Mithril Sync succeeds, Daedalus will restart Cardano node automatically and standard syncing
   will resume.`; `behind` → `!!!Your node is about {epochs} epochs behind the blockchain tip. Mithril
   Sync can restore verified chain data to help it catch up faster than waiting for standard sync.`;
   `confirm` → `!!!Start Mithril Sync`; `stepRestart` → `!!!Daedalus restarts Cardano node
   automatically and standard syncing resumes.`

### `MithrilPartialSyncRecommendation.tsx`
1. Delete `recommendationWithProgress` from `defineMessages` (lines 15-21).
2. Render: drop the `isSynced ? recommendation : recommendationWithProgress` select; always
   `intl.formatMessage(messages.recommendation)` (no `{syncPercentage}` arg).
3. Remove `formattedSyncPercentage` and `isSynced` from `Props` + destructure (now dead).
4. Update `defaultMessage` source strings (keep `!!!`): `recommendation` → `!!!If Cardano node catch-up
   is taking longer than you want, Mithril Sync can restore verified chain data to help it catch up
   faster.`; `buttonLabel` → `!!!Mithril Sync`; `buttonHintReady` → `!!!Review what will happen before
   Daedalus starts Mithril Sync.`

### `MithrilPartialSyncSection.tsx`
1. Remove `formattedSyncPercentage` (Props 20) and `isSynced` (Props 23) — both now dead after children drop them.
2. **Also remove both from the render destructure (~lines 117/119)** so tsc/lint do not flag unused vars.
3. Stop threading them to `<MithrilPartialSyncConfirmation>` (drop line 131) and
   `<MithrilPartialSyncRecommendation>` (drop lines 149 + 151).
4. Update `sectionLabel` `defaultMessage` → `!!!Mithril Sync`.

### `loading/mithril-bootstrap/MithrilBootstrap.messages.ts` — overlay source-of-truth `defaultMessage`
This file holds the `defineMessages` source for the ENTIRE overlay namespace; `i18n:extract` reads it
to regenerate `defaultMessages.json` + `translations/messages.json`. If left untouched it reintroduces
stale `!!!Mithril partial sync` into the source catalogs. Update these `defaultMessage` VALUES to mirror
the new EN (keep the `!!!` prefix). The `description:` fields stay as engineering metadata — "partial
sync" there is the internal name (vocabulary rule) and is acceptable; the grep gate (below) scopes to
`defaultMessage` lines so it does not flag descriptions.
- `partialSyncTitle` (282) → `!!!Mithril Sync`
- `partialSyncNodeStartingDetail` (302) → `!!!Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume standard syncing.`
- `partialSyncCompletedSubtitle` (322) → `!!!Mithril Sync completed successfully. Continue to return to the normal Daedalus app flow.`
- `partialSyncFailedTitle` (334) → `!!!Mithril Sync failed`
- `partialSyncFailedHint` (340) → `!!!Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril sync.`
- `partialSyncCancelledTitle` (345) → `!!!Mithril Sync was cancelled`
- `partialSyncCancelledHint` (351) → `!!!Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.` (cap. harmonize)
- `partialSyncErrorLatestDriftHint` (376) → `!!!A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed.` (cap. harmonize)
- `partialSyncRetry` (417) → `!!!Retry Mithril Sync`

The diagnostics components' `defaultMessage` strings are updated the same way (see Confirmation /
Recommendation / Section edits above). Decision: update ALL `defaultMessage` values (diagnostics +
overlay) to mirror new EN, keeping source-of-truth truthful and the regenerated catalogs clean.

### `DaedalusDiagnostics.tsx`
1. Remove `formattedSyncPercentage={formattedSyncPercentage}` (line 722) and `isSynced={isSynced}`
   (line 725) from the `<MithrilPartialSyncSection>` invocation.
2. KEEP `formattedSyncPercentage` computation (line 569) + table row (line 719) and `isSynced` table
   row (line 718) — different code path (D-A safe).

### `DaedalusDiagnostics.scss`
1. Delete the `.mithrilPartialSyncConfirmationBehindSyncContext { ... }` rule (lines 400-404).
2. Change `.mithrilPartialSyncConfirmationBehind` `margin: 0 0 4px` → `margin: 0 0 16px` (it previously
   relied on the deleted sync-context line for spacing before the steps `<ol>`).
3. Regenerate the `.scss.d.ts` (see verification) so `tsc` stops exporting the removed class.

### Specs (must update — they assert removed/renamed behaviour; ALL THREE will fail otherwise)
These specs render through an `IntlProvider` loaded with the real **en-US.json**, so they assert the
runtime English copy and will break the moment en-US changes. Every old-copy assertion below must move
to the new EN.

- `MithrilPartialSyncConfirmation.spec.tsx`:
  - Heading assertions `name: 'Before Mithril partial sync begins'` (line 33) → `'Before Mithril Sync begins'`.
  - Confirm-button assertions `name: 'Start Mithril partial sync'` (lines 98, 122, 133) → `'Start Mithril Sync'`.
  - "renders the epochs behind-ness line plus the compact sync-% reference" test (line 55+): behind text
    (line 58) → new EN ("...Mithril Sync... waiting for standard sync."); DROP the `getByText('(62.50% synced)')`
    (line 60) + any compareDocumentPosition ordering assertion; KEEP the `queryByText(/immutable/i)` null check.
  - "falls back to generic behind-ness copy ... but still shows sync-%" test: DROP the `(62.50% synced)`
    assertion (line 82). Remove `formattedSyncPercentage: '62.50'` fixture keys (lines 13, 55, 76).
  - `startError` fixture string (line 143) is arbitrary passthrough test data, not i18n — fine to leave.
- `MithrilPartialSyncSection.spec.tsx`:
  - Remove `formattedSyncPercentage: '62.50'` (line 24) and `isSynced: false` (line 27) from default props.
  - Button-name assertions `name: 'Mithril Partial Sync'` (lines 49, 62, 76, 98, 135, 161, 188, 245) →
    `'Mithril Sync'`; the `not.toMatch(/Mithril Partial Sync/)` (line 190) → `/Mithril Sync/`.
  - WithProgress copy assertions "Cardano node is currently 62.50% synced..." (lines 67, 179, 259) →
    the base recommendation EN ("If Cardano node catch-up is taking longer than you want, Mithril Sync
    can restore verified chain data to help it catch up faster.").
  - Confirm-button `name: 'Start Mithril partial sync'` (lines 77, 100, 136) → `'Start Mithril Sync'`.
  - Heading `name: 'Before Mithril partial sync begins'` (lines 53, 164, 202, 216, 226, 248) → `'Before Mithril Sync begins'`.
  - `(62.50% synced)` assertion (line 239) → DROP. Behind line (line 236) → new EN.
- `DaedalusDiagnostics.spec.tsx` (**omitted by prior plan — also asserts old copy**):
  - Default props keep `isSynced`/`syncPercentage` (lines 43/44) — these feed the diagnostics TABLE row,
    which is unchanged; leave them.
  - WithProgress recommendation copy (line 81) → base recommendation EN; the `isSynced: true` case (line 96+)
    already expects the base recommendation — update only the name token in its expected string (line 100).
  - buttonHintReady copy (line 86) → "Review what will happen before Daedalus starts Mithril Sync."
  - Button-name assertions `'Mithril Partial Sync'` (lines 90, 112, 123, 131, 143, 157, 166, 176, 191, 203, 216)
    → `'Mithril Sync'`; the `queryByText(/Mithril Partial Sync/)` (lines 133, 159) → `/Mithril Sync/`.
  - Heading `name: 'Before Mithril partial sync begins'` (line 180) → `'Before Mithril Sync begins'`.
  - Behind-line copy (lines 195, 220) → new EN ("...Mithril Sync... waiting for standard sync.").
- `loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (**also asserts changed overlay copy**):
  - Heading role query `name: /mithril partial sync/i` (line 69) → `/mithril sync/i`.
  - Retry-button role queries `name: /retry mithril partial sync/i` (lines 89, 151, 200) → `/retry mithril sync/i`.
  - The "...without placeholder markers" no-`!!!` test (line 261+) still passes unchanged. The `describe`/`it`
    title strings ("Mithril partial sync ...") are dev-only — optional tidy, default leave.

### Storybook (`storybook/stories/nodes/status/Diagnostics.stories.tsx`)
- `recommendationBaseProps`: remove `formattedSyncPercentage` + `isSynced`. Remove the 'Synced'
  recommendation story (variant no longer exists). `confirmationBaseProps`: remove
  `formattedSyncPercentage`. New copy flows automatically; verify no `!!!`.
- `MithrilPartialSyncOverlay.stories.tsx`: line 106 mock string "...retry partial sync..." → "...retry
  Mithril Sync..." (rendered preview). Story group titles are dev-only; reviewer call (default leave).

### Generated catalogs — DO NOT hand-edit
After component + en-US/ja-JP edits, run `yarn i18n:manage` to regenerate
`source/renderer/app/i18n/locales/defaultMessages.json` + `translations/messages.json`. The deleted ids
(`...BehindSyncContext`, `...RecommendationWithProgress`) must disappear from both.

## Verification plan (from repo root)
1. `node_modules/.bin/typed-scss-modules source/renderer/app/components/status/DaedalusDiagnostics.scss`
   — regen `.scss.d.ts` FIRST (the .scss changed), else tsc fails on the stale removed class.
2. `yarn i18n:manage`  — THE gate (extract + check): 4 catalogs consistent, NO `!!!`, deleted ids gone.
3. `yarn compile`  — `tsc --noEmit` (passes only after step 1).
4. `yarn lint`
5. `yarn stylelint`
6. Focused jest (env-aware) on ALL FOUR updated specs via the gitignored identity-obj-proxy scss sidecar
   `--config` (env quirk, not a regression): `MithrilPartialSyncConfirmation.spec.tsx`,
   `MithrilPartialSyncSection.spec.tsx`, `DaedalusDiagnostics.spec.tsx`,
   `loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`.
7. `yarn storybook:build`  — heavy/best-effort; verify the removed BehindSyncContext `<p>`/story doesn't
   break an import and no `!!!` renders.
8. Final grep gate (extended to the source-of-truth catalogs, not just the two runtime locales):
   - Runtime locales (pure value catalogs, no description fields) MUST be empty:
     `grep -rn "partial sync\|Partial Sync\|部分同期" source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json`
   - Source-of-truth catalogs — scope to user-facing VALUE lines (`defaultMessage`/value), since
     `description:` fields legitimately retain "partial sync" as the internal name:
     `grep -rn "partial sync\|Partial Sync\|部分同期" source/renderer/app/i18n/locales/defaultMessages.json translations/messages.json | grep -iv '"description"'` → MUST be empty.
   - Component source `defaultMessage` strings: `grep -rn "Mithril partial sync\|Mithril Partial Sync\|部分同期" source/renderer/app/components/status/*.tsx source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts | grep -i "defaultmessage"` → MUST be empty. (Spec files and `description:`/comment lines may still mention the internal name — out of scope.)
   - JA embedded-English check (catches the 165-style stray): `grep -rn "完全なMithril sync\|Mithril partial\|partial sync" source/renderer/app/i18n/locales/ja-JP.json` → MUST be empty.

## Glossary (EN+JA)
| Term (EN) | Definition (EN) | Term (JA) | Definition (JA) |
|---|---|---|---|
| partial sync | Internal/engineering name for the diagnostics-launched Mithril recovery flow (stop node → download/verify a bounded range → restore → cut over → restart). **Never user-facing** — user-facing name is "Mithril Sync". | 部分同期 | 診断画面から起動するMithril復旧フローの内部・技術用語。**ユーザー向けには使用しない**（ユーザー向け名称は「Mithril同期」）。 |
| verified data | Mithril-certified chain data restored to help the node catch up faster; the promise in the Behind/Recommendation copy (lock #4 — wording fixed). | 検証済みデータ | ノードがより速く追いつけるよう復元される、Mithrilで検証済みのチェーンデータ（ロック#4で文言固定）。 |
| snapshot | A bounded, verified Mithril artifact downloaded and validated before cutover ("the verified Mithril snapshot"). | スナップショット | カットオーバー前にダウンロード・検証される、範囲限定のMithril検証済みアーティファクト（「検証済みMithrilスナップショット」）。 |
| cutover | The install step where the validated staged snapshot replaces the live chain DB; after it, cancel is rejected and the only recovery is wipe-and-full-sync. Not surfaced verbatim in copy. | カットオーバー | 検証済みのステージング済みスナップショットが稼働中のチェーンDBを置き換えるインストール工程。これ以降はキャンセル不可で、復旧手段はチェーンデータ削除＋完全同期のみ。コピーには直接の語としては出さない。 |
| epochs behind | The user-facing measure of how far the node trails the blockchain tip — expressed in **epochs only** ({epochs}), never sync-%, immutable files, or slots. | エポック遅れ | ノードがブロックチェーンの先端からどれだけ遅れているかを示すユーザー向け指標。**エポック数のみ**（{epochs}）で表現し、同期率・イミュータブルファイル・スロットは使わない。 |
| sync percentage | Raw `{syncPercentage}` figure. Kept ONLY in the diagnostics table row (`getRow('syncPercentage')`). Removed from the confirmation behind-ness and recommendation per D-A. | 同期率 | 生の`{syncPercentage}`値。診断テーブルの行のみに残す。D-Aにより確認モーダルの遅れ表示と推奨文からは削除。 |

Additional canonical terms used above: **Mithril Sync** = ユーザー向け機能名「Mithril同期」;
**standard sync** = 標準同期 (the node's regular sync method, the contrast to Mithril Sync);
**full Mithril sync** = 完全なMithril同期 (bootstrap full sync — distinct, unchanged).
