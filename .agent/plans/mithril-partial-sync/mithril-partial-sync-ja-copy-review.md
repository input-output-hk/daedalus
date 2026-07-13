# Mithril Sync — Japanese Copy Review (EN → JA)

**Purpose:** Manual translator validation of all new and updated user-facing copy introduced by the Mithril partial sync feature.

**Scope:**
- Branch `feat/mithril-partial-sync-ux-refinement` (through commit `634f6e04c`)
- Merged PR #3333 `mithril_partial_sync` (merge commit `7405cc06`)

**Source files:** `source/renderer/app/i18n/locales/en-US.json` / `ja-JP.json` (keys listed per entry).

**Date generated:** 2026-07-02

**Totals:** 55 strings to validate (36 new on this branch · 5 new from PR #3333 · 13 introduced by PR #3333 then reworded on this branch · 1 updated pre-existing string), plus 6 informational strings (§9) and 7 removed strings (§10) that need no validation.

---

## How to read this document

Each entry shows the **current English** and **current Japanese** copy. For strings that were reworded during this work, the previous wording is shown so you can see what changed. Statuses:

| Status | Meaning |
| --- | --- |
| New (this branch) | First introduced by the UX-refinement branch — needs full review |
| New (PR #3333) | Introduced by the merged PR, unchanged since — needs full review |
| Revised | Introduced by PR #3333, reworded on this branch — review the current wording |
| Updated | Existed before this feature; wording changed — review the change |

**Placeholders** such as `{epochs}`, `{totalSize}`, `{snapshotDownloaded}` are replaced with live values at runtime and must remain **exactly as-is** in the Japanese text.

### Terminology decisions already made (validate against these)

- The user-facing feature name is **"Mithril Sync"** → **Mithril同期**. The terms *"partial sync"* / *Mithril部分同期* were deliberately removed from all user-facing copy.
- The fallback method is **"standard sync"** → **標準同期**.
- How far behind the node is, is expressed in **epochs** (エポック) only — sync percentages were deliberately dropped.

### ⚠ Points needing translator attention

1. **§1.5** — The proactive prompt's primary button keeps the English name in Japanese: 「Mithril Sync（高速）」, while every other occurrence uses 「Mithril同期」 (e.g. §1.1, §3.2, §4.5). Please confirm whether this should be 「Mithril同期（高速）」 for consistency.
2. **§1.8** — "Daedalus Diagnostics" is left in English inside the Japanese sentence. Please confirm it matches the actual name of that screen/menu item in the Japanese UI (opened via the Help menu / Ctrl+D).
3. **Ellipsis style** — In-progress titles use ASCII three dots (e.g. 「Cardanoノードを停止しています...」, §5.3/§5.5/§5.7). If the Japanese convention 「…」 is preferred, flag all of them together.
4. **Parentheses** — Speed qualifiers use full-width parentheses in Japanese: （高速）／（低速） (§1.5, §1.6, §7.15, §7.16). Confirm this is the preferred style.

---

## 1. Syncing screen — proactive Mithril Sync prompt

Shown on the loading/syncing screen when the node is detected to be behind. Offers the choice between Mithril Sync and standard sync, with an inline confirm step. All strings are **New (this branch)**.

#### 1.1 Prompt title
- **EN:** Mithril Sync
- **JA:** Mithril同期
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptTitle`

#### 1.2 Body, line 1 — behind by N epochs
- **EN:** Your node is about {epochs} epochs behind.
- **JA:** ノードは約{epochs}エポック遅れています。
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptBody`
- `{epochs}` is a number, e.g. "約12エポック".

#### 1.3 Body, line 1 — behind-ness unknown (fallback)
- **EN:** Your node is behind the blockchain tip.
- **JA:** ノードはブロックチェーンの先端より遅れています。
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown`
- Shown instead of 1.2 when the epochs figure is unavailable.

#### 1.4 Body, line 2 — benefit
- **EN:** Mithril can catch you up faster than the standard sync.
- **JA:** Mithrilを使えば、標準同期よりも速く追いつけます。
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptBodyBenefit`

#### 1.5 Primary button ⚠ (see attention point 1)
- **EN:** Mithril Sync (fast)
- **JA:** Mithril Sync（高速）
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton`

#### 1.6 Secondary button
- **EN:** Standard Sync (slow)
- **JA:** 標準同期（低速）
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptStandardButton`

#### 1.7 Note label
- **EN:** Note:
- **JA:** 注:
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNoteLabel`

#### 1.8 Handoff note ⚠ (see attention point 2)
- **EN:** If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)
- **JA:** スキップした場合でも、ヘルプメニューにあるDaedalus Diagnostics画面からMithril同期を開始できます。(Ctrl + D)
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote`

#### 1.9 Confirm step — title
- **EN:** Mithril Sync Process
- **JA:** Mithril同期の処理
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmTitle`

#### 1.10 Confirm step — start button
- **EN:** Start now
- **JA:** 今すぐ開始
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmStart`

#### 1.11 Confirm step — cancel button
- **EN:** Cancel
- **JA:** キャンセル
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmCancel`

---

## 2. Shared process summary

One canonical sentence explaining what the Mithril Sync process does. Appears in **both** the proactive prompt confirm step (§1) and the Diagnostics confirmation modal (§4).

#### 2.1 Process summary — New (this branch)
- **EN:** For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.
- **JA:** この処理を開始するには、お使いのCardanoノードをシャットダウンする必要があります。その後、Mithrilを使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。
- Key: `daedalus.diagnostics.dialog.mithrilSyncProcessSummary`

---

## 3. Daedalus Diagnostics — Mithril Sync row

The Diagnostics screen (Help menu / Ctrl+D) gained a "Mithril Sync" row with a button that opens the confirmation modal (§4).

#### 3.1 Row label — New (this branch)
- **EN:** Mithril Sync
- **JA:** Mithril同期
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel`

#### 3.2 Button label — Revised
- **EN:** Mithril Sync
- **JA:** Mithril同期
- Previous EN: Mithril Partial Sync
- Previous JA: Mithril Partial Sync
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncButtonLabel`

#### 3.3 Button tooltip — Revised
- **EN:** If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync.
- **JA:** Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、同期を高速化できます。
- Previous EN: If Cardano node catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.
- Previous JA: Cardanoノードの追いつきに時間がかかりすぎると感じる場合は、Mithril partial syncで検証済みのチェーンデータを復元し、より速く追いつけるようにできます。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation`

#### 3.4 Hint under disabled button — New (PR #3333)
- **EN:** Unavailable while Mithril work is already active.
- **JA:** Mithrilの処理がすでに進行中のため利用できません。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncButtonHintBlocked`
- Shown when the button is disabled because a Mithril operation is already running.

---

## 4. Mithril Sync confirmation modal (from Diagnostics)

Opens when the user clicks the Mithril Sync button in Diagnostics (§3.2). Also contains the shared process summary (§2.1).

#### 4.1 Modal title — Revised
- **EN:** Before Mithril Sync begins
- **JA:** Mithril同期を始める前に
- Previous EN: Before Mithril partial sync begins
- Previous JA: Mithril partial syncを始める前に
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationTitle`

#### 4.2 Behind-ness line — New (this branch)
- **EN:** Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.
- **JA:** ノードは約{epochs}エポック遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind`
- `{epochs}` is a number.

#### 4.3 Behind-ness line, fallback — New (this branch)
- **EN:** Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster.
- **JA:** ノードは最新の検証済みスナップショットより遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown`
- Shown instead of 4.2 when the epochs figure is unavailable.

#### 4.4 Confirm button — Revised
- **EN:** Start Mithril Sync
- **JA:** Mithril同期を開始
- Previous EN: Start Mithril partial sync
- Previous JA: Mithril partial syncを開始
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationConfirm`

#### 4.5 Cancel button — New (PR #3333)
- **EN:** Back to diagnostics
- **JA:** 診断に戻る
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel`

---

## 5. Mithril Sync overlay — progress states

Full-screen overlay shown while Mithril Sync runs (stop node → download/restore → restart node).

#### 5.1 Overlay headline — Revised
- **EN:** Mithril Sync
- **JA:** Mithril同期
- Previous EN: Mithril partial sync
- Previous JA: Mithril部分同期
- Key: `loading.mithrilPartialSync.title`

#### 5.2 Progress subtitle — New (PR #3333)
- **EN:** Daedalus is restoring verified Mithril chain data. Download and verification time can vary based on your network connection and storage performance.
- **JA:** Daedalusは検証済みのMithrilチェーンデータを復元しています。ダウンロードと検証にかかる時間は、ネットワーク接続とストレージ性能によって変わります。
- Key: `loading.mithrilPartialSync.progress.subtitle`

#### 5.3 Stopping node — title — New (this branch) ⚠ (ellipsis style)
- **EN:** Stopping Cardano node...
- **JA:** Cardanoノードを停止しています...
- Key: `loading.mithrilPartialSync.progress.nodeStoppingTitle`

#### 5.4 Stopping node — detail — New (this branch)
- **EN:** Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes.
- **JA:** Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。
- Key: `loading.mithrilPartialSync.progress.nodeStoppingDetail`

#### 5.5 Starting node — title — New (PR #3333) ⚠ (ellipsis style)
- **EN:** Starting Cardano node...
- **JA:** Cardanoノードを起動しています...
- Key: `loading.mithrilPartialSync.progress.nodeStartingTitle`

#### 5.6 Starting node — detail — Revised
- **EN:** Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume standard syncing.
- **JA:** Mithril同期によるチェーンデータの復元が完了しました。Daedalusが標準同期を再開できるよう、Cardanoノードを起動しています。
- Previous EN: Mithril partial sync has finished restoring chain data. Cardano node is starting so Daedalus can resume normal syncing.
- Previous JA: Mithril部分同期によるチェーンデータの復元が完了しました。Daedalusが通常の同期を再開できるよう、Cardanoノードを起動しています。
- Key: `loading.mithrilPartialSync.progress.nodeStartingDetail`

#### 5.7 Cancelling — title — New (this branch) ⚠ (ellipsis style)
- **EN:** Cleaning up...
- **JA:** 後処理しています...
- Key: `loading.mithrilPartialSync.progress.cancellingTitle`

#### 5.8 Cancelling — detail — New (this branch)
- **EN:** Daedalus is cleaning up Mithril Sync before you continue.
- **JA:** 続行する前に、DaedalusがMithril同期の後処理をしています。
- Key: `loading.mithrilPartialSync.progress.cancellingDetail`

#### 5.9 Disabled-Cancel tooltip — New (this branch)
- **EN:** Cancellation available once the node has stopped
- **JA:** ノードが停止した後にキャンセルできます
- Key: `loading.mithrilPartialSync.progress.cancelStoppingTooltip`
- Tooltip on the Cancel button while it is disabled (node still stopping).

---

## 6. Mithril Sync overlay — completion

#### 6.1 Success subtitle — Revised
- **EN:** Mithril Sync completed successfully.
- **JA:** Mithril同期が正常に完了しました。
- Previous EN: Mithril partial sync completed successfully. Continue to return to the normal Daedalus app flow.
- Previous JA: Mithril部分同期が正常に完了しました。続行すると通常のDaedalus画面に戻ります。
- Key: `loading.mithrilPartialSync.completed.subtitle`
- The second sentence was dropped because the overlay now returns to Daedalus automatically (see 6.2).

#### 6.2 Auto-handoff caption — New (this branch)
- **EN:** Returning to Daedalus...
- **JA:** Daedalusに戻っています...
- Key: `loading.mithrilPartialSync.completed.transition`
- Spinner caption while the overlay hands back to the normal loading flow.

---

## 7. Mithril Sync overlay — errors and recovery

Each failure type has a title + hint pair, followed by shared recovery buttons (7.16–7.19).

#### 7.1 Generic failure — title — Revised
- **EN:** Mithril Sync failed
- **JA:** Mithril同期に失敗しました
- Previous EN: Mithril partial sync failed
- Previous JA: Mithril部分同期に失敗しました
- Key: `loading.mithrilPartialSync.error.failed.title`

#### 7.2 Generic failure — hint — Revised
- **EN:** Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril sync.
- **JA:** 利用可能な復旧操作を使って、Mithril同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。
- Previous EN: Use one of the available recovery actions to retry Mithril partial sync, restart normally, or wipe chain data and do a full Mithril sync.
- Previous JA: 利用可能な復旧操作を使って、Mithril部分同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。
- Key: `loading.mithrilPartialSync.error.failed.hint`

#### 7.3 Cancelled by user — title — Revised
- **EN:** Mithril Sync was cancelled
- **JA:** Mithril同期はキャンセルされました
- Previous EN: Mithril partial sync was cancelled
- Previous JA: Mithril部分同期はキャンセルされました
- Key: `loading.mithrilPartialSync.error.cancelled.title`

#### 7.4 Cancelled by user — hint — Revised
- **EN:** Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.
- **JA:** Mithril同期は完了前に停止されました。既存のチェーンデータは変更されていません。下のオプションから続行方法を選択してください。
- Previous EN: Use one of the available recovery actions to retry Mithril partial sync, restart normally, or wipe chain data and do a full Mithril sync.
- Previous JA: 利用可能な復旧操作を使って、Mithril部分同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。
- Key: `loading.mithrilPartialSync.error.cancelled.hint`
- Deliberately calmer than the failure hint — cancelling is not an error.

#### 7.5 No verified snapshot available — title — New (this branch)
- **EN:** No verified Mithril snapshot is available yet
- **JA:** 利用可能な検証済みMithrilスナップショットがまだありません
- Key: `loading.mithrilPartialSync.error.noCertifiedRange.title`

#### 7.6 No verified snapshot available — hint — New (this branch)
- **EN:** Daedalus could not find a verified Mithril snapshot for your current chain position. Choose how to continue below — you can keep syncing on your existing chain data.
- **JA:** 現在のチェーン位置に対応する検証済みMithrilスナップショットが見つかりませんでした。下のオプションから続行方法を選択してください。既存のチェーンデータで同期を続けることもできます。
- Key: `loading.mithrilPartialSync.error.noCertifiedRange.hint`

#### 7.7 Snapshot moved on — title — New (this branch)
- **EN:** The verified Mithril snapshot moved on
- **JA:** 検証済みMithrilスナップショットが更新されました
- Key: `loading.mithrilPartialSync.error.latestDrift.title`

#### 7.8 Snapshot moved on — hint — New (this branch)
- **EN:** A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed.
- **JA:** 準備中に新しい検証済みスナップショットが利用可能になりました。Mithril同期を再試行して更新されたスナップショットを使用してください。チェーンデータは変更されていません。
- Key: `loading.mithrilPartialSync.error.latestDrift.hint`

#### 7.9 Snapshot could not be verified — title — New (this branch)
- **EN:** The Mithril snapshot could not be verified
- **JA:** Mithrilスナップショットを検証できませんでした
- Key: `loading.mithrilPartialSync.error.stagedDbInvalid.title`

#### 7.10 Snapshot could not be verified — hint — New (this branch)
- **EN:** The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below.
- **JA:** ダウンロードしたスナップショットが不完全か、期待される検証済みチェーンデータと一致しませんでした。下のオプションから続行方法を選択してください。
- Key: `loading.mithrilPartialSync.error.stagedDbInvalid.hint`

#### 7.11 Download failed — title — New (this branch)
- **EN:** Downloading the Mithril snapshot failed
- **JA:** Mithrilスナップショットのダウンロードに失敗しました
- Key: `loading.mithrilPartialSync.error.downloadFailed.title`

#### 7.12 Download failed — hint — New (this branch)
- **EN:** Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below.
- **JA:** Mithrilスナップショットのダウンロードと検証を完了できませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。
- Key: `loading.mithrilPartialSync.error.downloadFailed.hint`

#### 7.13 Snapshot preparation failed — title — New (this branch)
- **EN:** Preparing the Mithril snapshot failed
- **JA:** Mithrilスナップショットの準備に失敗しました
- Key: `loading.mithrilPartialSync.error.conversionFailed.title`

#### 7.14 Snapshot preparation failed — hint — New (this branch)
- **EN:** Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below.
- **JA:** 検証済みスナップショットをダウンロードしましたが、使用するための準備ができませんでした。下のオプションから続行方法を選択してください。
- Key: `loading.mithrilPartialSync.error.conversionFailed.hint`

#### 7.15 Recovery button — retry — Revised
- **EN:** Retry Mithril Sync (fast)
- **JA:** Mithril同期を再試行（高速）
- Previous EN: Retry Mithril partial sync
- Previous JA: Mithril部分同期を再試行
- Key: `loading.mithrilPartialSync.error.retry`

#### 7.16 Recovery button — restart normally — Revised
- **EN:** Restart Node Sync (slow)
- **JA:** ノード同期を再起動（低速）
- Previous EN: Restart normally
- Previous JA: 通常どおり再起動
- Key: `loading.mithrilPartialSync.error.restartNormally`

#### 7.17 Recovery button — wipe and full sync — New (PR #3333)
- **EN:** Wipe chain data and do full Mithril sync
- **JA:** チェーンデータを削除して完全なMithril同期を実行
- Key: `loading.mithrilPartialSync.error.wipeAndFullSync`

#### 7.18 Fallback button — quit — New (this branch)
- **EN:** Quit Daedalus
- **JA:** Daedalusを終了
- Key: `loading.mithrilPartialSync.error.quit`
- Only shown if no other recovery action is available, so the overlay is never a dead-end.

---

## 8. First-time Mithril bootstrap screen (existing screen, refined)

The pre-existing "Fast sync with Mithril" first-run screen received these additions/changes.

#### 8.1 Combined download detail — Updated
- **EN:** Snapshot files: {snapshotDownloaded} / {snapshotTotal} files | Fast sync: {fastSyncDownloaded} / {fastSyncTotal}
- **JA:** スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 | 高速同期: {fastSyncDownloaded} / {fastSyncTotal}
- Previous EN: Snapshot files: {snapshotDownloaded} / {snapshotTotal} | Fast sync: {fastSyncDownloaded} / {fastSyncTotal}
- Previous JA: スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} | 高速同期: {fastSyncDownloaded} / {fastSyncTotal}
- Key: `loading.mithrilBootstrap.progress.combinedDetail`
- Change: a unit ("files" / 「個」) was added after the snapshot file count. The fast-sync pair is byte sizes, so it has no unit.

#### 8.2 Total size context — New (this branch)
- **EN:** ≈ {totalSize} total
- **JA:** 約 {totalSize}（合計）
- Key: `loading.mithrilBootstrap.progress.snapshotSizeContext`
- `{totalSize}` is a formatted size such as "42.1 GB".

#### 8.3 Long-phase reassurance — New (this branch)
- **EN:** This can take several minutes — Daedalus is still working.
- **JA:** 数分かかることがあります。Daedalusは引き続き処理しています。
- Key: `loading.mithrilBootstrap.progress.longPhaseReassurance`
- Shown during long verify/unpack/convert phases so the screen never looks frozen.

#### 8.4 Stopping node — title — New (this branch) ⚠ (ellipsis style)
- **EN:** Stopping Cardano node...
- **JA:** Cardanoノードを停止しています...
- Key: `loading.mithrilBootstrap.progress.nodeStoppingTitle`

#### 8.5 Stopping node — detail — New (this branch)
- **EN:** Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes.
- **JA:** Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。
- Key: `loading.mithrilBootstrap.progress.nodeStoppingDetail`

---

## 9. Chain storage location picker — informational only

These six strings ship with the feature but are **identical to what is already merged and translated in `develop`** (they landed via an earlier Mithril PR). Listed for completeness; validate only if they were never reviewed before.

| Key (suffix of `chainStorage.locationPicker.`) | English | Japanese |
| --- | --- | --- |
| `dataFoundNotice` | Existing blockchain data found. Proceeding will reuse this data. | 既存のブロックチェーンデータが見つかりました。続行すると、このデータが再利用されます。 |
| `recoveryNotice` | We couldn't access your previous storage location. A default fallback location has been selected. You can proceed with this or choose a new one. | 以前の保存場所にアクセスできませんでした。既定の代替保存場所が選択されています。このまま続行するか、新しい保存場所を選択してください。 |
| `subdirectoryCreationNotice` | Daedalus will create a chain subdirectory inside the selected parent folder. | Daedalusは選択した親フォルダー内にchainサブディレクトリを作成します。 |
| `subdirectoryWarningExists` | Daedalus will use the existing chain subdirectory inside the selected parent folder. | Daedalusは選択した親フォルダー内にある既存のchainサブディレクトリを使用します。 |
| `subdirectoryErrorConflict` | The selected parent folder already contains a chain entry that is not a directory. | 選択した親フォルダーには、すでにディレクトリではないchainエントリーが存在します。 |
| `validation.isManagedChild` | Select the parent folder, not the existing chain subdirectory managed by Daedalus. | Daedalusが管理している既存のchainサブディレクトリではなく、その親フォルダーを選択してください。 |

---

## 10. Removed strings — no validation needed

These strings were added by PR #3333 but have since been **removed or replaced** on this branch. They no longer appear anywhere in the app. Listed so stale entries can be purged from any translation memory.

| Removed key | English (was) | Japanese (was) | Replaced by |
| --- | --- | --- | --- |
| `…mithrilPartialSyncButtonHint` | Available after the confirmation step is added. | 確認ステップの追加後に利用できます。 | — (transitional placeholder, dropped) |
| `…mithrilPartialSyncButtonHintReady` | Review what will happen before Daedalus starts Mithril partial sync. | DaedalusがMithril partial syncを開始する前に、実行内容を確認してください。 | Tooltip §3.3 |
| `…mithrilPartialSyncConfirmationIntro` | Daedalus will stop Cardano node automatically, then download and restore verified Mithril data. | DaedalusはCardanoノードを自動的に停止し、その後で検証済みのMithrilデータをダウンロードして復元します。 | Process summary §2.1 |
| `…mithrilPartialSyncConfirmationSuccess` | If Mithril partial sync succeeds, Daedalus will restart Cardano node automatically and normal syncing will resume. | Mithril partial syncが成功すると、DaedalusはCardanoノードを自動的に再起動し、通常の同期を再開します。 | Process summary §2.1 |
| `…mithrilPartialSyncConfirmationRecovery` | If the attempt fails, Daedalus can offer retry partial sync, restart normally on the current database, or wipe chain data and do a full Mithril sync. | 試行に失敗した場合、Daedalusはpartial syncの再試行、現在のデータベースでの通常再開、またはチェーンデータを消去して完全なMithril syncを実行する選択肢を提示できます。 | Failure overlay copy §7 |
| `…mithrilPartialSyncRecommendationWithProgress` | Cardano node is currently {syncPercentage}% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster. | Cardanoノードは現在 {syncPercentage}% 同期済みです。追いつきに時間がかかりすぎると感じる場合は、Mithril partial syncで検証済みのチェーンデータを復元し、より速く追いつけるようにできます。 | Epochs-based lines §4.2/§4.3 (sync-% deliberately dropped) |
| `loading.mithrilPartialSync.completed.continue` | Continue to Daedalus | Daedalusに戻る | Automatic handoff caption §6.2 |

(`…` = `daedalus.diagnostics.dialog.` prefix.)
