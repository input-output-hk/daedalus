# Mithril Sync — Japanese Copy Review, Round 2 (delta)

**Purpose:** Translator validation of the strings that are **new, revised, or unconfirmed** since the
round-1 review (`mithril-partial-sync-ja-copy-review.md`, 2026-07-02). Everything round 1 already
approved and that has not changed since is *not* repeated here — the full current-state reference is
`mithril-partial-sync-ja-copy-table.md` (regenerated 2026-07-12).

**Scope:** branch `feat/mithril-partial-sync-ux-refinement` after the task-ux-706
copy-standardization wave (2026-07-12).

**Source files:** `source/renderer/app/i18n/locales/en-US.json` / `ja-JP.json`.

**No screenshots in this package** — text-only by decision; please request visuals for any specific
entry if needed.

---

## How to read this document

| Status | Meaning |
| --- | --- |
| New — first-pass draft | The JA is an internal first-pass draft — needs full review |
| Revised (EN+JA) | Both languages were reworded (usually the vocabulary standardization) — review the JA rendering |
| Revised (JA only) | Only the JA changed — review the change |
| EN-only (FYI) | Only the EN changed (casing); the JA is untouched — no action needed |

**Placeholders** such as `{epochs}`, `{shortcut}`, `{fastSyncDownloaded}` are replaced with live
values at runtime and must remain **exactly as-is** in the Japanese text. `{shortcut}` renders
"Cmd + D" on macOS and "Ctrl + D" on Windows/Linux and stays untranslated.

### Locked terminology (validate the renderings, not the term choices)

The product locked a three-term sync vocabulary (DD-706-1); the *term split* is a product decision,
but the JA renderings are first-pass and yours to confirm or reword:

| Term | JA rendering | Meaning |
| --- | --- | --- |
| Mithril Sync | Mithril同期 | Fast verified-snapshot restore (already the established rendering; the one outlier button was fixed, §B1) |
| Blockchain Sync | ブロックチェーン同期 | Normal block-by-block sync / resume from the node's current tip |
| Blockchain Sync from Genesis | ジェネシスからのブロックチェーン同期 (name form); sentences use ジェネシスからブロックチェーン同期を実行… | Full sync from block 0 (fresh install / after wipe) |

Also locked: only "Mithril" (and product names Daedalus/Cardano) stays Latin in JA; the Diagnostics
page is named 「Daedalus診断」 (matching the existing Help-menu label); the bootstrap progress figure
formerly labeled "Fast sync:" is now "Ledger state:" / 「台帳状態:」 (round-1 question ❓#6 —
please confirm).

Deliberately unchanged (not an inconsistency): the first-run bootstrap screen keeps its feature-title
wording "Fast sync with Mithril" / 「Mithrilによる高速同期」 and accept button "Use Mithril fast
sync" / 「Mithril高速同期を使う」.

### Linked-term groups (please reword together, if at all)

1. **「ブロックチェーン同期」 family** — §A1, §A2, and every entry in §B/§C marked (vocabulary).
2. **「台帳状態」** — the C1 labels (§B7, §B8) and the two at/past-snapshot drafts (§A1, §A2) all
   render "ledger state"; keep them identical.
3. **「データの完全性を保つために…」** — the new caution (§A3) deliberately mirrors the existing
   shutdown string `loading.screen.stoppingCardanoDescription`: 「このプロセスはデータベースを更新
   するもので、数分間かかる場合があります。<br />データの完全性を保つために、このプロセスが完了する
   までお待ちください。」 If you reword the caution's opening, keep the mirrored voice.
4. **「最新ブロック」** — the two keys in §C2/§C3 replaced 先端 with 最新ブロック as the rendering
   of "(blockchain) tip"; keep them consistent.

---

## A. New strings — first-pass drafts (full review needed)

#### A1. Diagnostics tooltip — node at or past the latest snapshot — New — first-pass draft
- **EN (locked):** Your node is at or past the latest Mithril snapshot. Blockchain Sync will finish the remaining blocks on its own. If sync seems slow or runs into verification issues, Mithril Sync can restore a verified ledger state.
- **JA (draft):** ノードは最新のMithrilスナップショットに到達しているか、それを超えています。残りのブロックはブロックチェーン同期が自動的に同期します。同期が遅い場合や検証に問題がある場合は、Mithril同期で検証済みの台帳状態を復元できます。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationAtOrPastSnapshot`
- Context: a new state on the Diagnostics Mithril Sync row — the node needs no snapshot catch-up.
  The copy is deliberately a non-diagnostic *offer* (the button stays enabled); it must not claim
  the node is stuck, nor flatly say Mithril is unneeded.

#### A2. Diagnostics confirmation body — node at or past the latest snapshot — New — first-pass draft
- **EN (locked):** Your node is at or past the latest Mithril snapshot, so Blockchain Sync can finish the remaining blocks on its own. If sync seems slow or runs into verification issues, continuing will restore a verified ledger state at the snapshot position.
- **JA (draft):** ノードは最新のMithrilスナップショットに到達しているか、それを超えているため、残りのブロックはブロックチェーン同期が同期できます。同期が遅い場合や検証に問題がある場合は、続行するとスナップショット位置の検証済み台帳状態が復元されます。
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationAtOrPastSnapshot`
- Context: shown in the confirmation modal after the user clicked the button in the A1 state; the
  final clause describes what continuing does (the user already chose to open this dialog).

#### A3. Moving-stage caution — New — first-pass draft
- **EN (locked):** To preserve data integrity, please don't close Daedalus until this step is complete.
- **JA (draft):** データの完全性を保つために、この手順が完了するまでDaedalusを閉じないでください。
- Key: `loading.mithrilBootstrap.progress.moveCaution`
- Context: secondary-text caution under "Moving snapshot to storage" while that step is active, on
  both the bootstrap and the Mithril Sync overlays. Mirrors the shutdown voice (linked-term group 3).

#### A4. Ledger-replay interrupt button — New — first-pass draft
- **EN:** Use Mithril Sync instead
- **JA (draft):** 代わりにMithril同期を使う
- Key: `loading.screen.mithrilSyncInterrupt`
- Context: button on the loading screen during a slow ledger replay, offering to switch to Mithril
  Sync instead of waiting the replay out.

---

## B. Revised — vocabulary standardization and page naming (status per entry)

#### B1. Proactive prompt — primary button — Revised (JA only) ✔ round-1 flag #1
- **EN:** Mithril Sync (fast) *(unchanged)*
- **JA:** Mithril同期（高速）
- Previous JA: Mithril Sync（高速）
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton`
- This resolves round-1 attention point 1 exactly as you suggested.

#### B2. Proactive prompt — secondary button — Revised (EN+JA)
- **EN:** Blockchain Sync (slow)
- **JA:** ブロックチェーン同期（低速）
- Previous EN: Standard Sync (slow) · Previous JA: 標準同期（低速）
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptStandardButton`

#### B3. Proactive prompt — benefit line — Revised (EN+JA)
- **EN:** Mithril can catch you up faster than Blockchain Sync.
- **JA:** Mithrilを使えば、ブロックチェーン同期よりも速く追いつけます。
- Previous EN: Mithril can catch you up faster than the standard sync. · Previous JA: Mithrilを使えば、標準同期よりも速く追いつけます。
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptBodyBenefit`

#### B4. Proactive prompt — handoff note — Revised (EN+JA) ✔ round-1 flag #2
- **EN:** If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. ({shortcut})
- **JA:** スキップした場合でも、ヘルプメニューにあるDaedalus診断画面からMithril同期を開始できます。({shortcut})
- Previous EN: … under the Help menu. (Ctrl + D) · Previous JA: …ヘルプメニューにあるDaedalus Diagnostics画面から… (Ctrl + D)
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote`
- Two changes: the screen name now matches the JA Help-menu label 「Daedalus診断」 (round-1
  attention point 2), and the hardcoded shortcut became `{shortcut}` (renders "Cmd + D" on macOS,
  "Ctrl + D" elsewhere — must stay as the placeholder in JA).

#### B5. Recovery button — restart normally — Revised (EN+JA)
- **EN:** Restart Blockchain Sync (slow)
- **JA:** ブロックチェーン同期を再起動（低速）
- Previous EN: Restart Node Sync (slow) · Previous JA: ノード同期を再起動（低速）
- Key: `loading.mithrilPartialSync.error.restartNormally`

#### B6. Sync overlay — starting-node detail — Revised (EN+JA)
- **EN:** Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume Blockchain Sync.
- **JA:** Mithril同期によるチェーンデータの復元が完了しました。Daedalusがブロックチェーン同期を再開できるよう、Cardanoノードを起動しています。
- Previous EN: … so Daedalus can resume standard syncing. · Previous JA: …Daedalusが標準同期を再開できるよう…
- Key: `loading.mithrilPartialSync.progress.nodeStartingDetail`

#### B7. Bootstrap — combined progress label — Revised (EN+JA) ❓ round-1 question #6
- **EN:** Snapshot Files and Ledger State
- **JA:** スナップショットファイルと台帳状態
- Previous EN: Snapshot Files and Fast Sync · Previous JA: スナップショットファイルと高速同期
- Key: `loading.mithrilBootstrap.progress.combinedLabel`

#### B8. Bootstrap — combined download detail — Revised (EN+JA) ❓ round-1 question #6
- **EN:** Snapshot files: {snapshotDownloaded} / {snapshotTotal} files | Ledger state: {fastSyncDownloaded} / {fastSyncTotal}
- **JA:** スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 | 台帳状態: {fastSyncDownloaded} / {fastSyncTotal}
- Previous EN: … | Fast sync: … · Previous JA: … | 高速同期: …
- Key: `loading.mithrilBootstrap.progress.combinedDetail`
- Answer to your round-1 units question: the first pair is a *file count* (hence 個), the second
  pair is *byte sizes* of the ledger-state package — the "Fast sync" label was the confusing part
  and is now "Ledger state" / 「台帳状態」. Please confirm the JA label.

#### B9. Bootstrap — decline button — Revised (EN+JA)
- **EN:** Blockchain Sync from Genesis
- **JA:** ジェネシスからのブロックチェーン同期
- Previous EN: Sync from genesis · Previous JA: ジェネシスから同期する
- Key: `loading.mithrilBootstrap.decline`

#### B10. Bootstrap — description — Revised (EN+JA)
- **EN:** Mithril can download a verified snapshot to sync your blockchain data faster. Choose a snapshot and continue, or do a Blockchain Sync from Genesis.
- **JA:** Mithrilは検証済みのスナップショットをダウンロードして、ブロックチェーンデータの同期を高速化できます。スナップショットを選んで続行するか、ジェネシスからブロックチェーン同期を実行してください。
- Previous EN: … continue, or sync from genesis. · Previous JA: …続行するか、ジェネシスから同期してください。
- Key: `loading.mithrilBootstrap.description`

#### B11. Bootstrap — conversion-error hint — Revised (EN+JA)
- **EN:** Daedalus could not prepare the downloaded snapshot for use. Try again or do a Blockchain Sync from Genesis.
- **JA:** ダウンロードしたスナップショットをDaedalusで使用できる状態に準備できませんでした。もう一度試すか、ジェネシスからブロックチェーン同期を実行してください。
- Previous EN: … Try again or sync from genesis. · Previous JA: …もう一度試すか、ジェネシスから同期してください。
- Key: `loading.mithrilBootstrap.error.convert.hint`

#### B12. Bootstrap — verification-error hint — Revised (EN+JA)
- **EN:** Daedalus could not verify the snapshot integrity. Try another snapshot or do a Blockchain Sync from Genesis.
- **JA:** Daedalusはスナップショットの整合性を検証できませんでした。別のスナップショットを試すか、ジェネシスからブロックチェーン同期を実行してください。
- Previous EN: … Try another snapshot or sync from genesis. · Previous JA: …別のスナップショットを試すか、ジェネシスから同期してください。
- Key: `loading.mithrilBootstrap.error.verify.hint`

#### B13. Bootstrap — node-start-failure hint — Revised (EN+JA)
- **EN:** The node could not start with the restored chain data. Wipe the chain and try Mithril Sync again, or do a Blockchain Sync from Genesis.
- **JA:** 復元したチェーンデータではノードを起動できませんでした。チェーンデータを削除してMithril同期をもう一度試すか、ジェネシスからブロックチェーン同期を実行してください。
- Previous EN: … Wipe the chain and try Mithril again, or sync from genesis. · Previous JA: …チェーンデータを削除してMithrilをもう一度試すか、ジェネシスから同期してください。
- Key: `loading.mithrilBootstrap.startFailureHint`

#### B14. Diagnostics confirmation — cancel button — Revised (EN+JA)
- **EN:** Back to Daedalus Diagnostics
- **JA:** Daedalus診断に戻る
- Previous EN: Back to diagnostics · Previous JA: 診断に戻る
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel`
- Now uses the full page name, matching the Help-menu label 「Daedalus診断」.

---

## C. Revised — JA only

#### C1. Shared process summary — Revised (JA only)
- **EN:** For this process to begin your Cardano node will need to be shut down. … *(unchanged)*
- **JA:** この処理を開始するには、お使いのCardanoノードを停止する必要があります。その後、Mithrilを使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。
- Previous JA: …お使いのCardanoノードをシャットダウンする必要があります。…
- Key: `daedalus.diagnostics.dialog.mithrilSyncProcessSummary`
- Change: シャットダウン → 停止 (your round-1 consistency point — the rest of the feature says
  停止 for stopping the node).

#### C2. Proactive prompt — body, epochs unknown — Revised (JA only)
- **EN:** Your node is behind the blockchain tip. *(unchanged)*
- **JA:** ノードはブロックチェーンの最新ブロックより遅れています。
- Previous JA: ノードはブロックチェーンの先端より遅れています。
- Key: `daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown`
- Change: 先端 → 最新ブロック as the rendering of "tip" (linked-term group 4).

#### C3. Diagnostics tooltip — near tip — Revised (JA only)
- **EN:** Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data. *(unchanged)*
- **JA:** ノードはブロックチェーンの最新ブロックに近い状態です。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。
- Previous JA: ノードはブロックチェーンの先端に近い状態です。…
- Key: `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip`
- Change: 先端 → 最新ブロック (linked-term group 4).

---

## D. EN-only casing changes (FYI — no JA action)

| Key | EN now | EN was | JA (unchanged) |
| --- | --- | --- | --- |
| `loading.mithrilPartialSync.error.failed.hint` | … wipe chain data and do a full Mithril **S**ync. | … full Mithril sync. | 利用可能な復旧操作を使って、… |
| `loading.mithrilPartialSync.error.wipeAndFullSync` | Wipe chain data and do full Mithril **S**ync | … full Mithril sync | チェーンデータを削除して完全なMithril同期を実行 |
| `loading.mithrilBootstrap.stepIndicatorLabel` | Mithril **S**ync progress | Mithril sync progress | Mithril同期の進捗 |

---

## E. Open question (optional, low priority)

`daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationUnknown` (behind-ness check failed):

- **EN:** Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data.
- **JA:** ノードがどの程度遅れているかを確認できませんでした。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。

Should this get the same softer offer-framing as the new at/past-snapshot strings (§A1/§A2)? It was
left as-is in this wave; treat as optional — answer only if you have a preference.

---

## F. Chain-storage strings — confirm if not previously reviewed

These six strings ship with the feature but are identical to what is already merged and translated in
`develop`; round-1 coverage is unconfirmed. Please confirm they were reviewed at some point, or
review them now:

| Key (suffix of `chainStorage.locationPicker.`) | English | Japanese |
| --- | --- | --- |
| `dataFoundNotice` | Existing blockchain data found. Proceeding will reuse this data. | 既存のブロックチェーンデータが見つかりました。続行すると、このデータが再利用されます。 |
| `recoveryNotice` | We couldn't access your previous storage location. A default fallback location has been selected. You can proceed with this or choose a new one. | 以前の保存場所にアクセスできませんでした。既定の代替保存場所が選択されています。このまま続行するか、新しい保存場所を選択してください。 |
| `subdirectoryCreationNotice` | Daedalus will create a chain subdirectory inside the selected parent folder. | Daedalusは選択した親フォルダー内にchainサブディレクトリを作成します。 |
| `subdirectoryWarningExists` | Daedalus will use the existing chain subdirectory inside the selected parent folder. | Daedalusは選択した親フォルダー内にある既存のchainサブディレクトリを使用します。 |
| `subdirectoryErrorConflict` | The selected parent folder already contains a chain entry that is not a directory. | 選択した親フォルダーには、すでにディレクトリではないchainエントリーが存在します。 |
| `validation.isManagedChild` | Select the parent folder, not the existing chain subdirectory managed by Daedalus. | Daedalusが管理している既存のchainサブディレクトリではなく、その親フォルダーを選択してください。 |

---

## G. Removed since round 1 — purge from translation memory

Round-1 entries **§7.5 and §7.6** (`loading.mithrilPartialSync.error.noCertifiedRange.title` /
`.hint` — "No verified Mithril snapshot is available yet" / 「利用可能な検証済みMithrilスナップ
ショットがまだありません」 and its hint) were deleted: the underlying error can no longer occur.
No validation needed.
