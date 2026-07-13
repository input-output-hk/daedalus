# Mithril Sync — EN → JA Copy Table

All user-facing copy added or changed between the state **before PR #3333** and the **current branch
state** (`feat/mithril-partial-sync-ux-refinement`, regenerated 2026-07-12 after the task-ux-706
copy-standardization wave). Current wording only. Placeholders in curly braces (e.g. `{epochs}`,
`{totalSize}`, `{shortcut}`) are replaced at runtime and must stay exactly as-is in the Japanese text.

Canonical vocabulary (DD-706-1): **Mithril Sync** = 「Mithril同期」, **Blockchain Sync** =
「ブロックチェーン同期」, **Blockchain Sync from Genesis** = 「ジェネシスからのブロックチェーン同期」
(sentences use ジェネシスからブロックチェーン同期を実行…). Only "Mithril" (and product names like
Daedalus/Cardano) stays Latin in JA. The Diagnostics page is named 「Daedalus診断」 in JA.

Key prefixes: `D.` = `daedalus.diagnostics.dialog.` · `PS.` = `loading.mithrilPartialSync.` ·
`BS.` = `loading.mithrilBootstrap.`

| # | English | Japanese | Screen / Prompt | Key |
| --- | --- | --- | --- | --- |
| 1 | Mithril Sync | Mithril同期 | Syncing screen — Mithril prompt (title) | `D.mithrilProactivePromptTitle` |
| 2 | Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. | ノードは約{epochs, plural, other {#エポック}}遅れています。 | Syncing screen — Mithril prompt (body) | `D.mithrilProactivePromptBody` |
| 3 | Your node is behind the blockchain tip. | ノードはブロックチェーンの最新ブロックより遅れています。 | Syncing screen — Mithril prompt (body, epochs unknown) | `D.mithrilProactivePromptBodyUnknown` |
| 4 | Mithril can catch you up faster than Blockchain Sync. | Mithrilを使えば、ブロックチェーン同期よりも速く追いつけます。 | Syncing screen — Mithril prompt (body) | `D.mithrilProactivePromptBodyBenefit` |
| 5 | Mithril Sync (fast) | Mithril同期（高速） | Syncing screen — Mithril prompt (button) | `D.mithrilProactivePromptMithrilButton` |
| 6 | Blockchain Sync (slow) | ブロックチェーン同期（低速） | Syncing screen — Mithril prompt (button) | `D.mithrilProactivePromptStandardButton` |
| 7 | Note: | 注: | Syncing screen — Mithril prompt (note label) | `D.mithrilProactivePromptHandoffNoteLabel` |
| 8 | If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. ({shortcut}) | スキップした場合でも、ヘルプメニューにあるDaedalus診断画面からMithril同期を開始できます。({shortcut}) | Syncing screen — Mithril prompt (note); `{shortcut}` renders "Cmd + D" on macOS, "Ctrl + D" elsewhere (untranslated) | `D.mithrilProactivePromptHandoffNote` |
| 9 | Mithril Sync Process | Mithril同期の処理 | Syncing screen — Mithril prompt, confirm step (title) | `D.mithrilProactivePromptConfirmTitle` |
| 10 | Start now | 今すぐ開始 | Syncing screen — Mithril prompt, confirm step (button) | `D.mithrilProactivePromptConfirmStart` |
| 11 | Cancel | キャンセル | Syncing screen — Mithril prompt, confirm step (button) | `D.mithrilProactivePromptConfirmCancel` |
| 12 | For this process to begin your Cardano node will need to be shut down. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks. | この処理を開始するには、お使いのCardanoノードを停止する必要があります。その後、Mithrilを使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。 | Shared — prompt confirm step & Diagnostics confirmation modal | `D.mithrilSyncProcessSummary` |
| 13 | Use Mithril Sync instead | 代わりにMithril同期を使う | Loading screen — ledger-replay interrupt button | `loading.screen.mithrilSyncInterrupt` |
| 14 | Mithril Sync | Mithril同期 | Diagnostics — Mithril Sync row (label) | `D.mithrilPartialSyncSectionLabel` |
| 15 | Mithril Sync | Mithril同期 | Diagnostics — Mithril Sync row (button) | `D.mithrilPartialSyncButtonLabel` |
| 16 | If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync. | Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、同期を高速化できます。 | Diagnostics — button tooltip (significantly behind) | `D.mithrilPartialSyncRecommendation` |
| 17 | Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data. | ノードはブロックチェーンの最新ブロックに近い状態です。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。 | Diagnostics — button tooltip (near tip) | `D.mithrilPartialSyncRecommendationNearTip` |
| 18 | Your node is at or past the latest Mithril snapshot. Blockchain Sync will finish the remaining blocks on its own. If sync seems slow or runs into verification issues, Mithril Sync can restore a verified ledger state. | ノードは最新のMithrilスナップショットに到達しているか、それを超えています。残りのブロックはブロックチェーン同期が自動的に同期します。同期が遅い場合や検証に問題がある場合は、Mithril同期で検証済みの台帳状態を復元できます。 | Diagnostics — button tooltip (at/past latest snapshot) | `D.mithrilPartialSyncRecommendationAtOrPastSnapshot` |
| 19 | Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data. | ノードがどの程度遅れているかを確認できませんでした。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。 | Diagnostics — button tooltip (behind-ness check failed) | `D.mithrilPartialSyncRecommendationUnknown` |
| 20 | Unavailable while Mithril work is already active. | Mithrilの処理がすでに進行中のため利用できません。 | Diagnostics — Mithril Sync row (disabled-button hint) | `D.mithrilPartialSyncButtonHintBlocked` |
| 21 | Before Mithril Sync begins | Mithril同期を始める前に | Diagnostics — confirmation modal (title) | `D.mithrilPartialSyncConfirmationTitle` |
| 22 | Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは約{epochs, plural, other {#エポック}}遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | Diagnostics — confirmation modal (body) | `D.mithrilPartialSyncConfirmationBehind` |
| 23 | Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは最新の検証済みスナップショットより遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | Diagnostics — confirmation modal (body, epochs unknown) | `D.mithrilPartialSyncConfirmationBehindUnknown` |
| 24 | Your node is at or past the latest Mithril snapshot, so Blockchain Sync can finish the remaining blocks on its own. If sync seems slow or runs into verification issues, continuing will restore a verified ledger state at the snapshot position. | ノードは最新のMithrilスナップショットに到達しているか、それを超えているため、残りのブロックはブロックチェーン同期が同期できます。同期が遅い場合や検証に問題がある場合は、続行するとスナップショット位置の検証済み台帳状態が復元されます。 | Diagnostics — confirmation modal (body, at/past latest snapshot) | `D.mithrilPartialSyncConfirmationAtOrPastSnapshot` |
| 25 | Start Mithril Sync | Mithril同期を開始 | Diagnostics — confirmation modal (button) | `D.mithrilPartialSyncConfirmationConfirm` |
| 26 | Back to Daedalus Diagnostics | Daedalus診断に戻る | Diagnostics — confirmation modal (button) | `D.mithrilPartialSyncConfirmationCancel` |
| 27 | Unable to start Mithril Sync. | Mithril同期を開始できませんでした。 | Diagnostics — confirmation modal (start-failure fallback) | `PS.error.startFailure` |
| 28 | Mithril Sync | Mithril同期 | Sync overlay (headline) | `PS.title` |
| 29 | Daedalus is restoring verified Mithril chain data. Download and verification time can vary based on your network connection and storage performance. | Daedalusは検証済みのMithrilチェーンデータを復元しています。ダウンロードと検証にかかる時間は、ネットワーク接続とストレージ性能によって変わります。 | Sync overlay — progress (subtitle) | `PS.progress.subtitle` |
| 30 | Stopping Cardano node... | Cardanoノードを停止しています... | Sync overlay — progress (stopping node, title) | `PS.progress.nodeStoppingTitle` |
| 31 | Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes. | Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。 | Sync overlay — progress (stopping node, detail) | `PS.progress.nodeStoppingDetail` |
| 32 | Starting Cardano node... | Cardanoノードを起動しています... | Sync overlay — progress (starting node, title) | `PS.progress.nodeStartingTitle` |
| 33 | Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume Blockchain Sync. | Mithril同期によるチェーンデータの復元が完了しました。Daedalusがブロックチェーン同期を再開できるよう、Cardanoノードを起動しています。 | Sync overlay — progress (starting node, detail) | `PS.progress.nodeStartingDetail` |
| 34 | Cleaning up... | 後処理しています... | Sync overlay — progress (after cancel, title) | `PS.progress.cancellingTitle` |
| 35 | Daedalus is cleaning up Mithril Sync before you continue. | 続行する前に、DaedalusがMithril同期の後処理をしています。 | Sync overlay — progress (after cancel, detail) | `PS.progress.cancellingDetail` |
| 36 | Cancellation available once the node has stopped | ノードが停止した後にキャンセルできます | Sync overlay — progress (disabled Cancel button, tooltip) | `PS.progress.cancelStoppingTooltip` |
| 37 | Verifying snapshot... | スナップショットを検証しています... | Sync overlay — progress (stage item) | `PS.progress.stageVerifying` |
| 38 | Converting snapshot format... | スナップショットの形式を変換しています... | Sync overlay — progress (stage item) | `PS.progress.stageConverting` |
| 39 | Installing snapshot... | スナップショットをインストールしています... | Sync overlay — progress (stage item) | `PS.progress.stageInstalling` |
| 40 | Mithril Sync completed successfully. | Mithril同期が正常に完了しました。 | Sync overlay — completion (message) | `PS.completed.subtitle` |
| 41 | Returning to Daedalus... | Daedalusに戻っています... | Sync overlay — completion (handoff caption) | `PS.completed.transition` |
| 42 | Mithril Sync failed | Mithril同期に失敗しました | Sync overlay — error (generic failure, title) | `PS.error.failed.title` |
| 43 | Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril Sync. | 利用可能な復旧操作を使って、Mithril同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。 | Sync overlay — error (generic failure, hint) | `PS.error.failed.hint` |
| 44 | Mithril Sync was cancelled | Mithril同期はキャンセルされました | Sync overlay — error (cancelled, title) | `PS.error.cancelled.title` |
| 45 | Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below. | Mithril同期は完了前に停止されました。既存のチェーンデータは変更されていません。下のオプションから続行方法を選択してください。 | Sync overlay — error (cancelled, hint) | `PS.error.cancelled.hint` |
| 46 | The verified Mithril snapshot moved on | 検証済みMithrilスナップショットが更新されました | Sync overlay — error (snapshot updated mid-run, title) | `PS.error.latestDrift.title` |
| 47 | A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed. | 準備中に新しい検証済みスナップショットが利用可能になりました。Mithril同期を再試行して更新されたスナップショットを使用してください。チェーンデータは変更されていません。 | Sync overlay — error (snapshot updated mid-run, hint) | `PS.error.latestDrift.hint` |
| 48 | The Mithril snapshot could not be verified | Mithrilスナップショットを検証できませんでした | Sync overlay — error (verification failed, title) | `PS.error.stagedDbInvalid.title` |
| 49 | The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below. | ダウンロードしたスナップショットが不完全か、期待される検証済みチェーンデータと一致しませんでした。下のオプションから続行方法を選択してください。 | Sync overlay — error (verification failed, hint) | `PS.error.stagedDbInvalid.hint` |
| 50 | Downloading the Mithril snapshot failed | Mithrilスナップショットのダウンロードに失敗しました | Sync overlay — error (download failed, title) | `PS.error.downloadFailed.title` |
| 51 | Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below. | Mithrilスナップショットのダウンロードと検証を完了できませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。 | Sync overlay — error (download failed, hint) | `PS.error.downloadFailed.hint` |
| 52 | Preparing the Mithril snapshot failed | Mithrilスナップショットの準備に失敗しました | Sync overlay — error (preparation failed, title) | `PS.error.conversionFailed.title` |
| 53 | Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below. | 検証済みスナップショットをダウンロードしましたが、使用するための準備ができませんでした。下のオプションから続行方法を選択してください。 | Sync overlay — error (preparation failed, hint) | `PS.error.conversionFailed.hint` |
| 54 | Checking the latest Mithril snapshot failed | 最新のMithrilスナップショットの確認に失敗しました | Sync overlay — error (metadata unavailable, title) | `PS.error.metadataUnavailable.title` |
| 55 | Daedalus could not read the latest verified snapshot details. Check your internet connection, then choose how to continue below. | 最新の検証済みスナップショットの詳細を読み取れませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。 | Sync overlay — error (metadata unavailable, hint) | `PS.error.metadataUnavailable.hint` |
| 56 | Not enough disk space for Mithril Sync | Mithril同期に必要なディスクの空き容量が不足しています | Sync overlay — error (insufficient disk space, title) | `PS.error.insufficientDiskSpace.title` |
| 57 | Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync. | 検証済みスナップショットを準備するためのディスクの空き容量が不足しています。必要な容量と利用可能な容量はエラー詳細に表示されています。チェーンデータを保存しているディスクの空き容量を確保してから、Mithril同期を再試行してください。 | Sync overlay — error (insufficient disk space, hint) | `PS.error.insufficientDiskSpace.hint` |
| 58 | Finishing Mithril Sync failed | Mithril同期の後処理に失敗しました | Sync overlay — error (finalize failed, title) | `PS.error.finalizeFailed.title` |
| 59 | Mithril Sync completed, but Daedalus could not finish the final cleanup step. Try again to continue to Daedalus. | Mithril同期は完了しましたが、Daedalusは最後の後処理を完了できませんでした。再試行してDaedalusに戻ってください。 | Sync overlay — error (finalize failed, hint) | `PS.error.finalizeFailed.hint` |
| 60 | Try again | 再試行 | Sync overlay — error (finalize failed, retry button) | `PS.error.finalizeFailed.retry` |
| 61 | Retry Mithril Sync (fast) | Mithril同期を再試行（高速） | Sync overlay — error (recovery button) | `PS.error.retry` |
| 62 | Restart Blockchain Sync (slow) | ブロックチェーン同期を再起動（低速） | Sync overlay — error (recovery button) | `PS.error.restartNormally` |
| 63 | Wipe chain data and do full Mithril Sync | チェーンデータを削除して完全なMithril同期を実行 | Sync overlay — error (recovery button) | `PS.error.wipeAndFullSync` |
| 64 | Quit Daedalus | Daedalusを終了 | Sync overlay — error (fallback button when no recovery available) | `PS.error.quit` |
| 65 | Technical details | 技術的な詳細 | Sync overlay / bootstrap — error (details header) | `BS.errorDetailsHeader` |
| 66 | Blockchain Sync from Genesis | ジェネシスからのブロックチェーン同期 | First-run Mithril bootstrap screen (decline button) | `BS.decline` |
| 67 | Mithril can download a verified snapshot to sync your blockchain data faster. Choose a snapshot and continue, or do a Blockchain Sync from Genesis. | Mithrilは検証済みのスナップショットをダウンロードして、ブロックチェーンデータの同期を高速化できます。スナップショットを選んで続行するか、ジェネシスからブロックチェーン同期を実行してください。 | First-run Mithril bootstrap screen (description) | `BS.description` |
| 68 | Daedalus could not prepare the downloaded snapshot for use. Try again or do a Blockchain Sync from Genesis. | ダウンロードしたスナップショットをDaedalusで使用できる状態に準備できませんでした。もう一度試すか、ジェネシスからブロックチェーン同期を実行してください。 | First-run Mithril bootstrap screen — error (conversion, hint) | `BS.error.convert.hint` |
| 69 | Daedalus could not verify the snapshot integrity. Try another snapshot or do a Blockchain Sync from Genesis. | Daedalusはスナップショットの整合性を検証できませんでした。別のスナップショットを試すか、ジェネシスからブロックチェーン同期を実行してください。 | First-run Mithril bootstrap screen — error (verification, hint) | `BS.error.verify.hint` |
| 70 | The node could not start with the restored chain data. Wipe the chain and try Mithril Sync again, or do a Blockchain Sync from Genesis. | 復元したチェーンデータではノードを起動できませんでした。チェーンデータを削除してMithril同期をもう一度試すか、ジェネシスからブロックチェーン同期を実行してください。 | First-run Mithril bootstrap screen — error (node start failed, hint) | `BS.startFailureHint` |
| 71 | Mithril Sync progress | Mithril同期の進捗 | First-run Mithril bootstrap screen (step indicator label) | `BS.stepIndicatorLabel` |
| 72 | Snapshot Files and Ledger State | スナップショットファイルと台帳状態 | First-run Mithril bootstrap screen (combined progress label) | `BS.progress.combinedLabel` |
| 73 | Snapshot files: {snapshotDownloaded} / {snapshotTotal} files \| Ledger state: {fastSyncDownloaded} / {fastSyncTotal} | スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 \| 台帳状態: {fastSyncDownloaded} / {fastSyncTotal} | First-run Mithril bootstrap screen (download detail) | `BS.progress.combinedDetail` |
| 74 | ≈ {totalSize} total | 約 {totalSize}（合計） | First-run Mithril bootstrap screen (download size context) | `BS.progress.snapshotSizeContext` |
| 75 | This can take several minutes — Daedalus is still working. | 数分かかることがあります。Daedalusは引き続き処理しています。 | First-run Mithril bootstrap screen (long-phase reassurance) | `BS.progress.longPhaseReassurance` |
| 76 | To preserve data integrity, please don't close Daedalus until this step is complete. | データの完全性を保つために、この手順が完了するまでDaedalusを閉じないでください。 | Both overlays — caution inside the "Moving snapshot to storage" step while active | `BS.progress.moveCaution` |
| 77 | Stopping Cardano node... | Cardanoノードを停止しています... | First-run Mithril bootstrap screen (stopping node, title) | `BS.progress.nodeStoppingTitle` |
| 78 | Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes. | Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。 | First-run Mithril bootstrap screen (stopping node, detail) | `BS.progress.nodeStoppingDetail` |

Removed since the previous version of this table (task-ux-706 CAT-F, DD-706-3): the
`loading.mithrilPartialSync.error.noCertifiedRange.title` / `.hint` pair ("No verified Mithril
snapshot is available yet" / "Daedalus could not find a verified Mithril snapshot for your current
chain position. …") — the error code is dead and was deleted; purge from any translation memory.
