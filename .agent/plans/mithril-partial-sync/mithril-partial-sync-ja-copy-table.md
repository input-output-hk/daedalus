# Mithril Sync — EN → JA Copy Table

All user-facing copy added or changed between the state **before PR #3333** and the **current branch state** (`feat/mithril-partial-sync-ux-refinement`). Current wording only. Placeholders in curly braces (e.g. `{epochs}`, `{totalSize}`) are replaced at runtime and must stay exactly as-is in the Japanese text.

| # | English | Japanese | Screen / Prompt |
| --- | --- | --- | --- |
| 1 | Mithril Sync | Mithril同期 | Syncing screen — Mithril prompt (title) |
| 2 | Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. | ノードは約{epochs, plural, other {#エポック}}遅れています。 | Syncing screen — Mithril prompt (body) |
| 3 | Your node is behind the blockchain tip. | ノードはブロックチェーンの先端より遅れています。 | Syncing screen — Mithril prompt (body, epochs unknown) |
| 4 | Mithril can catch you up faster than the standard sync. | Mithrilを使えば、標準同期よりも速く追いつけます。 | Syncing screen — Mithril prompt (body) |
| 5 | Mithril Sync (fast) | Mithril Sync（高速） | Syncing screen — Mithril prompt (button) |
| 6 | Standard Sync (slow) | 標準同期（低速） | Syncing screen — Mithril prompt (button) |
| 7 | Note: | 注: | Syncing screen — Mithril prompt (note label) |
| 8 | If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D) | スキップした場合でも、ヘルプメニューにあるDaedalus Diagnostics画面からMithril同期を開始できます。(Ctrl + D) | Syncing screen — Mithril prompt (note) |
| 9 | Mithril Sync Process | Mithril同期の処理 | Syncing screen — Mithril prompt, confirm step (title) |
| 10 | Start now | 今すぐ開始 | Syncing screen — Mithril prompt, confirm step (button) |
| 11 | Cancel | キャンセル | Syncing screen — Mithril prompt, confirm step (button) |
| 12 | For this process to begin your Cardano node will need to be shut down. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks. | この処理を開始するには、お使いのCardanoノードをシャットダウンする必要があります。その後、Mithrilを使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。 | Shared — prompt confirm step & Diagnostics confirmation modal |
| 13 | Mithril Sync | Mithril同期 | Diagnostics — Mithril Sync row (label) |
| 14 | Mithril Sync | Mithril同期 | Diagnostics — Mithril Sync row (button) |
| 15 | If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync. | Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、同期を高速化できます。 | Diagnostics — Mithril Sync row (button tooltip) |
| 16 | Unavailable while Mithril work is already active. | Mithrilの処理がすでに進行中のため利用できません。 | Diagnostics — Mithril Sync row (disabled-button hint) |
| 17 | Before Mithril Sync begins | Mithril同期を始める前に | Diagnostics — confirmation modal (title) |
| 18 | Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは約{epochs, plural, other {#エポック}}遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | Diagnostics — confirmation modal (body) |
| 19 | Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは最新の検証済みスナップショットより遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | Diagnostics — confirmation modal (body, epochs unknown) |
| 20 | Start Mithril Sync | Mithril同期を開始 | Diagnostics — confirmation modal (button) |
| 21 | Back to diagnostics | 診断に戻る | Diagnostics — confirmation modal (button) |
| 22 | Mithril Sync | Mithril同期 | Sync overlay (headline) |
| 23 | Daedalus is restoring verified Mithril chain data. Download and verification time can vary based on your network connection and storage performance. | Daedalusは検証済みのMithrilチェーンデータを復元しています。ダウンロードと検証にかかる時間は、ネットワーク接続とストレージ性能によって変わります。 | Sync overlay — progress (subtitle) |
| 24 | Stopping Cardano node... | Cardanoノードを停止しています... | Sync overlay — progress (stopping node, title) |
| 25 | Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes. | Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。 | Sync overlay — progress (stopping node, detail) |
| 26 | Starting Cardano node... | Cardanoノードを起動しています... | Sync overlay — progress (starting node, title) |
| 27 | Mithril Sync has finished restoring chain data. Cardano node is starting so Daedalus can resume standard syncing. | Mithril同期によるチェーンデータの復元が完了しました。Daedalusが標準同期を再開できるよう、Cardanoノードを起動しています。 | Sync overlay — progress (starting node, detail) |
| 28 | Cleaning up... | 後処理しています... | Sync overlay — progress (after cancel, title) |
| 29 | Daedalus is cleaning up Mithril Sync before you continue. | 続行する前に、DaedalusがMithril同期の後処理をしています。 | Sync overlay — progress (after cancel, detail) |
| 30 | Cancellation available once the node has stopped | ノードが停止した後にキャンセルできます | Sync overlay — progress (disabled Cancel button, tooltip) |
| 31 | Mithril Sync completed successfully. | Mithril同期が正常に完了しました。 | Sync overlay — completion (message) |
| 32 | Returning to Daedalus... | Daedalusに戻っています... | Sync overlay — completion (handoff caption) |
| 33 | Mithril Sync failed | Mithril同期に失敗しました | Sync overlay — error (generic failure, title) |
| 34 | Use one of the available recovery actions to retry Mithril Sync, restart normally, or wipe chain data and do a full Mithril sync. | 利用可能な復旧操作を使って、Mithril同期の再試行、通常再起動、またはチェーンデータを削除して完全なMithril同期を実行できます。 | Sync overlay — error (generic failure, hint) |
| 35 | Mithril Sync was cancelled | Mithril同期はキャンセルされました | Sync overlay — error (cancelled, title) |
| 36 | Mithril Sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below. | Mithril同期は完了前に停止されました。既存のチェーンデータは変更されていません。下のオプションから続行方法を選択してください。 | Sync overlay — error (cancelled, hint) |
| 37 | No verified Mithril snapshot is available yet | 利用可能な検証済みMithrilスナップショットがまだありません | Sync overlay — error (no snapshot available, title) |
| 38 | Daedalus could not find a verified Mithril snapshot for your current chain position. Choose how to continue below — you can keep syncing on your existing chain data. | 現在のチェーン位置に対応する検証済みMithrilスナップショットが見つかりませんでした。下のオプションから続行方法を選択してください。既存のチェーンデータで同期を続けることもできます。 | Sync overlay — error (no snapshot available, hint) |
| 39 | The verified Mithril snapshot moved on | 検証済みMithrilスナップショットが更新されました | Sync overlay — error (snapshot updated mid-run, title) |
| 40 | A newer verified snapshot became available while Daedalus was preparing. Retry Mithril Sync to use the refreshed snapshot — your chain data was not changed. | 準備中に新しい検証済みスナップショットが利用可能になりました。Mithril同期を再試行して更新されたスナップショットを使用してください。チェーンデータは変更されていません。 | Sync overlay — error (snapshot updated mid-run, hint) |
| 41 | The Mithril snapshot could not be verified | Mithrilスナップショットを検証できませんでした | Sync overlay — error (verification failed, title) |
| 42 | The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below. | ダウンロードしたスナップショットが不完全か、期待される検証済みチェーンデータと一致しませんでした。下のオプションから続行方法を選択してください。 | Sync overlay — error (verification failed, hint) |
| 43 | Downloading the Mithril snapshot failed | Mithrilスナップショットのダウンロードに失敗しました | Sync overlay — error (download failed, title) |
| 44 | Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below. | Mithrilスナップショットのダウンロードと検証を完了できませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。 | Sync overlay — error (download failed, hint) |
| 45 | Preparing the Mithril snapshot failed | Mithrilスナップショットの準備に失敗しました | Sync overlay — error (preparation failed, title) |
| 46 | Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below. | 検証済みスナップショットをダウンロードしましたが、使用するための準備ができませんでした。下のオプションから続行方法を選択してください。 | Sync overlay — error (preparation failed, hint) |
| 47 | Retry Mithril Sync (fast) | Mithril同期を再試行（高速） | Sync overlay — error (recovery button) |
| 48 | Restart Node Sync (slow) | ノード同期を再起動（低速） | Sync overlay — error (recovery button) |
| 49 | Wipe chain data and do full Mithril sync | チェーンデータを削除して完全なMithril同期を実行 | Sync overlay — error (recovery button) |
| 50 | Quit Daedalus | Daedalusを終了 | Sync overlay — error (fallback button when no recovery available) |
| 51 | Snapshot files: {snapshotDownloaded} / {snapshotTotal} files \| Fast sync: {fastSyncDownloaded} / {fastSyncTotal} | スナップショットファイル: {snapshotDownloaded} / {snapshotTotal} 個 \| 高速同期: {fastSyncDownloaded} / {fastSyncTotal} | First-run Mithril bootstrap screen (download detail) |
| 52 | ≈ {totalSize} total | 約 {totalSize}（合計） | First-run Mithril bootstrap screen (download size context) |
| 53 | This can take several minutes — Daedalus is still working. | 数分かかることがあります。Daedalusは引き続き処理しています。 | First-run Mithril bootstrap screen (long-phase reassurance) |
| 54 | Stopping Cardano node... | Cardanoノードを停止しています... | First-run Mithril bootstrap screen (stopping node, title) |
| 55 | Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes. | Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。 | First-run Mithril bootstrap screen (stopping node, detail) |
| 56 | Not enough disk space for Mithril Sync | Mithril同期に必要なディスクの空き容量が不足しています | Sync overlay — error (insufficient disk space, title) |
| 57 | Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync. | 検証済みスナップショットを準備するためのディスクの空き容量が不足しています。必要な容量と利用可能な容量はエラー詳細に表示されています。チェーンデータを保存しているディスクの空き容量を確保してから、Mithril同期を再試行してください。 | Sync overlay — error (insufficient disk space, hint) |
| 58 | Verifying snapshot... | スナップショットを検証しています... | Sync overlay — progress (stage item) |
| 59 | Converting snapshot format... | スナップショットの形式を変換しています... | Sync overlay — progress (stage item) |
| 60 | Installing snapshot... | スナップショットをインストールしています... | Sync overlay — progress (stage item) |
| 61 | Technical details | 技術的な詳細 | Sync overlay / bootstrap — error (details header) |
| 62 | Unable to start Mithril Sync. | Mithril同期を開始できませんでした。 | Diagnostics — confirmation modal (start-failure fallback) |
| 63 | Checking the latest Mithril snapshot failed | 最新のMithrilスナップショットの確認に失敗しました | Sync overlay — error (metadata unavailable, title/hint) |
| 64 | Daedalus could not read the latest verified snapshot details. Check your internet connection, then choose how to continue below. | 最新の検証済みスナップショットの詳細を読み取れませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。 | Sync overlay — error (metadata unavailable, title/hint) |
