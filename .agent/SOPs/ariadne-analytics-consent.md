# Ariadne consent v2: English and Japanese review drafts

Neither version is approved release wording. Japanese text is an initial human-review
draft, not an approved translation. Sam and Adam must also decide Matomo replacement
versus fallback; this copy cannot settle that product decision. Recipient identity,
privacy notice, deletion authentication and backup retention need review before rollout.

The version is 2 because attempt identifiers and start/outcome events extend the
previous local notice. Existing Matomo or Ariadne v1 consent never carries over.
The two IPC channel names retain their V1 suffix (channel protocol); that is not
the consent notice or wire event version.

## ariadne.analytics.settings.accepted

English draft: Ariadne analytics is enabled with your permission. Revoking stops collection but does not delete previously received events.

Japanese draft: 同意に基づき、Ariadne 利用状況分析が有効です。同意を撤回すると収集は停止しますが、すでに受信されたイベントは削除されません。

## ariadne.analytics.settings.off

English draft: Ariadne analytics collection is off.

Japanese draft: Ariadne 利用状況分析の収集は無効です。

## ariadne.analytics.settings.title

English draft: Ariadne analytics (provisional)

Japanese draft: Ariadne 利用状況分析（暫定案・要レビュー）

## ariadne.analytics.v2.allow

English draft: Allow Ariadne analytics

Japanese draft: Ariadne 利用状況分析を許可する

## ariadne.analytics.v2.description

English draft: Optional pseudonymous usage data is sent to the configured Ariadne support deployment to improve Daedalus. A separate random installation identifier links your events across accepted restarts. This is pseudonymous information, not anonymous data. Events are not linked to support tickets. Previous Matomo permission does not apply.

Japanese draft: Daedalus の改善を目的として、任意の仮名化された利用状況データを、設定された Ariadne サポート環境に送信します。専用のランダムなインストール識別子により、同意が有効な間は再起動後もイベントが関連付けられます。これは仮名化された情報であり、匿名データではありません。イベントはサポートチケットと関連付けられません。以前の Matomo への同意は適用されません。

## ariadne.analytics.v2.details

English draft: What Ariadne receives

Japanese draft: Ariadne が受信する情報

## ariadne.analytics.v2.device

English draft: Operating system and numeric version where supported, CPU family, rounded RAM size, Daedalus version, and whether legacy or hardware wallets are present. These fields are pseudonymous, not anonymous.

Japanese draft: OSと、対応している場合はその数値形式のバージョン、CPUの系統、丸めたRAM容量、Daedalus のバージョン、および旧形式ウォレットまたはハードウェアウォレットの有無を送信します。これらの項目は仮名化されており、匿名ではありません。

## ariadne.analytics.v2.deviceTitle

English draft: Device categories

Japanese draft: 端末の区分

## ariadne.analytics.v2.disabled

English draft: Collection is disabled or unavailable in this configuration. Wallet features remain available.

Japanese draft: この設定では収集が無効になっているか、利用できません。ウォレット機能は引き続き利用できます。

## ariadne.analytics.v2.events

English draft: Allowlisted page names and actions, event time, network, installation identifier and hardware/software wallet type where applicable. Delegation submission and voting registration setup also send start, completion or pre-submission cancellation steps with a separate random identifier for each attempt. No wallet addresses, balances, transaction identifiers, messages, recovery material, raw routes or free-text labels. IP addresses are not analytics fields; separate infrastructure access logs may contain IP addresses.

Japanese draft: 許可リストに登録されたページ名と操作、イベント時刻、ネットワーク、インストール識別子、および該当する場合はハードウェア／ソフトウェアウォレットの区分を送信します。委任の送信と投票登録の準備では、試行ごとの専用ランダム識別子とともに、開始、完了、または送信開始前のキャンセルを送信します。ウォレットアドレス、残高、取引識別子、メッセージ、復元情報、生のルート、自由入力のラベルは含みません。IPアドレスは分析イベントの項目には含まれませんが、別途インフラのアクセスログに記録される場合があります。

## ariadne.analytics.v2.eventsTitle

English draft: Usage events

Japanese draft: 利用状況イベント

## ariadne.analytics.v2.reject

English draft: Keep off / revoke

Japanese draft: 無効のままにする／同意を撤回する

## ariadne.analytics.v2.retention

English draft: Events are retained for up to 24 months; private backups follow the deployment retention policy. Revoking stops collection, discards unsent events and clears this installation identifier. It does not delete events already received. Accepting again creates a new identifier. Do not share recovery phrases or spending passwords. Final recipient, privacy notice, backup retention and deletion-request arrangements require review before release.

Japanese draft: イベントは最長24か月間保存されます。非公開のバックアップには、運用環境の保存方針が適用されます。同意を撤回すると収集が停止し、未送信イベントとこのインストール識別子が削除されます。すでに受信されたイベントは削除されません。再び同意すると、新しい識別子が作成されます。復元フレーズや送金パスワードを共有しないでください。正式な送信先、プライバシーに関する通知、バックアップの保存期間、削除請求の手続きは、リリース前のレビューが必要です。

## ariadne.analytics.v2.saveFailed

English draft: The choice could not be saved. Collection is stopped for this session. Retry before restarting to ensure your choice is preserved.

Japanese draft: 選択を保存できませんでした。このセッションでは収集を停止しています。選択が保存されるよう、再起動する前にもう一度お試しください。

## ariadne.analytics.v2.title

English draft: Ariadne analytics — provisional notice v2

Japanese draft: Ariadne 利用状況分析 — 暫定案 v2（要レビュー）

