# task-ux-702a — Copy Overview (EN → JA, per surface)

> **Deliverable for D-702a-7.** This is the consolidated old → new copy diff for **every**
> user-facing string the `task-ux-702a` remediation introduced, changed, or removed across the
> Mithril partial-sync feature set. It exists so a **manual native Japanese translator** can review
> the provisional JA before release.
>
> **The Japanese in the "provisional JA" column is best-effort / provisional** (machine-assisted,
> applying the task-ux-601 ja-JP Mithril glossary) and **needs manual native review.** EN is the
> source of truth and was approved in the grill decision record (`task-ux-702a-decisions.md`).
>
> **Verification.** Every EN and JA value below was read directly from the on-disk catalogs
> (`source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`); OLD values are the pre-702a
> baseline from `git HEAD` (post task-ux-601/701). Nothing here is invented.
>
> **Locked vocabulary (must hold in all copy).** User-facing wording is **"Mithril Sync"**
> (JA: `Mithril同期`), **"Standard Sync"** (JA: `標準同期`), **"Node Sync"** (JA: `ノード同期`) —
> **never "partial sync"** in user copy. Behind-ness is expressed **in epochs only** (the `{epochs}`
> token) with a `behindUnknown` fallback when the figure is not yet computable — **never** percentages
> or immutable-file counts. "Partial Sync" survives only in code identifiers / Storybook kind labels.
>
> **Scope: 18 strings** — 7 changed, 2 added, 9 removed — across 5 surfaces.
> The catalogs `defaultMessages.json` and `translations/messages.json` were updated in lock-step and
> are not re-tabulated here (they carry the same EN strings prefixed with `!!!`).

---

## Surface 1 — Diagnostics: recommendation tooltip (CAT-B / D-702a-3)

The recommendation copy moved from an always-visible block into a **hover tooltip on the "Mithril Sync"
button**, and the separate "ready" hint was deleted.

| Key | Surface | Old EN | New EN | Provisional JA | Translator notes |
|---|---|---|---|---|---|
| `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation` | Diagnostics — hover tooltip on the "Mithril Sync" button | If Cardano node catch-up is taking longer than you want, Mithril Sync can restore verified chain data to help it catch up faster. | If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync. | Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、同期を高速化できます。 | Reworded to the verbatim D-702a-3 string. Note EN now says "Cardano Node syncing" / "speed up the sync" (was "catch-up … catch up faster"). Keep `Mithril同期` for "Mithril Sync". |
| `daedalus.diagnostics.dialog.mithrilPartialSyncButtonHintReady` | Diagnostics — old ready-hint under the button | Review what will happen before Daedalus starts Mithril Sync. | **(removed)** | **(removed)** — was: DaedalusがMithril同期を開始する前に、実行内容を確認してください。 | Deleted entirely; the tooltip now carries the only informational copy. No replacement. |

> Unchanged on this surface (NOT in scope, listed for translator context):
> `…mithrilPartialSyncButtonLabel` = "Mithril Sync" / `Mithril同期`;
> `…mithrilPartialSyncButtonHintBlocked` = "Unavailable while Mithril work is already active." /
> `Mithrilの処理がすでに進行中のため利用できません。` (still shown only in the blocked state).

---

## Surface 2 — Diagnostics: confirmation modal "Before Mithril Sync begins" (CAT-C / D-702a-4)

The verbose multi-step body collapsed to **two paragraphs**: (1) a subject line that keeps the
`{epochs}` figure (+ `behindUnknown` fallback) and the restore sentence; (2) the canonical
shutdown/restore/restart subtext, which is the **new shared key** `mithrilSyncProcessSummary`
(D-702a-6, also reused by the proactive prompt — see Surface 3). Six body strings were removed.

| Key | Surface | Old EN | New EN | Provisional JA | Translator notes |
|---|---|---|---|---|---|
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind` | Confirmation modal — subject paragraph (epochs known) | Your node is about {epochs} epochs behind the blockchain tip. Mithril Sync can restore verified chain data to help it catch up faster than waiting for standard sync. | Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは約{epochs}エポック遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | Shortened. Keep the `{epochs}` placeholder verbatim. Deliberate re-touch of a string previously locked by task-304/601 (sanctioned by D-702a-4 — not a regression). |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown` | Confirmation modal — subject paragraph (epochs unknown fallback) | Your node is behind the latest verified snapshot. | Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster. | ノードは最新の検証済みスナップショットより遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。 | The fallback now also carries the restore sentence (so both branches read consistently). No epochs figure on this branch by design. |
| `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` | **Shared** — confirmation modal subtext **and** proactive-prompt confirm body | **(new)** | For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks. | この処理を開始するには、お使いのCardanoノードをシャットダウンする必要があります。その後、Mithrilを使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。 | **New canonical D-702a-6 sentence, ONE key reused by two surfaces** — must read identically in both. Consolidates the old confirmation steps + the old `mithrilProactivePromptConfirmBody`. Glossary: shutdown=シャットダウン, restart=再起動, verified chain data=検証済みのチェーンデータ. |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationIntro` | Confirmation modal — removed intro line | Daedalus will stop Cardano node automatically, then download and restore verified Mithril data. | **(removed)** | **(removed)** — was: DaedalusはCardanoノードを自動的に停止し、その後で検証済みのMithrilデータをダウンロードして復元します。 | Folded into the shared summary. No replacement. |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepStop` | Confirmation modal — removed step 1 | Daedalus stops Cardano node. | **(removed)** | **(removed)** — was: DaedalusはCardanoノードを停止します。 | Removed (step list dropped). |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepDownload` | Confirmation modal — removed step 2 | Daedalus downloads and verifies Mithril data. | **(removed)** | **(removed)** — was: DaedalusはMithrilデータをダウンロードして検証します。 | Removed (step list dropped). |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepRestart` | Confirmation modal — removed step 3 | Daedalus restarts Cardano node automatically and standard syncing resumes. | **(removed)** | **(removed)** — was: DaedalusはCardanoノードを自動的に再起動し、標準同期が再開します。 | Removed (step list dropped). |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationSuccess` | Confirmation modal — removed success line | If Mithril Sync succeeds, Daedalus will restart Cardano node automatically and standard syncing will resume. | **(removed)** | **(removed)** — was: Mithril同期が成功すると、DaedalusはCardanoノードを自動的に再起動し、標準同期を再開します。 | Removed; superseded by the shared summary. |
| `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationRecovery` | Confirmation modal — removed recovery-options paragraph | You don't need to choose now. If the sync fails, the progress screen will then offer the available recovery options — such as retrying, restarting normally on your current data, or wiping chain data and running a full Mithril sync. | **(removed)** | **(removed)** — was: ここで選択する必要はありません。同期に失敗した場合は、進行状況画面で利用可能な復旧オプション（再試行、現在のデータでの通常再開、またはチェーンデータを消去して完全なMithril同期を実行など）が表示されます。 | i18n **key** removed. NB: the CSS class `mithrilPartialSyncConfirmationRecovery` is *reused* for the new subtext element — unrelated to this string. |

> Unchanged on this surface (NOT in scope): `…ConfirmationTitle` = "Before Mithril Sync begins" /
> `Mithril同期を始める前に`; `…ConfirmationConfirm` = "Start Mithril Sync" / `Mithril同期を開始`;
> `…ConfirmationCancel` = "Back to diagnostics" / `診断に戻る`.

---

## Surface 3 — Proactive prompt (loading screen + Wallet Summary) (CAT-F / D-702a-5)

The prompt's private confirm-body string was deleted in favour of the shared
`mithrilSyncProcessSummary` key (Surface 2), and "Mithril sync" was capitalized in the handoff note.

| Key | Surface | Old EN | New EN | Provisional JA | Translator notes |
|---|---|---|---|---|---|
| `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` | Proactive prompt — "skip" handoff note | If skipped, you can still start the Mithril sync from the Diagnostics screen. | If skipped, you can still start the Mithril Sync from the Diagnostics screen. | スキップした場合でも、Diagnostics画面からMithril同期を開始できます。 | EN change is only the capitalization "Mithril sync" → "Mithril Sync" (locked vocabulary). JA fixes the old Latin "Mithril sync" → `Mithril同期`. |
| `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmBody` | Proactive prompt — old confirm-view body | Mithril will stop your Cardano node, restore verified chain data, and restart it — so you catch up faster. | **(removed)** | **(removed)** — was: Mithrilはお使いのCardanoノードを停止し、検証済みのチェーンデータを復元してから再起動します。これにより、より速く追いつけます。 | Consolidated into the shared `mithrilSyncProcessSummary` key — the prompt's confirm view now renders that shared sentence (see Surface 2). |

> Reuses (no separate row): the prompt confirm view now displays
> `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` (tabulated in Surface 2) — it must read
> identically in both the prompt and the confirmation modal.
>
> Unchanged on this surface (NOT in scope, for context): `…mithrilProactivePromptMithrilButton` =
> "Mithril Sync (fast)" / `Mithril Sync（高速）`; `…mithrilProactivePromptStandardButton` =
> "Standard Sync (slow)" / `標準同期（低速）`; `…mithrilProactivePromptBody` ({epochs} line),
> `…BodyBenefit`, `…BodyUnknown`, `…ConfirmStart`, `…ConfirmCancel`.

---

## Surface 4 — Cancelled (pre-cutover) recovery dialogue buttons (CAT-E / D-702a-2)

The recovery-action button labels were reworded to spell out fast/slow paths (the "wipe chain data"
button is no longer emitted on the cancelled dialogue — a backend array trim, not a copy change).

| Key | Surface | Old EN | New EN | Provisional JA | Translator notes |
|---|---|---|---|---|---|
| `loading.mithrilPartialSync.error.retry` | Cancelled / error recovery dialogue — primary (right) button | Retry Mithril Sync | Retry Mithril Sync (fast) | Mithril同期を再試行（高速） | "(fast)" added for parity with the prompt's fast/slow framing. JA uses full-width `（高速）`. |
| `loading.mithrilPartialSync.error.restartNormally` | Cancelled / error recovery dialogue — secondary (left) button | Restart normally | Restart Node Sync (slow) | ノード同期を再起動（低速） | Renamed to the locked "Node Sync" vocabulary (was "Restart normally"). JA uses `ノード同期` + full-width `（低速）`. |

> Unchanged on this surface (NOT in scope): `loading.mithrilPartialSync.error.wipeAndFullSync`
> (wipe button label) — still defined, still shown on the post-cutover startup-failure dialog; only
> its emission on the *cancelled* dialogue was trimmed (no copy change).

---

## Surface 5 — Completion overlay (CAT-G / ADR D-702a-1)

The "Continue to Daedalus" button was removed (the overlay now auto-transitions on a timeout), the
success subtitle was shortened, and a new spinner caption was added for the auto-transition.

| Key | Surface | Old EN | New EN | Provisional JA | Translator notes |
|---|---|---|---|---|---|
| `loading.mithrilPartialSync.completed.subtitle` | Completion overlay — success subtitle | Mithril Sync completed successfully. Continue to return to the normal Daedalus app flow. | Mithril Sync completed successfully. | Mithril同期が正常に完了しました。 | Trailing "Continue to return…" sentence dropped (no button anymore). Deliberate re-touch of a 601-touched string, sanctioned by ADR D-702a-1. |
| `loading.mithrilPartialSync.completed.transition` | Completion overlay — auto-transition spinner caption | **(new)** | Returning to Daedalus... | Daedalusに戻っています... | New caption shown under the spinner while the overlay auto-dismisses and hands off to node startup. Keep the trailing ellipsis. |
| `loading.mithrilPartialSync.completed.continue` | Completion overlay — old "continue" button | Continue to Daedalus | **(removed)** | **(removed)** — was: Daedalusに戻る | Button removed per ADR D-702a-1 (auto-fire finalize on timeout replaces the manual click). |

---

## Summary counts

| Change type | Count | Keys |
|---|---|---|
| Changed | 7 | `mithrilPartialSyncRecommendation`, `mithrilPartialSyncConfirmationBehind`, `mithrilPartialSyncConfirmationBehindUnknown`, `mithrilProactivePromptHandoffNote`, `error.retry`, `error.restartNormally`, `completed.subtitle` |
| Added | 2 | `mithrilSyncProcessSummary` (shared), `completed.transition` |
| Removed | 9 | `mithrilPartialSyncButtonHintReady`, `…ConfirmationIntro`, `…ConfirmationStepStop`, `…ConfirmationStepDownload`, `…ConfirmationStepRestart`, `…ConfirmationSuccess`, `…ConfirmationRecovery`, `mithrilProactivePromptConfirmBody`, `completed.continue` |
| **Total** | **18** | across 5 surfaces |

> **Action for the Japanese translator:** review every value in the "provisional JA" column. Priority
> items are the **new shared sentence** `mithrilSyncProcessSummary` (appears in two surfaces, must read
> identically and naturally) and the **fast/slow button labels** (`error.retry`, `error.restartNormally`,
> handoff note) for natural Japanese phrasing of the locked `Mithril同期` / `標準同期` / `ノード同期`
> vocabulary and the full-width `（高速）`/`（低速）` parentheses convention.
