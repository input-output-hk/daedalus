# task-ux-706 · CAT-A — Three-term vocabulary sweep

Implements **DD-706-1**. Resolves JA review #1 and #4, and resolution #1. Copy/i18n only.

> Terminology contract (all three rendered in **Latin script even inside Japanese**):
> **Mithril Sync** = fast verified-snapshot restore · **Blockchain Sync** = normal block-by-block /
> resume-from-tip · **Blockchain Sync from Genesis** = full sync from block 0 (fresh install / wipe).

## i18n mechanics (applies to every step)

The source of truth for each `!!!`-prefixed default is either a component `defineMessages` block or
`MithrilBootstrap.messages.ts`. After editing a source default + `ja-JP.json`, run the **i18n-messaging**
skill's sync/validation to regenerate `defaultMessages.json` and confirm EN/JA/default parity. Do not
hand-desync the four sources. `en-US.json` and `ja-JP.json` share line numbers (Block A 156–178, Block
B 297–386). Keep every `{placeholder}` byte-identical.

## Step A1 — JA `Mithril同期` → `Mithril Sync` (ja-JP.json only, 30 keys / 31 occurrences)

The EN side already reads "Mithril Sync"; **only the JA value changes**, and only in `ja-JP.json`
(no `defaultMessages.json` entries — it is EN-only). Rule: replace the substring `Mithril同期` with the
Latin token `Mithril Sync` in place, leaving surrounding Japanese particles untouched
(e.g. `Mithril同期を開始` → `Mithril Syncを開始`; `Mithril同期の完了時に` → `Mithril Syncの完了時に`).

Keys (ja-JP.json line): 157, 158, 159, 161, 162, 163, 164, 165, 166, 172, 173, 177, 178, 344, 347, 349,
350, 355, 356, 357, 359, 360, 361, 362, 370, 373, 374, 376, 378, 386. (30 keys — plan-review corrected
the original 29-key list: line **378** also carries `Mithril同期`, and line **355 contains the substring
twice**, so the sweep replaces 31 occurrences. The acceptance grep — no `Mithril同期` remains — is the
real gate, not the count.)

Spot-checks:
- `mithrilProactivePromptTitle` (177): `Mithril同期` → `Mithril Sync`
- `mithrilPartialSyncConfirmationConfirm` (161): `Mithril同期を開始` → `Mithril Syncを開始`
- `error.retry` (370): `Mithril同期を再試行（高速）` → `Mithril Syncを再試行（高速）`
- `error.wipeAndFullSync` (374): `…完全なMithril同期を実行` → `…完全なMithril Syncを実行`
- `progress.nodeStartingDetail` (378): also touched by A3 (`標準同期`) — apply both edits.

Note keys 164/173/178 also carry CAT-B edits (B3 near-tip 先端, B1/B4 handoff note, B2 shutdown) —
apply all overlapping edits to those lines together. A3's only overlap with this list is line 378
(handled in the spot-checks).

## Step A2 — EN casing: `Mithril sync` → `Mithril Sync`

"Mithril Sync" is the product token; normalize the lowercase EN stragglers (and mirror in JA where the
phrase is not already handled by A1):

| Key | line | EN now → EN new |
| --- | --- | --- |
| `loading.mithrilBootstrap.stepIndicatorLabel` | 344 | `Mithril sync progress` → `Mithril Sync progress` |
| `loading.mithrilPartialSync.error.failed.hint` | 355 | `…do a full Mithril sync.` → `…do a full Mithril Sync.` |
| `loading.mithrilPartialSync.error.wipeAndFullSync` | 374 | `…do full Mithril sync` → `…do full Mithril Sync` |

Update the matching `!!!` defaults (`MithrilBootstrap.messages.ts` `stepIndicatorLabel`,
`partialSyncFailedHint`, `partialSyncWipeAndFullSync`) and re-sync `defaultMessages.json`.

## Step A3 — `Blockchain Sync` (replaces "Standard Sync" / 「標準同期」 / "Node Sync")

Resolves JA review #4. Rendered Latin in JA.

| Key | line | EN now → EN new | JA now → JA new |
| --- | --- | --- | --- |
| `…mithrilProactivePromptStandardButton` | 176 | `Standard Sync (slow)` → `Blockchain Sync (slow)` | `標準同期（低速）` → `Blockchain Sync（低速）` |
| `…mithrilProactivePromptBodyBenefit` | 168 | `Mithril can catch you up faster than the standard sync.` → `Mithril can catch you up faster than Blockchain Sync.` | `Mithrilを使えば、標準同期よりも速く追いつけます。` → `Mithrilを使えば、Blockchain Syncよりも速く追いつけます。` |
| `loading.mithrilPartialSync.progress.nodeStartingDetail` | 378 | `…resume standard syncing.` → `…resume Blockchain Sync.` | `…Daedalusが標準同期を再開できるよう…` → `…DaedalusがBlockchain Syncを再開できるよう…` |
| `loading.mithrilPartialSync.error.restartNormally` | 369 | `Restart Node Sync (slow)` → `Restart Blockchain Sync (slow)` | `ノード同期を再起動（低速）` → `Blockchain Syncを再起動（低速）` |

Source defs to edit + re-sync: `SyncingConnectingMithrilPrompt.tsx` (`promptStandardButton` ~58-63,
`promptBodyBenefit` ~32-38); `MithrilBootstrap.messages.ts` `partialSyncNodeStartingDetail`
(descriptor `:305`, defaultMessage `:308` — plan-review corrected the name; there is no
`partialSyncProgressNodeStartingDetail`) and `partialSyncRestartNormally` (485).

## Step A4 — `Blockchain Sync from Genesis` (replaces "Sync from genesis" / 「ジェネシスから同期」)

Reserved for the true block-0 case (bootstrap decline + wipe/restart hints). Rendered Latin in JA.

| Key | line | EN now → EN new | JA now → JA new |
| --- | --- | --- | --- |
| `loading.mithrilBootstrap.decline` | 299 | `Sync from genesis` → `Blockchain Sync from Genesis` | `ジェネシスから同期する` → `Blockchain Sync from Genesis` |
| `loading.mithrilBootstrap.description` | 300 | `…Choose a snapshot and continue, or sync from genesis.` → `…or do a Blockchain Sync from Genesis.` | `…スナップショットを選んで続行するか、ジェネシスから同期してください。` → `…スナップショットを選んで続行するか、Blockchain Sync from Genesis を実行してください。` |
| `loading.mithrilBootstrap.error.convert.hint` | 301 | `…Try again or sync from genesis.` → `…Try again or do a Blockchain Sync from Genesis.` | `…もう一度試すか、ジェネシスから同期してください。` → `…もう一度試すか、Blockchain Sync from Genesis を実行してください。` |
| `loading.mithrilBootstrap.error.verify.hint` | 306 | `…Try another snapshot or sync from genesis.` → `…Try another snapshot or do a Blockchain Sync from Genesis.` | `…別のスナップショットを試すか、ジェネシスから同期してください。` → `…別のスナップショットを試すか、Blockchain Sync from Genesis を実行してください。` |
| `loading.mithrilBootstrap.startFailureHint` | 339 | `…Wipe the chain and try Mithril again, or sync from genesis.` → `…Wipe the chain and try Mithril Sync again, or do a Blockchain Sync from Genesis.` | `…チェーンデータを削除してMithrilをもう一度試すか、ジェネシスから同期してください。` → `…チェーンデータを削除してMithril Syncをもう一度試すか、Blockchain Sync from Genesis を実行してください。` |

Source defs to edit + re-sync: `MithrilBootstrap.messages.ts` lines 19 (the `description` descriptor's
default — not a title), 29, 95, 149, 160 (the genesis-bearing defaults) — confirm exact descriptor by
id, not line, before editing.

> The JA phrasing above (`Blockchain Sync from Genesis を実行`) is a first pass; it goes to the JA
> translator in round 2 (this is a brand-new term not yet reviewed). Keep the term itself fixed;
> the translator's wording feedback may adjust the surrounding particles/verb.

## Step A5 — Native startup-dialog casing (added 2026-07-10, DD-706-9)

The interrupted-partial-sync recovery dialog in `source/main/mithril/mithrilPartialSyncNodeStartup.ts`
is a pre-renderer Electron `showMessageBox` — user-facing, non-i18n, EN-only by design (it must work
before the renderer exists). Two literals carry the lowercase token; casing-only edits:

- `:103` button: `'Wipe chain and full Mithril sync'` → `'Wipe chain and full Mithril Sync'`
- `:109` message: `…until the chain data is wiped and a full Mithril sync can run again.`
  → `…until the chain data is wiped and a full Mithril Sync can run again.`

No i18n extraction (the dialog stays native EN copy); no spec asserts these literals (verified —
`mithrilPartialSyncNodeStartup.spec.ts` does not match on the button/message text).

## Out of scope for CAT-A (owned elsewhere, listed to prevent double-edits)

- The bootstrap **feature title** `Fast sync with Mithril` (345) and **accept** button `Use Mithril fast
  sync` (297): these are the "fast sync" *name*, not the "Fast sync:" progress *label*. Product direction
  keeps "Mithril Sync" as the umbrella name — revisit only if JA translator round 2 asks; **do not** rename to
  "Ledger state" (that is CAT-C's progress-line-only change).
- `mithrilSyncInterrupt`: **owned end-to-end by CAT-F** (ownership locked 2026-07-09 — extraction + JA
  `代わりにMithril Syncを使う`). CAT-A does **not** edit this key; it only reviews that the JA uses the
  DD-706-1 vocabulary. Do not supply or wire it here even if CAT-A lands first.

## Acceptance

- No `Mithril同期` remains in `ja-JP.json` (31 occurrences across 30 keys removed); no `標準同期` /
  "Standard Sync" / "Node Sync" remains as a slow-path name; no user-facing "sync from genesis" /
  「ジェネシスから同期」 remains.
- EN "Mithril sync" (lowercase) no longer appears in user-facing strings — including the native
  startup-dialog literals (Step A5).
- EN/JA/`defaultMessages.json`/`*.messages.ts` in sync (i18n-messaging validation clean).
- `yarn compile` + lint clean. Scoped jest: the sweep breaks hardcoded old-copy assertions in
  `SyncingConnectingMithrilPrompt.spec.tsx:54,93,146`, `MithrilProactivePromptContainer.spec.tsx:68`,
  `MithrilPartialSyncOverlay.spec.tsx:99,292,307`, and `MithrilBootstrap.spec.tsx:168,243,263` (plus
  the stale-copy comment at `:239`) — update those assertions to the new strings and run those specs
  green.
