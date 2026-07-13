# task-ux-706 · CAT-A — Three-term vocabulary sweep

Implements **DD-706-1**. Resolves JA review #1 and #4, and resolution #1. Copy/i18n only.

> Terminology contract (JA renders each term in established native vocabulary; "Mithril" itself stays
> Latin as a proper noun, matching the existing JA copy): **Mithril Sync** = fast verified-snapshot
> restore → 「Mithril同期」 · **Blockchain Sync** = normal block-by-block / resume-from-tip →
> 「ブロックチェーン同期」 · **Blockchain Sync from Genesis** = full sync from block 0 (fresh install /
> wipe) → 「ジェネシスからのブロックチェーン同期」.

## i18n mechanics (applies to every step)

The source of truth for each `!!!`-prefixed default is either a component `defineMessages` block or
`MithrilBootstrap.messages.ts`. After editing a source default + `ja-JP.json`, run the **i18n-messaging**
skill's sync/validation to regenerate `defaultMessages.json` and confirm EN/JA/default parity. Do not
hand-desync the four sources. `en-US.json` and `ja-JP.json` share line numbers (Block A 156–178, Block
B 297–386). Keep every `{placeholder}` byte-identical.

## Step A1 — JA outlier fix: 「Mithril Sync（高速）」 → 「Mithril同期（高速）」 (ja-JP.json only, 1 key)

The JA rendering of "Mithril Sync" is 「Mithril同期」, which `ja-JP.json` already uses consistently
across 30 keys. The single outlier is the proactive prompt's primary button (JA review round-1
attention point 1 — the translator flagged exactly this inconsistency). **Only the JA value changes**,
and only in `ja-JP.json` (no `defaultMessages.json` entry — it is EN-only):

- `mithrilProactivePromptMithrilButton` (175): `Mithril Sync（高速）` → `Mithril同期（高速）`

The acceptance grep — no Latin `Mithril Sync` remains in any `ja-JP.json` value — is the real gate.
The EN side keeps "Mithril Sync (fast)" unchanged.

## Step A2 — EN casing: `Mithril sync` → `Mithril Sync`

"Mithril Sync" is the product token; normalize the lowercase EN stragglers (JA already reads
`Mithril同期` on these keys — no JA change):

| Key | line | EN now → EN new |
| --- | --- | --- |
| `loading.mithrilBootstrap.stepIndicatorLabel` | 344 | `Mithril sync progress` → `Mithril Sync progress` |
| `loading.mithrilPartialSync.error.failed.hint` | 355 | `…do a full Mithril sync.` → `…do a full Mithril Sync.` |
| `loading.mithrilPartialSync.error.wipeAndFullSync` | 374 | `…do full Mithril sync` → `…do full Mithril Sync` |

Update the matching `!!!` defaults (`MithrilBootstrap.messages.ts` `stepIndicatorLabel`,
`partialSyncFailedHint`, `partialSyncWipeAndFullSync`) and re-sync `defaultMessages.json`.

## Step A3 — `Blockchain Sync` (replaces "Standard Sync" / 「標準同期」 / "Node Sync")

Resolves JA review #4. JA: 「ブロックチェーン同期」 (ブロックチェーン is the established katakana —
36 pre-existing occurrences in `ja-JP.json`).

| Key | line | EN now → EN new | JA now → JA new |
| --- | --- | --- | --- |
| `…mithrilProactivePromptStandardButton` | 176 | `Standard Sync (slow)` → `Blockchain Sync (slow)` | `標準同期（低速）` → `ブロックチェーン同期（低速）` |
| `…mithrilProactivePromptBodyBenefit` | 168 | `Mithril can catch you up faster than the standard sync.` → `Mithril can catch you up faster than Blockchain Sync.` | `Mithrilを使えば、標準同期よりも速く追いつけます。` → `Mithrilを使えば、ブロックチェーン同期よりも速く追いつけます。` |
| `loading.mithrilPartialSync.progress.nodeStartingDetail` | 378 | `…resume standard syncing.` → `…resume Blockchain Sync.` | `…Daedalusが標準同期を再開できるよう…` → `…Daedalusがブロックチェーン同期を再開できるよう…` |
| `loading.mithrilPartialSync.error.restartNormally` | 369 | `Restart Node Sync (slow)` → `Restart Blockchain Sync (slow)` | `ノード同期を再起動（低速）` → `ブロックチェーン同期を再起動（低速）` |

Source defs to edit + re-sync: `SyncingConnectingMithrilPrompt.tsx` (`promptStandardButton` ~58-63,
`promptBodyBenefit` ~32-38); `MithrilBootstrap.messages.ts` `partialSyncNodeStartingDetail`
(descriptor `:305`, defaultMessage `:308` — plan-review corrected the name; there is no
`partialSyncProgressNodeStartingDetail`) and `partialSyncRestartNormally` (485).

## Step A4 — `Blockchain Sync from Genesis` (replaces "Sync from genesis" / 「ジェネシスから同期」)

Reserved for the true block-0 case (bootstrap decline + wipe/restart hints). JA: name form
「ジェネシスからのブロックチェーン同期」 for buttons/labels; flowing sentences use the verb form
「ジェネシスからブロックチェーン同期を実行してください」. Both keep the 「ブロックチェーン同期」 family
name visible, mirroring the EN family (ジェネシス is already the established rendering in these keys).

| Key | line | EN now → EN new | JA now → JA new |
| --- | --- | --- | --- |
| `loading.mithrilBootstrap.decline` | 299 | `Sync from genesis` → `Blockchain Sync from Genesis` | `ジェネシスから同期する` → `ジェネシスからのブロックチェーン同期` |
| `loading.mithrilBootstrap.description` | 300 | `…Choose a snapshot and continue, or sync from genesis.` → `…or do a Blockchain Sync from Genesis.` | `…スナップショットを選んで続行するか、ジェネシスから同期してください。` → `…スナップショットを選んで続行するか、ジェネシスからブロックチェーン同期を実行してください。` |
| `loading.mithrilBootstrap.error.convert.hint` | 301 | `…Try again or sync from genesis.` → `…Try again or do a Blockchain Sync from Genesis.` | `…もう一度試すか、ジェネシスから同期してください。` → `…もう一度試すか、ジェネシスからブロックチェーン同期を実行してください。` |
| `loading.mithrilBootstrap.error.verify.hint` | 306 | `…Try another snapshot or sync from genesis.` → `…Try another snapshot or do a Blockchain Sync from Genesis.` | `…別のスナップショットを試すか、ジェネシスから同期してください。` → `…別のスナップショットを試すか、ジェネシスからブロックチェーン同期を実行してください。` |
| `loading.mithrilBootstrap.startFailureHint` | 339 | `…Wipe the chain and try Mithril again, or sync from genesis.` → `…Wipe the chain and try Mithril Sync again, or do a Blockchain Sync from Genesis.` | `…チェーンデータを削除してMithrilをもう一度試すか、ジェネシスから同期してください。` → `…チェーンデータを削除してMithril同期をもう一度試すか、ジェネシスからブロックチェーン同期を実行してください。` |

Source defs to edit + re-sync: `MithrilBootstrap.messages.ts` lines 19 (the `description` descriptor's
default — not a title), 29, 95, 149, 160 (the genesis-bearing defaults) — confirm exact descriptor by
id, not line, before editing.

> The JA phrasing above is a first pass; it goes to the JA translator in round 2 (this is a brand-new
> term not yet reviewed). Keep the 「ブロックチェーン同期」 family name visible in both slow-path terms;
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
  `代わりにMithril同期を使う`). CAT-A does **not** edit this key; it only reviews that the JA uses the
  DD-706-1 vocabulary. Do not supply or wire it here even if CAT-A lands first.

## Acceptance

- No Latin `Mithril Sync` remains in any `ja-JP.json` value (the one outlier button is fixed; JA reads
  `Mithril同期` throughout); no `標準同期` / "Standard Sync" / "Node Sync" remains as a slow-path name;
  no user-facing "sync from genesis" / bare 「ジェネシスから同期」 remains (replaced by the
  「ブロックチェーン同期」 family).
- EN "Mithril sync" (lowercase) no longer appears in user-facing strings — including the native
  startup-dialog literals (Step A5).
- EN/JA/`defaultMessages.json`/`*.messages.ts` in sync (i18n-messaging validation clean).
- `yarn compile` + lint clean. Scoped jest: the sweep breaks hardcoded old-copy assertions in
  `SyncingConnectingMithrilPrompt.spec.tsx:54,93,146`, `MithrilProactivePromptContainer.spec.tsx:68`,
  `MithrilPartialSyncOverlay.spec.tsx:99,292,307`, and `MithrilBootstrap.spec.tsx:168,243,263` (plus
  the stale-copy comment at `:239`) — update those assertions to the new strings and run those specs
  green.
