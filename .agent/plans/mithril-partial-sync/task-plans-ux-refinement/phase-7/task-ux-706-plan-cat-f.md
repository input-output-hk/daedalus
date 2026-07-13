# task-ux-706 · CAT-F — Code-review cluster: dead code, broken test, message-structure integration, preflight fail-safe

Resolves the three original code-review comments plus the accepted PR #3337 follow-up finding (N1).
Implements **DD-706-3** and **DD-706-8**. Code/test/i18n.

## Step F1 — Remove dead `PARTIAL_SYNC_NO_CERTIFIED_RANGE` (DD-706-3) — gated on a reachability check

task-705 changed `derivePartialSyncRange` to **return** `{start:N, end:N}` (ledger-only restore) instead
of throwing `PARTIAL_SYNC_NO_CERTIFIED_RANGE` when `localImmutableNumber >= latestCertifiedImmutableNumber`
(`mithrilPartialSyncPreflight.ts:134-144`, called from `MithrilPartialSyncService.ts:202`). Grep confirms
no throw/emit site remains in `source/main`.

**Gate (do first):** confirm the error code is genuinely unreachable —
1. `grep -rn "NO_CERTIFIED_RANGE" source/main source/common` and confirm no `throw`, no
   `errorCode = …`, and no dynamic string construction that could yield it at runtime.
2. Confirm the "no certified snapshot exists at all" user situation now surfaces through a **different**
   existing error (e.g. metadata-unavailable / download-failed) or cannot occur. If a real path still
   needs a "no snapshot" message, **repoint that path to an existing error and keep its copy** instead of
   deleting — do not leave a reachable state with no message.

**✅ Gate result (verified 2026-07-09) — confirmed dead; DELETE, do not repoint.**
1. In `source/main` + `source/common`, `NO_CERTIFIED_RANGE` appears **only** as the type-union member at
   `mithril-partial-sync.types.ts:34` — no `throw`, no `errorCode =`, no dynamic construction.
   `derivePartialSyncRange` (`mithrilPartialSyncPreflight.ts:130-149`) returns `{start:N,end:N}` for the
   `localImmutableNumber >= latestCertifiedImmutableNumber` case and contains **no** throw at all
   (plan-review correction: the unrelated `PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE` throw lives in
   `resolveLocalImmutableNumber`, `:120-124`).
2. The "no snapshot at all" situation is covered by a **separate live** code:
   `MithrilPartialSyncService.ts:860-865` throws `PARTIAL_SYNC_METADATA_UNAVAILABLE` when the resolved
   snapshot list is empty. So no reachable state is left without a message after the deletion.
   → Proceed with the full removal below (no repoint).

**If confirmed dead, remove (procedure corrected 2026-07-10 — use the generated-artifact pipeline):**

Hand-edit only the source-of-truth sites:
- Union member: `source/common/types/mithril-partial-sync.types.ts:34`
  (`| 'PARTIAL_SYNC_NO_CERTIFIED_RANGE'`) — **atomically with** the copy-map entry: both are bound by
  `Record<MithrilPartialSyncErrorCode, …>` exhaustiveness, so removing one without the other fails
  `yarn compile`.
- Copy map + its local const: `partialSyncErrorCopy.ts:15-18` (the now-unused `NO_CERTIFIED_RANGE` copy
  object) and the map entry `:56`, plus its message imports.
- Message descriptors: `MithrilBootstrap.messages.ts:392-403` (span ends at 403, not 402)
  (`partialSyncErrorNoCertifiedRangeTitle` / `…Hint`).
- Spec tuple: `partialSyncErrorCopy.spec.ts:8-11` (drop `'PARTIAL_SYNC_NO_CERTIFIED_RANGE'` from the
  covered set) and any table/enumeration that iterates the codes.

Then regenerate the **generated** artifacts instead of hand-editing them: run `yarn i18n:manage`
(`i18n:extract` / `i18n:check`, package.json:52-54) and let it drop the entries from
`defaultMessages.json` (~:2892-2898) **and** `translations/messages.json:2890-2899` — the fifth JSON
the original removal list missed. **Residual to verify at build:** whether the manager auto-deletes
obsolete `en-US.json`/`ja-JP.json` keys or only reports them (702a's impl-review says it dropped
removed ids; not re-executed in plan-review) — if report-only, hand-delete `:366-367`
(`loading.mithrilPartialSync.error.noCertifiedRange.title` / `.hint`) in both locale files, keeping the
EN↔JA line alignment.

This is one of task-706's sanctioned behavior deltas (an unreachable error path removed; CAT-F's other
delta is F4's unknown-size preflight rejection, DD-706-8 — see the master behavior gate for the full
list). Record both in the impl-review. It also removes copy-table rows §7.5/§7.6 — reflect that in the
round-2 table.

## Step F2 — Fix the broken `MithrilPartialSyncService.spec.ts`

**Reality check (plan-review R-2, two independent live jest runs):** the spec fails **9 of 76** tests
today, not 4 — the primary `:414`; the behindness trio `:2168/:2181/:2194`; the concurrent
aggregator/local read `:2339`; the certified-epoch trio `:2377/:2391/:2404`; and the slot-clobber
regression at `:1565`.

**Primary (contract change) — `MithrilPartialSyncService.spec.ts:414-425`:** the test
`'fails when there is no certified immutable range to download'` sets local immutable = 25 and latest
certified = 25 and asserts `start()` rejects with `'The managed chain is not missing any certified
immutable files for Mithril partial sync.'` — a string that no longer exists in source (task-705 made
`derivePartialSyncRange` return `{start:25,end:25}`). **Rewrite is mandatory** — plan-review confirmed
no equivalent assertion exists anywhere (there is no `mithrilPartialSyncPreflight` spec), so the
"delete if duplicate" branch does not apply. And the rewrite is more than an assertion swap:
- `start()` resolves snapshot metadata **twice** (`:194` preparing, `:235` drift re-resolve) — the
  current `mockResolvedValueOnce` must become `mockResolvedValue`, or the second call trips the drift
  throw at `:242`.
- Assert the ledger-only contract by stubbing the download path far enough to check `_runCommand`
  receives `'--start', '25', '--end', '25'` (`MithrilPartialSyncService.ts:569-572`). Setup pattern:
  the sibling test at `:428`, and the staged-restore test declared at `:219` whose
  `--start 12 --end 25` args assertion sits at `:258-269` (`'--start'` at `:266`; the plan-review's
  `:220` cite pointed at the test's first line, not the assertion).

**Secondary (eight falsy-`pathExists` failures) — fix location corrected per R-2:** the shared root
cause: `_getCachedLocalImmutableNumber` (`MithrilPartialSyncService.ts:916-917`) short-circuits to `0`
when `fs.pathExists(immutablePath)` is falsy, and `pathExists` is an un-defaulted `jest.fn()`
(`spec.ts:16`) never re-mocked after `jest.clearAllMocks()`. A helper-local or behindness-scoped fix is
**not enough**: the slot-clobber test at `:1565` inlines its own `getManagedChainPath`/`readdirMock`
stubs (`:1561-1564`) and never calls `stubLocalImmutableNumber`, so it stays red. **Put the default in
the top-level `beforeEach` (`spec.ts:113-114`): `pathExists.mockResolvedValue(true)`.** Safe:
`fs.pathExists` has exactly one production consumer in this spec's reach
(`MithrilPartialSyncService.ts:916`), and the marker module is separately `jest.mock`ed
(`spec.ts:6-10`). This one line fixes all eight secondary failures.

Run the scoped spec green after both fixes; add no new red. Seam **S8**: CAT-D's spec additions land
first (A→G order) and apply this same top-level default — verify it is present rather than re-fixing.

## Step F3 — Integrate `mithrilSyncInterrupt` into the message structure (code-review comment a)

> **Ownership locked 2026-07-09: CAT-F owns this end-to-end** — the extraction into all three locale files
> (+ optional messages module) **and** the JA string (`代わりにMithril同期を使う`). CAT-A does **not**
> edit this key; it only reviews that the JA uses DD-706-1 vocabulary. Single owner resolves the S3 seam.

The interrupt-button label added in task-705 is an **un-extracted** inline `defineMessages` entry at
`SyncingConnectingStatus.tsx:85-90` (id `loading.screen.mithrilSyncInterrupt`, default
`Use Mithril Sync instead`). It is absent from all three locale files, so it has no JA and sits outside
the extracted message set.

- Extract it into the i18n structure via the generated-artifact pipeline (plan-review): `i18n:extract`
  alone writes only `translations/messages.json`; `defaultMessages.json` / `en-US.json` come from
  `i18n:check` — so run `yarn i18n:manage`, then add the JA string to `ja-JP.json` by hand under
  DD-706-1: `代わりにMithril同期を使う`. Keep the `!!!` prefix in the descriptor's defaultMessage
  (locale values are stored stripped). The key inserts outside locale Blocks A/B (~line 391, between
  `loading.screen.loadingWalletData` and `loading.screen.pushingLedger`) — keep the EN↔JA insertion
  line-parallel (master seam S1).
- Keep the id under the existing `loading.screen.*` namespace (matches its siblings). Inline placement
  is conventional here — the file has ~12 sibling inline `loading.screen.*` messages, and the loading
  tree is mixed (`SyncingProgress.messages.ts` exists), so either home works; leaving the
  `defineMessages` in the component is fine so long as it is extracted. Confirm parity with the
  i18n-messaging validation.

## Step F4 — Disk-space preflight fails safe on unknown/malformed snapshot size (DD-706-8, PR #3337 N1)

`_assertSufficientDiskSpace` (`MithrilPartialSyncService.ts:1062-1112`) derives its requirement from
`this._latestSnapshot?.size ?? 0` (`:1066`). Two validated fail-open holes:

1. **Missing size → 4 GB floor.** `normalizeSnapshotItem` (`mithrilSnapshotMetadata.ts:16-22`) defaults
   `size` to `0` when the aggregator metadata carries none of the recognized fields (`size`,
   `total_size`, `total_db_size_uncompressed`, `size_bytes`), so `requiredBytes` collapses to
   `DISK_SPACE_REQUIRED` = 4 GB (`config.ts:161`) — far below a real snapshot.
2. **Malformed size → no check at all.** `Number(garbage)` is `NaN`; `?? 0` does not catch NaN, it
   propagates through both `Math.max` calls, and `freeBytes < NaN` is `false` — the preflight silently
   passes with no floor. (The inline comment at `:80` claims "fails closed"; it does not.)

**Fix (locked 2026-07-10):**

- Export `hasKnownSnapshotSize` from `mithrilSnapshotMetadata.ts`: true iff
  `Number.isFinite(size) && size > 0` for the given `MithrilSnapshotItem` (null-safe on the item).
- At the top of `_assertSufficientDiskSpace`, when `!hasKnownSnapshotSize(this._latestSnapshot)`, throw
  the existing **`PARTIAL_SYNC_METADATA_UNAVAILABLE`** stage error at `preparing` via
  `_createStageError` — the same code and pattern as the empty-snapshot-list path (`:860-865`). Message
  along the lines of "Unable to determine the Mithril snapshot size for the disk-space check." No new
  error code, no new i18n (`partialSyncErrorCopy.ts:62` already maps the code to retryable copy).
- Do **not** validate in `resolveLatestSnapshotMetadata` or `normalizeSnapshotItem`: the behind-ness
  probe shares the resolver and never needs size (rejecting there would silently degrade the proactive
  prompt), and normalization is shared with the bootstrap snapshot selector where unknown size is
  display-tolerable. Preflight-consumer-only, per DD-706-8.
- Leave the two existing fallbacks untouched: unmeasurable local chain size (whole-snapshot bound,
  `:1077-1088`) and unmeasurable free space (fail open, `:1090-1100`, repo precedent) — F4 changes only
  the trustworthiness of the *size input*, not those measurement policies.
- Update or remove the stale "fails closed" wording in the comment at `:80` to match the new contract.

**Seam S7 (updated 2026-07-10):** with CAT-C C2 downgraded (DD-706-4 resolution / DD-706-9), F4 is the
**sole in-wave consumer** of `hasKnownSnapshotSize`. The seam contract binds the Tier-3 size-plumbing
follow-up instead: it must gate `snapshotSizeBytes` population on this same imported predicate — the
drift re-resolve (`:249`) re-assigns `_latestSnapshot` after this check passed, so an invalid size can
reappear post-validation. Single definition; the follow-up must not inline a duplicate.

**Blast radius:** the check runs only inside `start()` (`:210` is the sole call site). The proactive
prompt and the Diagnostics recommendation/confirmation surfaces are driven by
`getPartialSyncBehindness`, which never reads size — they cannot show this error. A user who starts a
sync against broken metadata gets the standard retryable metadata-unavailable overlay frame at
`preparing` instead of a sync that under-reserves disk or skips the check.

**Tests (`MithrilPartialSyncService.spec.ts`):**
- size absent/`0` → `start()` fails at `preparing` with `PARTIAL_SYNC_METADATA_UNAVAILABLE`, and
  `checkDiskSpace` is never invoked;
- size `NaN` (malformed metadata string) → same rejection;
- valid size → existing formula assertions unchanged
  (`max(snapshot − chainDir, 0) + margin`, floor, and the fail-open free-space catch).
- `hasKnownSnapshotSize` unit cases in `mithrilSnapshotMetadata.spec.ts` (0, NaN, negative, positive,
  null item).

## Acceptance

- No `PARTIAL_SYNC_NO_CERTIFIED_RANGE` / `noCertifiedRange` reference remains (type, copy map, messages,
  locales, spec) — unless the gate found a live path, in which case that path is repointed and documented.
- `MithrilPartialSyncService.spec.ts` passes: the primary test asserts the ledger-only range contract
  (`--start 25 --end 25`); the top-level `beforeEach` defaults `pathExists` to resolve `true`, turning
  all nine pre-existing failures green — incl. the inlined-mock slot-clobber test at `:1565`.
- `loading.screen.mithrilSyncInterrupt` exists in all three locale files with an EN + JA value and
  i18n-messaging validation is clean.
- `hasKnownSnapshotSize` is exported from `mithrilSnapshotMetadata.ts` and is the only size-validity
  predicate (no inlined duplicate); `_assertSufficientDiskSpace` rejects missing/0/NaN sizes with
  `PARTIAL_SYNC_METADATA_UNAVAILABLE` at `preparing` before `checkDiskSpace` runs; the F4 spec cases
  above are green and the `:80` comment no longer claims the floor "fails closed".
- `yarn compile` + lint clean; scoped jest over the service spec, `mithrilSnapshotMetadata.spec.ts`, and
  `partialSyncErrorCopy.spec.ts` green. (Plan-review: there is no `SyncingConnectingStatus` spec — only
  `SyncingConnectingMithrilPrompt.spec.tsx`, which doesn't reference the key — so F3's gate is the
  i18n-messaging validation, not a component spec.)
