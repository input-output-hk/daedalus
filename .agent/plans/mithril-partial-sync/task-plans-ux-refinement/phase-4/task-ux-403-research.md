# task-ux-403 — Research note

Durable findings recorded at planning (2026-06-26T11:31:11Z). Promote/extend at implementation.

## Verified backend error codes ↔ emitted stage (the error-copy map keys)

| Code (EXACT LIVE string) | Throw site | `stage` (`MithrilPartialSyncErrorStage`) | Tier |
|---|---|---|---|
| `PARTIAL_SYNC_NO_CERTIFIED_RANGE` | `mithrilPartialSyncPreflight.ts:152` | `preparing` | code |
| `PARTIAL_SYNC_LATEST_DRIFT` | `MithrilPartialSyncService.ts:197-201` (`PARTIAL_SYNC_LATEST_DRIFT_CODE` :60) | `preparing` | code — **retriable** (#17) |
| `PARTIAL_SYNC_STAGED_DB_INVALID` | `mithrilPartialSyncStaging.ts:60/63/70/88/91/102/103/122` (`PARTIAL_SYNC_STAGED_DB_INVALID_CODE`, preflight :6-7) | `verifying` (staged download) **and** `installing` (converted output) | code |
| `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` | `MithrilPartialSyncService.ts:479-485` | `downloading` **or** `verifying` (whichever phase the command ran in) | code |
| `PARTIAL_SYNC_CONVERSION_FAILED` | `MithrilPartialSyncService.ts:497-501` | `converting` | code |

All codes funnel through `MithrilPartialSyncService._createStageError(stage, message, code)` (`:955`), which sets `error.stage`/`error.code` on the snapshot the renderer receives.

## PARTIAL_SYNC_ prefix discrepancy — RESOLVED in favor of the LIVE repo

The tasks JSON (`task-ux-403.implementationNotes` and `description`) names the last two codes as
`DOWNLOAD_COMMAND_FAILED` and `CONVERSION_FAILED` **without** the `PARTIAL_SYNC_` prefix. The LIVE
backend strings carry the prefix: `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED`
(`MithrilPartialSyncService.ts:484`) and `PARTIAL_SYNC_CONVERSION_FAILED` (`:500`). **The error-copy
map keys on the exact LIVE strings.** A map keyed on the unprefixed JSON names would silently never
match → those two failures would fall through to generic copy, defeating the task. Downstream tasks /
reviewers: trust the LIVE codes.

## Resolution order — cancelled → code → stage → generic (and WHY code beats stage)

`resolvePartialSyncErrorCopy(status, error)`:
1. `status === 'cancelled'` → calmer cancelled copy (always; a user stop is never a "failure"). Locks D5e / gap #32.
2. `error.code` exact match in `COPY_BY_CODE` (the 5 codes).
3. `error.stage` in `COPY_BY_STAGE` (downloading→download, verifying/installing→staged-db, converting→conversion — reusing the code descriptors, no extra keys).
4. generic `partialSyncFailed*`.

**Code beats stage** because `PARTIAL_SYNC_STAGED_DB_INVALID` is emitted at **both** `verifying` and
`installing`; the code is the precise signal, the stage is not. The stage tier exists only to give a
**code-less** failed frame relevant copy at a meaningful phase.

## gap #32 — cancelled/failed hints were byte-identical (FIX)

Verified identical text in all three surfaces:
- `MithrilBootstrap.messages.ts:340-341` (`partialSyncFailedHint`) == `:350-353` (`partialSyncCancelledHint`).
- `en-US.json:351` (`…cancelled.hint`) == `:353` (`…failed.hint`).
- `ja-JP.json:351` == `:353`.
Fix: rewrite ONLY `partialSyncCancelledHint` to calmer/distinct copy ("Mithril sync was stopped before
it finished. Your existing chain data is unchanged — choose how to continue below."). Titles already
differed ("…was cancelled" vs "…failed"); leave them.

## gap #6 is structural — the overlay short-circuits the bootstrap-keyed fallback

`MithrilPartialSyncOverlay.tsx:129-138` ALWAYS passes an explicit `title`+`hint`, and
`MithrilErrorView.tsx:88-94` only consults `ERROR_COPY_BY_STAGE` when `hint === undefined`. That
fallback is keyed to `MithrilBootstrapErrorStage` (bootstrap vocab), not partial-sync codes. Therefore
the only correct fix is to compute partial-sync copy in the **partial-sync layer** (the new
`partialSyncErrorCopy.ts` pure helper) and pass it in. `MithrilErrorView` needs **no edit** — confirmed.

## Lock #18 boundary (no raw JSON as primary copy) — what is and isn't covered

The resolver guarantees the **primary copy (title + hint)** is always a localized `MessageDescriptor`,
never `error.message`. `MithrilErrorView` still renders `error.message` verbatim as a **secondary** `<p>`
(`:116`) and in the collapsible technical-details body (`:139-141`) — this is the pre-existing
diagnostic surface, shared with bootstrap, and out of this task's scope. The backend messages at the 5
sites are English sentences, not raw mithril-client JSON. #18's requirement is met for the primary copy.

## Residuals (non-blocking, owned elsewhere)

- **Vocabulary (task-ux-601).** Locked user-memory note prefers "Mithril sync" over "partial sync" in
  user-facing copy, but the existing catalog uniformly uses "Mithril partial sync" (and
  `MithrilPartialSyncOverlay.spec.tsx:46` asserts the "mithril partial sync" heading). New bespoke copy
  here uses "Mithril sync"; the existing generic titles/subtitle are left for the holistic 601 vocab/JA
  pass to avoid scope creep / spec churn in this functional task.
- **gap #12 confirmation surface (D5b/D6).** The start-rejection raw string at
  `MithrilPartialSyncConfirmation.tsx:74` (set in `MithrilPartialSyncStore.startFromConfirmation`) is a
  DIFFERENT surface not in this task's targetPaths. Closed here only for the overlay's primary copy.
- **Dead `onWipeRetry`/`onDecline` props (gap #33 / task-ux-404).** Left wired
  (`MithrilPartialSyncOverlay.tsx:179-180`); their removal is task-ux-404, not this task.
