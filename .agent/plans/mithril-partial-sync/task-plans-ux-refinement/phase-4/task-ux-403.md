# task-ux-403 — Stage/code-specific failure copy and distinct cancelled-vs-failed copy

- **Phase:** phase-4 (Renderer Progress And Error Overlay Honesty)
- **Implements:** PRD **D5b** (stage/code-specific failure copy, gap #6), **D5e** (cancelled ≠ failed copy, gap #32), and **D5 latest-drift UX / #17** (`PARTIAL_SYNC_LATEST_DRIFT` is a pre-cutover retriable error). Closes research-19 **gap #6** (generic copy structurally guaranteed), **gap #12 (UX)** (no raw/un-localized string as primary copy), **gap #17 (UX)** (latest-drift surfaced as retriable, not silent reset), **gap #32** (cancelled/failed byte-identical hints).
- **Planning status:** `approved`
- **Build status:** `completed`
- **Interaction mode:** `autonomous`
- **Single commit subject (planned):** `feat(mithril): task-ux-403 add partial-sync error-copy map and distinct cancelled copy to the overlay`

---

## Why now

task-ux-401 made the download readout truthful; task-ux-402 made the live overlay look alive. The remaining dishonesty is the **error frame**: every partial-sync failure shows one generic title+hint regardless of what actually broke.

- **gap #6 (structural):** the overlay ALWAYS passes an explicit `title`+`hint` into `<MithrilErrorView>` (`MithrilPartialSyncOverlay.tsx:129-138`), which short-circuits `MithrilErrorView`'s `ERROR_COPY_BY_STAGE` fallback (`MithrilErrorView.tsx:88-94`) before it is ever consulted — and that fallback map is keyed to the **bootstrap** `MithrilBootstrapErrorStage` vocabulary, not partial-sync codes anyway. The backend emits structured codes + a correctly-typed `MithrilPartialSyncErrorStage`, but the renderer never maps them. So the only fix is to compute partial-sync-specific copy **in the partial-sync layer** and pass it in as `title`+`hint`.
- **gap #32:** `cancelled` and `failed` share **byte-identical** recovery hints today (verified below). The user who *chose* to stop gets the same alarmed copy as a real failure.
- **gap #17 (UX):** `PARTIAL_SYNC_LATEST_DRIFT` (the verified snapshot advanced while preparing) has no user-facing surface decision; PRD D5/#17 fixes it as a **specific, retriable** message ("snapshot advanced while preparing; please retry"), not a silent reset.

This is the **error-copy layer** on top of task-ux-402's live affordances. It is renderer-only, changes **only title/hint COPY**, and does not touch which recovery actions render (lock #5 — those stay strictly from `allowedRecoveryActions`, owned by the overlay's existing `canRetry`/`canRestartNormally`/`canWipeAndFullSync` props; the dead `onWipeRetry`/`onDecline` prop removal is **task-ux-404**, not this task).

---

## Interaction mode + justification

**`autonomous`. `requiresUserDecision = false`.** This is **functional failure/recovery copy** governed entirely by ALREADY-LOCKED wording rules: verified-data wording **#4**, keep-it-simple **#34**, never-raw-JSON **#18**, latest-drift-retriable **#17**. It is not benefit-vs-waiting marketing copy (that was task-ux-302/304, which were `interactive_decision`). All copy is mechanical "what broke + how to continue" mapping with first-class EN+JA. No blocking decision exists that the locked rules + PRD D5b/D5e/D5#17 cannot resolve.

**One non-blocking tension recorded (NOT a blocker):** the user-memory vocabulary note prefers "Mithril sync" over "partial sync" in user-facing copy, but the **entire existing** partial-sync catalog (and the `MithrilPartialSyncOverlay.spec.tsx:46` heading assertion) uses "Mithril partial sync". This plan writes NEW bespoke copy in the cleaner "Mithril sync" voice and rewrites only the cancelled hint; it does **not** rename the existing generic titles/subtitle (a holistic vocab/JA pass is **task-ux-601**). Either reading ships correct copy, so this does not gate the task. See Open questions.

---

## Scope

1. **A pure, unit-testable resolver** `resolvePartialSyncErrorCopy(status, error) -> { title, hint }` in a new module `partialSyncErrorCopy.ts`, keyed **error.code first → `MithrilPartialSyncErrorStage` → generic**, returning `react-intl` `MessageDescriptor`s from `MithrilBootstrap.messages.ts`.
2. **Bespoke copy for the 5 live codes** (EN+JA): `PARTIAL_SYNC_NO_CERTIFIED_RANGE`, `PARTIAL_SYNC_LATEST_DRIFT` (retriable), `PARTIAL_SYNC_STAGED_DB_INVALID`, `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED`, `PARTIAL_SYNC_CONVERSION_FAILED`. Everything else falls back to generic failed/cancelled copy.
3. **Distinct, calmer `cancelled` copy** (rewrite the byte-identical `cancelled.hint`).
4. **Wire the resolver into `MithrilPartialSyncOverlay`** — replace the binary cancelled-vs-failed `title`/`hint` at `MithrilPartialSyncOverlay.tsx:129-138` with `resolvePartialSyncErrorCopy(status, error)`.
5. **Add a `MithrilPartialSyncErrorCode` union** to `mithril-partial-sync.types.ts` (the exact 5 LIVE code strings) so the resolver map is type-checked for completeness and the codes are documented in the contract.

### Non-goals (do NOT do here)

- **No** change to which recovery actions render (lock **#5**). `canRetry`/`canRestartNormally`/`canWipeAndFullSync` and the `actions[]` array in the overlay stay exactly as-is. Only `title`/`hint` change.
- **No** removal of `MithrilErrorView`'s dead `onWipeRetry`/`onDecline` props — that is **task-ux-404** (gap #33). They stay wired (`MithrilPartialSyncOverlay.tsx:179-180`).
- **No** change to `MithrilErrorView`'s `ERROR_COPY_BY_STAGE` (bootstrap-keyed) or its title/hint fallback logic — the empty-DB bootstrap flow depends on it (do-not-regress invariant). `MithrilErrorView` needs **no functional edit**; it already honors the explicit `title`/`hint` we pass.
- **No** `hideAction`/Cancel-visibility change (that is task-ux-404/D5c), **no** defensive-Quit fallback (D5d/task-ux-404), **no** store/IPC/backend change, **no** new error codes.
- **No** fix to the confirmation-surface start-rejection raw string (`MithrilPartialSyncConfirmation.tsx:74` / `MithrilPartialSyncStore.startFromConfirmation`). That is a **different surface** not in this task's targetPaths; gap #12 is addressed **here only insofar as** the overlay's primary copy (title/hint) is never derived from a raw string. See Open questions.

---

## Dependencies

- JSON `dependencies: []`. No hard dependency. Builds naturally on task-ux-402 (shipped) but does not require its changes. The overlay's `error` prop, `MithrilPartialSyncError` type, and `MithrilErrorView` title/hint plumbing already exist.

---

## Research / docs / workflows / skills consulted

- PRD `mithril-partial-sync-ux-refinement-prd.md`: **D5(b)** lines 299-305 (error-copy map keyed by `error.code` / `MithrilPartialSyncErrorStage`, 5 codes, generic fallback), **D5(e)** lines 319-320 (cancelled ≠ failed, byte-identical today), **D5 latest-drift** lines 333-335 (#17 retriable, not silent reset), lock callout lines 337-339 (render recovery strictly from `allowedRecoveryActions` #5), components-impact line 900, acceptance line 813.
- research-19 `19-ux-refinement-state-and-gaps.md`: **gap #6** (line 133, structurally-guaranteed generic copy; appendix line 371-373), **gap #12** (line 139, thin/un-localized start-rejection), **gap #17** (line 144, latest-drift UX undefined), **gap #32** (line 159, identical hints), **gap #33** (line 160, dead `onWipeRetry`/`onDecline` — owned by 404).
- prompt `prompt-ux-refinement.md` — locked boundary **#18** (no raw mithril-client JSON in UI copy; map structured codes → localized copy), **#11** (do not regress empty-chain bootstrap), "smallest truthful change, reuse seams".
- **task-ux-402** four docs — format template + the Node v24 dart-sass / identity-obj-proxy jest sidecar env workaround + i18n process (`defineMessages` `!!!` defaults → `yarn i18n:extract` writes only `translations/messages.json` → hand-edit `en-US.json`/`ja-JP.json` → `yarn i18n:check` regenerates `defaultMessages.json`).
- Workflows: `.agent/workflows/frontend.md`, `.agent/workflows/test.md`.

---

## Live-code verification (verified against the working tree 2026-06-26)

### Backend error codes + stages (LIVE — these are the map keys)

| Code (LIVE string) | Throw site | `stage` emitted | Notes |
|---|---|---|---|
| `PARTIAL_SYNC_NO_CERTIFIED_RANGE` | `mithrilPartialSyncPreflight.ts:152` | `preparing` | pre-cutover; restart-normal is the safe escape |
| `PARTIAL_SYNC_LATEST_DRIFT` | `MithrilPartialSyncService.ts:197-201` (const `PARTIAL_SYNC_LATEST_DRIFT_CODE` :60) | `preparing` | **retriable** (#17): snapshot advanced while preparing; chain data untouched |
| `PARTIAL_SYNC_STAGED_DB_INVALID` | `mithrilPartialSyncStaging.ts:60/63/70/88/91/102/103/122` (const `PARTIAL_SYNC_STAGED_DB_INVALID_CODE`, preflight :6-7) | `verifying` (staged download) **and** `installing` (converted output) | code disambiguates — that is why **code wins over stage** |
| `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` | `MithrilPartialSyncService.ts:479-485` | `downloading` **or** `verifying` (whichever phase the command ran in) | — |
| `PARTIAL_SYNC_CONVERSION_FAILED` | `MithrilPartialSyncService.ts:497-501` | `converting` | — |

> **PARTIAL_SYNC_ prefix discrepancy (RESOLVED in favor of LIVE).** The tasks JSON names the last two codes `DOWNLOAD_COMMAND_FAILED` and `CONVERSION_FAILED` **without** the `PARTIAL_SYNC_` prefix. The LIVE codes are `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` and `PARTIAL_SYNC_CONVERSION_FAILED`. **Key the map on the exact LIVE strings.** Recorded in `task-ux-403-research.md`.

### Renderer seams (LIVE)

| Anchor | Live finding | Status |
|---|---|---|
| Overlay binary title/hint | `MithrilPartialSyncOverlay.tsx:129-138` — `title=intl.formatMessage(status==='cancelled' ? partialSyncCancelledTitle : partialSyncFailedTitle)`, `hint=` same binary on `…CancelledHint`/`…FailedHint` | CONFIRMED — this is the block to replace |
| Overlay error type | `error?: MithrilPartialSyncError \| null` (`MithrilPartialSyncOverlay.tsx:24`), passed as `error={error as any}` to `MithrilErrorView` (L127) | CONFIRMED |
| `MithrilErrorView` honors explicit title/hint | `MithrilErrorView.tsx:88-94,114,117` — uses provided `title`/`hint`; `ERROR_COPY_BY_STAGE` fallback only when `hint === undefined` | CONFIRMED — **no edit needed**; passing defined title/hint short-circuits the bootstrap-keyed fallback |
| `MithrilErrorView` still renders `error.message` | `MithrilErrorView.tsx:116` (`<p>{error.message}</p>`) + collapsible details L139-141 | CONFIRMED — **secondary diagnostic text**, not the primary copy; see #18 note below |
| `cancelled.hint` == `failed.hint` (gap #32) | `MithrilBootstrap.messages.ts:340-341` and `:350-353` are byte-identical; `en-US.json:351` == `:353`; `ja-JP.json:351` == `:353` | CONFIRMED byte-identical in both catalogs |
| `MithrilPartialSyncError` type | `mithril-partial-sync.types.ts:32-37` (`{ message; code?; logPath?; stage? }`), `MithrilPartialSyncErrorStage` :22-30 | CONFIRMED |
| Existing partial-sync error keys | `MithrilBootstrap.messages.ts:332-371` (`partialSyncFailedTitle/Hint`, `partialSyncCancelledTitle/Hint`, retry/restartNormally/wipeAndFullSync labels) | CONFIRMED — reuse the generic + cancelled keys |
| Bootstrap error-stage copy format to mirror | `MithrilBootstrap.messages.ts:130-160` (`errorDownloadTitle/Hint`, `errorVerifyTitle/Hint`, `errorConvertTitle/Hint`) — `!!!`-prefixed, with `description` | CONFIRMED — mirror this exact shape |

---

## Locked invariants this change MUST NOT break (inline)

- **#18 — never route raw mithril-client JSON into UI copy.** The resolver maps the structured `error.code`/`stage` to localized `MessageDescriptor`s; the **primary copy (title + hint) is ALWAYS a localized message**, never `error.message`. The pre-existing secondary `<p>{error.message}</p>` and the collapsible technical-details body (`MithrilErrorView.tsx:116,139-141`) still show the backend message verbatim — this is the diagnostic detail surface, is shared with bootstrap, is **pre-existing and out of scope**, and the backend messages at these sites are English sentences (not raw client JSON). The lock #18 requirement (no raw string as primary copy) is satisfied by the title/hint guarantee.
- **#4 — preserve "verified … chain data … catch up faster" wording where relevant.** The bespoke hints use "verified … chain data" consistently (e.g. staged-db-invalid: "did not match the expected verified chain data"; latest-drift: "verified snapshot"). Do not invent behind-ness/sync-% figures.
- **#34 — keep copy simple.** No IOG-key / ancillary / no-ancillary nuance. One short sentence of cause + one short "how to continue" pointer.
- **#17 — `PARTIAL_SYNC_LATEST_DRIFT` is a pre-cutover (`preparing`) RETRIABLE error.** Its copy says the verified snapshot advanced while preparing and to retry; chain data was not changed. Not a silent reset.
- **#5 — recovery actions render STRICTLY from `allowedRecoveryActions`.** This task changes only `title`/`hint`. The `actions[]` array (built from `canRetry`/`canRestartNormally`/`canWipeAndFullSync`) is **untouched**. Hints therefore must NOT promise a specific button that may not render — they say "choose how to continue below" / "use one of the options below" (generic), except `latest-drift` whose retriable nature (#17 guarantees retry is allowed) lets it name "Retry Mithril sync".
- **Do not regress empty-chain bootstrap.** `MithrilErrorView` is shared; bootstrap relies on the `ERROR_COPY_BY_STAGE` fallback when no explicit hint is passed. We add NO edit to `MithrilErrorView` and the new resolver is **only** called by `MithrilPartialSyncOverlay`. Bootstrap's `MithrilBootstrap` path passes its own copy and is never routed through `partialSyncErrorCopy.ts`.

---

## Implementation approach — ordered, mechanical steps

> Paths relative to repo root `/workspaces/mithril-partial-sync-ux`. Line anchors are LIVE.

### Step 1 — Add the error-code union to the contract (`source/common/types/mithril-partial-sync.types.ts`)

After `MithrilPartialSyncErrorStage` (ends L30) and before `MithrilPartialSyncError` (L32), add the exact LIVE code strings:
```ts
export type MithrilPartialSyncErrorCode =
  | 'PARTIAL_SYNC_NO_CERTIFIED_RANGE'
  | 'PARTIAL_SYNC_LATEST_DRIFT'
  | 'PARTIAL_SYNC_STAGED_DB_INVALID'
  | 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
  | 'PARTIAL_SYNC_CONVERSION_FAILED';
```
Leave `MithrilPartialSyncError.code` as the broad `code?: string` (the backend may emit other codes; the union documents only the mapped ones). Do not narrow `code` to the union (would break the broad contract).

### Step 2 — Add the 10 new + rewrite 1 message (`source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`)

2a. **Rewrite the cancelled hint default** (L348-353) so cancelled is calmer/distinct from failed (gap #32). Replace `partialSyncCancelledHint.defaultMessage`:
```ts
  partialSyncCancelledHint: {
    id: 'loading.mithrilPartialSync.error.cancelled.hint',
    defaultMessage:
      '!!!Mithril sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.',
    description:
      'Calmer hint shown when the user cancelled Mithril partial sync (distinct from the failed hint).',
  },
```
Leave `partialSyncFailedHint` (L337-342) and both titles (L332-336, L343-347) **unchanged**.

2b. **Add 10 new entries** (5 title + 5 hint), grouped right after `partialSyncCancelledHint`, mirroring the `errorDownload*`/`errorVerify*` shape (L130-160), `!!!`-prefixed:
```ts
  partialSyncErrorNoCertifiedRangeTitle: {
    id: 'loading.mithrilPartialSync.error.noCertifiedRange.title',
    defaultMessage: '!!!No verified Mithril snapshot is available yet',
    description:
      'Title shown when PARTIAL_SYNC_NO_CERTIFIED_RANGE is emitted (no certified snapshot for the current chain position).',
  },
  partialSyncErrorNoCertifiedRangeHint: {
    id: 'loading.mithrilPartialSync.error.noCertifiedRange.hint',
    defaultMessage:
      '!!!Daedalus could not find a verified Mithril snapshot for your current chain position. Choose how to continue below — you can keep syncing on your existing chain data.',
    description: 'Hint shown for PARTIAL_SYNC_NO_CERTIFIED_RANGE.',
  },
  partialSyncErrorLatestDriftTitle: {
    id: 'loading.mithrilPartialSync.error.latestDrift.title',
    defaultMessage: '!!!The verified Mithril snapshot moved on',
    description:
      'Title shown when PARTIAL_SYNC_LATEST_DRIFT is emitted (a newer verified snapshot appeared while preparing).',
  },
  partialSyncErrorLatestDriftHint: {
    id: 'loading.mithrilPartialSync.error.latestDrift.hint',
    defaultMessage:
      '!!!A newer verified snapshot became available while Daedalus was preparing. Retry Mithril sync to use the refreshed snapshot — your chain data was not changed.',
    description:
      'Retriable hint shown for PARTIAL_SYNC_LATEST_DRIFT (pre-cutover; not a silent reset).',
  },
  partialSyncErrorStagedDbInvalidTitle: {
    id: 'loading.mithrilPartialSync.error.stagedDbInvalid.title',
    defaultMessage: '!!!The Mithril snapshot could not be verified',
    description:
      'Title shown when PARTIAL_SYNC_STAGED_DB_INVALID is emitted (staged output incomplete or invalid).',
  },
  partialSyncErrorStagedDbInvalidHint: {
    id: 'loading.mithrilPartialSync.error.stagedDbInvalid.hint',
    defaultMessage:
      '!!!The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_STAGED_DB_INVALID.',
  },
  partialSyncErrorDownloadFailedTitle: {
    id: 'loading.mithrilPartialSync.error.downloadFailed.title',
    defaultMessage: '!!!Downloading the Mithril snapshot failed',
    description:
      'Title shown when PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED is emitted.',
  },
  partialSyncErrorDownloadFailedHint: {
    id: 'loading.mithrilPartialSync.error.downloadFailed.hint',
    defaultMessage:
      '!!!Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED.',
  },
  partialSyncErrorConversionFailedTitle: {
    id: 'loading.mithrilPartialSync.error.conversionFailed.title',
    defaultMessage: '!!!Preparing the Mithril snapshot failed',
    description: 'Title shown when PARTIAL_SYNC_CONVERSION_FAILED is emitted.',
  },
  partialSyncErrorConversionFailedHint: {
    id: 'loading.mithrilPartialSync.error.conversionFailed.hint',
    defaultMessage:
      '!!!Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_CONVERSION_FAILED.',
  },
```

### Step 3 — Create the pure resolver (`source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`)

New module. Returns `react-intl` `MessageDescriptor`s (NO `intl` dependency → trivially unit-testable; the overlay formats). Three-tier resolution **code → stage → generic**, with `cancelled` short-circuited to the calmer copy:
```ts
import type { MessageDescriptor } from 'react-intl';
import messages from './MithrilBootstrap.messages';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorCode,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatus,
} from '../../../../../common/types/mithril-partial-sync.types';

export type PartialSyncErrorCopy = {
  title: MessageDescriptor;
  hint: MessageDescriptor;
};

const NO_CERTIFIED_RANGE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorNoCertifiedRangeTitle,
  hint: messages.partialSyncErrorNoCertifiedRangeHint,
};
const LATEST_DRIFT: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorLatestDriftTitle,
  hint: messages.partialSyncErrorLatestDriftHint,
};
const STAGED_DB_INVALID: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorStagedDbInvalidTitle,
  hint: messages.partialSyncErrorStagedDbInvalidHint,
};
const DOWNLOAD_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorDownloadFailedTitle,
  hint: messages.partialSyncErrorDownloadFailedHint,
};
const CONVERSION_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorConversionFailedTitle,
  hint: messages.partialSyncErrorConversionFailedHint,
};

const FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncFailedTitle,
  hint: messages.partialSyncFailedHint,
};
const CANCELLED: PartialSyncErrorCopy = {
  title: messages.partialSyncCancelledTitle,
  hint: messages.partialSyncCancelledHint,
};

// 1st tier — exact backend code (wins; code disambiguates a code shared across stages).
const COPY_BY_CODE: Record<MithrilPartialSyncErrorCode, PartialSyncErrorCopy> = {
  PARTIAL_SYNC_NO_CERTIFIED_RANGE: NO_CERTIFIED_RANGE,
  PARTIAL_SYNC_LATEST_DRIFT: LATEST_DRIFT,
  PARTIAL_SYNC_STAGED_DB_INVALID: STAGED_DB_INVALID,
  PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED: DOWNLOAD_FAILED,
  PARTIAL_SYNC_CONVERSION_FAILED: CONVERSION_FAILED,
};

// 2nd tier — stage, for a code-less failure at a meaningful phase (reuses the same descriptors,
// no extra i18n keys). preparing/finalizing/starting-node/stopping-node intentionally omitted
// (ambiguous or no bespoke copy) → fall through to generic.
const COPY_BY_STAGE: Partial<
  Record<MithrilPartialSyncErrorStage, PartialSyncErrorCopy>
> = {
  downloading: DOWNLOAD_FAILED,
  verifying: STAGED_DB_INVALID,
  converting: CONVERSION_FAILED,
  installing: STAGED_DB_INVALID,
};

export function resolvePartialSyncErrorCopy(
  status: MithrilPartialSyncStatus,
  error?: MithrilPartialSyncError | null
): PartialSyncErrorCopy {
  if (status === 'cancelled') {
    return CANCELLED;
  }
  const byCode = error?.code
    ? COPY_BY_CODE[error.code as MithrilPartialSyncErrorCode]
    : undefined;
  if (byCode) {
    return byCode;
  }
  const byStage = error?.stage ? COPY_BY_STAGE[error.stage] : undefined;
  if (byStage) {
    return byStage;
  }
  return FAILED;
}
```
**Design notes:** (i) `cancelled` short-circuits first so the user-stop case always gets the calmer copy even if a stray `error` rode along (locks gap #32 / D5e). (ii) code beats stage because `PARTIAL_SYNC_STAGED_DB_INVALID` is emitted at both `verifying` and `installing` — the code is the precise signal. (iii) the stage tier reuses the code descriptors (no new keys) so a code-less failed frame at download/verify/convert/install still gets relevant copy; unknown/ambiguous → generic `FAILED`.

### Step 4 — Wire the resolver into the overlay (`source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`)

4a. Add the import near the other local imports (after L16 `import MithrilProgressView …`):
```ts
import { resolvePartialSyncErrorCopy } from './partialSyncErrorCopy';
```
4b. In the function body, compute the copy once (e.g. right after `const activeHeadingId = …`, ~L73):
```ts
  const errorCopy = resolvePartialSyncErrorCopy(status, error);
```
4c. Replace the binary `title`/`hint` (**L129-138**) on `<MithrilErrorView>`:
```tsx
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
```
Leave everything else on `<MithrilErrorView>` (the `actions={[…]}` array, `onWipeRetry`, `onDecline`, `error`, `onOpenExternalLink`) **unchanged** (locks #5, and 404 owns the dead-prop removal).

### Step 5 — i18n catalogs

5a. `yarn i18n:extract` → regenerates `translations/messages.json` with the 10 new ids (and the changed cancelled-hint default). This is the only file extract writes.

5b. Hand-edit `source/renderer/app/i18n/locales/en-US.json` — **change** the cancelled hint and **add** 10 first-class EN strings (no `!!!`), in alphabetical id order among the `loading.mithrilPartialSync.error.*` keys (neighbors at en-US.json:351-357):
```
"loading.mithrilPartialSync.error.cancelled.hint": "Mithril sync was stopped before it finished. Your existing chain data is unchanged — choose how to continue below.",
"loading.mithrilPartialSync.error.conversionFailed.hint": "Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below.",
"loading.mithrilPartialSync.error.conversionFailed.title": "Preparing the Mithril snapshot failed",
"loading.mithrilPartialSync.error.downloadFailed.hint": "Daedalus could not finish downloading and verifying the Mithril snapshot. Check your internet connection, then choose how to continue below.",
"loading.mithrilPartialSync.error.downloadFailed.title": "Downloading the Mithril snapshot failed",
"loading.mithrilPartialSync.error.latestDrift.hint": "A newer verified snapshot became available while Daedalus was preparing. Retry Mithril sync to use the refreshed snapshot — your chain data was not changed.",
"loading.mithrilPartialSync.error.latestDrift.title": "The verified Mithril snapshot moved on",
"loading.mithrilPartialSync.error.noCertifiedRange.hint": "Daedalus could not find a verified Mithril snapshot for your current chain position. Choose how to continue below — you can keep syncing on your existing chain data.",
"loading.mithrilPartialSync.error.noCertifiedRange.title": "No verified Mithril snapshot is available yet",
"loading.mithrilPartialSync.error.stagedDbInvalid.hint": "The downloaded snapshot was incomplete or did not match the expected verified chain data. Choose how to continue below.",
"loading.mithrilPartialSync.error.stagedDbInvalid.title": "The Mithril snapshot could not be verified",
```

5c. Hand-edit `source/renderer/app/i18n/locales/ja-JP.json` — same slots, first-class JA (no `!!!`):
```
"loading.mithrilPartialSync.error.cancelled.hint": "Mithril同期は完了前に停止されました。既存のチェーンデータは変更されていません。下のオプションから続行方法を選択してください。",
"loading.mithrilPartialSync.error.conversionFailed.hint": "検証済みスナップショットをダウンロードしましたが、使用するための準備ができませんでした。下のオプションから続行方法を選択してください。",
"loading.mithrilPartialSync.error.conversionFailed.title": "Mithrilスナップショットの準備に失敗しました",
"loading.mithrilPartialSync.error.downloadFailed.hint": "Mithrilスナップショットのダウンロードと検証を完了できませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。",
"loading.mithrilPartialSync.error.downloadFailed.title": "Mithrilスナップショットのダウンロードに失敗しました",
"loading.mithrilPartialSync.error.latestDrift.hint": "準備中に新しい検証済みスナップショットが利用可能になりました。Mithril同期を再試行して更新されたスナップショットを使用してください。チェーンデータは変更されていません。",
"loading.mithrilPartialSync.error.latestDrift.title": "検証済みMithrilスナップショットが更新されました",
"loading.mithrilPartialSync.error.noCertifiedRange.hint": "現在のチェーン位置に対応する検証済みMithrilスナップショットが見つかりませんでした。下のオプションから続行方法を選択してください。既存のチェーンデータで同期を続けることもできます。",
"loading.mithrilPartialSync.error.noCertifiedRange.title": "利用可能な検証済みMithrilスナップショットがまだありません",
"loading.mithrilPartialSync.error.stagedDbInvalid.hint": "ダウンロードしたスナップショットが不完全か、期待される検証済みチェーンデータと一致しませんでした。下のオプションから続行方法を選択してください。",
"loading.mithrilPartialSync.error.stagedDbInvalid.title": "Mithrilスナップショットを検証できませんでした",
```

5d. `yarn i18n:check` (or `yarn i18n:manage`) → regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (and may sort the catalogs). **Stage** the regenerated `defaultMessages.json` + `translations/messages.json` alongside `en-US.json`/`ja-JP.json` (do not hand-edit `defaultMessages.json`). `git diff --stat` should show `defaultMessages.json` gaining the 10 new ids + the changed cancelled-hint value.

### Step 6 — Tests

6a. **New pure-helper spec** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts` (no rendering, no scss import → **no jest sidecar needed**). Assert by `MessageDescriptor.id`:
```ts
import { resolvePartialSyncErrorCopy } from './partialSyncErrorCopy';

const err = (code?: string, stage?: any) =>
  ({ message: 'raw backend string', code, stage } as any);

describe('resolvePartialSyncErrorCopy', () => {
  it.each([
    ['PARTIAL_SYNC_NO_CERTIFIED_RANGE', 'loading.mithrilPartialSync.error.noCertifiedRange.title'],
    ['PARTIAL_SYNC_LATEST_DRIFT', 'loading.mithrilPartialSync.error.latestDrift.title'],
    ['PARTIAL_SYNC_STAGED_DB_INVALID', 'loading.mithrilPartialSync.error.stagedDbInvalid.title'],
    ['PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED', 'loading.mithrilPartialSync.error.downloadFailed.title'],
    ['PARTIAL_SYNC_CONVERSION_FAILED', 'loading.mithrilPartialSync.error.conversionFailed.title'],
  ])('maps code %s to bespoke copy', (code, titleId) => {
    expect(resolvePartialSyncErrorCopy('failed', err(code)).title.id).toBe(titleId);
  });

  it('latest-drift hint is the retriable message', () => {
    expect(resolvePartialSyncErrorCopy('failed', err('PARTIAL_SYNC_LATEST_DRIFT')).hint.id)
      .toBe('loading.mithrilPartialSync.error.latestDrift.hint');
  });

  it('falls back to generic failed copy for an unknown code', () => {
    expect(resolvePartialSyncErrorCopy('failed', err('SOMETHING_ELSE')).title.id)
      .toBe('loading.mithrilPartialSync.error.failed.title');
  });

  it('uses the stage tier when no code matches', () => {
    expect(resolvePartialSyncErrorCopy('failed', err(undefined, 'converting')).title.id)
      .toBe('loading.mithrilPartialSync.error.conversionFailed.title');
  });

  it('returns calmer cancelled copy for cancelled status (distinct from failed)', () => {
    const cancelled = resolvePartialSyncErrorCopy('cancelled', null);
    const failed = resolvePartialSyncErrorCopy('failed', null);
    expect(cancelled.title.id).toBe('loading.mithrilPartialSync.error.cancelled.title');
    expect(cancelled.hint.id).not.toBe(failed.hint.id);
  });

  it('never returns error.message as the copy (no raw JSON in primary copy)', () => {
    const copy = resolvePartialSyncErrorCopy('failed', err('PARTIAL_SYNC_LATEST_DRIFT'));
    expect(copy.title.defaultMessage).not.toContain('raw backend string');
    expect(copy.hint.defaultMessage).not.toContain('raw backend string');
  });
});
```

6b. **Edit `MithrilPartialSyncOverlay.spec.tsx`** — add render-level coverage (this file already runs with the identity-obj-proxy scss sidecar):
```tsx
  it('shows bespoke copy for a mapped error code and never the raw backend message as the title', () => {
    renderComponent({
      status: 'failed',
      error: { message: '{"raw":"mithril-client json"}', code: 'PARTIAL_SYNC_LATEST_DRIFT', stage: 'preparing' },
    });
    expect(
      screen.getByRole('heading', { name: /verified mithril snapshot moved on/i })
    ).toBeInTheDocument();
    expect(screen.getByText(/retry mithril sync to use the refreshed snapshot/i)).toBeInTheDocument();
    // raw message still appears only as secondary diagnostic text, never as the heading
    expect(
      screen.queryByRole('heading', { name: /mithril-client json/i })
    ).not.toBeInTheDocument();
  });

  it('gives cancelled a calmer hint distinct from failed', () => {
    const { unmount } = renderComponent({ status: 'cancelled', error: null });
    const cancelledHint = screen.getByText(/was stopped before it finished/i);
    expect(cancelledHint).toBeInTheDocument();
    unmount();
    renderComponent({ status: 'failed', error: null });
    expect(screen.queryByText(/was stopped before it finished/i)).not.toBeInTheDocument();
  });
```
The existing "ships polished runtime strings without placeholder markers" test (`MithrilPartialSyncOverlay.spec.tsx:149-159`) automatically extends to the new `loading.mithrilPartialSync.error.*` keys — it asserts no `!!!` in en-US **and** ja-JP — so it is the catalog completeness guard. Re-run it.

### Step 7 — `MithrilErrorView` — verify only (no edit)

Confirm `MithrilErrorView.tsx:88-94` still honors the explicit `title`/`hint` we pass (it does). No code change. The targetPaths listing of `MithrilErrorView.tsx` is satisfied by verification, mirroring task-ux-402's `MithrilStepIndicator.scss` reconciliation. Do **not** remove `onWipeRetry`/`onDecline` (task-ux-404).

---

## Acceptance criteria (from the tasks JSON `acceptance`)

- Failures show stage/code-specific copy via a partial-sync error-copy map. → resolver + 5 bespoke code entries wired into the overlay.
- At least the five enumerated codes have bespoke copy; others fall back gracefully. → `COPY_BY_CODE` (5) + stage tier + generic `FAILED`.
- `cancelled` and `failed` have distinct copy. → rewritten `cancelled.hint`; resolver short-circuits `cancelled`.
- Latest-drift surfaces as a specific retriable error. → `PARTIAL_SYNC_LATEST_DRIFT` bespoke retriable hint (#17).

### testCases (from the tasks JSON)

- Each enumerated error code maps to bespoke title/hint → `partialSyncErrorCopy.spec.ts` `it.each` (5 codes).
- Unknown codes fall back to generic failed/cancelled copy → helper spec unknown-code + cancelled tests.
- `PARTIAL_SYNC_LATEST_DRIFT` shows a specific retriable message → helper spec latest-drift-hint test + overlay render test.
- cancelled copy differs from failed copy → helper spec distinct-id test + overlay render test.
- No raw client JSON reaches UI copy → helper spec "never returns error.message" + overlay "never the raw message as the title".

---

## Verification plan (exact commands, from repo root)

```bash
cd /workspaces/mithril-partial-sync-ux
yarn i18n:extract            # regenerate translations/messages.json with the 10 new ids + changed cancelled hint
yarn i18n:check              # validate ids exist in en-US/ja-JP + ordering; regenerates defaultMessages.json
git status --porcelain source/renderer/app/i18n/locales/defaultMessages.json   # → modified; STAGE it
grep -c 'mithrilPartialSync.error.noCertifiedRange\|latestDrift\|stagedDbInvalid\|downloadFailed\|conversionFailed' \
  source/renderer/app/i18n/locales/defaultMessages.json   # → 10
# gap #32 proof: cancelled.hint now differs from failed.hint in BOTH catalogs
grep 'error.cancelled.hint\|error.failed.hint' source/renderer/app/i18n/locales/en-US.json   # → two DIFFERENT values
grep 'error.cancelled.hint\|error.failed.hint' source/renderer/app/i18n/locales/ja-JP.json   # → two DIFFERENT values
grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json   # → no NEW !!! for the new error keys
node_modules/.bin/tsc --noEmit -p .   # authoritative TS gate (allow up to 600s)
yarn lint
node_modules/.bin/jest \
  source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx
yarn prettier:check
```

> **KNOWN ENV WORKAROUNDS (Node v24, recorded by task-ux-301/303/304/401/402):**
> - `yarn compile`'s `typedef:sass` precompile hook crashes under Node v24 dart-sass. Run the authoritative TS gate directly: `node_modules/.bin/tsc --noEmit -p .`. **This task touches NO scss**, so a scss/`.scss.d.ts` error is the env quirk, not this change — regenerate only if a touched component imports a stale `.scss.d.ts` (none here; `partialSyncErrorCopy.ts` imports no scss).
> - `MithrilPartialSyncOverlay.spec.tsx` imports `MithrilBootstrap.scss`, so it needs the gitignored `identity-obj-proxy` jest sidecar (`--config <scratchpad>/jest.scss-override.js`, NOT staged) — the same workaround as 401/402. The **new** `partialSyncErrorCopy.spec.ts` imports NO scss, so it runs with the committed jest config directly.

---

## Risks / open questions

- **Risk — over-promising actions in hints (lock #5).** Mitigated: bespoke hints say "choose how to continue below" / "use one of the options below" (generic), never naming a button that may not render — except `latest-drift`, which names "Retry Mithril sync" because #17 guarantees retry is allowed for that pre-cutover error. Reviewer should confirm no hint names a specific action other than retry-for-latest-drift.
- **Risk — bootstrap regression.** Mitigated structurally: the new resolver is imported and called **only** by `MithrilPartialSyncOverlay`; `MithrilErrorView` and its bootstrap-keyed `ERROR_COPY_BY_STAGE` are untouched. Re-running the overlay + helper specs + (optionally) `MithrilBootstrap.spec.tsx` confirms.
- **Open question (vocabulary, NON-BLOCKING).** New copy uses "Mithril sync" (per the locked user-memory vocab note: name it "Mithril Sync", never "partial sync"); the existing generic titles/subtitle still say "Mithril partial sync". A holistic vocab/JA rename across the catalog is **task-ux-601**. Both readings ship correct copy → not a blocker. Flagged so 601 picks up the existing strings.
- **Open question (gap #12 scope, NON-BLOCKING).** The confirmation-surface start-rejection raw string (`MithrilPartialSyncConfirmation.tsx:74` via `MithrilPartialSyncStore.startFromConfirmation`) is a **different surface** not in this task's targetPaths. This task closes #12 only for the **overlay**'s primary copy (title/hint never derived from a raw string). If QA wants the confirmation-surface string localized too, that is a separate change (D5b/D6) — recorded in research note.
- **Residual — `error.message` still shown as secondary text.** `MithrilErrorView` renders the backend message verbatim in a secondary `<p>` and the collapsible details. This is pre-existing diagnostic surface (shared with bootstrap) and out of scope; #18's "no raw string as primary copy" is satisfied by the title/hint guarantee.

---

## Required doc / research updates

- **No** `.agent/system/api-endpoints.md` change (renderer-only; no IPC/contract behavior change — the new `MithrilPartialSyncErrorCode` union is a renderer-facing type label, not a wire change).
- At completion: fill "Final outcome", set the JSON task `status: completed` (+ `completedAt`), and record durable findings in `task-ux-403-research.md` (the verified code↔stage table, the `PARTIAL_SYNC_` prefix discrepancy resolution, the code-beats-stage rationale, the cancelled-hint gap #32 fix, and the vocab/#12 residuals).

## Review-log paths

- Planning review: `task-plans-ux-refinement/phase-4/task-ux-403-plan-review.md`
- Implementation review: `task-plans-ux-refinement/phase-4/task-ux-403-impl-review.md`
- Research note: `task-plans-ux-refinement/phase-4/task-ux-403-research.md`

## Final outcome

Implemented as planned (2026-06-26). Added `MithrilPartialSyncErrorCode` to the contract; created the
pure `partialSyncErrorCopy.ts` resolver (cancelled → code → stage → generic) keyed on the 5 EXACT LIVE
code strings (incl. the `PARTIAL_SYNC_` prefix); added 10 bespoke EN+JA messages (latest-drift
retriable); rewrote the byte-identical `partialSyncCancelledHint` to calmer distinct copy (gap #32);
wired `resolvePartialSyncErrorCopy(status, error)` into `MithrilPartialSyncOverlay`'s `<MithrilErrorView>`
title/hint (no action/`onWipeRetry`/`onDecline` change — locks #5/404; `MithrilErrorView` needed no
edit). Regenerated `defaultMessages.json`/`translations/messages.json`.

**Verification:** `tsc --noEmit` EXIT 0; new `partialSyncErrorCopy.spec.ts` + overlay render tests
**21/21 PASS**; full `mithril-bootstrap/` suite **56/56 PASS** (no bootstrap regression); eslint 0
errors (pre-existing warnings only); prettier clean; gap #32 distinctness verified in en-US + ja-JP; no
new `!!!` markers.

**Deviations (2, both test-env only, documented in task-ux-403-impl-review.md):** (1) overlay spec
stubs react-polymorph `Link` (it resolves to `undefined` without a skin context and crashes the
collapsible details render when an `error.message` is present) — mirrors `DaedalusDiagnostics.spec.tsx`;
(2) the "raw message never a heading" assertion refined to "never the **level-1** title" because the
pre-existing `CollapsibleSection` legitimately renders the raw message as an `<h2>` diagnostic detail
header — lock #18 (no raw JSON as primary copy) is satisfied by the level-1 guarantee.

See `task-ux-403-impl-review.md` (Implementation entry) for full detail.
