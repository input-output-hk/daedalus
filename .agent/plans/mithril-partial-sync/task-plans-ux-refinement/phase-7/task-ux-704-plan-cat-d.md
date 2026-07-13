# task-ux-704 — CAT-D plan: Type-contract tightening (D-1…D-6)

> Per-CAT implementation doc for task-ux-704 (code-quality remediation wave). **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent:
> `task-ux-704.md` (traceability rows D6-narrowed, D9, D3-typing-only, E8, E9b/c, D7-optional).
> If this doc ever disagrees with live code, prefer live code, locate anchors by the QUOTED
> snippets (never by line number), and reconcile here. All line numbers below are approximate
> as of 2026-07-04 (verified against HEAD `7c9f7de20` + working tree) and are a reading aid only.

Everything in this CAT is a compile-time tightening: **no IPC channel names, no emitted status
shapes, no user-visible copy, and no runtime behavior may change** (wave gate #4; the only
sanctioned "shape" effects are the typed-only D6/D9 tightenings this doc implements).

## Sequencing position

Fourth section (order A → B → C → **D** → E → F → G → H). Hard dependency — **seam S2 (B↔D)**:
CAT-B has already

1. moved `class MithrilPartialSyncStageError` out of `MithrilPartialSyncService.ts` (pre-CAT-B
   home ~:111-125) into `source/main/mithril/mithrilErrors.ts`, alongside a new exported
   helper `createPartialSyncStageError(stage, message, code?)` (CAT-B doc, Step B-2.1), and
2. deleted the `PartialSyncStageErrorFactory` threading **entirely, with NO successor**
   (CAT-B doc, Steps B-2.2/B-2.3): the preflight/staging functions lost their
   `createStageError` parameter and now construct directly via `createPartialSyncStageError`
   imported from `./mithrilErrors`; `statRequiredPath` became
   `statRequiredPath(targetPath, message, stage = 'preparing', code?)`; the six service
   threading lambdas are gone (call sites shortened, B-2.4). The service KEEPS its
   `_createStageError` method byte-identical, now constructing the imported class.

D-1 is therefore written against that end state, not HEAD: it types the class, the
`createPartialSyncStageError` helper, `statRequiredPath`'s trailing `code` param, and the
service's `_createStageError` — there is no threading signature left to type. If the class
still lives inside `MithrilPartialSyncService.ts` when you start, CAT-B has not landed — stop
and escalate; do not apply D-1 at the old home.

Shared-file coordination (letter order prevents conflicts, but be aware):

- `MithrilPartialSyncSection.tsx` — D-5a edits Props `:29`; CAT-E (E-1) later deletes the
  adjacent `showConfirmationOnOpen` Props line and the `componentDidMount` branch. Touch only
  the line named in D-5a.
- `DaedalusDiagnosticsDialog.tsx` — D-5b edits `:13-18`; CAT-E (E-1) later deletes `:139`.
  Non-overlapping regions.
- CAT-H (last) re-locates its comment-inventory entries by quoted text; the one comment D-2
  rewrites (`mithril-partial-sync.types.ts` ~:90-93) and the story-fixture comment Step 1.7
  rewrites (`MithrilPartialSyncOverlay.stories.tsx` ~:136-139) are marked "handled by CAT-D"
  in `task-ux-704-plan-cat-h.md` (see the D-2 cross-reference note and Step 1.7).

## Anchor corrections from plan-authoring verification (2026-07-04)

1. **The "union closes cleanly" claim from the wave review is REFUTED.** The 13-member
   `MithrilPartialSyncErrorCode` union does NOT cover every `.code` write site. Five stage-error
   write sites pass literals absent from the union:
   - `mithrilPartialSyncPreflight.ts` ~:74-78 → `'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'`
   - `mithrilPartialSyncPreflight.ts` ~:89-93 → `'PARTIAL_SYNC_IMMUTABLE_INVALID'`
   - `mithrilPartialSyncPreflight.ts` ~:111-115 → `'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'`
   - `mithrilPartialSyncPreflight.ts` ~:133-137 → `'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'`
   - `mithrilPartialSyncStaging.ts` ~:33-37 → `'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN'`

   All five flow into `MithrilPartialSyncStageError.code` → `_buildError` → the wire, so they
   are real wire codes today. Resolution (Step 1.1/1.2): **append the five codes to the union**
   (append-only, the pattern task-ux-703 already sanctioned for this union) and add five
   entries to the total-`Record` copy map, all mapping to the existing generic `FAILED` pair.
   This is provably copy-neutral: every one of the five is emitted with stage `'preparing'`,
   which `COPY_BY_STAGE` deliberately omits, so today they already fall through to `FAILED` —
   the explicit mapping renders the identical copy. No i18n message is added or changed.
2. `_buildError` opens at service ~:1336; the `code: error.code` copy the brief cited is the
   line at ~:1343 inside it.
3. The stale `certifiedEpoch` comment in `mithril-partial-sync.types.ts` spans ~:90-93 (the
   field itself is :94).
4. Only ONE site in the whole codebase writes `MithrilPartialSyncError.code` from a variable —
   `_buildError` (`code: error.code,`). Spec files use only valid union literals
   (`'PARTIAL_SYNC_STAGED_DB_INVALID'`, `'PARTIAL_SYNC_LATEST_DRIFT'`,
   `'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'`, `'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE'`), and no
   production code compares `.code` against a non-union literal — no spec or read-site compile
   fallout is expected.
5. **Storybook fixtures WILL break `yarn compile` unless handled** (tsc typechecks
   `storybook/**`): `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`
   annotates four fixtures as `MithrilPartialSyncError` with story-invented codes absent from
   the union — `'MITHRIL_PARTIAL_SYNC_CANCELLED'` (~:112), `'MITHRIL_PARTIAL_SYNC_VERIFY_FAILED'`
   (~:121), `'MITHRIL_PARTIAL_SYNC_NODE_START_FAILED'` (~:130), and
   `'PARTIAL_SYNC_FINALIZE_FAILED'` (~:169, `finalizingError`). Step 1.7 handles them.

## Locked invariants that constrain CAT-D

- **CRITICAL LIMIT — the code-as-MESSAGE channel stays `string`-typed.** Several service sites
  intentionally throw the code as the Error MESSAGE, because Electron IPC rejections strip
  custom properties (only `message` survives the wire):

  ```ts
  throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);      // service ~:188
  throw new Error(PARTIAL_SYNC_CANCEL_NOT_ALLOWED_CODE);   // service ~:370
  throw new Error(PARTIAL_SYNC_START_NOT_ALLOWED_CODE);    // service ~:589, ~:602
  throw new Error(PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE); // service ~:841, ~:849
  ```

  The renderer-side lookup for that channel,
  `resolvePartialSyncErrorCopyByCode(code: string)` (`partialSyncErrorCopy.ts` ~:111-114), keys
  on **message text** and MUST keep its `string` parameter and its internal
  `as MithrilPartialSyncErrorCode` cast. Do NOT "finish the job" by typing that parameter as
  the union or deleting the `:114` cast — the value arriving there is a rejection `message`,
  which the type system cannot promise is a code. Only the `:91` cast (the `error.code`
  property path) is deleted.
- Wave behavior gate: the five new `COPY_BY_CODE` entries must map to `FAILED` and nothing
  else (Step 1.2) — any other mapping would change rendered copy.
- Comments: only where the logic isn't self-evident, 1-3 plain lines stating the why; never
  task/finding IDs (D-1, CAT-D, D9…) in source comments or test titles.
- `noImplicitAny` is `false` (tsconfig :80) — the compiler will not catch missed spots; the
  file-by-file steps below are the checklist.

## i18n

**No new message ids, no defaultMessage changes, no locale-file edits.** Step 1.2 adds map
entries in `partialSyncErrorCopy.ts` that reference the EXISTING `FAILED` descriptor pair
(`messages.partialSyncFailedTitle` / `messages.partialSyncFailedHint`) — no descriptor is
created, renamed, or reworded. `en-US.json` / `ja-JP.json` / defaultMessages stay untouched.

---

# D-1 — Error-code union: type `.code` end-to-end (narrowed scope)

**Defect:** `mithril-partial-sync.types.ts` declares the `MithrilPartialSyncErrorCode` union
(:33-46) but `MithrilPartialSyncError.code` is `code?: string` (:50), so the renderer casts the
value back at `partialSyncErrorCopy.ts:91` and the compiler cannot catch a typo'd code at any
write site.

### Step 1.1 — `source/common/types/mithril-partial-sync.types.ts`

(a) Locate the union tail:

```ts
  | 'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'
  | 'PARTIAL_SYNC_METADATA_UNAVAILABLE';
```

Replace with (append five members; union grows 13 → 18):

```ts
  | 'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'
  | 'PARTIAL_SYNC_METADATA_UNAVAILABLE'
  // Preflight/staging invariant failures — real wire codes with generic copy.
  | 'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'
  | 'PARTIAL_SYNC_IMMUTABLE_INVALID'
  | 'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'
  | 'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'
  | 'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN';
```

(b) Locate:

```ts
export type MithrilPartialSyncError = {
  message: string;
  code?: string;
```

Replace the `code` line with:

```ts
  code?: MithrilPartialSyncErrorCode;
```

### Step 1.2 — `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`

(a) `COPY_BY_CODE` is a **total** `Record<MithrilPartialSyncErrorCode, PartialSyncErrorCopy>`
(:54), so Step 1.1(a) forces five new entries. Locate:

```ts
    PARTIAL_SYNC_RECOVERY_NOT_ALLOWED: FAILED,
  };
```

Replace with:

```ts
    PARTIAL_SYNC_RECOVERY_NOT_ALLOWED: FAILED,
    // Preflight/staging invariant codes have no bespoke copy; they resolve to
    // the generic failure pair.
    PARTIAL_SYNC_MANAGED_CHAIN_INVALID: FAILED,
    PARTIAL_SYNC_IMMUTABLE_INVALID: FAILED,
    PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID: FAILED,
    PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE: FAILED,
    PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN: FAILED,
  };
```

(b) Delete the cast at ~:90-92. Locate:

```ts
  const byCode = error?.code
    ? COPY_BY_CODE[error.code as MithrilPartialSyncErrorCode]
    : undefined;
```

Replace with:

```ts
  const byCode = error?.code ? COPY_BY_CODE[error.code] : undefined;
```

(c) Leave `resolvePartialSyncErrorCopyByCode` (~:111-114) **completely untouched**, including
its `code: string` parameter and its `COPY_BY_CODE[code as MithrilPartialSyncErrorCode]` cast
(see the CRITICAL LIMIT above). The `MithrilPartialSyncErrorCode` import stays (used at :54
and :114).

### Step 1.3 — `source/main/mithril/mithrilErrors.ts` (class + helper, at their post-CAT-B home)

CAT-B (Step B-2.1) added both the class (moved verbatim) and the construction helper:

```ts
export class MithrilPartialSyncStageError extends Error {
  stage: MithrilPartialSyncErrorStage;
  code?: string;

  constructor(
    message: string,
    stage: MithrilPartialSyncErrorStage,
    code?: string
  ) {
```

```ts
export function createPartialSyncStageError(
  stage: MithrilPartialSyncErrorStage,
  message: string,
  code?: string
): MithrilPartialSyncStageError {
```

Change all THREE `code?: string` occurrences — the class field, the constructor parameter, AND
`createPartialSyncStageError`'s parameter — to `code?: MithrilPartialSyncErrorCode`. All three
are required: leaving the helper's param as `string` would pass `string` into the union-typed
constructor and fail compilation inside this very file. Add `MithrilPartialSyncErrorCode` to
the partial-sync type import CAT-B introduced here (CAT-B's B-2.1 block explicitly leaves
`code?: string` in place because "CAT-D owns the union tightening at this new home" — this is
that step).

### Step 1.4 — `source/main/mithril/mithrilPartialSyncPreflight.ts`

(a) Extend the type import. Locate:

```ts
import type { MithrilPartialSyncErrorStage } from '../../common/types/mithril-partial-sync.types';
```

Replace with:

```ts
import type {
  MithrilPartialSyncErrorCode,
  MithrilPartialSyncErrorStage,
} from '../../common/types/mithril-partial-sync.types';
```

(b) Verify `PartialSyncStageErrorFactory` is gone with no successor (CAT-B removed the
threading; this file's functions construct via the imported `createPartialSyncStageError`,
which Step 1.3 already typed). Nothing to edit here.

(c) Type the pass-through parameter. Locate the post-CAT-B signature:

```ts
export async function statRequiredPath(
  targetPath: string,
  message: string,
  stage: MithrilPartialSyncErrorStage = 'preparing',
  code?: string
```

and change `code?: string` to `code?: MithrilPartialSyncErrorCode`.

The direct `createPartialSyncStageError('preparing', '…', 'PARTIAL_SYNC_…')` calls in this
file need **no edits** — after Step 1.1(a) every literal they pass is a union member.

### Step 1.5 — `source/main/mithril/MithrilPartialSyncService.ts`

(a) Extend the type import. Locate:

```ts
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
```

Add `MithrilPartialSyncErrorCode,` to the list (alphabetical position after
`MithrilPartialSyncError`).

(b) Locate:

```ts
  _createStageError(
    stage: MithrilPartialSyncErrorStage,
    message: string,
    code?: string
  ): MithrilPartialSyncStageError {
```

Change `code?: string` to `code?: MithrilPartialSyncErrorCode`.

No other service edits: the six union-literal write sites (~:272 `PARTIAL_SYNC_LATEST_DRIFT`,
~:707 `'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'`, ~:723 `'PARTIAL_SYNC_CONVERSION_FAILED'`,
~:945/~:1128 `PARTIAL_SYNC_METADATA_UNAVAILABLE`, ~:1193
`PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`; HEAD line numbers) already pass union members (the
module consts at ~:66-75 carry literal types); the six threading lambdas were deleted by
CAT-B (B-2.4 shortened those call sites, keeping `_createStageError` itself — the
`createFailure: (message) => this._createStageError('converting', …)` lambda it retained
infers from context); and `_buildError`'s `code: error.code,` (~:1343) now flows union →
union without annotation. The six `throw new Error(PARTIAL_SYNC_*_CODE)` sites are untouched
by design (see CRITICAL LIMIT).

### Step 1.6 — `source/main/mithril/mithrilPartialSyncStaging.ts` — verify-only

No edits: post-CAT-B this file constructs directly via the imported
`createPartialSyncStageError` and calls `statRequiredPath` with trailing
`'verifying', PARTIAL_SYNC_STAGED_DB_INVALID_CODE` arguments — every code it passes is a
union member after Step 1.1(a) (`PARTIAL_SYNC_STAGED_DB_INVALID_CODE` and, at ~:36 in HEAD
numbering, `'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN'`). If `yarn compile` flags this file,
the post-CAT-B state diverges from B-2.3 — Escalation E1.

### Step 1.7 — `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

`yarn compile` typechecks `storybook/**`, and four fixtures here are annotated
`MithrilPartialSyncError` with story-invented codes outside the union (anchor correction 5).
Constraints that shape the fix:

- `MithrilErrorView` renders `error.code` verbatim inside the collapsible technical-details
  section (`{error?.code && (<div className={styles.errorCode}>{error.code}</div>)}`,
  ~:154-156) — so dropping a code removes that one line of story-visible technical-detail
  text (the resolved TITLE/HINT copy is unaffected, argued per fixture below).
- Once `COPY_BY_CODE` is total (Step 1.2a), `finalizingError`'s documented purpose —
  a code-tier MISS that exercises the generic `FAILED` fall-through — is inexpressible with
  any union code.

**Chosen fix: drop the `code` field from the four fixtures** (rejected alternative: a locally
widened fixture type such as `Omit<MithrilPartialSyncError, 'code'> & { code?: string }` —
it preserves the code lines but cannot be assigned to `StoryProps.error` /
`MithrilPartialSyncOverlay`'s `error` prop without new casts, colliding with CAT-F's
typed-stories work on this same file). Copy-neutrality of the drop, per fixture:

- `cancelledError` (~:110-117): status `'cancelled'` short-circuits to `CANCELLED` copy
  before any code lookup — code never influenced the copy.
- `restartAllowedError` (~:119-126): stage `'verifying'` resolves via `COPY_BY_STAGE` —
  its invented code missed `COPY_BY_CODE` already.
- `wipeOnlyError` (~:128-134): stage `'starting-node'` is absent from `COPY_BY_STAGE` →
  generic `FAILED` — same before and after.
- `finalizingError` (~:167-174): stage `'finalizing'` is absent from `COPY_BY_STAGE` →
  generic `FAILED`; with no code, the fixture still exercises exactly the fall-through its
  comment documents.

(a) Delete the `code: 'MITHRIL_PARTIAL_SYNC_CANCELLED',`,
`code: 'MITHRIL_PARTIAL_SYNC_VERIFY_FAILED',`, `code: 'MITHRIL_PARTIAL_SYNC_NODE_START_FAILED',`
and `code: 'PARTIAL_SYNC_FINALIZE_FAILED',` lines from their fixtures. Leave
`downloadingError` / `convertingError` / `installingError` untouched (real union codes with
bespoke copy).

(b) The adjacent fixture comment is falsified by (a). Locate:

```ts
// Per-stage failure fixtures. Codes resolve through partialSyncErrorCopy.ts:
// downloading/converting/installing map to bespoke title+hint copy by code;
// finalizing's code is intentionally absent from COPY_BY_CODE and `finalizing`
// is absent from COPY_BY_STAGE, so it exercises the generic FAILED fallthrough.
```

Replace with:

```ts
// Per-stage failure fixtures. Codes resolve through partialSyncErrorCopy.ts:
// downloading/converting/installing map to bespoke title+hint copy by code;
// finalizing carries no code and `finalizing` is absent from COPY_BY_STAGE,
// so it exercises the generic FAILED fallthrough.
```

Accepted story-visible delta (record in the implementation review): in these four stories the
error-code line inside the collapsed technical-details section disappears; all resolved
titles/hints/messages/log-path links are unchanged.

**Cross-reference (CAT-H, seam S4):** the CAT-H inventory entry
`MithrilPartialSyncOverlay.stories.tsx:136-139 [REWRITE]` covers this same comment; the CAT-H
doc is being updated in parallel to mark it handled-by-CAT-D. Step 1.7(b)'s replacement is
the authoritative text (it reflects the code-less `finalizingError`, which CAT-H's original
rewrite predates).

**D-1 gate:** after Steps 1.1–1.7, `yarn compile` must pass with ZERO new casts or
`@ts-ignore`s anywhere. A compile error at any `.code` write site means that site passes a
code outside the 18-member union — do NOT widen with a cast; Escalation E2.

---

# D-2 — `certifiedEpoch`: drop the `| null` arm that never crosses the wire

**Evidence:** the backend produces the value (service ~:1040
`const certifiedEpoch = this._getCachedCertifiedEpoch();`) and spreads it into the result
**only** when `!= null` (~:1046 and ~:1052 `...(certifiedEpoch != null ? { certifiedEpoch } :
{})`), so a `null` never crosses the wire; the wire-type comment carries a stale rollout
narrative ("OPTIONAL so the renderer type-checks before the backend produces the value…" —
the backend produces it today). Consumers already guard both ways
(`mithrilBehindness.ts` ~:57 and `MithrilProactivePromptContainer.tsx` ~:61 use
`!= null` / `Number.isFinite`).

### Step 2.1 — `source/common/types/mithril-partial-sync.types.ts`

Locate:

```ts
  behindByImmutables?: number;
  // The Mithril certified-beacon epoch — the horizon-free,
  // early-resolving fallback anchor for cardano-wallet's late `networkTip.epoch`.
  // OPTIONAL so the renderer type-checks before the backend produces the value; until
  // then it is `undefined` ⇒ the figure degrades to networkTip-only (no regression).
  certifiedEpoch?: number | null;
```

Replace with (the replacement comment text is the one the CAT-H inventory already records for
this block — copy it verbatim):

```ts
  behindByImmutables?: number;
  // Mithril certified-beacon epoch: early-resolving fallback anchor for the
  // late networkTip.epoch. Optional; when absent the figure degrades to
  // networkTip-only.
  certifiedEpoch?: number;
```

### Step 2.2 — `source/main/mithril/MithrilPartialSyncService.ts`

Locate:

```ts
  async getPartialSyncBehindness(): Promise<{
    isSignificantlyBehind: boolean;
    isProbeFailed?: boolean;
    behindByImmutables?: number;
    certifiedEpoch?: number | null;
  }> {
```

Change the `certifiedEpoch` line to `certifiedEpoch?: number;`. The body compiles unchanged:
the local `certifiedEpoch` is `number | null` (from the cached metadata read) but is only
spread under the `!= null` guard, which narrows it to `number`.

### Step 2.3 — `source/renderer/app/stores/MithrilPartialSyncStore.ts`

Locate:

```ts
  @observable certifiedEpoch: number | null | undefined = undefined;
```

Replace with:

```ts
  @observable certifiedEpoch: number | undefined = undefined;
```

Leave the multi-line comment block directly above it (~:73-77) alone — the CAT-H inventory
owns that block (see the cross-reference note).

### D-2 boundary — sites that deliberately stay wider (do NOT "tighten" them)

- `mithrilSnapshotMetadata.ts` ~:9 `certifiedEpoch: number | null;` — backend-internal
  metadata shape where `null` is meaningful ("beacon absent from this snapshot's metadata").
- `mithrilBehindness.ts` ~:47 `certifiedEpoch?: number | null | undefined` — read-side util
  parameter; staying null-tolerant is harmless and keeps the util decoupled from the wire type.
- `DaedalusDiagnostics.tsx` ~:410 `certifiedEpoch?: number | null;` — read-side prop; same
  reasoning. (CAT-F owns story/props typing work in that area.)

### D-2 cross-reference note (CAT-H coordination, seam S4)

`task-ux-704-plan-cat-h.md` already accounts for this item:

- its entry `mithril-partial-sync.types.ts:90-93 [REWRITE]` (~:46 and ~:389) is marked
  "handled by CAT-D (D9)" — Step 2.1 implements exactly the replacement text that entry
  records, so when CAT-H runs it will find the rewrite already in place (its re-location by
  quoted text will simply not match the old text; that is the expected "mooted by an earlier
  CAT" outcome listed in its doc);
- its store entry `:312-314 [DELETE]` notes the duplicate of the store field comment `:73-77` —
  D-2 does NOT touch either comment; both remain CAT-H's.

---

# D-3 — `progressItems` observable typing

`MithrilPartialSyncStore.ts` ~:54 `@observable progressItems = []` infers `any[]`
(`noImplicitAny: false` masks it). `MithrilBootstrapStore.ts` ~:75 shows the house pattern
(`@observable progressItems: MithrilProgressItem[] = [];`).

### Step 3.1 — `source/renderer/app/stores/MithrilPartialSyncStore.ts`

(a) Add the type import (the store does not import it yet). After the existing
`import type { … } from '../../../common/types/mithril-partial-sync.types';` block, add:

```ts
import type { MithrilProgressItem } from '../../../common/types/mithril-bootstrap.types';
```

(b) Locate:

```ts
  @observable progressItems = [];
```

Replace with:

```ts
  @observable progressItems: MithrilProgressItem[] = [];
```

The snapshot assignments in `_updateStatus` already carry `MithrilProgressItem[]` (the
snapshot type's field), so no other edit follows.

**Out of scope (REFUTED — do not do):** the wider nine-observables → `.ref` snapshot
refactor. Refuted in the master's register: `_updateStatus` is a single `@action`, MobX
batches the writes, no torn state exists — only this typing fix survives. Do not re-raise.

---

# D-4 — Status-guard double-casts in `MithrilStepIndicator.tsx`

Three sites repeat the same double-cast pair
(`isRestoreCompleteStatus(status as MithrilBootstrapStatus) ||
isMithrilPartialSyncRestoreCompleteStatus(status as MithrilPartialSyncStatus)`): inside
`isVerificationOrLater` (~:132-133), `getActiveStepIndex` (~:299-303), and
`deriveTopLevelState` (~:327-331). Note the bootstrap guard is imported under a local alias:
`isMithrilBootstrapRestoreCompleteStatus as isRestoreCompleteStatus` (~:14).

**Chosen variant (verified):** one local helper in this component. Do NOT widen the bootstrap
guard's parameter in `mithril-bootstrap.types.ts` — that would require importing the
partial-sync status type there, creating a reverse import (`mithril-partial-sync.types.ts`
already imports from `mithril-bootstrap.types.ts` at :1).

### Step 4.1 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`

(a) Insert a module-level helper immediately **above** the existing
`const isVerificationOrLater = (` declaration:

```ts
// Each guard narrows within its own status family and only compares against
// members of that family, so funnelling the union through both is safe; the
// two casts live here once instead of at every call site.
const isAnyRestoreCompleteStatus = (
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus
): boolean =>
  isRestoreCompleteStatus(status as MithrilBootstrapStatus) ||
  isMithrilPartialSyncRestoreCompleteStatus(status as MithrilPartialSyncStatus);
```

(b) In `isVerificationOrLater`, replace the final two lines of the expression:

```ts
  isRestoreCompleteStatus(status as MithrilBootstrapStatus) ||
  isMithrilPartialSyncRestoreCompleteStatus(status as MithrilPartialSyncStatus);
```

with:

```ts
  isAnyRestoreCompleteStatus(status);
```

(c) In `getActiveStepIndex`, replace:

```ts
  if (
    isRestoreCompleteStatus(status as MithrilBootstrapStatus) ||
    isMithrilPartialSyncRestoreCompleteStatus(
      status as MithrilPartialSyncStatus
    )
  ) {
```

with:

```ts
  if (isAnyRestoreCompleteStatus(status)) {
```

(d) In `deriveTopLevelState`, apply the identical replacement to its copy of the same `if`.

No import changes (all names already imported). Rendering is bit-for-bit identical.

---

# D-5 — Loose handler/param types in new code

### Step 5.1 — `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`

Locate in `Props`:

```ts
  onStartMithrilPartialSync: (...args: Array<any>) => any;
```

Replace with (the component awaits it in `startFromConfirmation` — ~:95
`await this.props.onStartMithrilPartialSync();`):

```ts
  onStartMithrilPartialSync: () => Promise<void>;
```

Assignability check (no caller edits needed): `DaedalusDiagnostics` forwards its own
`onStartMithrilPartialSync: (...args: Array<any>) => any` prop, which is assignable to
`() => Promise<void>`; the dialog passes `mithrilPartialSync.startPartialSync` (async); the
section spec's `jest.fn()` also satisfies it. (The only story supplying this prop passes
`action(...)` inside an `as any`-cast baseProps to `DaedalusDiagnostics`, so it is never
typechecked against the section's Props.)

**Decision recorded — leave `DaedalusDiagnostics.tsx` Props alone.** The `(...args:
Array<any>) => any` pattern at ~:417 is the file's legacy house style for the entire handler
block (~:417-423), and `onRestartNode` (~:420) is actually passed an action OBJECT
(`{ trigger }` — see the dialog's `onRestartNode={restartNode}` and the story's
`onRestartNode: { trigger: action(…) }`), a pre-existing Props inaccuracy that CAT-F's F6
documents. Retyping that block here would either codify or collide with that finding — out of
CAT-D's scope.

### Step 5.2 — `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`

(a) Add a type import after the existing value import from the same module (~:5):

```ts
import type { MithrilPartialSyncStatus } from '../../../../common/types/mithril-partial-sync.types';
```

(b) Locate:

```ts
export const shouldCloseDiagnosticsForPartialSyncOverlay = (
  previousStatus,
  nextStatus
) =>
```

Replace with:

```ts
export const shouldCloseDiagnosticsForPartialSyncOverlay = (
  previousStatus: MithrilPartialSyncStatus,
  nextStatus: MithrilPartialSyncStatus
) =>
```

Both call-site arguments (`stores.mithrilPartialSync.status`, typed
`MithrilPartialSyncStatus` on the store) already conform.

---

# D-6 — Prod-dead `isActive` / `isMithrilPartialSyncActiveStatus` pair (optional micro)

**Verified:** the store computed `isActive` (~:135-138) and the type-module guard
`isMithrilPartialSyncActiveStatus` (types ~:132-134) are production-dead — their only non-spec
consumers are each other; the sole remaining read is one spec assertion
(`MithrilPartialSyncStore.spec.ts` ~:166). No storybook usage. Deletion is congruent with the
wave's dead-code standard (CAT-A/C precedent), so this doc includes it; it is the one step
that may be dropped without invalidating the CAT if plan review prefers keeping the
convenience computed (record the choice in the implementation review either way).

### Step 6.1 — `source/renderer/app/stores/MithrilPartialSyncStore.ts`

Delete the computed:

```ts
  @computed
  get isActive(): boolean {
    return isMithrilPartialSyncActiveStatus(this.status);
  }
```

and remove `isMithrilPartialSyncActiveStatus,` from the value import list (~:9). Leave
`isWorking` / `isTerminal` / the other computeds untouched.

### Step 6.2 — `source/common/types/mithril-partial-sync.types.ts`

Delete:

```ts
export const isMithrilPartialSyncActiveStatus = (
  status: MithrilPartialSyncStatus
): boolean => status !== 'idle';
```

**KEEP `isMithrilPartialSyncBlockingNodeStart`** (~:140-145) — it is the deliberate task-703
T24 alias with real callers (`MithrilStartupGate.ts` ~:488, `MithrilController.ts` ~:200) and
is NOT part of this deletion.

### Step 6.3 — `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

In the test containing it, delete the single assertion line:

```ts
    expect(store.isActive).toBe(true);
```

(the neighbouring `expect(store.isWorking)…` / `expect(store.isTerminal)…` assertions stay —
the test itself remains).

---

## Verification

Environment prep first (do not misread as regressions): under Node v24 regenerate the
`.scss.d.ts` typings via `typed-scss-modules` and apply the gitignored jest
`identity-obj-proxy` sidecar per the repo verify-env note **before** judging `yarn compile` /
`yarn test:jest` output.

```bash
yarn test:jest --testPathPattern "Mithril|Diagnostics|ChainStorage"
yarn lint
yarn compile
```

Plus targeted suites for the touched specs:

```bash
yarn test:jest --testPathPattern "source/main/mithril/MithrilPartialSyncService\.spec\.ts|source/renderer/app/stores/MithrilPartialSyncStore\.spec\.ts|MithrilStepIndicator|MithrilPartialSyncOverlay|MithrilPartialSyncSection"
```

Expected test-count delta: **zero tests added or removed** (D-6 removes one assertion inside a
surviving test). Any other pass/fail count change is a regression.

`yarn prettier:check` on touched files; classify failures against pre-existing HEAD drift
first (`git show HEAD:<f> | prettier --stdin-filepath <f>`); never reformat
`toHaveBeenCalledWith('str', {obj})` call shapes (prettier 2.1.2 oscillation).

Commit: ONE commit for all of CAT-D, Conventional Commits **subject only** (no body, no
trailers). Suggested subject:

```
refactor(mithril): task-ux-704 CAT-D type-contract tightening
```

Rollback: revert the CAT-D commit (`git revert <sha>`); no other CAT depends on these types
being tightened (CAT-E/F compile against either state).

## Files touched

- `source/common/types/mithril-partial-sync.types.ts` — D-1 (union +5, `code` field), D-2
  (comment + field), D-6 (guard deletion)
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` — D-1
  (map +5, cast deletion at ~:91 only)
- `source/main/mithril/mithrilErrors.ts` — D-1 (class field + ctor param +
  `createPartialSyncStageError` param at their post-CAT-B home)
- `source/main/mithril/mithrilPartialSyncPreflight.ts` — D-1 (`statRequiredPath` `code` param
  + import)
- `source/main/mithril/MithrilPartialSyncService.ts` — D-1 (`_createStageError` + import),
  D-2 (return type)
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` — D-1 (Step 1.7:
  four fixture `code` drops + comment rewrite)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — D-2, D-3, D-6
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` — D-6 (one assertion line)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` — D-4
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — D-5a (one Props line)
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — D-5b

No i18n files. No IPC channel files. One storybook file (Step 1.7 — compile-forced).

## Out of scope (owned elsewhere / deferred / refuted — do not do)

- D2 availability **push model** — deferred post-merge (re-plumbs the locked DD-703-10 seam;
  pull must survive for window-reload resume anyway).
- D8 dead `logPath` wire field — deferred post-merge.
- The 9-observables → snapshot refactor — REFUTED (MobX `@action` batches; no torn state).
- D11 `partialSyncErrorCopy` layering move — deferred post-merge.
- `resolvePartialSyncErrorCopyByCode` typing (`:114`) — permanently out (code-as-message
  channel; see CRITICAL LIMIT).
- `DaedalusDiagnostics.tsx` handler-block Props (`~:417-423`) and the `onRestartNode` Props
  inaccuracy — CAT-F (F6) documents; nobody fixes in this wave.
- All comment-inventory work — CAT-H (S4), except the single types-comment rewrite Step 2.1
  performs on CAT-H's behalf.

## Acceptance checks

- **D-1:** `yarn compile` green with the union at 18 members, `MithrilPartialSyncError.code`
  union-typed, all four declaration sites union-typed (class field, ctor param,
  `createPartialSyncStageError`, `statRequiredPath` — plus the service's `_createStageError`),
  the `:91` cast gone, `:111-114` byte-identical to before, and zero new casts/`@ts-ignore`.
  The six `new Error(PARTIAL_SYNC_*_CODE)` throw sites byte-identical. Zero
  `PartialSyncStageErrorFactory` references repo-wide (CAT-B's own acceptance, re-confirmed).
  The four overlay-story fixtures carry no `code` field; the three real-coded fixtures do;
  the rewritten fixture comment matches Step 1.7(b) verbatim. Overlay/store/service spec
  suites pass with unchanged counts (their code literals are all union members).
- **D-2:** no `| null` on `certifiedEpoch` at types ~:94, service ~:1029, store ~:78; the
  replacement comment matches CAT-H's recorded text verbatim; behindness/prompt/diagnostics
  suites green; the guarded-spread lines (~:1046, ~:1052) untouched.
- **D-3:** `progressItems` declared `MithrilProgressItem[]`; store suite green.
- **D-4:** exactly one occurrence of `as MithrilBootstrapStatus` and one of
  `as MithrilPartialSyncStatus` remains in `MithrilStepIndicator.tsx` (both inside the
  helper); step-indicator rendering specs/stories unchanged.
- **D-5:** section Props awaits cleanly; `shouldCloseDiagnosticsForPartialSyncOverlay` params
  typed; dialog and section suites green.
- **D-6:** repo-wide grep for `isMithrilPartialSyncActiveStatus` and `\.isActive` (partial-sync
  store) returns no hits outside this doc's deletions; store suite green with the same test
  count.

## Escalations

- **E1 (S2 seam mismatch):** D-1 is written against CAT-B's built end state (B-2.1/B-2.2/
  B-2.3/B-2.4, restated in Sequencing position). If, at implementation time, the built code
  diverges from that state — `MithrilPartialSyncStageError` or `createPartialSyncStageError`
  missing from `mithrilErrors.ts`, `PartialSyncStageErrorFactory` still referenced anywhere,
  `statRequiredPath` not matching the `(targetPath, message, stage = 'preparing', code?)`
  arity, or the service missing `_createStageError` — stop and escalate to the wave owner
  rather than adapting D-1's steps structurally.
- **E2 (union under-enumeration at build time):** if `yarn compile` fails at a `.code` write
  site not listed in this doc (new code landed between planning and build), do NOT cast.
  If the new code's emit stage is absent from `COPY_BY_STAGE` (like the five above), append it
  to the union and map it to `FAILED` following the Step 1.1/1.2 pattern and note it in the
  implementation review. If its stage IS in `COPY_BY_STAGE` (`downloading`/`verifying`/
  `converting`/`installing`), an explicit `FAILED` mapping would CHANGE its rendered copy —
  stop and escalate.
- **E3 (D-6 review pushback):** dropping Step 6.1–6.3 alone is a sanctioned outcome; no other
  step references the deleted pair.
