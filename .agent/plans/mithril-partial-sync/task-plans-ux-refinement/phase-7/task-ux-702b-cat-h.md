# task-ux-702b-cat-h — Backend probe-cost cache + beacon-epoch production

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-H is **backend-only** (`MithrilPartialSyncService.ts`/`.spec.ts`, `mithrilSnapshotMetadata.ts`/`.spec.ts`,
`MithrilController.ts` verify-only, plus the IPC payload-forward spec) — **zero overlap** with the renderer
files touched by CAT-A…G (none of A…G edit the backend service), so it is collision-free and could run
anywhere. It is placed **right after CAT-E** to keep the two poll-cost fixes adjacent (CAT-E = poll
**frequency** + anti-pin, renderer; CAT-H = per-probe **cost** + beacon-epoch production, backend) and
reviewable together. CAT-H is the **#16 producer** (it extracts `beacon.epoch` and surfaces `certifiedEpoch`
on the behind-ness result + availability payload); CAT-B/CAT-A are the **#16 consumers** (util fallback param +
container gate/figure). Because an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only
(= today, no regression), producer and consumers may land in any order without regression.

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| Finding # | Short desc | Decision | Severity |
|---|---|---|---|
| #15 | (grill-added 2026-06-30) Per-probe backend CPU cost: each enabled 30s availability probe **forks `checkDiskSpace`** (df/PowerShell) via `getManagedChainPath → getConfig → getDefaultStorageConfig` **and** re-reads the whole `immutable/` dir (`resolveLocalImmutableNumber → fs.readdir` + ~5 `stat`/`access`); only the aggregator is cached | D-702b-9 | Perf/High |
| #16 | (grill-added 2026-06-30) "epoch visibility" defect: gate + display anchor on cardano-wallet's late-resolving `networkTip.epoch`, so the prompt is suppressed during early/mid sync. CAT-H produces the horizon-free `certifiedEpoch` (beacon epoch); CAT-B/CAT-A consume it | D-702b-10 | High |

## Locked invariants this change must NOT break

> - **#4 Backend owns the OFFER** (`isSignificantlyBehind`, immutable gap). CAT-E/CAT-H must not change the offer verdict for any given (latest, local) pair; CAT-H only deduplicates the I/O to obtain `local` and adds the optional `certifiedEpoch`.
> - **No ISSUE-1 regression.** A stale-low cached local read makes `gap = latest − local` LARGER ⇒ reads as MORE behind (conservative), never a premature not-behind. CAT-E's back-off must be gated behind ≥2 consecutive stable reads so it cannot engage on a premature first not-behind read.
> - **Epochs-only user-facing vocabulary (D13 / `ux-copy-cardano-vocabulary`):** `certifiedEpoch` is a parsed integer used as an epochs figure; never surface immutable/file numbers or sync-% to the user.
> - **Kill switch (#10):** when partial sync is disabled, `MithrilController.getPartialSyncAvailability` short-circuits before the probe (`:169-171`) and returns no `certifiedEpoch`.

Additional CAT-H-specific locks:
- **Aggregator cache semantics preserved.** `_getCachedLatestCertifiedImmutableNumber` keeps its TTL; CAT-H
  only *adds* invalidation to it (a strict freshness improvement at lifecycle transitions).
- **No backend behavior change to the offer signal.** `isSignificantlyBehind` / `behindByImmutables` values
  are identical to today for any given (latest, local) pair; only the I/O to obtain `local` is deduplicated.
- **Normal-sync freshness tradeoff (D-702b-9 correction — accepted, documented).** The invalidation list
  covers Mithril *lifecycle* transitions only. During ordinary genesis sync with no Mithril attempt, the
  node advances `immutable/` continuously with **no** lifecycle transition, so `_localImmutableCache` is
  refreshed only by the 5-min TTL. Consequence: `gap = latest − cachedLocal` reads **stale-high**, so
  `isSignificantlyBehind` can stay `true` for up to ~5 min after the node actually drops below threshold.
  This is **conservative** (over-offers; never a premature not-behind ⇒ no ISSUE-1) but is a responsiveness
  regression from today's 30s freshness. The proactive **prompt** is additionally protected by the renderer
  near-tip hide (`computeBehindByEpochs` over live `localTip`/`networkTip` flips promptly); **Diagnostics**
  has no second gate and may show "Mithril available" for up to ~5 min after catch-up. Accepted as the cost
  of removing the per-tick fork.

## Exact files (full repo-relative paths)

- `source/main/mithril/MithrilPartialSyncService.ts` — `_localImmutableCache` +
  `_getCachedLocalImmutableNumber()`, `_invalidateBehindnessCaches()`, switch `getPartialSyncBehindness` to
  the cached local read; (#16) carry `epoch` on the aggregator cache, `_getCachedCertifiedEpoch()`, return
  `certifiedEpoch` conditionally.
- `source/main/mithril/MithrilPartialSyncService.spec.ts` — local-read caching, invalidation, and #16
  probe-returns-epoch cases.
- `source/main/mithril/mithrilSnapshotMetadata.ts` — (#16) `extractCertifiedEpoch` multi-path extractor;
  `certifiedEpoch` on `ResolvedLatestSnapshot`; set it in `normalizeResolvedLatestSnapshot`.
- `source/main/mithril/mithrilSnapshotMetadata.spec.ts` — (#16) extractor cases.
- `source/main/mithril/MithrilController.ts` — **VERIFY-ONLY, no edit.** `getPartialSyncAvailability`
  (`:167-174`) already returns `{ isEnabled, ...behindness }` (`:173`), so a `certifiedEpoch` on the
  behind-ness result **auto-forwards** via the spread; the disabled short-circuit (`:169-171`) returns no
  epoch. Confirm the spread carries it; no code change.
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts` — **CAT-H test home for the payload-forward** assertion
  (that `certifiedEpoch` rides the availability payload).

> **Note on the IPC payload TYPE field:** `MithrilPartialSyncAvailability.certifiedEpoch?: number | null` is
> the *consumer-owned optional type field* added by **CAT-A** (`source/common/types/mithril-partial-sync.types.ts`)
> so the renderer type-checks before CAT-H's value-production lands. CAT-H **supplies the value**; it does not
> need to edit the type (but the field must exist for the IPC spec to assert against — CAT-A lands earlier in
> the order, so it is already present).

## Implementation steps (ordered, mechanical)

### 0. Beacon-epoch key is confirmed upstream — implement `beacon.epoch` first (NO pre-implementation gate)

**`beacon.epoch` is the confirmed live upstream Mithril key** — no operator run is required before writing
the extractor. Mithril's `CardanoDbBeacon` is `required: [epoch, immutable_file_number]` (upstream
`openapi.yaml`) and the struct (`mithril-common/src/entities/cardano_db_beacon.rs`) carries exactly those
two **snake_case** fields (`epoch`, `immutable_file_number`; the deprecated `network` field was removed —
`epoch`, **not** `epoch_number`). The `cardano-db snapshot show latest --json` / `... list --json` output
already carries this beacon (under `beacon` / `cardano_db_beacon`) — it is the same JSON already fetched
for `immutable_file_number`, just not extracted yet (no extra CLI call). So write
`extractCertifiedEpoch`'s `explicitPaths` with **`['beacon','epoch']` first** (the canonical key, already
first in step 4's list) and proceed — **#16 is done on implementation; it does NOT block on an operator
run.**

The `epoch_number` / `epochNumber` and the top-level `['epoch']` paths are **defense-in-depth only** (no
live Mithril version emits them; the top-level `['epoch']` could match a non-beacon certificate epoch, so
it must be last). Keep them, but ordered **after** `beacon.epoch` so the real key always wins. See step 4.

**The `cardano-db snapshot show latest --json` operator run is reclassified to a POST-implementation
verify, NOT a gate** (see "Operator / verify-only gates"). Its only residual purpose is to confirm the
**pinned fork** (`flake.nix:23` → `mithril/sl/fix-mismatch-rust-versions`, a **branch** not a tag) has not
diverged from upstream on this key. The multi-path extractor + the absent-epoch safe-degrade (→
networkTip-only = today, no regression) mean a divergence cannot regress anything — it would only leave the
early-sync fix inert until the path is added. Run it **after** implementing to confirm the fix is live;
do not hold implementation on it. (This de-risks CAT-H landing silently inert: the extractor is written
against the verified upstream shape, not a guessed one.)

### 1. Add `_localImmutableCache` + `_getCachedLocalImmutableNumber()`

Next to `_latestCertifiedImmutableCache` (`:111-114`) add:

```ts
_localImmutableCache: { value: number; fetchedAt: number } | null = null;
```

Add a private `async _getCachedLocalImmutableNumber(): Promise<number>` that mirrors
`_getCachedLatestCertifiedImmutableNumber` (`:645-660`): on a fresh hit (within
`PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS`, the existing 5-min TTL const at `:72` whose comment literally says
"only the aggregator query is cached") return `this._localImmutableCache.value`; on a miss resolve
`getManagedChainPath()` → `resolveLocalImmutableNumber()` (the **only** place these run), store
`{ value, fetchedAt: now }`, return. **Reuse the existing 5-min TTL const (`:72`)** — do not add a new
interval const.

Background on the cost being deduped (grounded live 2026-06-30): on a cache **miss**,
`getManagedChainPath()` (`:675-676`; the pure path helper is at
`source/main/utils/chainStoragePathResolver.ts:14-20`, invoked via `chainStorageManager.ts:194-197` — the
`getConfig()` it calls is what forks) transitively **forks `checkDiskSpace`** (df/PowerShell) via
`getConfig → getDefaultStorageConfig` (`source/main/utils/chainStorageManagerConfig.ts:23`) even though it
only uses `config.customPath` and discards the `free`-bytes figure; and `resolveLocalImmutableNumber()`
(`:677-680`) does a full `fs.readdir(immutable/)` + ~5 `stat`/`access`
(`source/main/mithril/mithrilPartialSyncPreflight.ts:63-141`). On a cache **hit** we never resolve the
path, so the fork never happens — both costs are deduped in one place. **`checkDiskSpace` reaches the probe
via `getManagedChainPath → getConfig`, NOT the `start()` preflight `_assertSufficientDiskSpace`
(`:786-815`, called once at `:173`)** — easy to miss by grepping `checkDiskSpace` call sites alone.

### 2. Switch `getPartialSyncBehindness` to the cached local read

In `getPartialSyncBehindness` (`:669-698`), inside the `try` block (`:673-689`) replace the inline
`getManagedChainPath()` + `resolveLocalImmutableNumber()` (`:675-680`) with:

```ts
const localImmutableNumber = await this._getCachedLocalImmutableNumber();
```

Leave the **gap/threshold logic byte-for-byte unchanged**: `gap = latest - localImmutableNumber`, the
`gap <= 0` guard (`:682-685`), the threshold compare (`:687`; the `20` at `:71` is the **offer threshold**
for `isSignificantlyBehind`, **not** a display divisor), and the degrade-to-not-behind `catch` (`:690-697`).
The result is still computed fresh from `cachedLatest − cachedLocal` each call. A cache **miss** still throws
into the same `catch` (degrade to not-behind), so error behavior is preserved.

> **Scope note (supersedes for the return literals):** "byte-for-byte unchanged" here means the
> gap/threshold *computation*, NOT the return objects. Step 5 (#16) appends an optional `certifiedEpoch`
> to the **success** return (`:686-689`) and the **`gap <= 0`** return (`:682-685`); those additions
> supersede this "unchanged" claim for those two literals (the `catch` return stays literal). The live
> success return is a plain literal `{ isSignificantlyBehind, behindByImmutables: gap }` — step 5 only
> *appends* to it; it does not refactor `behindByImmutables`.

### 3. Invalidate both caches on lifecycle transitions

Add a private:

```ts
_invalidateBehindnessCaches(): void {
  this._latestCertifiedImmutableCache = null;
  this._localImmutableCache = null;
}
```

Call it at every point where the local immutable position (or the offer) can change so the next probe is
fresh:
- at the **start** of `start()` (`:133`, after the in-progress guard),
- at the **start** of `cancel()` (`:264`),
- inside `_resetToIdleStatus()` (`:576-587`) — which already runs on
  `restartNormal`/`wipeAndFullSync`/`finalizeWipeAndFullSync`/`finalizeCompletedPartialSync`.

(Folding it into `_resetToIdleStatus()` covers all four reset paths in one place; `start()`/`cancel()` get
explicit calls because they do **not** route through it.) **Rationale:** invalidating on completion is what
makes the proactive prompt + Diagnostics flip to "caught up" promptly after a cutover instead of waiting
out the 5-min TTL.

### 4. (#16) Extract the beacon epoch (multi-path, undefined-safe)

In `mithrilSnapshotMetadata.ts`, add `export const extractCertifiedEpoch = (raw): number | null` mirroring
`extractLatestCertifiedImmutableNumber` (`:66-88`) — reuse `getNestedValue` + `toPositiveInteger`, walk
`explicitPaths`:

```
['beacon','epoch'], ['cardano_db_beacon','epoch'], ['beacon','epoch_number'],
['cardanoDbBeacon','epochNumber'], ['epoch']
```

→ first hit wins, else `null`. (`['beacon','epoch']` is the confirmed upstream key — keep it **first**;
the `epoch_number`/`epochNumber` paths are dead defense-in-depth and the top-level `['epoch']` stays
**last** so it can't shadow the beacon epoch. See step 0.) Add
`certifiedEpoch: number | null` to `ResolvedLatestSnapshot` and set it in `normalizeResolvedLatestSnapshot`
(`:124-146`). **Do NOT gate the existing `latestCertifiedImmutableNumber == null ⇒ return null` on the
epoch** — a present immutable number with a `null` epoch must still resolve (the immutable path stays the
offer signal; `certifiedEpoch` is best-effort/optional). The current extractor and every fixture
(`mithrilSnapshotMetadata.spec.ts:28-31`, `MithrilPartialSyncService.spec.ts:149-150/176-184`) read **only**
`immutable_file_number` today — no epoch is plumbed yet; this adds it.

### 5. (#16) Carry the epoch on the aggregator cache + return it from the probe

Extend `_latestCertifiedImmutableCache` (`:111-114`) to
`{ value: number; epoch: number | null; fetchedAt: number }`. In `_getCachedLatestCertifiedImmutableNumber`
(`:645-660`), store `snapshot.certifiedEpoch` too — **but keep its return type a `number`** for the
immutable value. Expose the epoch via a **sibling** `private _getCachedCertifiedEpoch(): number | null`
(do **NOT** change `_getCachedLatestCertifiedImmutableNumber`'s `number` return). Both `_show…`/`_list…`
snapshot paths funnel through the single `normalizeResolvedLatestSnapshot`, so setting `certifiedEpoch`
there covers every `ResolvedLatestSnapshot` construction.

In `getPartialSyncBehindness` (`:669-698`) add `certifiedEpoch` (read via `_getCachedCertifiedEpoch()`) to
the **success** return and the `gap <= 0` return; leave the degrade-to-not-behind `catch` unchanged (⇒
`certifiedEpoch` undefined there, which the renderer treats as "no early anchor"). The same cache
invalidation (`_invalidateBehindnessCaches`, step 3) already refreshes the epoch on lifecycle transitions.

**Add `certifiedEpoch` to each return CONDITIONALLY** — a `...(certifiedEpoch != null ? { certifiedEpoch }
: {})` spread — and **leave `behindByImmutables` as the existing unconditional literal**. The live success
return (`:686-689`) is a plain literal that always sets `behindByImmutables: gap` (`gap` is always `> 0` on
that path), so do **NOT** refactor it into a conditional spread. The minimal, invariant-preserving edit:

```ts
// success return (:686-689):
return {
  isSignificantlyBehind: gap >= this._getBehindnessThresholdImmutables(),
  behindByImmutables: gap,            // unchanged literal
  ...(certifiedEpoch != null ? { certifiedEpoch } : {}),
};

// gap <= 0 return (:682-685):
return {
  isSignificantlyBehind: false,
  ...(certifiedEpoch != null ? { certifiedEpoch } : {}),
};
```

**Why the `!= null` guard, not an unconditional `certifiedEpoch`:** `_getCachedCertifiedEpoch()` returns
**`null`** (not `undefined`) when the beacon has no epoch, and Jest `toEqual` ignores `undefined`-valued
keys but **not** `null` ones — so an unconditional `certifiedEpoch: null` would add an extra key and break
the exact-match cases. Of the four existing `getPartialSyncBehindness` cases, only **two** carry
`behindByImmutables`: `:1228` asserts `{ isSignificantlyBehind: true, behindByImmutables: 20 }` and `:1241`
asserts `{ isSignificantlyBehind: false, behindByImmutables: 5 }`; the other two assert
`{ isSignificantlyBehind: false }` only — `:1254` (gap ≤ 0) and `:1266` (catch path). Their fixtures carry
no beacon epoch ⇒ `_getCachedCertifiedEpoch()` returns `null` ⇒ the conditional spread omits the key ⇒ all
four stay green. (Note `behindByImmutables?` is optional only at the **type** level — `:670-672`; the
service *emits* it unconditionally on the success path, so "mirroring `behindByImmutables?`" describes the
type, not the runtime return.)

### 6. (#16) Forward = free; verify MithrilController (no edit)

`getPartialSyncAvailability` (`:173`) returns `{ isEnabled, ...behindness }`, so `certifiedEpoch` rides the
spread automatically — **no edit**, just confirm. The disabled short-circuit (`:169-171`) returns no epoch.
The renderer payload type field (`MithrilPartialSyncAvailability.certifiedEpoch?`) is added by **CAT-A**
(consumer-owns-optional-type, compile-order); CAT-H supplies the value. (Backend horizon context: the probe
works entirely in immutable files and is horizon-free, unlike the late `network_tip.epoch`; the
`node_tip.epoch_number` required vs `network_tip.epoch_number: number | null | undefined` asymmetry is at
`source/renderer/app/api/network/types.ts:48-78`.)

## New symbols

- field `_localImmutableCache: { value: number; fetchedAt: number } | null` (default `null`).
- private `_getCachedLocalImmutableNumber(): Promise<number>`.
- private `_invalidateBehindnessCaches(): void`.
- **#16:** `extractCertifiedEpoch(raw): number | null` (exported, `mithrilSnapshotMetadata.ts`).
- **#16:** `certifiedEpoch: number | null` on `ResolvedLatestSnapshot`.
- **#16:** `epoch: number | null` added to `_latestCertifiedImmutableCache`'s shape
  (`{ value: number; epoch: number | null; fetchedAt: number }`).
- **#16:** private `_getCachedCertifiedEpoch(): number | null` (sibling; does NOT change
  `_getCachedLatestCertifiedImmutableNumber`'s `number` return).
- **#16:** `certifiedEpoch?` on the `getPartialSyncBehindness` return (added conditionally).

## Acceptance

- Within the 5-min TTL, repeated `getPartialSyncBehindness()` calls resolve `getManagedChainPath()` and
  `resolveLocalImmutableNumber()` (hence `checkDiskSpace` + `fs.readdir`) **once**, not per call; after the
  TTL expires the next call re-resolves them once.
- After any lifecycle transition (`start`/`cancel`/`restart-normal`/`wipe`/`finalize-wipe`/
  `finalize-completed`) the next probe re-resolves **both** local and aggregator inputs (caches
  invalidated), so post-cutover behind-ness is fresh without waiting out the TTL.
- The behind-ness verdict for any (latest, local) pair is unchanged vs. today; the existing
  `getPartialSyncBehindness` spec cases (`MithrilPartialSyncService.spec.ts:1221-1269`) stay green.
- **#16:** when the beacon carries an epoch, `getPartialSyncBehindness` returns it as `certifiedEpoch` and
  `getPartialSyncAvailability` forwards it (spread); when the beacon has **no** epoch field (the pinned-fork
  risk), `certifiedEpoch` is `null`/absent and every existing behavior is byte-for-byte unchanged (the
  renderer degrades to networkTip-only — no regression). The immutable offer signal is unaffected by epoch
  presence/absence.

## Tests (add/update)

**`source/main/mithril/MithrilPartialSyncService.spec.ts`** — mirror the existing aggregator-cache test
(`:1271-1292`): `Date.now` spy + the `stubLocalImmutableNumber` helper (`:1208-1219`, which spies
`getManagedChainPath` and drives `readdirMock`); `check-disk-space` mocked `:12`:
- **local-read caching:** spy `getManagedChainPath` (and/or assert `readdirMock` call count); two
  `getPartialSyncBehindness()` calls within the TTL ⇒ the spy fired **once**; a third call past the TTL ⇒
  fired **twice**.
- **invalidation:** after a cached read, drive a lifecycle transition that resets to idle (e.g.
  `finalizeCompletedPartialSync()` with the `fs-extra`/marker mocks already in the suite, or call
  `restartNormal()` from an allowed boundary) and assert the next `getPartialSyncBehindness()` re-runs the
  local read (spy/`readdirMock` count increments without advancing `Date.now`).
- keep the existing `getPartialSyncBehindness` cases (`:1221-1292`) and the aggregator-cache test green.
  The four exact-match `.toEqual` cases are: `:1228` `{ isSignificantlyBehind: true, behindByImmutables: 20 }`
  and `:1241` `{ isSignificantlyBehind: false, behindByImmutables: 5 }` (carry `behindByImmutables`); `:1254`
  and `:1266` `{ isSignificantlyBehind: false }` (gap ≤ 0 / catch — no `behindByImmutables`). All four are
  preserved because `_getCachedCertifiedEpoch()` returns `null` for these epoch-less fixtures and the
  `certifiedEpoch != null` spread omits the key.
- **#16 probe returns epoch:** a `getPartialSyncBehindness()` over a beacon with
  `{ epoch, immutable_file_number }` returns `certifiedEpoch` on the success result; a beacon without an
  epoch returns `certifiedEpoch: null`/absent with the immutable verdict unchanged.

**`source/main/mithril/mithrilSnapshotMetadata.spec.ts`** — #16 extractor cases:
- `extractCertifiedEpoch` returns the epoch from each `explicitPaths` location. Mirror the **real**
  existing immutable fixture at `:30-32` — `cardano_db_beacon: { immutable_file_number: '25' }` (key
  `cardano_db_beacon`, string value) — i.e. add an epoch to that same shape, e.g.
  `cardano_db_beacon: { epoch: 320, immutable_file_number: '25' }`. Parse string epochs via
  `toPositiveInteger`, and return `null` when no epoch key is present (the pinned-fork-absent case) while
  `extractLatestCertifiedImmutableNumber` still returns the immutable number from the same payload.
- Add a fixture using the known-good production beacon shape (the same object the immutable extractor's
  `:28-31` fixture uses) with an `epoch` added, so the extractor is covered by a test reflecting the real
  shape.

**`source/main/ipc/mithrilPartialSyncChannel.spec.ts`** — payload-forward: assert `certifiedEpoch` rides the
availability payload (`getPartialSyncAvailability` → IPC).

## Verify commands

Per-step (from the master §Sequencing, step 6):

```
yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts
yarn test:jest source/main/mithril/mithrilSnapshotMetadata.spec.ts
yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts
yarn compile
yarn lint
```

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`). (CAT-H is backend-only ⇒ no `.scss.d.ts` regen.)

CAT-H is **backend-only**, so there is **no `.scss.d.ts` regen** for this category; the sidecar caveat still
applies to any jest run.

## Operator / verify-only gates

- **#16 beacon-epoch field presence — POST-implementation verify (D-702b-10 §5).** `beacon.epoch` is the
  **confirmed live upstream Mithril key** (`openapi.yaml` `required: [epoch, immutable_file_number]`;
  `mithril-common/src/entities/cardano_db_beacon.rs` — snake_case, `network` removed), so implement
  `extractCertifiedEpoch` with `['beacon','epoch']` first and treat **#16 as done on implementation** — it
  does **not** gate on an operator run (step 0). **After** landing, the operator runs
  `cardano-db snapshot show latest --json` (or the aggregator REST snapshot endpoint) against the pinned
  aggregator (`flake.nix:23` → `mithril/sl/fix-mismatch-rust-versions`, a fork **branch**, not a tag) to
  confirm the field is present on this aggregator — its only residual purpose is catching a fork divergence
  from upstream. The extractor is multi-path + undefined-safe, so an absent/renamed field degrades to
  networkTip-only (= today, no regression); the early-sync fix would simply stay inert until the path is
  added. **Not a blocking gate: run it to confirm the fix is live, then adjust the path list only if the
  fork diverged.**
- **`MithrilController.ts` — verify-only, no edit.** Confirm `getPartialSyncAvailability` (`:167-174`) still
  spreads `{ isEnabled, ...behindness }` (`:173`) so `certifiedEpoch` auto-forwards, and that the disabled
  short-circuit (`:169-171`) returns no epoch.

## Cross-category coupling notes

- **Backend-only, collision-free.** None of CAT-A…G edit `MithrilPartialSyncService.ts`,
  `mithrilSnapshotMetadata.ts`, or `MithrilController.ts`, so CAT-H has no file collisions.
- **#16 producer/consumer split.** CAT-H *produces* `certifiedEpoch` (this doc). CAT-B adds the
  `computeBehindByEpochs(localTip, networkTip, certifiedEpoch?)` fallback param; CAT-A declares/populates the
  `certifiedEpoch` store observable (in `_applyAvailability`), adds the IPC payload **type** field
  (`MithrilPartialSyncAvailability.certifiedEpoch?: number | null` in
  `source/common/types/mithril-partial-sync.types.ts`), and ORs `certifiedKnown` into the container gate.
  Because absent `certifiedEpoch` degrades to today's networkTip-only behavior, the three can land in any
  order; end-to-end behavior is live once all three land — the `beacon.epoch` key is confirmed upstream
  (step 0), so no pre-implementation gate blocks it; the operator run is a post-landing confirmation that
  the pinned fork still emits the key (see "Operator / verify-only gates").
- **Scope split with CAT-E.** CAT-E = poll **frequency** + anti-pin (renderer); CAT-H = per-probe **cost** +
  beacon-epoch (backend). CAT-E's back-off is gated off while behind, so only CAT-H lowers the per-tick CPU
  during the catch-up window — that is why CAT-H is required, not optional.
