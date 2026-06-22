# task-ux-102 — Implementation Review Log (append-only)

Implementation: 2026-06-22T10:53:46Z
- `source/main/mithril/MithrilPartialSyncService.ts` — added `DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20` + `PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS = 5*60*1000` constants, `_latestCertifiedImmutableCache` field, `_getCachedLatestCertifiedImmutableNumber()` (TTL-cached wrapper over `resolveLatestSnapshotMetadata()` keyed on `Date.now()`), `_getBehindnessThresholdImmutables()` (launcherConfig override → default), and `getPartialSyncBehindness()` computing `gap = latest - local` via `resolveLocalImmutableNumber(getManagedChainPath(), ...)`, degrading every failure (incl. `gap<=0`) to `{isSignificantlyBehind:false}`. Read-only, lock-free; no mutation lock, no `_awaitPendingMutations`.
- `source/main/mithril/MithrilController.ts` — `getPartialSyncAvailability()` is now `async`, reads `isEnabled` from the coordinator, short-circuits `{isEnabled:false,isSignificantlyBehind:false}` when disabled, else merges `{ isEnabled, ...await this._partialSyncService.getPartialSyncBehindness() }`. Resolves the task-ux-101 sync→async open question (handler already awaits, so channel/type/renderer stay frozen).
- `source/main/utils/chainStorageCoordinator.ts` — kept `getPartialSyncAvailability()` body as the kill-switch source; only narrowed the inline comments (no longer "task-ux-102 replaces here").
- `source/main/config.ts` — added `mithrilPartialSyncThresholdImmutables?: number;` to `LauncherConfig` after `mithrilPartialSyncEnabled?`.
- `nix/internal/launcher-config.nix` — added `mithrilPartialSyncThresholdImmutables = 20;` beside `mithrilPartialSyncEnabled = true;` (pass-through carries it via the un-whitelisted YAML parse).
- `source/main/mithril/MithrilPartialSyncService.spec.ts` — added a `getPartialSyncBehindness` describe block: gap≥20⇒true+gap; gap 0<g<20⇒false+gap; `local>=latest`⇒false(no throw); `resolveLatestSnapshotMetadata` rejects⇒false(no throw); TTL cache via `jest.spyOn(Date,'now')` (1 call within TTL, re-queries past TTL). `getManagedChainPath` spied + local driven through the immutable readdir mock (no real path resolver, since fs-extra mock lacks lstat/realpath).

Verification:
- `yarn compile` → PASS (`tsc --noEmit` Done in 19.26s; validates async signature change + new config field).
- `./node_modules/.bin/eslint <6 touched files>` → exit 0, 0 errors, 18 warnings (all pre-existing: unused signature params, `PARTIAL_SYNC_STAGED_DB_INVALID_CODE`, `@ts-ignore`, `any`; none introduced by this task). `yarn lint` was not run repo-wide; lint scoped to touched files per env guidance.
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` → PASS 30/30 (25 prior + 5 new behind-ness cases).
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` → PASS 6/6 (3 task-ux-101 availability tests still green with the now-async controller method).
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts` → PASS 37/37 (no regression).

Deviations: none. (Test setup drives `local` through the existing immutable-directory readdir mock rather than mocking `resolveLocalImmutableNumber` directly — the plan explicitly allowed either the function mock OR the dir read it relies on.)

---

Code Review: 2026-06-22T10:57:03Z
Re-verification:
- `yarn compile` (`tsc --noEmit`) → PASS (Done in 17.89s). Validates the controller async signature change + the new `mithrilPartialSyncThresholdImmutables?: number` config field.
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` → PASS, 30/30 (25 prior + 5 new behind-ness cases).
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` → PASS, 6/6 (the 3 ux-101 availability tests still green with the now-async controller method).

Independent findings (re-verified, not taken from the impl log):
- Correctness vs plan + PRD D2: gap = `latest - local` (matches `derivePartialSyncRange` length `latest - local` but WITHOUT its `PARTIAL_SYNC_NO_CERTIFIED_RANGE` throw). `gap<=0 ⇒ {isSignificantlyBehind:false}` (no `behindByImmutables`). `gap>0 ⇒ behindByImmutables = gap` always set; `isSignificantlyBehind = gap >= threshold`. The single try/catch wraps the cached aggregator read, `getManagedChainPath()`, AND `resolveLocalImmutableNumber()` — every failure path (aggregator reject, missing/invalid managed-chain or immutable dir, unreadable dir, no parseable immutable file → all the `createStageError` throws in `resolveLocalImmutableNumber`) degrades to `{isSignificantlyBehind:false}` with a `logger.warn`, never throws. The signal never nudges toward a dead-end. Threshold = launcherConfig override (`typeof === 'number' && > 0`) → default 20. CONFIRMED.
- Caching: TTL cache wraps ONLY `resolveLatestSnapshotMetadata()` via `_getCachedLatestCertifiedImmutableNumber()`. The local FS read (`getManagedChainPath` + `resolveLocalImmutableNumber`) runs on EVERY `getPartialSyncBehindness()` call, OUTSIDE the cache, so the gap tracks local progress immediately. TTL math `now - fetchedAt < 300000` is correct. On aggregator rejection the `await resolveLatestSnapshotMetadata()` throws BEFORE the `this._latestCertifiedImmutableCache = {...}` assignment, so a failed query NEVER caches a stale/garbage value. No tight poll. CONFIRMED.
- Option A wiring: `MithrilController.getPartialSyncAvailability()` is `async`, reads `isEnabled` from the coordinator (the sync kill-switch method at `chainStorageCoordinator.ts:69` — NOT self-recursion), short-circuits `{isEnabled:false,isSignificantlyBehind:false}` when disabled (so the aggregator query is skipped when the kill switch is off), else merges `{ isEnabled, ...behindness }`. Coordinator stays the kill-switch source (comment-only change; body unchanged). CONFIRMED.
- Lock-free / no restore: NO `_awaitPendingMutations`, `_withMutationLock`, `setDirectory`, `ensureManagedChainLayout`, or restore introduced (only a comment contains the word "restore"). The probe uses the lock-free `getManagedChainPath()` accessor. CONFIRMED.
- Frozen-contract integrity: the availability channel (`source/main/ipc/mithrilPartialSyncChannel.ts`), the `MithrilPartialSyncAvailability` type (`source/common/types/mithril-partial-sync.types.ts` — still exactly `{ isEnabled, isSignificantlyBehind, behindByImmutables? }`, NO threshold field), the renderer wrapper (`source/renderer/app/ipc/mithrilPartialSyncChannel.ts`), and `source/common/ipc/api.ts` are ALL absent from the diff. Renderer still cannot see the threshold. CONFIRMED.
- Async ripple: grep for all callers of `controller.getPartialSyncAvailability()`. The ONLY production caller is the already-`async` IPC handler at `mithrilPartialSyncChannel.ts:116-118` (`onRequest(async () => controller.getPartialSyncAvailability())`), which returns/awaits the promise. The only other references are in `mithrilPartialSyncChannel.spec.ts` (mocks). Nothing else breaks. CONFIRMED.
- Tests: 5 cases deterministic and correctly targeted. The cache test pins `Date.now` (1000 / 61000 within TTL → 1 call; 361000 past TTL → 2 calls). The gap-20→true / gap-5→false assertions depend on DEFAULT=20: the spec's mocked `launcherConfig` (lines 26-33) sets ONLY `mithrilPartialSyncEnabled: true` and does NOT set `mithrilPartialSyncThresholdImmutables`, so `_getBehindnessThresholdImmutables()` returns the genuine default 20 — the assertions are NOT coincidental. `local` is parsed from `00005.chunk`-style readdir entries via `parseImmutableFileNumber` (stem `/^\d+$/` → Number), and the `not-an-immutable-entry` decoy is correctly filtered. No false-green. CONFIRMED.
- Complexity: minimal, additive change (≈65 lines of service code + 5 controller lines + config/nix one-liners + tests). No over-engineering.

No blockers.
Decision: approved
