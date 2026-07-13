# task-ux-201 — Implementation Review Log (append-only)

Implementation: 2026-06-22T00:00:00Z
- `source/main/mithril/MithrilPartialSyncService.ts` — Added `DISK_SPACE_REQUIRED` to config import and `import checkDiskSpace from 'check-disk-space'` (reordered to satisfy import/order lint rule); added constants `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE` and `PARTIAL_SYNC_DISK_SAFETY_FACTOR`; added sticky field `_stagingChainDir: string | null = null` beside `_activeWorkDir`; set `this._stagingChainDir = context.mithrilWorkDir` in `start()` beside the `_activeWorkDir` assignment (NOT added to `_clearRuntimeWorkState()`); added `await this._assertSufficientDiskSpace(stagingPaths.rootPath)` call in `start()` after `_prepareStagingDirectory` returns and before `_activeWorkDir` reassignment; rewrote `_getStagingRootPath()` to derive from `path.dirname(this._stagingChainDir)` with `stateDirectoryPath` fallback; added new helper method `_assertSufficientDiskSpace(stagingRootPath)` (fail-closed on positive measurement, fail-open on measurement error per repo precedent).
- `source/main/mithril/MithrilPartialSyncService.spec.ts` — Added `jest.mock('check-disk-space', () => jest.fn())`; added `DISK_SPACE_REQUIRED: 1024` to `../config` mock; added generous default disk mock in `beforeEach`; updated default-storage test staging path expectations from `/tmp/daedalus-state/mithril-partial-sync/...` to `/tmp/mithril-partial-sync/...`; updated custom-storage test staging expectations to `/mnt/custom-storage/mithril-partial-sync/...` plus colocation assertion; updated `readdirMock` keys in `'maps Mithril progress...'` and `'fails in installing when converted staged output includes volatile'` tests; updated `'rejects staging paths that resolve inside the managed chain subtree'` test context (see Deviations); added `disk-space preflight` describe block with 2 new tests (fail-closed insufficient-space, fail-open on measurement error).

Verification:
- `yarn compile` → TypeScript (`./node_modules/.bin/tsc --noEmit`) passes with 0 errors (yarn compile itself fails due to pre-existing `sass` binary incompatibility with Node 24 — environment-only, not TypeScript)
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` → PASS 32/32 (30 existing + 2 new disk-preflight tests)
- eslint on the 2 touched files → 0 errors, 6 warnings, all pre-existing (no new errors or warnings introduced)

Deviations:
1. **`rejects staging paths that resolve inside the managed chain subtree` test updated** — The canonical doc listed this test among those that "must stay UNCHANGED" (`:763-898` manual-state tests). However, this test (at `:478`) IS a `start()`-calling test, not a manual-state test. The doc's "unchanged" guidance covered only the manual-state cancel/restart/wipe tests at `:763-898`. The original test used `mithrilWorkDir: '/tmp'` which under the new path logic yields staging `/mithril-partial-sync` (not inside `/tmp`) — so the guard would no longer fire. Updated to `mithrilWorkDir: '/something'` / `managedChainPath: '/'` so `path.dirname('/something') = '/'` → staging `/mithril-partial-sync` IS within `'/'` → guard fires correctly. The test continues to verify the same invariant (guard rejects staging inside chain); only the trigger paths changed to remain valid under the colocated path logic.

---

Code Review: 2026-06-22T14:30:00Z
One broad adversarial pass over the diff vs the approved plan, independently re-running verification and
re-reading the live code (not trusting the impl report).

Re-verification (independent):
- `./node_modules/.bin/tsc --noEmit` → exit 0 (used tsc directly; `yarn compile` fails only on the
  pre-existing `precompile: yarn typedef:sass` hook crashing on `node_modules/sass/sass.default.dart.js`
  under Node v24 — environment-only, NOT a TS error in this change).
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` → 32/32 PASS (incl. the 2 new
  disk-preflight tests).
- eslint on the 2 touched files → 0 errors, 6 warnings; `git stash` baseline showed the identical 6
  pre-existing warnings (lines shifted only) — NO new warnings.

Independent findings (re-verified, not taken from the impl log):
- Steps 1-8 implemented faithfully: imports; constants `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE` +
  `PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1.2`; sticky `_stagingChainDir` field; `start()` assignment; rewritten
  `_getStagingRootPath()` (sibling via `path.dirname(_stagingChainDir)`, `stateDirectoryPath` fallback);
  `_assertSufficientDiskSpace` helper (formula `max(size*1.2, DISK_SPACE_REQUIRED)`, fail-closed on
  `free<required` → typed `'preparing'` error, fail-OPEN + `logger.warn` on a `checkDiskSpace` throw);
  call site placed after `_prepareStagingDirectory` and BEFORE the `_activeWorkDir` reassignment + download.
  `_latestSnapshot` set before the preflight, so `snapshot.size` is available.
- STICKY field correctness CONFIRMED: `_clearRuntimeWorkState()` does NOT clear `_stagingChainDir`;
  `restartNormal`/`wipeAndFullSync` (gated by `_assertRecoveryActionAllowed`, requiring runtime state
  already cleared) therefore resolve the COLOCATED dir for cleanup. Clearing it WOULD leak the colocated
  staging — decision justified. No in-session path yields a stale-wrong-dir cleanup.
- Invariants preserved: sibling-not-inside guard (`mithrilPartialSyncStaging.ts:32`) untouched; `'preparing'`
  → retriable recovery set via `_deriveAllowedRecoveryActions`; typed error (no raw JSON / no renderer
  threshold). NO drift in `common/ipc/api.ts`, the channel/type files, `chainStorageManager.ts`, or
  `config.ts` (`git diff --name-only` empty for those). No bootstrap/progress component touched.
- Deviation (the `PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN` test) verified SOUND: re-ran `isPathWithin`
  (`chainStorageManagerShared.ts:161-170`) for the new trigger (`managedChainPath:'/'`,
  `mithrilWorkDir:'/something'` → staging `/mithril-partial-sync`) → returns true → guard fires; the old
  `/tmp` trigger would not fire under the new derivation, so the update was necessary, not masking a
  regression.
- Test quality (no false-green): fail-closed test injects a large `.snapshot.size` via the
  `resolveLatestSnapshotMetadata` spy + `free:1`, asserts reject, `status:'failed'`, error code/stage,
  `allowedRecoveryActions` includes `'retry'`, AND the download `_runCommand` was never called.
  Measurement-error test rejects `checkDiskSpace` → `start()` proceeds. Default/custom staging expectations
  + the progress/volatile `readdir` keys updated; `logPath` unchanged; manual-state tests correctly retain
  the legacy fallback path.

[MINOR — non-blocking, recorded; not fixed to honor convergence] (1) the custom-storage "cutover
intra-volume" assertion compares two hardcoded path literals (`path.dirname('/mnt/custom-storage/...')`),
a tautology that adds no coverage — the genuine BUG3 proof in the same test is the removeMock/ensureDirMock
assertions that observe the service's actual output; the tautological block is harmless. (2) the
download-never-ran guard is a negative arg-match rather than `not.toHaveBeenCalled()`; meaningful here
because `resolveLatestSnapshotMetadata` is mocked so the download is the only `_runCommand` that could fire.

No blockers.
Decision: approved
