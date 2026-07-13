# task-ux-201 — Colocate partial-sync staging on the resolved chain volume + disk-space preflight

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `in_progress`
- Interaction mode: `autonomous`
- Priority: high · Estimated: 4h · Dependencies: none

## Why now
Partial-sync staging is hardcoded to the **top-level Daedalus state directory**
(`_getStagingRootPath()` → `path.join(stateDirectoryPath, 'mithril-partial-sync')`,
`MithrilPartialSyncService.ts:577-578`). This **contradicts the backend PRD's own locked restore
strategy** (`mithril-partial-sync-prd.md:250`, step 3: "Download the missing range into a staging area
**under the resolved Mithril work directory**"). On a relocated/symlinked chain it fills the **wrong
volume** and makes cutover a **cross-device copy** (~2× transient destination usage). This task derives
staging from the resolved chain volume (a **sibling** of the managed chain) so cutover becomes an
intra-volume rename, and adds a **disk-space preflight**. Implements PRD **D7/BUG3**
(`...-ux-refinement-prd.md:330-339`, checklist `:615-616`, `:656-657`); closes research-19 **gap #42**
(row 395) and the disk-preflight portion. No phase-3+ task depends on this; it is an independent
UX-enabling backend correctness fix.

## Interaction mode justification
`autonomous`: backend-only code change. The engineering placement of scratch/staging space is explicitly
**in scope** and **not** a user decision (PRD non-goal `:546-548` excludes only *user-facing*
storage-location/snapshot pickers; D7/BUG3 `:337-339` calls scratch placement "the engineering placement
of scratch space"). The disk preflight failing closed on insufficient space is a safety behavior, not a
product decision. No user-facing copy is authored here (renderer error copy is task-ux-403). No
destructive/irreversible action: staging is scratch, never the live chain.

## Scope
1. Derive the partial-sync staging root from the **resolved chain work directory**
   (`context.mithrilWorkDir`, realpath-resolved) as a **sibling** of the managed chain, instead of the
   hardcoded top-level state dir. Cutover (`fs.move`) then stays intra-volume (no `EXDEV` copy fallback).
2. Add a **disk-space preflight** in `start()` — before the download/cutover steps — that fails closed
   with a typed, retriable `'preparing'`-stage error (`PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`) when free
   space on the chain volume is below the required size.
3. Update the existing staging-path tests (they currently encode the BUG3 location) and add disk-preflight
   + colocation tests.

## Non-goals
- **No user-facing storage-location picker / snapshot-selection** (locked non-goal `:546-548`). Placement
  is computed automatically from the resolved chain; no UI, no config key for the *location*.
- **Do NOT** author renderer error copy for the new code — that is **task-ux-403** (it maps localized copy
  off `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`). This task emits only the structured/typed backend error.
- **Do NOT** change the cutover/move machinery (`installValidatedPartialSyncSnapshot`, `movePath`'s
  `EXDEV` fallback). Colocation makes the existing `fs.move` a rename; the EXDEV fallback simply stops
  firing. (Cutover/merge re-validation is task-ux-701.)
- **Do NOT** handle cross-session leftover-staging reclamation at startup — that is **task-ux-202**
  (extends the C2 startup branch to `fs.remove(stagingRoot)`). See "Residual gaps / hand-off".
- **Do NOT** mutate the live chain in place; staging stays a sibling, never inside the chain subtree.

## Dependencies
None. (Independent of phase-1; uses only existing `context.mithrilWorkDir` already plumbed by the
coordinator.)

## Research / docs / workflows / skills consulted
- UX PRD **D7/BUG3** (`...-ux-refinement-prd.md:319-339`); checklist items `:615-616`, technical-design
  `:656-657`; non-goal clarification `:546-548`; gap-disposition table `:779` (BUG3 → D7).
- **Backend PRD** locked restore strategy step 3 (`mithril-partial-sync-prd.md:250`: "staging area under
  the resolved Mithril work directory").
- Research-19 **gap #42** (row 395) — exact bug + D7 disposition; **gap #16** (restart-safe cleanup) for
  cross-reference (the colocated staging must still be removed by the recovery cleanups — see Risks).
- Live code: `MithrilPartialSyncService.ts` (`start()` flow, `_getStagingRootPath`, cleanup paths,
  `_deriveAllowedRecoveryActions`, `_clearRuntimeWorkState`), `mithrilPartialSyncStaging.ts` (the
  sibling-not-inside guard), `chainStoragePathResolver.ts` (`resolveMithrilWorkDir`),
  `chainStorageManagerShared.ts` (`movePath` EXDEV fallback, `isPathWithin`), `config.ts`
  (`DISK_SPACE_REQUIRED`), and the existing `check-disk-space` usages.
- Format/rigor template: `task-ux-102.md` (phase-1).

## Verified seams (checked against live code at planning time; tasks-JSON line anchors had drifted)

| What | Live location | Note |
|------|---------------|------|
| `_getStagingRootPath()` | `MithrilPartialSyncService.ts:577-578` | JSON said `:567-568` (drifted). Body: `return path.join(stateDirectoryPath, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);` |
| `PARTIAL_SYNC_STAGING_DIRECTORY_NAME` | `:57` (`= 'mithril-partial-sync'`) | constant, not a literal (matches JSON note 1) |
| `_activeWorkDir` field | declared `:95`; set `:129` (`= context.mithrilWorkDir`); **reassigned `:163`** (`= stagingPaths.downloadParentPath`); cleared in `_clearRuntimeWorkState` `:566` | JSON said `:153` (drifted). **`_activeWorkDir` is NOT a safe source — it is the chain dir only between `:129`–`:163`, then points inside the staging tree, and is null in post-terminal recovery.** |
| `start()` staging prep | `:159-161` `_prepareStagingDirectory(context.layoutResult.managedChainPath)` → first `_getStagingRootPath()` call (via `:748`) | preflight insertion point is right after this returns |
| First download step | `:194` `_downloadAndVerifyPartialSnapshot(...)` | first heavy write; preflight must precede it |
| Cutover (touches live chain) | `:218` `_installValidatedStagedSnapshot(stagingPaths.dbPath)` → `chainStorageManager.installValidatedPartialSyncSnapshot` → `movePath` | preflight precedes it |
| Cutover move + EXDEV fallback | `chainStorageManagerShared.ts:369-384` `movePath`: `fs.move` then on `EXDEV` → `fs.copy` + `fs.remove` | **confirms cross-device copy today; colocation removes EXDEV** |
| Sibling-not-inside guard | `mithrilPartialSyncStaging.ts:32-38`: `if (isPathWithin(resolvedManagedChainPath, resolvedRootPath)) throw …'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN'` | a sibling is NOT within → passes; preserved unchanged |
| `resolveMithrilWorkDir` | `chainStoragePathResolver.ts:124-125` → `resolveManagedChainPathFromEntryPoint(stateDir)` (`:37-92`) | returns the **realpath-resolved chain dir** (`<stateDir>/chain` or the symlink target, e.g. `/mnt/x/chain`) — i.e. the chain DIRECTORY, not its parent |
| `context.mithrilWorkDir` source | `chainStorageCoordinator.ts:255-256` `await this._chainStorageManager.resolveMithrilWorkDir()` | built fresh per `startPartialSync`; type `mithrilWorkDir: string` (`:17`) |
| `_deriveAllowedRecoveryActions` | `:516-528`: `'preparing'` → `['retry','restart-normal','wipe-and-full-sync']` (retriable) | new error uses `'preparing'` → retriable |
| `_createStageError(stage, message, code?)` | `:884-890` → `new MithrilPartialSyncStageError(message, stage, code)` | error seam |
| `DISK_SPACE_REQUIRED` | `config.ts:161` `= 4 * 1073741274` (≈4 GB) | floor for required bytes |
| `check-disk-space` | default import (`import checkDiskSpace from 'check-disk-space'`) used in 4 utils; dep `package.json:216`; signature `(path) => Promise<{ free: number; size: number }>` | mock pattern: `jest.mock('check-disk-space', () => jest.fn())` (see `chainStorageValidation.spec.ts:20`) |
| `_latestSnapshot` | field `:99`; set `:148` and again `:192`, both **before download** `:194`; `MithrilSnapshotItem.size: number` | `snapshot.size` is available pre-download |

## DESIGN DECISIONS (RESOLVED)

### (a) Derive staging from `path.dirname(context.mithrilWorkDir)` via a NEW sticky field `_stagingChainDir`
**Chosen.** Store the resolved chain dir in a dedicated field set at the top of `start()`
(`this._stagingChainDir = context.mithrilWorkDir`), and have `_getStagingRootPath()` return
`path.join(path.dirname(this._stagingChainDir), PARTIAL_SYNC_STAGING_DIRECTORY_NAME)` (with a
`stateDirectoryPath` fallback when the field is unset).

**Why `mithrilWorkDir` and not `_activeWorkDir` or `managedChainPath`:**
- `mithrilWorkDir` is **realpath-resolved** (`resolveManagedChainPathFromEntryPoint` follows the
  symlink). For a chain symlinked `<stateDir>/chain` → `/mnt/x/chain`, `mithrilWorkDir = /mnt/x/chain`
  (real volume) whereas `context.layoutResult.managedChainPath = <stateDir>/chain` (the symlink). Only
  `path.dirname(mithrilWorkDir) = /mnt/x` lands on the **chain's real volume**; `dirname(managedChainPath)`
  = `<stateDir>` would be the **wrong volume** — exactly the relocated-chain bug. So `mithrilWorkDir` is
  the correct, PRD:250-faithful source.
- `_activeWorkDir` is unsafe: it equals the chain dir only transiently (`:129`–`:163`), is then reassigned
  to a path *inside* the staging tree (`:163`), and is `null` during post-terminal recovery — using it
  would compute a wrong/doubly-nested staging path.

**Why `path.dirname` (sibling), reconciling PRD:250 vs D7:** PRD:250 says "under the resolved Mithril work
directory"; D7 says "sibling of the managed chain". Because the resolved work directory **is** the chain
directory, and the staged-only-restore lock forbids nesting staging inside the **live** chain (the
`isPathWithin` guard would throw `PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN`), the faithful realization is
a **sibling on the same volume**: `path.dirname(chainDir)/mithril-partial-sync`. A directory and its
parent are on the same filesystem, so cutover (`fs.move`) is an intra-volume rename. For the default
(non-relocated) case `mithrilWorkDir = <stateDir>/chain` ⇒ staging `= <stateDir>/mithril-partial-sync` —
**byte-identical to today**, so no default-storage regression.

**Rejected:** (i) derive literally from `_activeWorkDir` as the JSON wording suggests — it is the chain
dir / mutated mid-flow / null in recovery (wrong & unsafe); (ii) place staging *inside* the work dir —
violates the not-inside guard and the staged-only lock; (iii) add a `resolveStagingRoot()` to
`chainStorageManager` — over-engineering; the resolved dir is already on `context`.

### (b) `_stagingChainDir` is STICKY — set in `start()`, NOT cleared in `_clearRuntimeWorkState()`
**Critical for cleanup correctness.** `restart-normal` and `wipe-and-full-sync` are post-terminal recovery
actions: `_assertRecoveryActionAllowed` (`:531-545`) requires `_activeWorkDir`/`_currentProcess` to be
`null`, i.e. they run **after** `start()`'s `finally` already ran `_clearRuntimeWorkState()` (`:246`).
Their cleanup (`fs.remove(this._getStagingRootPath())` at `:327` / `_cleanupPartialSyncArtifacts()` →
`:548`) must still resolve the **colocated** staging path. Therefore `_stagingChainDir` must **survive**
the runtime-state clear:
- Set it in `start()` at `:129` (beside `_activeWorkDir = context.mithrilWorkDir`).
- Do **NOT** add it to `_clearRuntimeWorkState()`; it is overwritten on the next `start()`.
- If it were cleared, post-failure recovery would clean `stateDirectoryPath/mithril-partial-sync` (legacy)
  and **leak** the colocated `/<chain-parent>/mithril-partial-sync` dir (re-introducing a BUG2-class
  orphan). `cancel()` cleans before its clear (`:278` < `:308`), so it is safe either way — but
  `restart-normal`/`wipe` after a `failed`/`finalizing` state are NOT, which is why sticky is required.

### (c) Disk preflight: placement, stage/code, and required-bytes formula
- **Placement:** in `start()`, immediately after `_prepareStagingDirectory(...)` returns (after `:161`)
  and **before** the `_activeWorkDir` reassignment (`:163`), the latest-drift recheck (`:179`), and the
  download (`:194`). At this point `_latestSnapshot.size` is known (`:148`) and the staging root exists
  (so `checkDiskSpace` measures the colocated volume). `_prepareStagingDirectory`'s `fs.remove`/`ensureDir`
  touch only scratch (never the live chain), so the preflight still runs before any destructive/download
  step (acceptance "before any destructive/download step").
- **Stage/code:** stage `'preparing'` (already valid; maps to the **retriable** recovery set). New code
  constant `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` (string literal beside `PARTIAL_SYNC_LATEST_DRIFT_CODE`
  at `:58`). Emit via `this._createStageError('preparing', <msg>, PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE)`.
  **Hand-off:** task-ux-403's renderer error-copy map must add bespoke copy for this code.
- **Required bytes:** `requiredBytes = Math.max((snapshot.size ?? 0) * 1.2, DISK_SPACE_REQUIRED)`.
  `snapshot.size` is the FULL certified-DB size (a conservative over-estimate of the partial range — good
  for a fail-closed check); 1.2× covers LSM-conversion + filesystem slack; the `DISK_SPACE_REQUIRED`
  (≈4 GB) floor guarantees the check still fails closed on a near-full disk when `size` is `0`/absent
  (`normalizeSnapshotItem` defaults `size` to `0`).
- **Fail-closed semantics (RESOLVED):** **block on a positive measurement** (`free < requiredBytes` →
  throw the typed error — this satisfies the acceptance "fails closed … when space is insufficient":
  insufficiency = a positive `free < required` reading). On a `checkDiskSpace` **throw** (cannot measure),
  **log a warning and proceed** — matching the repo's fail-open-on-measurement-error precedent in
  `handleDiskSpace.ts:176-181` ("we could not check disk space, but we will try… anyway"). (NOTE: do NOT
  model this on `preflightLegacyMigration` `chainStorageManagerLayout.ts:452` — it calls `checkDiskSpace`
  with no try/catch, so a measurement throw aborts it; that is the opposite of what we want here.) Fail-open
  on measurement error is safe: pre-download nothing is destructive, and a true mid-download disk-full fails
  the download stage retriably with no live-chain mutation (staged-only lock). Blocking on a transient
  measurement error would wrongly prevent all starts.

## Files expected to change (exact paths)
1. `source/main/mithril/MithrilPartialSyncService.ts` — imports; new code constant + safety-factor
   constant; new sticky `_stagingChainDir` field; set it in `start()`; rewrite `_getStagingRootPath()`;
   add `_assertSufficientDiskSpace()` + its call site in `start()`.
2. `source/main/mithril/MithrilPartialSyncService.spec.ts` — add the `check-disk-space` mock +
   `DISK_SPACE_REQUIRED` to the `../config` mock; update the hardcoded staging-path expectations to the
   colocated paths; add disk-preflight + colocation tests.

**Targets in the JSON that do NOT change (and why):**
- `source/main/utils/chainStorageManager.ts` — listed in JSON targetPaths, but no change is needed: the
  resolved dir is already provided via `context.mithrilWorkDir`, and `DISK_SPACE_REQUIRED` already exists
  in `config.ts`. (Smallest truthful change; cf. task-ux-102 precedent of not editing a listed target.)
- `mithrilPartialSyncStaging.ts` — the guard already enforces sibling-not-inside; it receives the new root
  unchanged.
- `mithrilPartialSyncPreflight.ts` — the disk preflight belongs inline in `start()` (consistent with how
  the immutable-range preflight is already inlined), not in this module.
- `chainStorageCoordinator.ts`, `config.ts` — no change (context + constant already exist).

## Implementation approach — ordered, mechanical steps

### Step 1 — Imports (`MithrilPartialSyncService.ts`)
- Extend the config import at `:6` to add `DISK_SPACE_REQUIRED`:
  ```ts
  import { launcherConfig, stateDirectoryPath, DISK_SPACE_REQUIRED } from '../config';
  ```
- Add a default import for `check-disk-space` near the top imports (e.g. after the `path`/`fs` imports):
  ```ts
  import checkDiskSpace from 'check-disk-space';
  ```

### Step 2 — Constants (after `:58`, beside `PARTIAL_SYNC_LATEST_DRIFT_CODE`)
```ts
const PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE =
  'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE';
// Scratch-space requirement. snapshot.size is the FULL certified DB (a conservative over-estimate of the
// partial range); 1.2× covers LSM-conversion + FS slack. Floored at DISK_SPACE_REQUIRED so a missing/zero
// size still fails closed on a near-full disk. Renderer copy for the code lives in task-ux-403.
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1.2;
```

### Step 3 — New sticky field (beside `_activeWorkDir` at `:95`)
```ts
_stagingChainDir: string | null = null;
```

### Step 4 — Set it in `start()` (at `:129`, beside the `_activeWorkDir` assignment)
```ts
this._activeWorkDir = context.mithrilWorkDir;
this._stagingChainDir = context.mithrilWorkDir;
```
Do **NOT** add `_stagingChainDir` to `_clearRuntimeWorkState()` (Decision (b) — it must survive into
post-terminal recovery cleanup; it is overwritten on the next `start()`).

### Step 5 — Rewrite `_getStagingRootPath()` (`:577-578`)
Before:
```ts
_getStagingRootPath(): string {
  return path.join(stateDirectoryPath, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
}
```
After:
```ts
_getStagingRootPath(): string {
  // Colocate staging as a SIBLING of the resolved managed chain (backend PRD:250 / D7/BUG3): same volume
  // as the chain so cutover is an intra-volume rename, never inside the live chain subtree (the
  // isPathWithin guard in preparePartialSyncStagingDirectory still enforces that). _stagingChainDir is the
  // realpath-resolved chain dir captured at start(); the stateDirectoryPath fallback preserves the legacy
  // default location when no sync has run this session (cross-session reclaim is task-ux-202).
  const stagingParent = this._stagingChainDir
    ? path.dirname(this._stagingChainDir)
    : stateDirectoryPath;
  return path.join(stagingParent, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
}
```

### Step 6 — Disk preflight helper (new method; place near `_prepareStagingDirectory`, ~after `:752`)
```ts
async _assertSufficientDiskSpace(stagingRootPath: string): Promise<void> {
  const snapshotSize = this._latestSnapshot?.size ?? 0;
  const requiredBytes = Math.max(
    snapshotSize * PARTIAL_SYNC_DISK_SAFETY_FACTOR,
    DISK_SPACE_REQUIRED
  );

  let freeBytes: number;
  try {
    ({ free: freeBytes } = await checkDiskSpace(stagingRootPath));
  } catch (error) {
    // Could not measure — do NOT false-block (repo precedent: handleDiskSpace.ts:176-181 fail-open).
    logger.warn(
      'MithrilPartialSyncService: disk-space preflight could not measure free space; proceeding',
      { error, stagingRootPath }
    );
    return;
  }

  if (freeBytes < requiredBytes) {
    const requiredGb = Math.ceil(requiredBytes / 1073741824);
    const freeGb = Math.floor(freeBytes / 1073741824);
    throw this._createStageError(
      'preparing',
      `Not enough free disk space to stage the Mithril partial sync on the chain storage volume. ` +
        `Required ~${requiredGb} GB, available ~${freeGb} GB.`,
      PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE
    );
  }
}
```

### Step 7 — Call the preflight in `start()` (insert between `:161` and `:163`)
```ts
      const stagingPaths = await this._prepareStagingDirectory(
        context.layoutResult.managedChainPath
      );

      await this._assertSufficientDiskSpace(stagingPaths.rootPath);   // <-- NEW (D7/BUG3 preflight)

      this._activeWorkDir = stagingPaths.downloadParentPath;
```
`stagingPaths.rootPath` is the resolved colocated staging root, so `checkDiskSpace` measures the chain
volume. The throw lands in the existing `start()` `catch` (`:230`), which sets `status: 'failed'` with
`allowedRecoveryActions = _deriveAllowedRecoveryActions(error)` = the retriable set for `'preparing'`.

### Step 8 — No other call-site changes
`_cleanupPartialSyncArtifacts` (`:548`) and `wipeAndFullSync` (`:327`) keep calling `_getStagingRootPath()`;
with the sticky `_stagingChainDir` they clean the colocated dir.

## Locked invariants this task honors (inline)
- **#1 Staged-only restore / no in-place mutation:** new staging root is a **sibling**
  (`path.dirname(chainDir)/mithril-partial-sync`), never inside the chain; the `isPathWithin` guard
  (`mithrilPartialSyncStaging.ts:32`) still rejects any inside-subtree path. No code now writes into the
  live chain earlier than today; the preflight runs at `'preparing'`, well before cutover.
- **#4 No auto-trigger / no renderer-computed threshold:** preflight is backend-internal inside an
  already-confirmed `start()`; the required-bytes formula is backend-owned constants, never sent to the
  renderer.
- **#5 No user-facing storage-location picker:** placement is computed automatically; no UI, no location
  config key. (This is the in-scope *engineering* scratch placement, per `:546-548`.)
- **#6 Cancellation forbidden after cutover:** untouched (`cancel()` guard `:259-263` and cutover ordering
  unchanged).
- **#8 No synthetic throughput / no raw JSON in copy:** the preflight emits a typed
  `MithrilPartialSyncStageError` with a structured code, not raw mithril-client output.
- **#2 Recovery backend-authoritative (note-only):** the new error's `'preparing'` stage maps via
  `_deriveAllowedRecoveryActions` to `['retry','restart-normal','wipe-and-full-sync']`; renderer renders
  strictly from `allowedRecoveryActions`. No renderer change here.
- **#11 No bootstrap regression:** this task does not touch the bootstrap path or shared progress
  components. Confirmed.

## Acceptance criteria (carried verbatim from tasks JSON)
1. Staging is colocated on the resolved chain volume, honoring backend PRD line 250.
2. A disk-space preflight runs before any destructive/download step.
3. No user-facing storage picker is introduced.

## Verification plan (exact commands)
From repo root:
- `yarn compile` — validates the new field, helper, import, and constant.
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` — all existing tests (updated to
  the colocated paths) + new disk/colocation tests pass.
- `./node_modules/.bin/eslint source/main/mithril/MithrilPartialSyncService.ts source/main/mithril/MithrilPartialSyncService.spec.ts`
  — clean on touched files (scoped lint per env guidance; compare warning count to pre-change baseline).

### Test plan (map each tasks-JSON testCase to a concrete test)
Mock setup additions in `MithrilPartialSyncService.spec.ts`:
- Add `jest.mock('check-disk-space', () => jest.fn());` at the top (mirrors `chainStorageValidation.spec.ts:20`).
- Add `DISK_SPACE_REQUIRED: 1024` to the `jest.mock('../config', () => ({ ... }))` object (`:24-34`) so
  the floor is small and testable (mirrors `chainStorageManager.spec.ts`).
- In `beforeEach`, default the disk mock generous so existing pass-through tests stay green:
  `require('check-disk-space').mockResolvedValue({ free: 1_000_000_000_000, size: 2_000_000_000_000 });`

1. **"Staging path resolves under the chain work dir, not the top-level state dir"** — `createContext()`
   uses `mithrilWorkDir: '/tmp/mithril-workdir'` (`:52`) ⇒ `path.dirname` = `/tmp` ⇒ staging root
   `/tmp/mithril-partial-sync`. **Update the existing default-storage test** (`:190-282`): change every
   `'/tmp/daedalus-state/mithril-partial-sync...'` expectation to `'/tmp/mithril-partial-sync...'`
   (the `--download-dir` arg `:239-240`, the install `dbPath` `:255`, `removeMock` `:276-277`, `ensureDirMock`
   `:279-280`, the readdir/move targets `:218,:223,:249`). Leave `logPath` (`:271`,
   `/tmp/daedalus-state/Logs/...`) unchanged — `_getLogPath()` is independent of staging.
2. **"Relocated/symlinked chain stages on the same volume as the chain"** — **update the custom-storage
   test** (`:408-465`): with `mithrilWorkDir: '/mnt/custom-storage/chain'` (`:455`), staging root must be
   `/mnt/custom-storage/mithril-partial-sync` (same volume as the chain), NOT
   `/tmp/daedalus-state/mithril-partial-sync`. Update `removeMock` (`:459-460`), `ensureDirMock`
   (`:462-463`), and the readdir/move/download-dir keys (`:436,:441`) accordingly. This is the key BUG3
   assertion (today the test encodes the bug).
3. **"Disk-space preflight fails closed with an actionable error when space is insufficient"** — new test:
   the `createLatestSnapshot` fixture (`:55-62`) hardcodes `size: 2`, so to drive a large required size
   either (a) extend `createLatestSnapshot` to accept an optional `size` and pass a large value, or (b)
   spy `resolveLatestSnapshotMetadata` to resolve a snapshot object whose `.snapshot.size` is large (e.g.
   `10_000_000_000`) — do NOT assume `createLatestSnapshot(25)` yields a large size. Then
   set `require('check-disk-space').mockResolvedValue({ free: 1, size: 2 })`; assert
   `service.start(createContext())` rejects, and `service.status` has `status: 'failed'`,
   `error.code === 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE'`, `error.stage === 'preparing'`, and
   `allowedRecoveryActions` includes `'retry'`. Also assert the download command spy was **never** called
   (preflight precedes download). Add a sibling positive case (ample `free` + generous mock ⇒ start
   resolves and download runs) — covered by the updated test #1 with the default generous mock.
   Add a measurement-error case: `mockRejectedValueOnce(new Error('no measure'))` ⇒ start proceeds (no
   throw from the preflight) — proves fail-open-on-measurement-error.
4. **"Cutover is an intra-volume operation when staging is colocated"** — `fs.move` is mocked, so assert
   the colocation precondition: in the custom-storage test, the install/move source `dbPath`
   (`/mnt/custom-storage/mithril-partial-sync/download/db`) shares the parent dir of the chain
   (`/mnt/custom-storage`) with the chain target — i.e. `path.dirname` of the staging root equals
   `path.dirname` of `mithrilWorkDir`. (The real `EXDEV`→copy fallback is `movePath`'s own unit concern in
   `chainStorageManagerShared`, not this service.)

**REQUIRED — two MORE `start()`-driving tests also hardcode the old staging path and WILL break (caught
by critique):** besides the default (`:190-282`) and custom (`:408-465`) tests above, these two key their
`readdirMock` on `/tmp/daedalus-state/mithril-partial-sync/...` and must move to `/tmp/mithril-partial-sync/...`
(since `createContext().mithrilWorkDir = '/tmp/mithril-workdir'`):
- `'maps Mithril progress into downloading and verifying status updates'` (`:510-594`) — update the
  `readdirMock` keys at `:538` and `:543`. If left unchanged, the staged-db `readdir` no longer matches,
  `validateConvertedStagedOutput` throws `PARTIAL_SYNC_STAGED_DB_INVALID`, and `start()` REJECTS while the
  test asserts `.resolves.toBeUndefined()` (`:570`) — a HARD failure of the plan's own `yarn test:jest`
  gate.
- `'fails in installing when converted staged output includes volatile'` (`:705-761`) — update the
  `readdirMock` keys at `:727` and `:732`, else the `volatile` branch is never exercised (the test would
  pass for the wrong reason).
**Tests that must stay UNCHANGED (verified by critique):** the manual-state cancel/restart/wipe tests
(`:763-898`) never call `start()`, so `_stagingChainDir` stays `null` ⇒ `_getStagingRootPath()` returns the
`stateDirectoryPath` fallback `/tmp/daedalus-state/mithril-partial-sync`; their `removeMock` assertions
(`:799,:863,:892`) remain correct as-is. Do not touch them, and do not touch `logPath` (`:271`).

## Risks / open questions
- **Sticky `_stagingChainDir` (RESOLVED, Decision (b)).** Not clearing it in `_clearRuntimeWorkState()` is
  deliberate and load-bearing for recovery-cleanup correctness; the implementer must NOT "tidy" it into
  the clear. It is overwritten on the next `start()`.
- **Cross-session leftover staging (HAND-OFF to task-ux-202).** A crash/close before any in-session
  `start()` leaves `_stagingChainDir` null ⇒ the fallback targets the legacy default. Reclaiming a
  colocated leftover at startup is task-ux-202's C2-branch `fs.remove(stagingRoot)` extension; that task
  must derive the same colocated path from the marker's `managedChainPath` / resolved work dir. Noted in
  research.
- **`path.dirname` of a mounted chain dir (LOW).** If the realpath-resolved chain dir is itself a mount
  point, `path.dirname` is its mount parent (possibly a different volume). This is an exotic setup;
  realpath handles the common symlink case correctly, and it is strictly better than today's top-level
  state dir. Flag for QA (task-ux-702 custom-storage matrix), not a blocker.
- **`snapshot.size` over-estimate (ACCEPTED).** Requiring 1.2× the full certified-DB size is conservative
  (partial sync downloads less); this is desirable for a fail-closed preflight and avoids summing
  per-immutable sizes the aggregator does not expose pre-download. Do not over-engineer a range-aware
  estimate.
- **Fail-open on measurement error (RESOLVED, Decision (c)).** Matches repo precedent and is safe under
  staged-only restore. If the Critiquer/PRD insists on fail-closed even on measurement error, flip the
  `catch` to rethrow a typed `'preparing'` error.

## Required doc / research updates
- Record in `task-ux-201-research.md`: the `mithrilWorkDir`-realpath rationale (symlink case is the
  decider), the PRD:250-vs-D7 reconciliation (sibling = "under the work dir" given work dir == chain dir),
  the sticky-`_stagingChainDir` correctness reasoning, the disk-preflight placement/code/formula, and the
  task-ux-202 cross-session hand-off.
- No PRD edit required — D7/BUG3 and PRD:250 already specify the intent; the code is reconciled to them.
  Optionally note the chosen new error code `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` for task-ux-403.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-201-plan-review.md` (append-only).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-201-impl-review.md` (created at impl time).

## Final outcome (completed 2026-06-22)
Implemented exactly as planned. In `MithrilPartialSyncService.ts`: added `import checkDiskSpace from
'check-disk-space'` + `DISK_SPACE_REQUIRED` to the config import; added `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE`
and `PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1.2`; added the **sticky** `_stagingChainDir` field set in `start()`
from `context.mithrilWorkDir` and deliberately **not** cleared in `_clearRuntimeWorkState()`; rewrote
`_getStagingRootPath()` to derive staging as a SIBLING via `path.dirname(this._stagingChainDir)` (with a
`stateDirectoryPath` fallback), so the default case is byte-identical to today and the relocated/symlinked
case lands on the chain's real volume (cutover `fs.move` becomes an intra-volume rename, no `EXDEV` copy);
added `_assertSufficientDiskSpace()` (required = `max(snapshot.size * 1.2, DISK_SPACE_REQUIRED)`, fail-closed
on `free < required` via a typed retriable `'preparing'`-stage `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` error,
fail-open + `logger.warn` on a `checkDiskSpace` measurement throw per `handleDiskSpace.ts:176-181` precedent)
and called it in `start()` right after `_prepareStagingDirectory` returns and before the `_activeWorkDir`
reassignment / download / cutover. No IPC/type/channel change; `chainStorageManager.ts` and `config.ts`
unchanged (resolved dir already on `context.mithrilWorkDir`; `DISK_SPACE_REQUIRED` already in `config.ts:161`).

Tests (`MithrilPartialSyncService.spec.ts`): added the `check-disk-space` mock + `DISK_SPACE_REQUIRED` to
the config mock + a generous `beforeEach` default; migrated all staging-path expectations to the colocated
locations (default `/tmp/mithril-partial-sync/…`; custom `/mnt/custom-storage/mithril-partial-sync/…`),
including the `readdir` keys in the progress-mapping and volatile-install tests caught by critique; added 2
new disk-preflight tests (fail-closed insufficient-space + fail-open measurement-error).

Verification: `tsc --noEmit` exit 0 (`yarn compile` fails only on the pre-existing `precompile: typedef:sass`
sass/Node-v24 hook — environment-only, not TS); `yarn test:jest …MithrilPartialSyncService.spec.ts` → 32/32
(2 new); eslint on both touched files → 0 errors, 6 pre-existing warnings, none new.

Plan critique: approved after one fix pass (added the two missed `readdir`-key test updates; corrected the
fail-open precedent citation; pinned the large-size test setup). Code review: approved, independently
re-verified (tsc/jest/lint re-run, sticky-field correctness re-reasoned, deviation confirmed sound).
Deviation: the `'rejects staging paths inside the managed chain subtree'` test (a `start()`-calling test, not
one of the manual-state tests the plan froze) needed its trigger changed to `managedChainPath:'/'` /
`mithrilWorkDir:'/something'` so the guard still fires under the new derivation — same invariant, valid
trigger. Two non-blocking MINOR test-quality nits (a tautological colocation assertion; a negative-match
download guard) left as-is per convergence; recorded in the impl-review log and research note. The
pre-existing `M .gitignore` and `M prompt-ux-refinement.md` are unrelated and excluded from this task's commit.

## Status
- Planning status: `approved`
- Build status: `completed`
