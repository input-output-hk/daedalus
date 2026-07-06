# task-ux-704 — CAT-C per-section plan: Chain-storage & disk-space layer (C-1…C-5)

> Per-CAT implementation doc for task-ux-704 (code-quality remediation wave). **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent doc:
> `task-ux-704.md` (traceability rows C3, C4, C5+C10f, C6-partial, C10a–h + C1-partial). Every change here is
> behavior-preserving. If this doc ever disagrees with live code, prefer live code, locate anchors
> by the QUOTED snippets (never by line number), and reconcile here. All line numbers below were
> verified against the working tree on 2026-07-04 and are provided only as a reading aid.

## Sequencing position and seam contracts

CAT-C is the **third** section (order A → B → C → D → E → F → G → H) and lands as **one commit**.

- **Co-touch with CAT-B (`handleDiskSpace.ts`):** CAT-B wires new injected dependencies at the
  `new MithrilPartialSyncNodeStartup({ ... })` construction (~line 124). CAT-C's only edit in
  that file is the inert `chainEmpty` branch inside
  `case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:` (~lines 251–308) plus one spec
  assertion. Different regions of the same file; CAT-B commits first, so locate CAT-C's edit by
  the quoted switch-case snippet, not by line number.
- Nothing in this section touches `mithrilPartialSyncChannel.ts` (S1), `mithrilErrors.ts` (S2),
  or any story/renderer file (S3). CAT-H (S4) re-locates its comment-inventory entries after
  this commit by quoted text.

Suggested commit subject (single line, no body, no trailer):

```
refactor(mithril): de-fork chain path resolution and prune dead chain-storage code
```

## Locked invariants that constrain CAT-C

- **Behavior gate:** no IPC channel names or payload shapes change; no validation semantics
  change (`getConfig`, `validate`, `setDirectory` decision logic untouched); no mutation-lock
  semantics change (FIFO order, swallow-previous-failure, warn-and-rethrow, synchronous queue
  swap all preserved); no user-visible copy changes. Log **message strings** stay byte-identical
  except the ONE new warn line introduced in Step 1.3; log lines are not IPC.
- **D-702b-9 stands:** the 5-minute TTL behind-ness caches in `MithrilPartialSyncService`
  (`PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS`, `_latestCertifiedImmutableCache`,
  `_localImmutableCache`, `_invalidateBehindnessCaches`) keep their exact semantics. CAT-C only
  corrects three rationale comments that C-1 makes factually stale (Step 1.4).
- **Comments/test titles:** never cite task/finding IDs (C-1, CAT-C, D-702b-9…) in source
  comments or test titles. The rationale comments below are written accordingly — copy them
  verbatim. 1–3 plain lines, invariant/why only.
- **Prettier 2.1.2:** never reformat `toHaveBeenCalledWith('string', { ... })` call shapes
  (known oscillation). New assertions below already avoid that shape. Classify any
  `prettier:check` failure against pre-existing HEAD drift first
  (`git show HEAD:<file> | prettier --stdin-filepath <file>`) before treating it as caused here.
- **i18n:** no new i18n messages; no `en-US.json`/`ja-JP.json` changes; nothing here renders to
  the user.

---

# 1. C-1 — De-fork `getManagedChainPath()` / `resolveDiskSpaceCheckPath()` (the perf fix)

**Defect (perf, HIGH payoff):** `ChainStorageManager.getManagedChainPath()` resolves a path by
calling `getConfig()` → `_getDefaultStorageConfig()` → `checkDiskSpace(diskCheckPath)`
(`chainStorageManagerConfig.ts:15-30`). `checkDiskSpace` **forks a child process** and its result
is **discarded** — `getConfig` only contributes `customPath`, derived from
`_captureChainPathState()` (`chainStorageManagerConfig.ts:37-41`). Consequences at HEAD:

- `getDiskSpaceStatusChannel.onReceive` (`handleDiskSpace.ts:508-513`) forks `checkDiskSpace`
  **twice** per renderer request: `resolveDiskSpaceCheckPath()` → `getConfig` (fork #1, wasted),
  then `getDiskCheckReport` (fork #2, the real probe).
- `isManagedChainEmpty()` (`chainStorageManager.ts:210-220`) pays the wasted fork on every
  stopped-node poll tick (`DISK_SPACE_CHECK_LONG/MEDIUM/SHORT_INTERVAL` = 10 min / 1 min / 10 s,
  `source/main/config.ts:167-171`).
- The behind-ness probe (`MithrilPartialSyncService.ts:991-992`) pays it on each cache miss;
  `removeManagedDirectory`, `emptyManagedContents` (manager), and the two snapshot installers
  (`chainStorageManagerLayout.ts:533`, `:580` via `ctx.getManagedChainPath()`) pay it per call.

**Fix:** derive `customPath` from `_captureChainPathState()` directly (the exact
`chainStorageManagerConfig.ts:37-41` logic, minus the probe) in a path-only reader; point
`getManagedChainPath()` and `resolveDiskSpaceCheckPath()` at it. `getConfig()` itself is
**untouched in behavior** (its callers legitimately need `availableSpaceBytes`); it merely reuses
the extracted pure derivation. `setDirectory` (`getConfig` at `chainStorageManager.ts:113`) and
`prepareForLocationChange` (`:224`) deliberately keep calling `getConfig` — user-triggered,
mutation-locked paths where the probe cost is immaterial; do not touch them.

### Equivalence argument (do not re-derive; copy into the PR discussion if challenged)

Old and new both feed `_getManagedChainPath(customPath)`; only the derivation of `customPath`
moves. `_captureChainPathState()` output cases:

1. **missing** / **plain directory** → old: `customPath = null` (the
   `chainState.type === 'symlink'` condition fails) → new: helper returns `null`. Same bytes.
2. **healthy symlink or Windows junction** → both compute
   `path.dirname(chainState.resolvedPath)` — literally the same two lines. Same bytes.
   (Supporting invariant: every manager-created entry point targets a directory named `chain` —
   `setDirectory` links to `_getManagedChainPath(validation.path)` = `<parent>/chain` — so the
   `dirname` → `join(..., 'chain')` round-trip reproduces the target.)
3. **broken symlink** (readlink ok, realpath fails) → `resolvedPath` is `undefined` in both →
   `null` → default path. Same bytes.
4. **`_captureChainPathState()` throws** (e.g. `EACCES` from `_safeLstat`) → old: `getConfig`'s
   catch returns `customPath: null`; new: local `try/catch → null`. Same bytes.
5. **Sole divergence — probe-failure error path:** old code, when `_getDefaultStorageConfig()`
   itself threw (i.e. `checkDiskSpace` or the state-dir `realpath` failed) while a healthy
   symlink existed, fell into `getConfig`'s catch and returned `customPath: null` — silently
   redirecting emptiness/disk checks to the **default** location. New code never runs the probe,
   so it returns the correct custom path. This is a strict correctness improvement inherent to
   removing the probe coupling; it is accepted, not a regression.

### Step 1.1 — Pure derivation helper in `chainStorageManagerShared.ts`

File: `source/main/utils/chainStorageManagerShared.ts`

Locate (by content):

```ts
export async function captureChainPathState(
```

Insert **immediately above** it (blank line after the inserted block):

```ts
// The chain entry point encodes the active custom location: a symlink (or
// Windows junction) targets `<customParent>/chain`; anything else means the
// default location. getConfig and the path-only readers share this rule.
export const deriveCustomPathFromChainState = (
  chainState: ChainPathState
): string | null =>
  chainState.type === 'symlink' && chainState.resolvedPath
    ? path.dirname(chainState.resolvedPath)
    : null;
```

(`path` and `ChainPathState` already exist in this file — no new imports.)

### Step 1.2 — `getConfig` reuses the helper

File: `source/main/utils/chainStorageManagerConfig.ts`

Locate:

```ts
    const chainState = await ctx._captureChainPathState();
    let customPath = null;

    if (chainState.type === 'symlink' && chainState.resolvedPath) {
      customPath = path.dirname(chainState.resolvedPath);
    }
```

Replace with:

```ts
    const chainState = await ctx._captureChainPathState();
    const customPath = deriveCustomPathFromChainState(chainState);
```

Add to the existing imports from `./chainStorageManagerShared` (the file currently imports only
types from it — add a value import line):

```ts
import { deriveCustomPathFromChainState } from './chainStorageManagerShared';
```

`getConfig`'s try/catch structure, fallbacks, and all log lines stay untouched.

### Step 1.3 — De-fork the two manager readers

File: `source/main/utils/chainStorageManager.ts`

(a) Locate:

```ts
  async getManagedChainPath(): Promise<string> {
    const config = await this.getConfig();
    return this._getManagedChainPath(config.customPath);
  }
```

Replace with:

```ts
  // Path-only twin of getConfig's customPath derivation: reads the chain entry
  // point without getConfig's disk-space probe, so hot callers (emptiness
  // checks, the behind-ness probe, disk-check path resolution) never fork
  // checkDiskSpace just to learn a path.
  async _getActiveCustomPath(): Promise<string | null> {
    try {
      return deriveCustomPathFromChainState(
        await this._captureChainPathState()
      );
    } catch (error) {
      logger.warn(
        'ChainStorageManager: failed to derive custom chain parent from entry point',
        {
          error,
          chainPath: this._chainPath,
        }
      );
      return null;
    }
  }

  async getManagedChainPath(): Promise<string> {
    return this._getManagedChainPath(await this._getActiveCustomPath());
  }
```

(b) Locate:

```ts
  async resolveDiskSpaceCheckPath(): Promise<string> {
    const config = await this.getConfig();
    const managedChainPath = this._getManagedChainPath(config.customPath);
    const managedChainExists = await fs.pathExists(managedChainPath);
```

Replace the first two body lines so the method reads:

```ts
  async resolveDiskSpaceCheckPath(): Promise<string> {
    const managedChainPath = await this.getManagedChainPath();
    const managedChainExists = await fs.pathExists(managedChainPath);

    return managedChainExists
      ? managedChainPath
      : path.dirname(managedChainPath);
  }
```

(c) Add `deriveCustomPathFromChainState` to the existing value-import block from
`./chainStorageManagerShared` (the block that already imports `captureChainPathState`,
`CHAIN_DIRECTORY_NAME`, …).

### Step 1.4 — Comment maintenance this change makes stale (comment-only; NOT CAT-H's density work)

C-1 falsifies four rationale comments that cite the fork. The commit that removes the fork
corrects them.

File: `source/main/mithril/MithrilPartialSyncService.ts` — cache field comment (~:157-159).
Locate:

```ts
  // Dedupe the per-probe local immutable read (getManagedChainPath → fork
  // checkDiskSpace via getConfig, plus the immutable/ readdir) under the SAME 5-min TTL as the
  // aggregator cache. Invalidated on Mithril lifecycle transitions via _invalidateBehindnessCaches().
```

Replace with:

```ts
  // Dedupe the per-probe local immutable read (managed-chain path resolution
  // plus the immutable/ readdir) under the same 5-min TTL as the aggregator
  // cache. Invalidated on Mithril lifecycle transitions via _invalidateBehindnessCaches().
```

Same file — getter comment (~:979-981). Locate:

```ts
  // Cache the local immutable read under the same 5-min TTL as the aggregator. On a
  // hit we skip BOTH getManagedChainPath (which transitively forks checkDiskSpace via getConfig) AND
  // resolveLocalImmutableNumber's immutable/ readdir — the per-probe CPU cost the 30s poll paid every tick.
```

Replace with:

```ts
  // Cache the local immutable read under the same 5-min TTL as the aggregator. On a
  // hit we skip both the managed-chain path resolution and resolveLocalImmutableNumber's
  // immutable/ readdir — the per-probe cost the 30s poll paid every tick.
```

Same file — behind-ness probe comment inside `getPartialSyncBehindness()` (~:1032-1034).
Locate:

```ts
      // Cached local read dedupes the checkDiskSpace fork + immutable/ readdir;
      // the aggregator query and the local read are independent, so run them
      // concurrently.
```

Replace with (byte-identical to the rewrite the CAT-H inventory specifies for this comment;
CAT-H marks that entry handled by this commit):

```ts
      // The aggregator query and local read are independent, so run them concurrently.
```

File: `source/main/mithril/MithrilPartialSyncService.spec.ts` (~:2380-2381). Locate:

```ts
      // A cache hit must skip getManagedChainPath (which forks checkDiskSpace via
      // getConfig) AND the immutable/ readdir — count getManagedChainPath as the proxy for both.
```

Replace with:

```ts
      // A cache hit must skip getManagedChainPath and the immutable/ readdir —
      // count getManagedChainPath as the proxy for both.
```

Beyond these four comment replacements, no other line in either file changes. Cache logic,
invalidation sites, and the spec's assertions are untouched.

### Step 1.5 — Spec additions pinning the de-fork

File: `source/main/utils/chainStorageManager.spec.ts`

The suite already mocks `check-disk-space` (`jest.mock('check-disk-space', () => jest.fn())`)
and re-primes it in `beforeEach` — the `.not.toHaveBeenCalled()` assertions below are valid.
Locate the closing `});` of the test titled
`'getConfig ignores stale legacy config metadata and derives state from the live chain entry point'`
and insert **after** it:

```ts
  it('getManagedChainPath derives the custom parent without probing disk space', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });

    const result = await manager.getManagedChainPath();

    expect(result).toBe('/mnt/custom-parent/chain');
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('getManagedChainPath falls back to the default chain path when the entry point is unreadable', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest
      .spyOn(manager, '_captureChainPathState')
      .mockRejectedValue(Object.assign(new Error('denied'), { code: 'EACCES' }));

    const result = await manager.getManagedChainPath();

    expect(result).toBe('/tmp/state/chain');
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('resolveDiskSpaceCheckPath returns the managed chain directory without probing disk space', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(true);

    const result = await manager.resolveDiskSpaceCheckPath();

    expect(result).toBe('/mnt/custom-parent/chain');
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('resolveDiskSpaceCheckPath falls back to the parent when the managed chain is missing', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(false);

    const result = await manager.resolveDiskSpaceCheckPath();

    expect(result).toBe('/mnt/custom-parent');
  });
```

Note: `handleDiskSpace.spec.ts` mocks the manager/coordinator wholesale, so its
`checkDiskSpace` call-count assertions (`toHaveBeenCalledTimes(1)` etc.) count only
`getDiskCheckReport` probes and are **unaffected** by C-1 — do not touch them.
`MithrilPartialSyncService.spec.ts` spies `getManagedChainPath` directly — also unaffected.

---

# 2. C-2 — Delete the inert `chainEmpty` branch in `handleDiskSpace.ts`

**Finding (verified byte-by-byte):** in
`case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:` (~:251-308) both arms of
`if (chainEmpty)` invoke `mithrilController.handleStoppedNodeStartup({ currentGeneration,
getStaleResponse, response })` with identical arguments and identical
`if (startupResult.handled) { return startupResult.response; }` handling. The empty arm then
`break`s (~:268); the other falls through to the same `break` (~:308) — same switch exit, same
enclosing catch. `chainEmpty` has no other reader. `MithrilStartupGate.handleStoppedNodeStartup`
recomputes `chainStorageCoordinator.isManagedChainEmpty()` internally
(`MithrilStartupGate.ts:248`) **and** performs its own generation staleness check right after it
(`:249-251`), so the pre-check and the staleness re-check at ~:255-257 (which becomes dead once
no await precedes it) lose nothing. Removing the pre-check also removes one
`isManagedChainEmpty()` call per stopped-node poll tick (compounding C-1's de-fork).

### Step 2.1 — Collapse to a single call

File: `source/main/utils/handleDiskSpace.ts`

Locate (the full region to replace, quoted exactly):

```ts
            const chainEmpty =
              await chainStorageCoordinator.isManagedChainEmpty();
            if (currentGeneration !== directoryChangeGeneration) {
              return getStaleResponse();
            }
            if (chainEmpty) {
              const startupResult =
                await mithrilController.handleStoppedNodeStartup({
                  currentGeneration,
                  getStaleResponse,
                  response,
                });
              if (startupResult.handled) {
                return startupResult.response;
              }
              break;
            }

            const startupResult =
              await mithrilController.handleStoppedNodeStartup({
                currentGeneration,
                getStaleResponse,
                response,
              });
            if (startupResult.handled) {
              return startupResult.response;
            }
```

Replace with:

```ts
            const startupResult =
              await mithrilController.handleStoppedNodeStartup({
                currentGeneration,
                getStaleResponse,
                response,
              });
            if (startupResult.handled) {
              return startupResult.response;
            }
```

The surrounding `try { ... } catch (error) { ... }` and the trailing `break;` stay untouched.
`chainStorageCoordinator` remains imported (still used by the `wipeChainAndSnapshots` wiring and
`onDirectoryChanged`).

### Step 2.2 — Spec pin + rejection-path note

File: `source/main/utils/handleDiskSpace.spec.ts`

In the test titled
`'starts cardano-node after recovery when the default managed chain already has data'`, locate
(quoted verbatim in its live multi-line form):

```ts
    expect(
      chainStorageManagerMock.resolveDiskSpaceCheckPath
    ).not.toHaveBeenCalled();
```

and insert **after** it:

```ts
    expect(
      chainStorageCoordinatorMock.isManagedChainEmpty
    ).toHaveBeenCalledTimes(1);
```

(Pins one emptiness read per stopped-node tick — the gate's own.)

No other spec change. In particular the test
`'falls back to starting cardano-node when the non-empty chain check throws'`
(`isManagedChainEmpty.mockRejectedValueOnce`) stays green **unchanged**: the single remaining
call now happens inside the gate, the rejection propagates out of
`handleStoppedNodeStartup` into the same `catch (error)` fallback, and the same
`'[MITHRIL] Failed to handle bootstrap decision'` log fires. All empty/non-empty routing tests
keep working because the spec drives the real `MithrilStartupGate`, whose internal call consumes
the same coordinator mock. If any of these tests fails after this step, stop and escalate — do
not adjust their assertions to fit.

---

# 3. C-3 — Dead public API deletion + resolver collapse

**Verified zero production callers** (repo-wide grep over `source/`, `storybook/`, `tests/`,
2026-07-04):

| Symbol | Home | Callers at HEAD |
| --- | --- | --- |
| `migrateData` | `chainStorageManager.ts` ~:298-332 | spec-only (details below) |
| `getResolvedManagedChainPath` | manager ~:199-201 | none anywhere |
| `getManagedParentPath` | manager ~:203-208 | none anywhere |
| `resolveChainStoragePath` (manager method) | manager ~:176-178 | one spec test (manager.spec ~:428-441) |
| `resolveChainStoragePath` (resolver export) | `chainStoragePathResolver.ts` ~:114-118 | manager import + its own spec describe (~:45-133) |
| `getMithrilPartialSyncDisabledError` | `chainStorageCoordinator.ts` ~:592-593 | none anywhere |

`resolveMithrilWorkDir` **IS ALIVE** (coordinator `_syncMithrilWorkDir` ~:161 and
`startPartialSync` ~:254, via manager ~:180-182) — it survives. Both resolver exports are
byte-identical `return resolveManagedChainPathFromEntryPoint(stateDir);` delegations, and both
doc comments are stale: ~:110-113 describes a nonexistent "custom-path in config" fallback;
~:120-123 claims the function returns "the state directory" (it returns the resolved managed
chain directory).

### Step 3.1 — Manager deletions

File: `source/main/utils/chainStorageManager.ts` — delete these four methods in full:

- `async resolveChainStoragePath(): Promise<string> { return resolveChainStoragePathFn(...); }`
- `async getResolvedManagedChainPath(): Promise<string> { ... }`
- `async getManagedParentPath(): Promise<string> { ... }`
- `async migrateData(fromPath: string, toPath: string, options ...): Promise<void> { ... }`
  (the whole ~35-line body through its closing `}`)

Then remove `resolveChainStoragePath as resolveChainStoragePathFn,` from the
`./chainStoragePathResolver` import block. None of the four is on the
`ChainStorageManagerContext` interface — no interface edit.

### Step 3.2 — Coordinator deletion

File: `source/main/utils/chainStorageCoordinator.ts` — delete:

```ts
export const getMithrilPartialSyncDisabledError = (): string =>
  PARTIAL_SYNC_DISABLED_CODE;
```

Keep the `PARTIAL_SYNC_DISABLED_CODE` const — it is still thrown by
`_assertPartialSyncFeatureEnabled`.

### Step 3.3 — Resolver collapse to one correctly-documented export

File: `source/main/utils/chainStoragePathResolver.ts`

Delete the `resolveChainStoragePath` export **and** its doc comment (both quoted):

```ts
/**
 * Resolves the actual chain storage path, preferring the symlink target if
 * present, falling back to the custom-path in config, then stateDir.
 */
export async function resolveChainStoragePath(
  stateDir: string
): Promise<string> {
  return resolveManagedChainPathFromEntryPoint(stateDir);
}
```

Replace the stale `resolveMithrilWorkDir` doc comment:

```ts
/**
 * Resolves the working directory that Mithril should use. Returns the
 * resolved custom path when configured, otherwise the state directory.
 */
```

with:

```ts
/**
 * Resolves the directory Mithril work products live in: the managed chain
 * directory the chain entry point currently targets — the resolved custom
 * target when a symlink/junction is configured, otherwise `<stateDir>/chain`.
 */
```

### Step 3.4 — Spec surgery

File: `source/main/utils/chainStorageManager.spec.ts`

Delete these four tests in full:

- `'migrateData moves source entries into target and removes source directory'` (~:255-277)
- `'migrateData uses copy+remove fallback for EXDEV move failures'` (~:279-301)
- `'migrateData can preserve the source root directory'` (~:408-426)
- `'resolveChainStoragePath prefers resolved chain symlink target'` (~:428-441; the adjacent
  `'resolveMithrilWorkDir resolves the active symlink target'` test stays)

Three other tests spy on `migrateData` as a not-called guard — `jest.spyOn` on a deleted method
throws, so in each of them remove the spy block and swap the assertion for the equivalent
fs-level pin (the behavior stays pinned; the deleted method's name does not):

(a) `'setDirectory does not migrate when switching between custom locations'` — remove:

```ts
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);
```

and replace `expect(migrateSpy).not.toHaveBeenCalled();` with
`expect(fs.move).not.toHaveBeenCalled();`

(b) `'resetToDefault removes the symlink without migrating data back'` — same removal/swap
(`migrateSpy` → `expect(fs.move).not.toHaveBeenCalled();`).

(c) `'rollback restores the previous symlink target without rewriting config'` — same
removal/swap.

File: `source/main/utils/chainStoragePathResolver.spec.ts`

- Remove `resolveChainStoragePath,` from the import.
- Delete the `describe('resolveChainStoragePath', ...)` block, **but first move its two
  win32-only tests** (`'resolves junction target on win32 when directory entry is a junction'`
  and `'returns entry-point chain path on win32 when directory is not a junction'`) into the
  existing `describe('resolveMithrilWorkDir', ...)` block, changing each
  `await resolveChainStoragePath('/tmp/state')` to `await resolveMithrilWorkDir('/tmp/state')`.
  Those two exercise junction handling of the shared internal
  (`resolveManagedChainPathFromEntryPoint`) that the `resolveMithrilWorkDir` describe does not
  yet cover; the other three are duplicates of tests already present there and are deleted with
  the describe.

---

# 4. C-4 — Mutation-lock dedupe

**Finding:** `_withMutationLock` is line-for-line identical in `ChainStorageManager` (~:505-529)
and `ChainStorageCoordinator` (~:557-581) except the logger prefix
(`'ChainStorageManager: serialized mutation failed'` vs
`'ChainStorageCoordinator: serialized mutation failed'`). The two locks NEST benignly: a
coordinator-locked operation calls manager methods that re-take the **manager** lock —
independent FIFO queues, strictly coordinator→manager order (the manager never calls the
coordinator), so extraction cannot introduce deadlock.

### Step 4.1 — Shared helper

File: `source/main/utils/chainStorageManagerShared.ts`

Insert (e.g. immediately after the `toIsoString` export):

```ts
type SerializedMutationHost = { _mutationQueue: Promise<void> };

// FIFO-serializes mutations per host. The queue swap must stay synchronous
// (before the first await) so concurrent callers chain in call order; a
// failed operation is logged and rethrown but never blocks the queue.
export async function runSerializedMutation<T>(
  host: SerializedMutationHost,
  logPrefix: string,
  label: string,
  operation: () => Promise<T>
): Promise<T> {
  const previousMutation = host._mutationQueue;
  let releaseLock: (() => void) | undefined;

  host._mutationQueue = new Promise<void>((resolve) => {
    releaseLock = resolve;
  });

  await previousMutation.catch(() => undefined);

  try {
    return await operation();
  } catch (error) {
    logger.warn(`${logPrefix}: serialized mutation failed`, {
      error,
      label,
    });
    throw error;
  } finally {
    releaseLock?.();
  }
}
```

The emitted warn strings stay byte-identical to today's. Do NOT restructure the body — the
synchronous swap before `await previousMutation` is the FIFO invariant.

### Step 4.2 — Both classes delegate

File: `source/main/utils/chainStorageManager.ts` — replace the whole `_withMutationLock` method
body with:

```ts
  async _withMutationLock<T>(
    label: string,
    operation: () => Promise<T>
  ): Promise<T> {
    return runSerializedMutation(this, 'ChainStorageManager', label, operation);
  }
```

Add `runSerializedMutation` to the existing shared value-import block.

File: `source/main/utils/chainStorageCoordinator.ts` — same replacement with
`'ChainStorageCoordinator'`. Add a value import (the coordinator currently has only
`import type { ManagedChainLayoutResult } from './chainStorageManagerShared';`):

```ts
import { runSerializedMutation } from './chainStorageManagerShared';
```

No import cycle: `chainStorageManagerShared` does not import the coordinator (its only resolver
dependency is type-only). Keep the thin `_withMutationLock` methods on both classes (internal
callers and any future spec spies stay stable), and keep `_mutationQueue` fields where they are
(`_awaitPendingMutations` reads the coordinator's directly).

**Explicitly out of scope:** the `handleDiskSpace.ts` single-flight queue
(`activeDiskSpaceCheckPromise` / `pendingDiskSpaceCheckArgs` / `pendingDiskSpaceCheckWaiters`,
~:100-110 and ~:347-440) is semantically different — it coalesces runs and merges args; it is
not a FIFO mutation lock. Do not unify it with this helper.

No new specs: the coordinator suite's serialization tests
(`'serializes directory mutations through the coordinator lock'`,
`'waits for an in-flight layout mutation before reading config'`) and the manager suite pin the
behavior through public methods and must stay green unchanged.

---

# 5. C-5 — Trivia batch (each independently verified)

### 5a — Unreachable throw in `startBootstrap`

File: `source/main/utils/chainStorageCoordinator.ts`. Locate inside
`_withMutationLock('startBootstrap', ...)`:

```ts
      this._assertBootstrapMutationAllowed('start Mithril bootstrap');

      if (this._bootstrapInProgress) {
        throw new Error('Mithril bootstrap is already in progress.');
      }
```

Delete the `if (this._bootstrapInProgress) { ... }` block (and its preceding blank line).
`_assertBootstrapMutationAllowed` already throws for `_bootstrapInProgress` on the previous
line, synchronously — the branch is unreachable. No spec anywhere pins the
`'Mithril bootstrap is already in progress.'` message (verified by repo-wide grep).

### 5b — Merge the twin switch arms in `ensureManagedChainLayout`

File: `source/main/utils/chainStorageManagerLayout.ts`. Locate:

```ts
    case 'managed-custom-root': {
      await ctx._adoptManagedLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }

    case 'inconsistent': {
      await ctx._adoptManagedLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }
```

Replace with:

```ts
    case 'managed-custom-root':
    case 'inconsistent': {
      await ctx._adoptManagedLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }
```

### 5c — Rename the shadowed inner `validation` in `setDirectory`

File: `source/main/utils/chainStorageManager.ts`. The outer
`const validation = await this.validate(normalizedTargetDir);` is shadowed four lines later.
Locate:

```ts
      if (validation.path == null) {
        const validation = await this._resetToDefault();
        this._isRecoveryFallback = false;
        return validation;
      }
```

Replace with:

```ts
      if (validation.path == null) {
        const defaultValidation = await this._resetToDefault();
        this._isRecoveryFallback = false;
        return defaultValidation;
      }
```

(The earlier `const validation` in the `normalizedTargetDir == null` branch is a sibling scope,
not a shadow — leave it.)

### 5d — `_assertPartialSyncFeatureEnabled` delegates to `isPartialSyncEnabled()`

File: `source/main/utils/chainStorageCoordinator.ts`. Locate:

```ts
  _assertPartialSyncFeatureEnabled(): void {
    if (launcherConfig.mithrilPartialSyncEnabled !== true) {
```

Replace the condition line with:

```ts
    if (!this.isPartialSyncEnabled()) {
```

(`isPartialSyncEnabled()` is `launcherConfig.mithrilPartialSyncEnabled === true` — logically
identical. Warn line and thrown code unchanged. `launcherConfig` KEEPS a reader in this file —
`isPartialSyncEnabled` itself (:75-76) — so do NOT remove it from the `'../config'` import.)

### 5e — `buildLayout` helper for `detectLayout`'s six near-identical literals

File: `source/main/utils/chainStorageManagerLayout.ts`. `detectLayout` returns SIX 11-field
`ChainStorageLayout` literals (kind + 10 shared fields) that differ **only** in `kind`; all
other fields are the same local variables in every literal (verified field-by-field).

(a) Add `ChainStorageLayoutKind` to the existing `import type { ... } from
'./chainStorageManagerShared';` list.

(b) Insert above `export async function detectLayout(`:

```ts
const buildLayout = (
  kind: ChainStorageLayoutKind,
  common: Omit<ChainStorageLayout, 'kind'>
): ChainStorageLayout => ({ kind, ...common });
```

(c) Inside `detectLayout`, after the line ending
`await ctx._listLegacyManagedEntries(normalizedCustomPath, managedChainPath);`, insert:

```ts
  const common: Omit<ChainStorageLayout, 'kind'> = {
    customPath,
    resolvedCustomPath: normalizedCustomPath,
    managedChainPath,
    resolvedManagedChainPath,
    currentChainSource,
    entryPointState,
    managedChainExists,
    managedChainIsDirectory,
    managedLegacyEntries: managedEntries,
    ignoredLegacyEntries: ignoredEntries,
  };
```

(d) Replace each of the six `return { kind: '<kind>', ...ten fields... };` literals with the
matching one-liner, preserving branch order exactly:

- `return buildLayout('managed-custom-root', common);` (entry-point matches managed path)
- `return buildLayout('legacy-custom-root', common);` (entry-point matches custom root)
- `return buildLayout('inconsistent', common);` (managed dir exists + local dir entry point)
- `return buildLayout('managed-custom-root', common);` (managed dir exists, other entry point)
- `return buildLayout('legacy-custom-root', common);` (legacy entries present)
- `return buildLayout('broken-link', common);` (fallthrough)

### 5f — Delete the seven unused `_ctx` parameters

These free functions ignore their context parameter entirely; only the manager's trampolines
call them (verified: no other module imports them, and no spec calls or mocks them directly —
`chainStorageManagerShared.spec.ts` only tests `captureChainPathState`).

File: `source/main/utils/chainStorageManagerShared.ts` — remove the leading
`_ctx: ChainStorageManagerContext,` parameter from:

- `createSymlink` (~:358)
- `movePath` (~:370)
- `safeReadDir` (~:387)
- `safeLstat` (~:401)
- `resolveExistingDirectory` (~:422)
- `resolveRealPathOrInput` (~:444)

File: `source/main/utils/chainStorageManagerLayout.ts` — remove
`_ctx: ChainStorageManagerContext,` from `assertNodeStopped` (~:498).
(`ChainStorageManagerContext` stays imported in both files — other functions still use it.)

File: `source/main/utils/chainStorageManager.ts` — drop the `this` argument at the seven
matching trampolines, e.g. `return createSymlink(this, targetPath, symlinkPath);` →
`return createSymlink(targetPath, symlinkPath);`, and
`return assertNodeStopped(this, nodeState, reason);` →
`return assertNodeStopped(nodeState, reason);`. The trampoline **methods** keep their
signatures — every `ctx._createSymlink(...)`-style caller in the layout/shared modules is
unaffected.

Do NOT touch `pathExistsViaLstat`, `listLegacyManagedEntries`, `getEntriesSizeBytes`,
`getPathSizeBytes`, `captureChainPathState`, `emptyManagedContents`, `resetToDefault`, etc. —
their `ctx` parameters are used.

---

## Verification

Environment prep first (do not misread env failures as regressions): under Node v24 regenerate
`.scss.d.ts` typings via typed-scss-modules and apply the gitignored jest `identity-obj-proxy`
sidecar per the repo verify-env note before judging `yarn compile` / `yarn test:jest` output.

```bash
yarn test:jest --testPathPattern "chainStorage|handleDiskSpace|mithril"
yarn lint
yarn compile
```

Then `yarn prettier:check` on touched files only; classify failures against pre-existing HEAD
drift (`git show HEAD:<f> | prettier --stdin-filepath <f>`) before attributing them here, and
never reformat `toHaveBeenCalledWith('str', {obj})` shapes.

Expected suite deltas (everything else must keep identical pass/fail counts):

- `chainStorageManager.spec.ts`: −4 tests (two `migrateData` tests, the preserve-source-root
  test, the `resolveChainStoragePath` test), +4 tests (Step 1.5) — net 0 (40 → 40).
- `chainStoragePathResolver.spec.ts`: −3 tests (describe deleted = 5, two moved back in)
  (10 → 7).
- `handleDiskSpace.spec.ts`: 0 (one added assertion inside an existing test; 22 stays 22).
- `chainStorageCoordinator.spec.ts`, `chainStorageManagerShared.spec.ts`,
  `MithrilPartialSyncService.spec.ts`: unchanged counts.

Behavior gate checks (all must hold by inspection of the diff):

- No IPC channel names/payloads touched; `getDiskSpaceStatusChannel.onReceive` still returns the
  same report shape from the same `resolveDiskSpaceCheckPath()` → `getDiskCheckReport()` chain
  (now one fork instead of two).
- `getConfig`/`validate` semantics and every log message string byte-identical (the ONE **new**
  warn line allowed: the `_getActiveCustomPath` failure; none other).
- Lock semantics: FIFO order, previous-failure swallow, warn-and-rethrow, synchronous queue swap
  — all preserved verbatim in `runSerializedMutation`.
- Never run `git add -A`/`-u`; never stage `.gitignore` or `.agent/skills/`; single commit with
  the subject suggested above; no pushes/GitHub writes from this task.

Rollback: `git revert <the CAT-C commit>` — the commit is self-contained and touches no file
another CAT depends on being changed (CAT-D types no symbol CAT-C deletes).

## Files touched

- `source/main/utils/chainStorageManager.ts` — C-1 (readers + helper wiring), C-3 (four method
  deletions + import trim), C-4 (lock delegation), C-5c, C-5f (trampolines)
- `source/main/utils/chainStorageManagerConfig.ts` — C-1 (`getConfig` reuses derivation)
- `source/main/utils/chainStorageManagerShared.ts` — C-1 (`deriveCustomPathFromChainState`),
  C-4 (`runSerializedMutation`), C-5f (six signatures)
- `source/main/utils/chainStorageManagerLayout.ts` — C-5b, C-5e, C-5f (`assertNodeStopped`)
- `source/main/utils/chainStorageCoordinator.ts` — C-3 (dead export), C-4 (lock delegation),
  C-5a, C-5d
- `source/main/utils/chainStoragePathResolver.ts` — C-3 (export deletion + doc fix)
- `source/main/utils/handleDiskSpace.ts` — C-2 only (the switch-case region; CAT-B owns the
  dependency wiring above it)
- `source/main/mithril/MithrilPartialSyncService.ts` — C-1 comment maintenance only (no logic)
- Specs: `chainStorageManager.spec.ts`, `chainStoragePathResolver.spec.ts`,
  `handleDiskSpace.spec.ts`, `MithrilPartialSyncService.spec.ts` (comment only)

No i18n files. No renderer files. No `common/` types. No IPC modules.

## Out of scope (do not do in this commit)

- C1 full `ChainStorageManager` re-decomposition and the C11 spec rebalance — deferred
  post-merge (see master doc).
- C2 coordinator re-architecture (incl. building the handler bag once) — deferred.
- C8 layout-error taxonomy move (`MANAGED_CHAIN_LAYOUT_ERROR` marker helpers in
  `handleDiskSpace.ts` stay exactly where they are).
- C9 validation ternary normalization (`chainStorageValidation.ts` untouched).
- The `handleDiskSpace.ts` single-flight queue (coalescing + arg merge — not a mutation lock).
- C7 — REFUTED, do not "fix": `path.win32.relative` already lowercases both sides;
  `isPathWithin` is case-insensitive on win32 as-is.
- Observation only (leave alone): `ChainStorageCoordinator.resolveDiskSpaceCheckPath()`
  (~:148-150) has no production caller at HEAD (`handleDiskSpace` calls the manager directly).
  Not in the verified C-3 list, and CAT-B may retarget disk-path seams — flag for a post-merge
  sweep instead of deleting here.
- De-forking `setDirectory`/`prepareForLocationChange`'s `getConfig` reads — deliberate keep
  (mutation-locked user paths; probe cost immaterial; minimal diff).

## Acceptance checks

- **C-1:** the four Step 1.5 tests are green; grep confirms `getManagedChainPath` and
  `resolveDiskSpaceCheckPath` no longer reference `getConfig`; a renderer disk-status request
  path now contains exactly one `checkDiskSpace` invocation.
- **C-2:** `CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME` case contains exactly one
  `handleStoppedNodeStartup` call and no `isManagedChainEmpty`/`chainEmpty` reference; the
  existing empty/non-empty/rejection routing tests pass unchanged; the new
  `toHaveBeenCalledTimes(1)` pin is green.
- **C-3:** `grep -rn "migrateData\|getResolvedManagedChainPath\|getManagedParentPath\|resolveChainStoragePath\|getMithrilPartialSyncDisabledError" source/` returns no hits;
  `resolveMithrilWorkDir` still resolves through both the coordinator call sites; resolver spec's
  `resolveMithrilWorkDir` describe now carries the two win32-junction tests.
- **C-4:** one `runSerializedMutation` definition; both `_withMutationLock` bodies are one-line
  delegations; coordinator serialization tests green unchanged.
- **C-5:** no `_bootstrapInProgress` re-check inside `startBootstrap`; the layout switch has a
  shared `'managed-custom-root'`/`'inconsistent'` arm; no variable shadowing warning in
  `setDirectory`; the `launcherConfig` feature flag is read in exactly one coordinator method:
  `isPartialSyncEnabled()`;
  `detectLayout` contains one field list; no `_ctx`-prefixed parameter remains in
  `chainStorageManagerShared.ts`/`chainStorageManagerLayout.ts`.

## Escalations

- **E1 (CAT-B drift):** if the quoted C-2 region or the `handleDiskSpace.spec.ts` anchors moved
  under CAT-B's commit, re-locate by the quoted snippets. If the snippets themselves changed
  (not just moved), stop and reconcile with the CAT-B implementation notes before editing.
- **E2 (unexpected caller):** if any Step 3 symbol has gained a caller since 2026-07-04
  (`compile` will fail loudly), do not delete that symbol — flag it and continue with the rest.
- **E3 (test drift):** if any handleDiskSpace routing test fails after Step 2.1, do not weaken
  assertions — the collapse is then not behavior-preserving in the current tree; stop and
  escalate.

## Anchor corrections vs. the review evidence (recorded 2026-07-04)

1. `detectLayout` literals are **11-field** (kind + 10 shared), not 12-field as the review notes
   said. All six still differ only in `kind` — the finding stands.
2. The dead resolver export has a **second** spec consumer beyond `manager.spec:438`: its own
   `describe('resolveChainStoragePath')` in `chainStoragePathResolver.spec.ts` (~:45-133, five
   tests, two of them unique win32-junction coverage). Step 3.4 preserves that coverage by
   moving the two win32 tests to the surviving export's describe.
3. `migrateData`'s spec footprint includes three not-called **spies inside other tests**
   (~:156-158, ~:310-312, ~:366-368) in addition to the dedicated tests; deleting the method
   without removing the spies would crash those tests. Step 3.4 swaps them for `fs.move` pins.
4. C-1 equivalence is byte-exact across all entry-point states; the one divergence is the
   probe-failure error path, where the old code silently degraded to the default path — accepted
   as a strict improvement (documented in the equivalence argument above).
5. C-1 makes four in-tree comments false (`MithrilPartialSyncService.ts` ~:157-159, ~:979-981,
   and ~:1032-1034; its spec ~:2380-2381); Step 1.4 corrects them in this commit rather than leaving
   them for the comment-cleanup CAT, since this commit creates the staleness.
