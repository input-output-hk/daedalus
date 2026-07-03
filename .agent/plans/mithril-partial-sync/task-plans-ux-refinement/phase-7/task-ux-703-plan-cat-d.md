# task-ux-703 — CAT-D per-section plan: Disk preflight (DD-703-5)

- Repo: `/workspaces/mithril-partial-sync-ux`, branch `feat/mithril-partial-sync-ux-refinement` (PR base `develop`).
- Scope: **T7** (major — preflight uses the whole-chain snapshot size) and **T18** (minor — `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` missing from the error-code union / copy map). Canonical plan: `task-ux-703.md` steps 14–15; locked decision **DD-703-5** (formula is user-authored — implement, never relitigate).
- All anchors below verified against the working tree on 2026-07-03. **Locate every edit by the quoted snippet, never by line number** (CAT-A/B/C land first and may shift lines).
- Locked boundaries touched here: recovery actions stay backend-emitted (this plan never changes `allowedRecoveryActions`); no renderer-side threshold (all disk math stays in the main-process service); no synthetic values over IPC (the error object shape `{message, code, logPath, stage}` is unchanged); user-facing copy says "Mithril Sync", never "partial sync"/"immutable"/percentages; bootstrap flow untouched.
- Comments/test names: no task/finding IDs (no T7/T18/DD-703-x/CAT-D) in source comments or test titles — plain rationale comments only.
- Git: no commits, no pushes, no `gh`, no `git add -A`/`-u`; never touch `.gitignore` or `.agent/skills/`.

---

# (1) T7 preflight delta formula + T18 dedicated error code and disk-space copy EN+JA, with unit tests

## 1.1 — Service: constants + rationale comment

**File:** `source/main/mithril/MithrilPartialSyncService.ts`

**Step 1.** Locate (currently near the top of the file):

```ts
// Scratch-space requirement. snapshot.size is the FULL certified DB (a conservative over-estimate of the
// partial range); 1.2× covers LSM-conversion + FS slack. Floored at DISK_SPACE_REQUIRED so a missing/zero
// size still fails closed on a near-full disk.
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1.2;
```

Replace the whole block (comment + const) with exactly:

```ts
// Disk preflight. snapshot.size is the FULL certified DB, but bytes already present in the
// local chain dir do not need to be fetched again, so the requirement is the missing delta
// (snapshot − chain dir, floored at 0) plus a margin proportional to the FULL snapshot —
// deliberately not to the delta — as headroom for chain growth, LSM-conversion and FS slack.
// Floored at DISK_SPACE_REQUIRED so a missing/zero snapshot size still fails closed on a
// near-full disk. If the local chain size cannot be measured, fall back to the conservative
// whole-snapshot bound (snapshot × safety factor).
const PARTIAL_SYNC_DISK_MARGIN_FACTOR = 0.2;
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1 + PARTIAL_SYNC_DISK_MARGIN_FACTOR;
```

(`PARTIAL_SYNC_DISK_SAFETY_FACTOR` has exactly two references in this file: this definition and the usage inside `_assertSufficientDiskSpace`, rewritten in Step 3. Do not rename anything else.)

## 1.2 — Service: thread the managed chain path into the preflight

**File:** `source/main/mithril/MithrilPartialSyncService.ts`

**Step 2.** Locate the single call site inside `start()`:

```ts
      await this._assertSufficientDiskSpace(stagingPaths.rootPath); // preflight
```

Replace with:

```ts
      await this._assertSufficientDiskSpace(
        stagingPaths.rootPath,
        context.layoutResult.managedChainPath
      ); // preflight
```

Rationale (do not re-derive): `context.layoutResult.managedChainPath` is already resolved a few lines above (see `this._prepareStagingDirectory(context.layoutResult.managedChainPath)`); calling `this._chainStorageManager.getManagedChainPath()` here instead would re-run the config/layout resolution (which transitively forks a `checkDiskSpace` probe). Passing the resolved path implements the same DD-703-5 mechanics without extra work.

## 1.3 — Service: delta formula with measurement-failure fallback

**File:** `source/main/mithril/MithrilPartialSyncService.ts`

**Step 3.** Locate the whole current method:

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

Replace the whole method with exactly:

```ts
  async _assertSufficientDiskSpace(
    stagingRootPath: string,
    managedChainPath: string
  ): Promise<void> {
    const snapshotSize = this._latestSnapshot?.size ?? 0;

    let requiredBytes: number;
    try {
      const chainDirBytes = await this._chainStorageManager._getPathSizeBytes(
        managedChainPath
      );
      requiredBytes = Math.max(
        Math.max(snapshotSize - chainDirBytes, 0) +
          snapshotSize * PARTIAL_SYNC_DISK_MARGIN_FACTOR,
        DISK_SPACE_REQUIRED
      );
    } catch (error) {
      // Local size unmeasurable — require the conservative whole-snapshot bound
      // instead of under-requiring.
      logger.warn(
        'MithrilPartialSyncService: disk-space preflight could not measure the local chain size; using the whole-snapshot bound',
        { error, managedChainPath }
      );
      requiredBytes = Math.max(
        snapshotSize * PARTIAL_SYNC_DISK_SAFETY_FACTOR,
        DISK_SPACE_REQUIRED
      );
    }

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
        `Not enough free disk space on the chain storage volume for Mithril Sync. ` +
          `Required ~${requiredGb} GB, available ~${freeGb} GB.`,
        PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE
      );
    }
  }
```

Notes for the implementer (no decisions to make):
- `_getPathSizeBytes` is the existing recursive size helper (`chainStorageManagerShared.ts` `getPathSizeBytes`, exposed as `ChainStorageManager._getPathSizeBytes`). **Do not write new du/size logic** (explicit verification-grill correction in the approved plan).
- `requiredGb` is computed from `requiredBytes` after the branch, so the reported figure is automatically consistent with whichever formula ran (delta or fallback).
- The thrown message keeps the `Not enough free disk space` prefix (asserted by an existing test) and the `Required ~X GB, available ~Y GB` figures, but drops the phrase "the Mithril partial sync": this string is user-visible today (`MithrilErrorView` renders `error.message` under the title), and user-visible copy must say "Mithril Sync". Invariant: no "partial sync", no "immutable", no percentages in this string.
- Do not touch `allowedRecoveryActions`, the error object shape, or anything else in `start()`.

## 1.4 — Types: append the error code (append-only union edit)

**File:** `source/common/types/mithril-partial-sync.types.ts`

**Step 4.** Locate:

```ts
export type MithrilPartialSyncErrorCode =
  | 'PARTIAL_SYNC_NO_CERTIFIED_RANGE'
  | 'PARTIAL_SYNC_LATEST_DRIFT'
  | 'PARTIAL_SYNC_STAGED_DB_INVALID'
  | 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
  | 'PARTIAL_SYNC_CONVERSION_FAILED';
```

Replace with:

```ts
export type MithrilPartialSyncErrorCode =
  | 'PARTIAL_SYNC_NO_CERTIFIED_RANGE'
  | 'PARTIAL_SYNC_LATEST_DRIFT'
  | 'PARTIAL_SYNC_STAGED_DB_INVALID'
  | 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
  | 'PARTIAL_SYNC_CONVERSION_FAILED'
  | 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE';
```

Invariant: **append-only** — do not reorder or remove members (CAT-E appends further codes after this section). The service keeps its local `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE` const (same pattern as `PARTIAL_SYNC_LATEST_DRIFT_CODE`) — do not change it.

## 1.5 — i18n messages (EN defaults)

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`

**Step 5.** Locate:

```ts
  partialSyncErrorConversionFailedHint: {
    id: 'loading.mithrilPartialSync.error.conversionFailed.hint',
    defaultMessage:
      '!!!Daedalus downloaded the verified snapshot but could not prepare it for use. Choose how to continue below.',
    description: 'Hint shown for PARTIAL_SYNC_CONVERSION_FAILED.',
  },
  partialSyncRetry: {
```

Insert between the `partialSyncErrorConversionFailedHint` block's closing `},` and `partialSyncRetry: {` exactly:

```ts
  partialSyncErrorInsufficientDiskSpaceTitle: {
    id: 'loading.mithrilPartialSync.error.insufficientDiskSpace.title',
    defaultMessage: '!!!Not enough disk space for Mithril Sync',
    description:
      'Title shown when PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE is emitted (disk-space preflight failed).',
  },
  partialSyncErrorInsufficientDiskSpaceHint: {
    id: 'loading.mithrilPartialSync.error.insufficientDiskSpace.hint',
    defaultMessage:
      '!!!Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync.',
    description:
      'Hint shown for PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE. The needed/available GB figures come from the backend error message rendered by MithrilErrorView.',
  },
```

(The hint says "in the error details" — accurate both today, where `error.message` is the collapsible details header, and after CAT-E's T19 demotes raw messages into the details section. Do not put runtime placeholders in these messages: the overlay formats them without `values`.)

## 1.6 — Renderer copy map

**File:** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`

**Step 6.** Locate:

```ts
const CONVERSION_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorConversionFailedTitle,
  hint: messages.partialSyncErrorConversionFailedHint,
};
```

Insert immediately after it:

```ts
const INSUFFICIENT_DISK_SPACE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorInsufficientDiskSpaceTitle,
  hint: messages.partialSyncErrorInsufficientDiskSpaceHint,
};
```

**Step 7.** In the same file, locate:

```ts
    PARTIAL_SYNC_CONVERSION_FAILED: CONVERSION_FAILED,
  };
```

Replace with:

```ts
    PARTIAL_SYNC_CONVERSION_FAILED: CONVERSION_FAILED,
    PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE: INSUFFICIENT_DISK_SPACE,
  };
```

(`COPY_BY_CODE` is an exhaustive `Record<MithrilPartialSyncErrorCode, …>` — after Step 4, `yarn compile` fails unless this entry exists; that is the intended coupling.) Do **not** add a `preparing` entry to `COPY_BY_STAGE` — its omission is deliberate and documented in the file's comment.

## 1.7 — Locale files (EN + JA)

**File:** `source/renderer/app/i18n/locales/en-US.json`

**Step 8.** Locate:

```json
  "loading.mithrilPartialSync.error.failed.title": "Mithril Sync failed",
```

Insert immediately after that line (keys stay alphabetically sorted; `insufficientDiskSpace` sorts between `failed` and `latestDrift`, and `hint` before `title`):

```json
  "loading.mithrilPartialSync.error.insufficientDiskSpace.hint": "Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync.",
  "loading.mithrilPartialSync.error.insufficientDiskSpace.title": "Not enough disk space for Mithril Sync",
```

**File:** `source/renderer/app/i18n/locales/ja-JP.json`

**Step 9.** Locate:

```json
  "loading.mithrilPartialSync.error.failed.title": "Mithril同期に失敗しました",
```

Insert immediately after that line:

```json
  "loading.mithrilPartialSync.error.insufficientDiskSpace.hint": "検証済みスナップショットを準備するためのディスクの空き容量が不足しています。必要な容量と利用可能な容量はエラー詳細に表示されています。チェーンデータを保存しているディスクの空き容量を確保してから、Mithril同期を再試行してください。",
  "loading.mithrilPartialSync.error.insufficientDiskSpace.title": "Mithril同期に必要なディスクの空き容量が不足しています",
```

JA derivation: no disk-space row exists in the EN→JA copy table; wording is composed from its closest patterns — "Mithril同期" for Mithril Sync (rows 1/33), "検証済みスナップショット" (rows 40/46), "チェーンデータ" (row 36), and the "…してから、Mithril同期を再試行してください" retry construction (rows 40/44). **Flagged as an uncertainty in the planner log** — do not improvise beyond these strings.

**Step 10.** Regenerate the extracted catalog and validate both locales:

```bash
yarn i18n:extract   # refreshes translations/messages.json (tracked file — include its diff)
yarn i18n:check     # must report no missing/obsolete keys for en-US and ja-JP
```

(`yarn i18n:manage` runs both; either form is fine.) If `i18n:check` rewrites `en-US.json` from the `!!!`-stripped defaultMessages, accept its output — it must equal the Step 8 strings; if it does not, stop and escalate (wording mismatch between messages.ts and locale file).

## 1.8 — Unit tests: service preflight

**File:** `source/main/mithril/MithrilPartialSyncService.spec.ts`

**Step 11.** Make the mocked `fs-extra` module lstat-aware so the now-really-invoked size helper degrades deterministically (returns `undefined` → helper reports 0 bytes) in every suite that does not stub it. Locate:

```ts
  stat: jest.fn(),
  access: jest.fn(),
```

Replace with:

```ts
  stat: jest.fn(),
  lstat: jest.fn(),
  access: jest.fn(),
```

**Step 12.** Pin the two existing disk-space tests to an explicit local-size stub. In the test titled `'fails closed with PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE when free space is insufficient'`, locate:

```ts
      const runCommandSpy = jest.spyOn(service, '_runCommand');
```

Insert immediately before it:

```ts
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(0);
```

In the test titled `'proceeds when disk measurement throws (fail-open on measurement error)'`, locate:

```ts
      require('check-disk-space').mockRejectedValueOnce(
        new Error('no measure')
      );
```

Insert immediately before it:

```ts
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(0);
```

**Step 13.** Add the new tests inside the existing `describe('disk-space preflight', …)` block, immediately after the closing `});` of the `'proceeds when disk measurement throws (fail-open on measurement error)'` test (and before the describe's closing `});`). Context the implementer must not rediscover: this spec mocks `../config` with `DISK_SPACE_REQUIRED: 1024`, so the floor in tests is 1,024 bytes; `createContext()` uses `managedChainPath: '/tmp/chain'`; the preflight throws only when `free < required`, so `free === required` proceeds. Insert exactly:

```ts
    const mockSnapshotMetadata = (
      service: MithrilPartialSyncService,
      size: number
    ) => {
      jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValue({
        snapshot: {
          digest: 'latest-digest',
          createdAt: '2026-05-20T00:00:00Z',
          size,
        },
        latestCertifiedImmutableNumber: 25,
        certifiedEpoch: null,
      });
    };

    it('requires only the missing delta plus the snapshot-proportional margin when local data is measured', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);
      mockSnapshotMetadata(service, 10_000);
      const sizeSpy = jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(10_000);

      // delta = 0; margin = 0.2 × 10,000 = 2,000 (above the mocked 1,024 floor)
      require('check-disk-space').mockResolvedValue({ free: 1_999, size: 1 });

      await expect(service.start(createContext())).rejects.toThrow(
        'Not enough free disk space'
      );
      expect(service.status.error).toEqual(
        expect.objectContaining({
          code: 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE',
          stage: 'preparing',
        })
      );
      expect(sizeSpy).toHaveBeenCalledWith('/tmp/chain');
      expect(service.status.error.message).toContain('Mithril Sync');
      expect(service.status.error.message).not.toMatch(/partial sync/i);
    });

    it('proceeds near the tip when free space covers just the margin', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);
      mockSnapshotMetadata(service, 10_000);
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(10_000);

      // 2,000 = exactly the margin; the old whole-snapshot bound (12,000) would block this
      require('check-disk-space').mockResolvedValue({ free: 2_000, size: 1 });

      await expect(service.start(createContext())).resolves.toBeUndefined();
    });

    it('enforces the absolute floor when the margin falls below it', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);
      mockSnapshotMetadata(service, 4_000);
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(4_000);

      // margin = 800 < mocked DISK_SPACE_REQUIRED (1,024) → required = 1,024
      require('check-disk-space').mockResolvedValue({ free: 1_023, size: 1 });

      await expect(service.start(createContext())).rejects.toThrow(
        'Not enough free disk space'
      );
      expect(service.status.error).toEqual(
        expect.objectContaining({
          code: 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE',
        })
      );
    });

    it('falls back to the whole-snapshot bound when the local chain size cannot be measured', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);
      mockSnapshotMetadata(service, 10_000);
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockRejectedValue(new Error('unreadable'));

      // fallback bound = 10,000 × 1.2 = 12,000
      require('check-disk-space').mockResolvedValue({ free: 11_999, size: 1 });

      await expect(service.start(createContext())).rejects.toThrow(
        'Not enough free disk space'
      );
      expect(require('../utils/logging').logger.warn).toHaveBeenCalledWith(
        expect.stringContaining('could not measure the local chain size'),
        expect.objectContaining({ managedChainPath: '/tmp/chain' })
      );
    });

    it('keeps roughly the whole-snapshot bound when nothing is held locally', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);
      mockSnapshotMetadata(service, 10_000);
      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(0);

      // delta = 10,000; + margin 2,000 → 12,000, same as the fallback bound
      require('check-disk-space').mockResolvedValue({ free: 12_000, size: 1 });

      await expect(service.start(createContext())).resolves.toBeUndefined();
    });
```

Key assertions delivered: delta ≈ 0 → floor-or-margin requirement (both the margin-dominant and floor-dominant cases); measurement failure → old 1.2× bound plus the warn log; huge delta → roughly the old bound; managed-chain-path threading; vocabulary of the thrown message. Test titles contain no task/finding IDs.

## 1.9 — Unit test: renderer copy resolver

**File:** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts`

**Step 14.** Locate:

```ts
    [
      'PARTIAL_SYNC_CONVERSION_FAILED',
      'loading.mithrilPartialSync.error.conversionFailed.title',
    ],
  ])('maps code %s to bespoke copy', (code, titleId) => {
```

Replace with:

```ts
    [
      'PARTIAL_SYNC_CONVERSION_FAILED',
      'loading.mithrilPartialSync.error.conversionFailed.title',
    ],
    [
      'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE',
      'loading.mithrilPartialSync.error.insufficientDiskSpace.title',
    ],
  ])('maps code %s to bespoke copy', (code, titleId) => {
```

## 1.10 — Docs kept honest (DD-703-7: cheat sheet must track behavior changes)

**Step 15.** File `.agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md` — locate:

```
- [ ] **Low disk:** with less than ~4 GB free (or < 1.2× snapshot size) on the chain
      volume, starting Mithril Sync fails cleanly during Preparing with a
      "not enough free disk space" message quoting required vs available GB, plus
      retry/restart recovery — never a crash.
```

Replace with:

```
- [ ] **Low disk:** with less free space on the chain volume than Mithril Sync needs
      (snapshot size minus current chain-data size, plus a 20%-of-snapshot margin;
      never less than ~4 GB), starting Mithril Sync fails cleanly during Preparing
      with a "not enough free disk space" message quoting required vs available GB,
      plus retry/restart recovery — never a crash. A node already near the tip now
      needs far less free space than the full snapshot.
```

**Step 16.** File `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md` — append two rows at the **end** of the table, numbering them with the next sequential row numbers present at implementation time (currently 56 and 57; CAT-C may have appended rows first — locate the last existing `| NN |` row and continue from it):

```
| 56 | Not enough disk space for Mithril Sync | Mithril同期に必要なディスクの空き容量が不足しています | Sync overlay — error (insufficient disk space, title) |
| 57 | Daedalus does not have enough free disk space to prepare the verified snapshot. The required and available amounts are shown in the error details. Free up space on the disk that stores your chain data, then retry Mithril Sync. | 検証済みスナップショットを準備するためのディスクの空き容量が不足しています。必要な容量と利用可能な容量はエラー詳細に表示されています。チェーンデータを保存しているディスクの空き容量を確保してから、Mithril同期を再試行してください。 | Sync overlay — error (insufficient disk space, hint) |
```

## 1.11 — Verification commands

Run, in order (trim output to failing tails; env prep for Node 24 — scss typings regen + jest sidecar — per the canonical `task-ux-703.md` verification notes before treating `yarn compile`/renderer-jest failures as regressions):

```bash
yarn test:jest --testPathPattern 'source/main/mithril/MithrilPartialSyncService.spec.ts'
yarn test:jest --testPathPattern 'partialSyncErrorCopy'
yarn i18n:manage        # extract + check; commit the regenerated translations/messages.json diff
yarn compile            # tsc — proves the union/COPY_BY_CODE exhaustiveness coupling
```

Expected: all MithrilPartialSyncService suites green (the untouched suites now route through the lstat-degraded 0-byte local size → old bound → unchanged outcomes); copy-resolver spec green with the new row; `i18n:check` reports zero missing/obsolete keys; `tsc` clean.

---

## Files touched

- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/common/types/mithril-partial-sync.types.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`
- `translations/messages.json` (regenerated by `yarn i18n:extract`; tracked)
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md`

## Out of scope (owned by other sections)

- Every other prose→code conversion in `MithrilPartialSyncService.ts` and `chainStorageCoordinator.ts` (`PARTIAL_SYNC_DISABLED_ERROR` etc.) — CAT-E T16/DD-703-12. The T16 site list annotates the disk-space throw as already handled here; CAT-E must not re-reword it.
- Further `MithrilPartialSyncErrorCode` additions (`PARTIAL_SYNC_DISABLED`, `PARTIAL_SYNC_ALREADY_RUNNING`, `PARTIAL_SYNC_LAYOUT_UNSUPPORTED`, …) — CAT-E, append-only after this section's member.
- `MithrilErrorView` raw-message demotion into the details section (T19), the shared start-failure fallback message (T22/T23), finalize-failed copy (CAT-C), all remaining copy work — CAT-C/E/F.
- Diagnostics section rework (T11/N7), spawn-helper extraction (T26), N3/N4 nits — CAT-B/F/G.
- No IPC contract, store, overlay, or bootstrap-flow changes of any kind in this section.

## Acceptance checks

- **T7**: `requiredBytes = max(snapshotBytes − chainDirBytes, 0) + 0.2 × snapshotBytes`, floored at `DISK_SPACE_REQUIRED`; `chainDirBytes` measured with the existing recursive `_getPathSizeBytes` helper (no new du logic); on measurement failure the requirement falls back to `snapshotBytes × 1.2` (floored); the reported required-GB figure derives from whichever formula ran; the explanatory constant comment is updated. Unit tests prove: delta ≈ 0 → margin-or-floor requirement (2,000-byte margin case and 1,024-byte floor case); measurement failure → 12,000-byte old bound + warn log; huge delta → 12,000-byte ≈ old bound; and that a near-tip node passes with only the margin free (old bound would have blocked).
- **T18**: `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` appended to `MithrilPartialSyncErrorCode`; dedicated `COPY_BY_CODE` entry with new EN+JA copy (needed-vs-available figures surfaced via the error details it points at, plus how to free space and retry); keys present in both `en-US.json` and `ja-JP.json`; `yarn i18n:manage` green; copy-resolver spec maps the code to the new title id.
- **Boundaries**: recovery actions untouched (backend-emitted only); no renderer threshold or disk math; IPC error shape unchanged (no synthetic throughput/percent); copy says "Mithril Sync" with no "partial sync"/"immutable"/percentages (asserted in a unit test for the thrown message); bootstrap specs untouched and unaffected (no bootstrap files edited); internal `partialSync*` identifiers retained.

## Escalations

1. **User-visible vocabulary in the thrown message (planned fix, escalate on mismatch).** `MithrilErrorView` renders `error.message` verbatim under the title, so the current backend string "…the Mithril partial sync…" is user-visible and violates the vocabulary boundary. Step 3 rewords it minimally (keeping the test-asserted "Not enough free disk space" prefix and the GB figures). The canonical plan's T16 (CAT-E) lists this site as "(T18)" — i.e., delegated here — so there is no ownership conflict; if the implementer finds the string already reworded (unexpected earlier-section drift), stop and escalate instead of double-editing.
2. **`getManagedChainPath()` vs threading the context path.** The section brief says the service "holds `_chainStorageManager` and `getManagedChainPath()`"; the plan threads `context.layoutResult.managedChainPath` (already resolved at the call site) instead of re-calling `getManagedChainPath()`, which would re-fork a `checkDiskSpace` probe per start. Same DD-703-5 mechanics, cheaper; flagged for the reviewer rather than silently chosen.
3. **JA translation uncertainty.** No disk-space row exists in the EN→JA copy table; the JA strings in Steps 9/16 are composed from the closest table patterns (rows 1/33 "Mithril同期", 36 "チェーンデータ", 40/44 retry construction, 40/46 "検証済みスナップショット"). A native-speaker pass may refine them; do not deviate from the given strings during implementation.
4. **Test-environment constant.** The service spec mocks `../config` with `DISK_SPACE_REQUIRED: 1024`, so unit tests express the 4 GB floor as 1,024 bytes — expected, not drift. (Production floor remains `4 * 1073741274` in `source/main/config.ts`; its `1073741274` vs `1073741824` literal is a pre-existing quirk this section must not touch.)
