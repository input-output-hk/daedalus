# task-ux-703 — CAT-A per-section plan: Cross-session recovery and startup correctness (T5, T6, T8, T9, T10, T14)

> Per-section implementation doc for task-ux-703 (PR #3337 remediation). **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent task:
> `task-ux-703.md`; decisions DD-703-1..14 are locked — this doc translates them into mechanics.
> If this doc ever disagrees with live code, prefer live code, locate anchors by the QUOTED
> snippets (never by line number), and reconcile here. All line numbers below are approximate
> as of 2026-07-03 and provided only as a reading aid.

## Sequencing position

CAT-A is the **first** section (order A → B → C → D → E → F → G). No other section's edits
precede it. Files edited here (`mithrilPartialSyncNodeStartup.ts`, `MithrilPartialSyncService.ts`,
`MithrilController.ts` and their specs) are later touched near-by by CAT-B/CAT-G (T11/T21/N3
seams in `chainStorageCoordinator`/`MithrilController`) — leave everything not named in a step
strictly untouched.

## Locked invariants that constrain CAT-A (restated inline at the steps they bind)

- **Boundary 2:** recovery actions come strictly from backend `allowedRecoveryActions`. T5
  widens what the backend **honors from its own prior emission** — it must never let the
  renderer inject or infer an action. `adoptRecoverySnapshot` therefore only ever copies a
  snapshot the main process itself broadcast.
- **Boundary 9 / DD-703-9:** auto-finalize stands; only the cancel-cleanup FAILURE path changes
  (T9). Do NOT restore any wipe behavior into cancel (ADR D-702a-2 stands).
- **Boundary 11:** bootstrap flow untouched — no file in this section is on the bootstrap path,
  and none of the bootstrap specs may be edited.
- **Vocabulary:** no new user-facing copy is added in CAT-A (all strings below are log lines or
  internal error messages). Do NOT "fix" the existing native-dialog copy that says
  "Mithril partial sync" — all remaining copy work is owned by CAT-E.
- **Comments/test titles:** never cite task/finding IDs (T5, CAT-A, DD-703-x) in source comments
  or test titles; the rationale comments below are written accordingly — copy them verbatim.

## i18n

**No new i18n messages in CAT-A.** Every string added below is a `logger.warn`/`logger.info`
line or a main-process internal message; none is rendered to the user. No changes to
`en-US.json` / `ja-JP.json`, no defaultMessages regeneration.

---

# 1. T5 — Cross-session recovery snapshot adoption (service + controller wiring + repro unit test)

**Defect (critical):** after an app restart, startup-owned emissions
(`mithrilPartialSyncNodeStartup.ts` → `emitMithrilPartialSyncStatus`, e.g. `failed` with
`allowedRecoveryActions: ['wipe-and-full-sync']`) update only the **controller-held** snapshot
(`MithrilController._partialSyncStatus` via `broadcastPartialSyncStatus`). The
**service** `_status` stays `{ status: 'idle', allowedRecoveryActions: [] }`. When the user
clicks the only offered button (wipe), the chain
`wipeAndFullSyncFromPartialSync → coordinator → handlers.wipeAndFullSync →
MithrilPartialSyncService.wipeAndFullSync → _assertRecoveryActionAllowed('wipe-and-full-sync')`
rejects with `Mithril partial sync cannot wipe-and-full-sync from the current recovery boundary.`
— the sole recovery button always fails.

### Step 1.1 — Add `adoptRecoverySnapshot` to the service

File: `source/main/mithril/MithrilPartialSyncService.ts`

Locate (by content) the method opening:

```ts
  async restartNormal(): Promise<void> {
```

Insert **immediately above** that line (blank line after the inserted block):

```ts
  // Startup-owned recovery emissions update only the controller-held broadcast
  // snapshot; this service starts each session idle, so the allowed-action
  // assertion would reject the very action the backend itself offered. Before a
  // recovery action delegates here, the controller hands us its snapshot and we
  // adopt it as the local boundary — only from idle, and only when the snapshot
  // is a real recovery boundary (non-idle with at least one allowed action).
  // No re-emit: the broadcast that produced the snapshot already delivered it.
  adoptRecoverySnapshot(snapshot: MithrilPartialSyncStatusSnapshot): void {
    if (this._status.status !== 'idle') {
      return;
    }
    if (
      snapshot.status === 'idle' ||
      snapshot.allowedRecoveryActions.length === 0
    ) {
      return;
    }
    this._status = { ...snapshot };
  }
```

Notes for the implementer (no decisions to make):
- `MithrilPartialSyncStatusSnapshot` is already imported as a type at the top of the file — do
  not add an import.
- Assign `this._status` directly. Do NOT call `_updateStatus` (it emits on `_statusEmitter`,
  which the controller re-broadcasts — the snapshot was already delivered).
- **Boundary 2 invariant:** this method only copies a backend-emitted snapshot; it must not
  compute, add, or reorder actions.

### Step 1.2 — Seed the service in the controller recovery wirings

File: `source/main/mithril/MithrilController.ts`

Inside `_getPartialSyncDependencies()`, locate the two handler lines (quoted exactly):

```ts
        restartNormal: async () => this._partialSyncService.restartNormal(),
        wipeAndFullSync: async () => this._partialSyncService.wipeAndFullSync(),
```

Replace those two lines with:

```ts
        // Recovery actions may target a boundary reached in a previous session
        // (startup-owned emission); seed the service with the broadcast snapshot
        // before delegating so its allowed-action assertion sees that boundary.
        restartNormal: async () => {
          this._partialSyncService.adoptRecoverySnapshot(
            this.getPartialSyncStatus()
          );
          await this._partialSyncService.restartNormal();
        },
        wipeAndFullSync: async () => {
          this._partialSyncService.adoptRecoverySnapshot(
            this.getPartialSyncStatus()
          );
          await this._partialSyncService.wipeAndFullSync();
        },
```

Notes:
- Use `this.getPartialSyncStatus()` (defined in this class; returns
  `this._partialSyncStatus`). Do NOT import `getMithrilPartialSyncStatus` from
  `../ipc/mithrilPartialSyncChannel` — that module imports this controller (circular).
- Do not touch the other handlers (`assertStartAllowed`, `start`, `cancel`, `finalizeCancel`,
  `forceKill`, `abandonCancel`, `finalizeWipeAndFullSync`) — retry-from-idle is already legal in
  `assertStartAllowed` (`'idle'` is a permitted start state), so `start` needs no seeding.

### Step 1.3 — Service repro + guard specs

File: `source/main/mithril/MithrilPartialSyncService.spec.ts`

Locate the existing test (quoted title):

```ts
  it('rejects start reuse from wipe-only failed boundaries', () => {
```

Insert **after the closing `});` of that test** a new describe block, verbatim:

```ts
  describe('adoptRecoverySnapshot (cross-session recovery boundary)', () => {
    const failedStartupSnapshot = (): MithrilPartialSyncStatusSnapshot => ({
      status: 'failed',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: {
        message:
          'Cardano node failed to start after Mithril partial sync cutover.',
        stage: 'starting-node',
      },
    });

    it('adopts a broadcast failure snapshot while idle so the offered wipe succeeds', async () => {
      const service = new MithrilPartialSyncService();

      service.adoptRecoverySnapshot(failedStartupSnapshot());

      expect(service.status).toEqual(
        expect.objectContaining({
          status: 'failed',
          allowedRecoveryActions: ['wipe-and-full-sync'],
        })
      );
      await expect(service.wipeAndFullSync()).resolves.toBeUndefined();
      expect(removeMock).toHaveBeenCalledWith(
        '/tmp/daedalus-state/mithril-partial-sync'
      );
    });

    it('still rejects the wipe from idle when no snapshot was adopted', async () => {
      const service = new MithrilPartialSyncService();

      await expect(service.wipeAndFullSync()).rejects.toThrow(
        'Mithril partial sync cannot wipe-and-full-sync from the current recovery boundary.'
      );
    });

    it('does not adopt a snapshot while the service is mid-run', () => {
      const service = new MithrilPartialSyncService();
      service._status = {
        status: 'downloading',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      };

      service.adoptRecoverySnapshot(failedStartupSnapshot());

      expect(service.status.status).toBe('downloading');
    });

    it('ignores idle snapshots and snapshots without recovery actions', () => {
      const service = new MithrilPartialSyncService();

      service.adoptRecoverySnapshot({
        status: 'idle',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });
      expect(service.status.status).toBe('idle');

      service.adoptRecoverySnapshot({
        status: 'completed',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });
      expect(service.status.status).toBe('idle');
    });

    it('does not re-emit the adopted snapshot', () => {
      const service = new MithrilPartialSyncService();
      const emissions: MithrilPartialSyncStatusSnapshot[] = [];
      service.onStatus((update) => emissions.push(update));

      service.adoptRecoverySnapshot(failedStartupSnapshot());

      expect(emissions).toHaveLength(0);
    });
  });
```

(`MithrilPartialSyncStatusSnapshot` is already imported in this spec; `removeMock` is the
existing `fs.remove` mock handle declared in the outer describe.)

### Step 1.4 — Controller wiring specs

File: `source/main/mithril/MithrilController.spec.ts`

(a) Locate the existing top-of-file mock handles:

```ts
const mockGetPartialSyncBehindness = jest.fn();
const mockForceKillForShutdown = jest.fn();
```

Insert **after** them:

```ts
const mockAdoptRecoverySnapshot = jest.fn();
const mockServiceWipeAndFullSync = jest.fn();
const mockServiceRestartNormal = jest.fn();
```

(b) Locate inside the `jest.mock('./MithrilPartialSyncService', ...)` factory:

```ts
    forceKillForShutdown: (...args) => mockForceKillForShutdown(...args),
```

Insert **after** that line (inside the same object literal):

```ts
    adoptRecoverySnapshot: (...args) => mockAdoptRecoverySnapshot(...args),
    wipeAndFullSync: (...args) => mockServiceWipeAndFullSync(...args),
    restartNormal: (...args) => mockServiceRestartNormal(...args),
```

(c) Locate the closing of the `describe('reapPartialSyncOnShutdown (shutdown reap)', ...)`
block — the final `});` before the outermost `});` of `describe('MithrilController', ...)` —
and insert this new describe **after it, still inside the outer describe**:

```ts
  describe('recovery-action wiring (cross-session snapshot adoption)', () => {
    it('seeds the service with the controller-held snapshot before delegating wipe-and-full-sync', async () => {
      const controller = createController();
      const failedSnapshot: MithrilPartialSyncStatusSnapshot = {
        status: 'failed',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'node failed to start',
          stage: 'starting-node',
        },
      };
      controller.setPartialSyncStatus(failedSnapshot);

      await controller._getPartialSyncDependencies().handlers.wipeAndFullSync();

      expect(mockAdoptRecoverySnapshot).toHaveBeenCalledWith(failedSnapshot);
      expect(mockServiceWipeAndFullSync).toHaveBeenCalledTimes(1);
      expect(
        mockAdoptRecoverySnapshot.mock.invocationCallOrder[0]
      ).toBeLessThan(mockServiceWipeAndFullSync.mock.invocationCallOrder[0]);
    });

    it('seeds the service with the controller-held snapshot before delegating restart-normal', async () => {
      const controller = createController();
      const failedSnapshot: MithrilPartialSyncStatusSnapshot = {
        status: 'failed',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'download failed',
          stage: 'downloading',
        },
      };
      controller.setPartialSyncStatus(failedSnapshot);

      await controller._getPartialSyncDependencies().handlers.restartNormal();

      expect(mockAdoptRecoverySnapshot).toHaveBeenCalledWith(failedSnapshot);
      expect(mockServiceRestartNormal).toHaveBeenCalledTimes(1);
      expect(
        mockAdoptRecoverySnapshot.mock.invocationCallOrder[0]
      ).toBeLessThan(mockServiceRestartNormal.mock.invocationCallOrder[0]);
    });
  });
```

---

# 2. T6, T8, T9, T10 — startup / cancel / staging hardening (T14 is checklist-only, no code)

## 2A. `source/main/mithril/mithrilPartialSyncNodeStartup.ts` (T6 + T8 + T10 startup half)

### Step 2A.1 — Imports (serves T6, T8, T10)

Locate:

```ts
import fs from 'fs-extra';
import { dialog } from 'electron';
```

After the existing import block (immediately after the line
`} from './mithrilPartialSyncMarker';`), add:

```ts
import { logger } from '../utils/logging';
import { safeExitWithCode } from '../utils/safeExitWithCode';
```

**T8 invariant:** import `safeExitWithCode` only — do NOT import electron `app` and do NOT add a
new injected dependency to `NodeStartupDependencies`.

### Step 2A.2 — T6: best-effort staging reclaim in the node-start-verified branch

Locate:

```ts
      if (marker.stagingRootPath) {
        await fs.remove(marker.stagingRootPath);
      }
      await clearMithrilPartialSyncMarker();
      return false;
```

Replace with:

```ts
      if (marker.stagingRootPath) {
        try {
          await fs.remove(marker.stagingRootPath);
        } catch (error) {
          // Reclaim is best-effort: a locked or busy staging directory must not
          // block a normal boot. The next partial sync start reclaims it when it
          // prepares the staging directory.
          logger.warn(
            'MithrilPartialSyncNodeStartup: staging reclaim failed; continuing normal boot',
            { error, stagingRootPath: marker.stagingRootPath }
          );
        }
      }
      await clearMithrilPartialSyncMarker();
      return false;
```

Accepted tradeoff (locked): on reclaim failure the stale staging dir is only reclaimed at the
next partial-sync start.

### Step 2A.3 — T10 (startup half): the startup wipe branch also reclaims the marker-persisted staging root

Locate the wipe branch:

```ts
    if (response === 0) {
      await this._wipeChainAndSnapshots(
        'Interrupted Mithril partial sync detected on startup. Wiped chain directory and Mithril snapshots.',
        this._cardanoNode.state
      );
      await clearMithrilPartialSyncMarker();
      return false;
    }
```

Replace with:

```ts
    if (response === 0) {
      await this._wipeChainAndSnapshots(
        'Interrupted Mithril partial sync detected on startup. Wiped chain directory and Mithril snapshots.',
        this._cardanoNode.state
      );
      if (marker.stagingRootPath) {
        try {
          await fs.remove(marker.stagingRootPath);
        } catch (error) {
          // Best-effort: a stuck staging directory must not block the wipe
          // recovery; the next partial sync start reclaims it.
          logger.warn(
            'MithrilPartialSyncNodeStartup: staging reclaim failed during startup wipe recovery; continuing',
            { error, stagingRootPath: marker.stagingRootPath }
          );
        }
      }
      await clearMithrilPartialSyncMarker();
      return false;
    }
```

Note: the wipe callback (`_wipeChainAndSnapshots`) covers chain + Mithril snapshots only; the
partial-sync staging root can live on a custom volume and is otherwise orphaned forever once the
marker is cleared. The marker is the only staging-root source in this module (there is no
session resolver here — see Escalations E1).

### Step 2A.4 — T8: Quit branch exits via `safeExitWithCode`

Locate the tail of `handleInterruptedRecovery` — the `return true;` that directly follows the
closing brace of the `if (response === 0) { ... }` block (NOT the earlier
`if (currentGeneration !== this._getGeneration()) { return true; }` guard):

```ts
    return true;
  }
```

Replace with:

```ts
    // The user chose Quit: actually exit. Startup stays blocked (return true)
    // while the log stream flushes and the process exits.
    logger.warn(
      'MithrilPartialSyncNodeStartup: user chose Quit at the interrupted-cutover recovery dialog; exiting',
      { markerState: marker.state }
    );
    safeExitWithCode(0);
    return true;
  }
```

**T8 invariant:** the `response === 0` wipe branch above is correct — beyond Step 2A.3's staging
reclaim insertion, leave its wipe + clear + `return false` behavior alone.

### Step 2A.5 — Spec updates: `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`

(a) After the existing `jest.mock('../ipc/mithrilPartialSyncChannel', ...)` block, add:

```ts
jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

jest.mock('../utils/safeExitWithCode', () => ({
  safeExitWithCode: jest.fn(),
}));
```

(b) After the existing helper `const { dialog } = require('electron') ...` block, add:

```ts
const { safeExitWithCode } = require('../utils/safeExitWithCode') as {
  safeExitWithCode: jest.Mock;
};
```

(c) **T6 test.** After the existing test titled
`'C2 branch: skips fs.remove when stagingRootPath is absent, still clears and returns false'`,
insert:

```ts
  it('C2 branch: continues normal boot and clears the marker even when staging reclaim fails', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
      managedChainPath: '/chain',
      stagingRootPath: '/vol/mithril-partial-sync',
    });
    const busyError = Object.assign(new Error('resource busy or locked'), {
      code: 'EBUSY',
    });
    fsMock.remove.mockRejectedValueOnce(busyError);
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
    expect(dialog.showMessageBox).not.toHaveBeenCalled();
    expect(safeExitWithCode).not.toHaveBeenCalled();
  });
```

(d) **T10 test.** After the existing test titled
`'unsafe cutover, Wipe (response 0): wipes chain + snapshots, clears the marker, returns false'`,
insert:

```ts
  it('unsafe cutover, Wipe (response 0): also reclaims the marker-persisted staging root', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
      stagingRootPath: '/vol/mithril-partial-sync',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 0 });
    const { instance, wipeChainAndSnapshots } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(wipeChainAndSnapshots).toHaveBeenCalledTimes(1);
    expect(fsMock.remove).toHaveBeenCalledWith('/vol/mithril-partial-sync');
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
  });
```

(e) **T8 test.** Replace the body of the existing test titled
`'unsafe cutover, Quit (response 1): returns true (blocked) without wiping or clearing the marker'`
so it also pins the exit call — full replacement test:

```ts
  it('unsafe cutover, Quit (response 1): exits via safeExitWithCode without wiping or clearing the marker', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 });
    const { instance, wipeChainAndSnapshots } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(true);
    expect(safeExitWithCode).toHaveBeenCalledTimes(1);
    expect(safeExitWithCode).toHaveBeenCalledWith(0);
    expect(wipeChainAndSnapshots).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
  });
```

(f) In the existing test titled
`'unsafe cutover, generation changed during the dialog: returns true without wiping'`, add one
assertion after `expect(wipeChainAndSnapshots).not.toHaveBeenCalled();`:

```ts
    expect(safeExitWithCode).not.toHaveBeenCalled();
```

(The stale-generation `return true` must NOT quit — only the explicit Quit branch does.)

## 2B. `source/main/mithril/MithrilPartialSyncService.ts` (T9 + T10 service half)

### Step 2B.1 — T10: marker-first staging-root resolution in `_cleanupPartialSyncArtifacts`

Locate:

```ts
  async _cleanupPartialSyncArtifacts(): Promise<void> {
    await fs.remove(this._getStagingRootPath());
    await clearMithrilPartialSyncMarker();
  }
```

Replace with:

```ts
  async _cleanupPartialSyncArtifacts(): Promise<void> {
    // Resolve the staging root from the durable marker first so cleanup is
    // correct cross-session and on custom volumes; fall back to the in-session
    // resolver when no persisted path exists.
    const marker = await readMithrilPartialSyncMarker();
    const stagingRoot = marker?.stagingRootPath ?? this._getStagingRootPath();
    await fs.remove(stagingRoot);
    await clearMithrilPartialSyncMarker();
  }
```

(`readMithrilPartialSyncMarker` is already imported at the top of the file.) This covers both
callers: `finalizeCancel` and `restartNormal`.

### Step 2B.2 — T10: marker-first resolution in `wipeAndFullSync`

Locate:

```ts
  async wipeAndFullSync(): Promise<void> {
    this._assertRecoveryActionAllowed('wipe-and-full-sync');

    try {
      await fs.remove(this._getStagingRootPath());
      this._resetToIdleStatus();
    } finally {
      this._clearRuntimeWorkState();
    }
  }
```

Replace with:

```ts
  async wipeAndFullSync(): Promise<void> {
    this._assertRecoveryActionAllowed('wipe-and-full-sync');

    try {
      // Marker-first staging resolution (see _cleanupPartialSyncArtifacts); the
      // marker itself is retained here until finalizeWipeAndFullSync clears it.
      const marker = await readMithrilPartialSyncMarker();
      await fs.remove(marker?.stagingRootPath ?? this._getStagingRootPath());
      this._resetToIdleStatus();
    } finally {
      this._clearRuntimeWorkState();
    }
  }
```

**Invariant:** do NOT clear the marker here — the existing test
`'retains the marker during wipe-and-full-sync cleanup until finalization runs'` pins that
`finalizeWipeAndFullSync` owns the clear.

`finalizeCompletedPartialSync` already uses the marker-first pattern — leave it untouched.

### Step 2B.3 — T9: `finalizeCancel` staging removal becomes best-effort; locked staging lands on `cancelled`

Locate the whole method (opening quoted):

```ts
  async finalizeCancel(): Promise<void> {
    if (this._status.status !== 'cancelling') {
      return;
    }
```

Replace the **entire method** (through its closing `}` after the `finally` block) with:

```ts
  async finalizeCancel(): Promise<void> {
    if (this._status.status !== 'cancelling') {
      return;
    }

    logger.info('MithrilPartialSyncService: finalizing cancel', {
      status: this._status.status,
    });

    try {
      await this._cleanupPartialSyncArtifacts();
      logger.info(
        'MithrilPartialSyncService: cancel finalized; partial sync artifacts cleaned up',
        null
      );
    } catch (error) {
      // Staging removal is best-effort on cancel: a locked or busy staging
      // directory must not strand the user on a failed screen with a cleanup
      // retry loop. No cutover has happened yet, so nothing installed is at
      // risk and no marker exists; the orphaned staging directory is reclaimed
      // when the next partial sync prepares its staging directory.
      logger.warn(
        'MithrilPartialSyncService: finalizeCancel cleanup failed; landing on cancelled and leaving staging for the next start to reclaim',
        {
          error,
          cancelFallbackErrorStage: this._cancelFallbackErrorStage,
        }
      );
    }

    this._progressItems = [];
    this._updateStatus({
      status: 'cancelled',
      allowedRecoveryActions: ['retry', 'restart-normal'],
      error: null,
      logPath: this._getLogPath(),
      progressItems: [],
      transferProgress: {},
    });
    this._clearRuntimeWorkState();
  }
```

**Locked constraints at this step:**
- DD-703-9 / ADR D-702a-2: do NOT restore any wipe into the cancel path.
- `failed` remains reserved for unrecoverable teardown: `abandonCancel` (unsettled run /
  process-kill failure) is untouched and still emits `failed` with no recovery actions.
- Both exits of `finalizeCancel` now land on `cancelled` with
  `allowedRecoveryActions: ['retry', 'restart-normal']` — actions still originate from the
  backend only (boundary 2/4 untouched).

### Step 2B.4 — Spec updates: `source/main/mithril/MithrilPartialSyncService.spec.ts`

(a) **T9 — failure path lands on cancelled.** Replace the existing test titled
`'emits cancelling and surfaces a boundary-a failure from finalizeCancel when cleanup fails'`
in full with:

```ts
  it('lands on cancelled with retry and restart-normal when finalizeCancel cleanup fails', async () => {
    const service = new MithrilPartialSyncService();

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };
    removeMock.mockRejectedValueOnce(new Error('cleanup failed'));

    await expect(service.cancel()).resolves.toBeUndefined();

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'cancelling',
        allowedRecoveryActions: [],
      })
    );

    await expect(service.finalizeCancel()).resolves.toBeUndefined();

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'cancelled',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        error: null,
      })
    );
  });
```

(b) **T9 — observability warn text.** In the test titled
`'logs a warn on the previously-silent finalizeCancel cleanup-failure catch (observability)'`,
retitle it to
`'logs a warn when finalizeCancel cleanup fails and cancel proceeds (observability)'` and
replace its final assertion with:

```ts
    expect(warnLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: finalizeCancel cleanup failed; landing on cancelled and leaving staging for the next start to reclaim',
      expect.objectContaining({ error: expect.any(Error) })
    );
```

(c) **T10 — marker-first resolution tests.** After the closing `});` of the
`describe('finalizeCompletedPartialSync', ...)` block, insert:

```ts
  describe('staging root resolution from the durable marker', () => {
    const readMithrilPartialSyncMarkerMock =
      require('./mithrilPartialSyncMarker')
        .readMithrilPartialSyncMarker as jest.Mock;

    it('removes the marker-persisted staging root on wipe-and-full-sync and retains the marker', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValueOnce({
        state: 'cutover-in-progress',
        updatedAt: '2026-06-01T00:00:00.000Z',
        stagingRootPath: '/custom-volume/mithril-partial-sync',
      });
      const service = new MithrilPartialSyncService();
      service._status = {
        status: 'failed',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'unsafe install',
          stage: 'installing',
        },
      };

      await expect(service.wipeAndFullSync()).resolves.toBeUndefined();

      expect(removeMock).toHaveBeenCalledWith(
        '/custom-volume/mithril-partial-sync'
      );
      expect(clearMithrilPartialSyncMarkerMock).not.toHaveBeenCalled();
    });

    it('removes the marker-persisted staging root on restart-normal cleanup', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValueOnce({
        state: 'cutover-in-progress',
        updatedAt: '2026-06-01T00:00:00.000Z',
        stagingRootPath: '/custom-volume/mithril-partial-sync',
      });
      const service = new MithrilPartialSyncService();
      service._status = {
        status: 'failed',
        allowedRecoveryActions: ['restart-normal', 'wipe-and-full-sync'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'download failed',
          stage: 'downloading',
        },
      };

      await expect(service.restartNormal()).resolves.toBeUndefined();

      expect(removeMock).toHaveBeenCalledWith(
        '/custom-volume/mithril-partial-sync'
      );
      expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(1);
    });
  });
```

Fallback behavior (marker absent → `_getStagingRootPath()` default
`/tmp/daedalus-state/mithril-partial-sync`) stays pinned by the untouched existing tests
`'resets to idle after restart-normal cleanup when that recovery is allowed'`,
`'retains the marker during wipe-and-full-sync cleanup until finalization runs'`, and
`'emits cancelling and defers cleanup until finalizeCancel when cancellation succeeds before cutover'`
(the outer `beforeEach` re-primes `readMithrilPartialSyncMarker` to resolve `null`).

## 2C. T14 — checklist-only, NO code change (DD-703-6 revised)

Record in the PR checklist response; verify-only steps for the implementer:

1. Confirm `source/main/mithril/killProcessTree.ts` still kills the POSIX group via
   `process.kill(-pid, signal)` (anchor content: `process.kill(-` around line 72).
2. Confirm both spawn sites in `source/main/mithril/mithrilCommandRunner.ts` still pass
   `detached: !environment.isWindows` (approx. lines 162 and 258) and still carry the rationale
   comment beginning `// Detach on POSIX so the child leads its own process group` (approx.
   lines 155-158 and 251-254).
3. Change nothing. Detachment is load-bearing for group kill; the rationale comment already
   exists at both sites. If any of the three anchors is missing, escalate (see E3) — do not
   re-add or reword them from this section.

---

## Verification

Run after each chunk (chunk 1, then chunk 2):

```bash
yarn test:jest --testPathPattern "source/main/mithril/(MithrilPartialSyncService|mithrilPartialSyncNodeStartup|MithrilController)\.spec\.ts"
```

Then once at the end:

```bash
yarn test:jest --testPathPattern "source/main/(mithril|ipc|utils)/.*\.spec\.ts"   # boundary net: bootstrap + coordinator + channel specs stay green
yarn compile
```

Key assertions the suite must show green:
- service spec: `adoptRecoverySnapshot` describe (5 tests), `finalizeCancel` failure lands on
  `cancelled`, marker-first staging describe (2 tests);
- startup spec: reclaim-failure boot continues; Quit branch calls `safeExitWithCode(0)`; wipe
  branch reclaims `/vol/mithril-partial-sync`;
- controller spec: adopt-before-delegate order for both recovery handlers.

Environment notes (do not misread as regressions):
- `yarn compile` under Node v24 can fail on stale `.scss.d.ts` typings unrelated to these
  main-process edits — regenerate with typed-scss-modules per the repo verify-env note before
  judging.
- prettier 2.1.2 oscillates on `mithrilCommandRunner*.ts` — those files are NOT touched here;
  do not reformat them. New test assertions above already use `expect.objectContaining` for
  `toHaveBeenCalledWith('string', {...})` shapes to avoid the known oscillation.
- Never run `git add -A`/`-u`; never stage `.gitignore` or `.agent/skills/`; no commits/pushes/
  GitHub writes from this task.

## Files touched

- `source/main/mithril/MithrilPartialSyncService.ts` — T5 (`adoptRecoverySnapshot`), T9
  (`finalizeCancel`), T10 (`_cleanupPartialSyncArtifacts`, `wipeAndFullSync`)
- `source/main/mithril/MithrilController.ts` — T5 (recovery-handler seeding in
  `_getPartialSyncDependencies`)
- `source/main/mithril/mithrilPartialSyncNodeStartup.ts` — T6, T8, T10 (imports + three branch
  edits)
- `source/main/mithril/MithrilPartialSyncService.spec.ts` — T5/T9/T10 tests
- `source/main/mithril/MithrilController.spec.ts` — T5 wiring tests
- `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` — T6/T8/T10 tests + two new
  module mocks

No other file. No i18n files. No renderer files.

## Out of scope (owned elsewhere)

- N7 diagnostics re-check (`DaedalusDiagnostics.tsx`) — CAT-B (T11); CAT-G must not plan it.
- Finalize-failed copy key (EN+JA) — CAT-C. Insufficient-disk-space copy key (T18) — CAT-D.
- All remaining copy work — CAT-E, including the native dialog text in
  `mithrilPartialSyncNodeStartup.ts` that currently says "Mithril partial sync" (vocabulary
  cleanup is NOT CAT-A's to fix) and the `PARTIAL_SYNC_DISABLED` coded error at
  `chainStorageCoordinator.ts` (T16).
- `MithrilPartialSyncErrorCode` union additions — CAT-D then CAT-E, append-only; CAT-A adds no
  codes.
- Shared start-failure fallback message + `mithrilErrorMessage.ts` helper — CAT-E (T22) /
  CAT-F (T23).
- Spawn-helper extraction preserving the POSIX process-group comment — CAT-F (T26). CAT-A's T14
  is verify-only.
- `MithrilPartialSyncOverlay.tsx` / `MithrilProgressView.tsx` (N4, T13) — CAT-C/CAT-G.

## Acceptance checks

- **T5:** unit test broadcasts a cross-session failure snapshot while the service is idle
  (service adopts it via the controller wiring); `wipeAndFullSync` does not throw and removes
  the staging root — repro spec `'adopts a broadcast failure snapshot while idle so the offered
  wipe succeeds'` plus controller adopt-before-delegate order tests. Backend-only actions
  (boundary 2) preserved: adoption copies a backend-emitted snapshot verbatim.
- **T6:** unit test `'C2 branch: continues normal boot and clears the marker even when staging
  reclaim fails'` — `fs.remove` rejects (EBUSY), `handleInterruptedRecovery` returns `false`
  (node start proceeds) and the marker is cleared.
- **T8:** Quit branch invokes `safeExitWithCode` (`'unsafe cutover, Quit (response 1): exits via
  safeExitWithCode...'`); continue/wipe branch still wipes and returns `false` (existing test
  retained, extended by the staging-reclaim test); stale-generation return does not exit.
- **T9:** finalizeCancel specs updated — the locked-staging case lands on `cancelled` (with
  `['retry', 'restart-normal']`), not `failed`; `abandonCancel` `failed` behavior untouched.
- **T10:** unit tests — a marker with a custom-volume `stagingRootPath` causes wipe (and
  restart-normal cleanup, and the startup wipe branch) to remove exactly that path; marker
  retained on `wipeAndFullSync` until finalization.
- **T14:** no-op recorded; checklist answer only; `detached: !environment.isWindows` and the
  POSIX process-group rationale comments verified present at both spawn sites.

## Escalations

- **E1 (interpretation note, planned as closest faithful step — escalate only on reviewer
  mismatch):** the brief's T10 wording "resolve the staging root FROM THE MARKER when present,
  session resolver as fallback" cannot apply a session-resolver fallback to the startup wipe
  branch: `MithrilPartialSyncNodeStartup` has no `_getStagingRootPath` (the resolver lives in
  the service). Step 2A.3 therefore uses the marker-persisted path only, mirroring the existing
  node-start-verified reclaim in the same file. If review expects a resolver fallback in the
  startup module, escalate rather than inventing one.
- **E2 (behavior note, no action needed):** with T9's best-effort cleanup, when `fs.remove`
  throws inside `_cleanupPartialSyncArtifacts` the trailing `clearMithrilPartialSyncMarker()` is
  skipped. This is safe by DD-703-9's own premise — no marker exists at cancel time
  (pre-cutover) — so no compensating clear is planned.
- **E3 (T14 tripwire):** if the `process.kill(-pid` group-kill in `killProcessTree.ts`, either
  `detached: !environment.isWindows` spawn option, or either rationale comment in
  `mithrilCommandRunner.ts` is missing at implementation time, stop and escalate — CAT-F's T26
  owns those seams and CAT-A must not edit them.
- No other brief/code mismatch found: every quoted anchor above was verified against the
  working tree on 2026-07-03 (channel exports at `mithrilPartialSyncChannel.ts`,
  `_assertRecoveryActionAllowed` rejection, `safeExitWithCode` export, `finalizeCancel` /
  `wipeAndFullSync` / `restartNormal` / `_cleanupPartialSyncArtifacts` /
  `finalizeCompletedPartialSync` bodies, marker shape with optional `stagingRootPath`).
