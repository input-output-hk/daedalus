import fs from 'fs-extra';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { MithrilPartialSyncNodeStartup } from './mithrilPartialSyncNodeStartup';

// --- Module mocks ---

jest.mock('fs-extra', () => ({
  remove: jest.fn().mockResolvedValue(undefined),
}));

jest.mock('electron', () => ({
  dialog: {
    showMessageBox: jest.fn(),
  },
}));

jest.mock('./mithrilPartialSyncMarker', () => ({
  readMithrilPartialSyncMarker: jest.fn(),
  clearMithrilPartialSyncMarker: jest.fn().mockResolvedValue(undefined),
  writeMithrilPartialSyncMarker: jest.fn().mockResolvedValue(undefined),
}));

jest.mock('../ipc/mithrilPartialSyncChannel', () => ({
  emitMithrilPartialSyncStatus: jest.fn().mockResolvedValue(undefined),
  getMithrilPartialSyncStatus: jest.fn(() => ({
    status: 'starting-node',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  })),
}));

// --- Helpers ---

const {
  readMithrilPartialSyncMarker,
  clearMithrilPartialSyncMarker,
  writeMithrilPartialSyncMarker,
} = require('./mithrilPartialSyncMarker') as {
  readMithrilPartialSyncMarker: jest.Mock;
  clearMithrilPartialSyncMarker: jest.Mock;
  writeMithrilPartialSyncMarker: jest.Mock;
};

const {
  emitMithrilPartialSyncStatus,
} = require('../ipc/mithrilPartialSyncChannel') as {
  emitMithrilPartialSyncStatus: jest.Mock;
  getMithrilPartialSyncStatus: jest.Mock;
};

const { dialog } = require('electron') as unknown as { dialog: { showMessageBox: jest.Mock } };

const fsMock = fs as unknown as { remove: jest.Mock };

const makeCardanoNodeMock = (state = CardanoNodeStates.RUNNING) => ({
  state,
  start: jest.fn().mockResolvedValue(undefined),
});

const makeInstance = (cardanoNode?: ReturnType<typeof makeCardanoNodeMock>) => {
  const node = cardanoNode ?? makeCardanoNodeMock();
  const getGeneration = jest.fn(() => 0);
  const wipeChainAndSnapshots = jest.fn().mockResolvedValue(undefined);
  const instance = new MithrilPartialSyncNodeStartup({
    mainWindow: {} as never,
    cardanoNode: node as never,
    wipeChainAndSnapshots,
    getGeneration,
  });
  return { instance, node, getGeneration, wipeChainAndSnapshots };
};

beforeEach(() => {
  jest.clearAllMocks();
  writeMithrilPartialSyncMarker.mockResolvedValue(undefined);
  clearMithrilPartialSyncMarker.mockResolvedValue(undefined);
  fsMock.remove.mockResolvedValue(undefined);
  emitMithrilPartialSyncStatus.mockResolvedValue(undefined);
});

// --- handleInterruptedRecovery tests ---

describe('handleInterruptedRecovery', () => {
  it('returns false with no marker (no interrupted recovery)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue(null);
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
    expect(fsMock.remove).not.toHaveBeenCalled();
  });

  it('C2 branch: removes stagingRootPath then clears marker and returns false (normal boot)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
      managedChainPath: '/chain',
      stagingRootPath: '/vol/mithril-partial-sync',
    });
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(fsMock.remove).toHaveBeenCalledWith('/vol/mithril-partial-sync');
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
  });

  it('C2 branch: skips fs.remove when stagingRootPath is absent, still clears and returns false', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
      managedChainPath: '/chain',
      // no stagingRootPath
    });
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(fsMock.remove).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
  });

  it('C2 branch: does NOT emit failed status and does NOT call dialog.showMessageBox (not a C1 re-drive)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
      stagingRootPath: '/vol/mithril-partial-sync',
    });
    const { instance } = makeInstance();

    await instance.handleInterruptedRecovery(0);

    expect(emitMithrilPartialSyncStatus).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'failed' })
    );
    expect(dialog.showMessageBox).not.toHaveBeenCalled();
  });

  it('installed-awaiting-node-start: returns false without clearing (B/C1 hand-off)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'installed-awaiting-node-start',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
  });

  // --- Unsafe cutover (cutover-in-progress): native dialog is the SINGLE recovery surface (PRD D5a / gap #7) ---

  it('unsafe cutover: shows ONLY the native dialog and does NOT emit a failed status (single surface, PRD D5a)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 }); // Quit
    const { instance } = makeInstance();

    await instance.handleInterruptedRecovery(0);

    // The native dialog IS the recovery surface ...
    expect(dialog.showMessageBox).toHaveBeenCalledTimes(1);
    // ... and the redundant React `failed` emission is GONE.
    expect(emitMithrilPartialSyncStatus).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'failed' })
    );
  });

  it('unsafe cutover, Wipe (response 0): wipes chain + snapshots, clears the marker, returns false', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 0 });
    const { instance, wipeChainAndSnapshots } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(wipeChainAndSnapshots).toHaveBeenCalledTimes(1);
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
    expect(emitMithrilPartialSyncStatus).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'failed' })
    );
  });

  it('unsafe cutover, Quit (response 1): returns true (blocked) without wiping or clearing the marker', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 });
    const { instance, wipeChainAndSnapshots } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(true);
    expect(wipeChainAndSnapshots).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
  });

  it('unsafe cutover, generation changed during the dialog: returns true without wiping', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 0 }); // user picked Wipe...
    const { instance, getGeneration, wipeChainAndSnapshots } = makeInstance();
    getGeneration.mockReturnValue(1); // ...but generation moved on (re-check sees 0 !== 1)

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(true); // stale generation short-circuits before the wipe
    expect(wipeChainAndSnapshots).not.toHaveBeenCalled();
  });
});

// --- finalizeInstalledNodeStart tests ---

describe('finalizeInstalledNodeStart', () => {
  // Override the startup delay to 0 ms for all finalize tests so no fake timers needed
  beforeEach(() => {
    jest.spyOn(global, 'setTimeout').mockImplementation(((
      fn: () => void
    ) => {
      fn();
      return 0 as unknown as ReturnType<typeof global.setTimeout>;
    }) as typeof global.setTimeout);
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it('stamps node-start-verified (not clears) then emits completed', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'installed-awaiting-node-start',
      updatedAt: '2026-06-01T00:00:00.000Z',
      managedChainPath: '/chain',
      stagingRootPath: '/vol/mithril-partial-sync',
    });

    const node = makeCardanoNodeMock(CardanoNodeStates.RUNNING);
    const { instance } = makeInstance(node);

    await instance.finalizeInstalledNodeStart(0);

    expect(writeMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
    expect(writeMithrilPartialSyncMarker).toHaveBeenCalledWith(
      'node-start-verified',
      {
        managedChainPath: '/chain',
        stagingRootPath: '/vol/mithril-partial-sync',
      }
    );
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
    expect(emitMithrilPartialSyncStatus).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'completed' })
    );
  });

  it('carries forward managedChainPath and stagingRootPath even when only stagingRootPath is in the marker', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'installed-awaiting-node-start',
      updatedAt: '2026-06-01T00:00:00.000Z',
      stagingRootPath: '/vol/mithril-partial-sync',
      // no managedChainPath
    });

    const node = makeCardanoNodeMock(CardanoNodeStates.RUNNING);
    const { instance } = makeInstance(node);

    await instance.finalizeInstalledNodeStart(0);

    expect(writeMithrilPartialSyncMarker).toHaveBeenCalledWith(
      'node-start-verified',
      {
        managedChainPath: undefined,
        stagingRootPath: '/vol/mithril-partial-sync',
      }
    );
  });

  it('returns early without writing if the marker has the wrong state', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });

    const { instance } = makeInstance();
    await instance.finalizeInstalledNodeStart(0);

    expect(writeMithrilPartialSyncMarker).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
  });

  it('throws if the node is not RUNNING after the startup delay', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'installed-awaiting-node-start',
      updatedAt: '2026-06-01T00:00:00.000Z',
      managedChainPath: '/chain',
    });

    const node = makeCardanoNodeMock(CardanoNodeStates.STOPPED);
    const { instance } = makeInstance(node);

    await expect(instance.finalizeInstalledNodeStart(0)).rejects.toThrow(
      'Cardano node stopped responding during startup after Mithril partial sync cutover.'
    );
    expect(writeMithrilPartialSyncMarker).not.toHaveBeenCalled();
  });
});
