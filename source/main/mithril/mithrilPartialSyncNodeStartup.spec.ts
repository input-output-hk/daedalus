import fs from 'fs-extra';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import { MithrilPartialSyncNodeStartup } from './mithrilPartialSyncNodeStartup';

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

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

jest.mock('../utils/safeExitWithCode', () => ({
  safeExitWithCode: jest.fn(),
}));

const {
  readMithrilPartialSyncMarker,
  clearMithrilPartialSyncMarker,
  writeMithrilPartialSyncMarker,
} = require('./mithrilPartialSyncMarker') as {
  readMithrilPartialSyncMarker: jest.Mock;
  clearMithrilPartialSyncMarker: jest.Mock;
  writeMithrilPartialSyncMarker: jest.Mock;
};

const emitPartialSyncStatus = jest.fn().mockResolvedValue(undefined);
const getPartialSyncStatus = jest.fn(
  (): MithrilPartialSyncStatusSnapshot => ({
    status: 'starting-node',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  })
);

const { dialog } = require('electron') as unknown as {
  dialog: { showMessageBox: jest.Mock };
};

const { safeExitWithCode } = require('../utils/safeExitWithCode') as {
  safeExitWithCode: jest.Mock;
};

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
    emitPartialSyncStatus,
    getPartialSyncStatus,
  });
  return {
    instance,
    node,
    getGeneration,
    wipeChainAndSnapshots,
    emitPartialSyncStatus,
    getPartialSyncStatus,
  };
};

beforeEach(() => {
  jest.clearAllMocks();
  writeMithrilPartialSyncMarker.mockResolvedValue(undefined);
  clearMithrilPartialSyncMarker.mockResolvedValue(undefined);
  fsMock.remove.mockResolvedValue(undefined);
  emitPartialSyncStatus.mockResolvedValue(undefined);
});

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
    });
    const { instance } = makeInstance();

    const result = await instance.handleInterruptedRecovery(0);

    expect(result).toBe(false);
    expect(fsMock.remove).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
  });

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

  it('C2 branch: does NOT emit failed status and does NOT call dialog.showMessageBox (not a C1 re-drive)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'node-start-verified',
      updatedAt: '2026-06-01T00:00:00.000Z',
      stagingRootPath: '/vol/mithril-partial-sync',
    });
    const { instance } = makeInstance();

    await instance.handleInterruptedRecovery(0);

    expect(emitPartialSyncStatus).not.toHaveBeenCalledWith(
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

  it('unsafe cutover: shows ONLY the native dialog and does NOT emit a failed status (single surface)', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'cutover-in-progress',
      updatedAt: '2026-06-01T00:00:00.000Z',
    });
    (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 }); // Quit
    const { instance } = makeInstance();

    await instance.handleInterruptedRecovery(0);

    expect(dialog.showMessageBox).toHaveBeenCalledTimes(1);
    expect(emitPartialSyncStatus).not.toHaveBeenCalledWith(
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
    expect(emitPartialSyncStatus).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'failed' })
    );
  });

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
    expect(safeExitWithCode).not.toHaveBeenCalled();
  });
});

describe('finalizeInstalledNodeStart', () => {
  // Override the startup delay to 0 ms for all finalize tests so no fake timers needed
  beforeEach(() => {
    jest.spyOn(global, 'setTimeout').mockImplementation(((fn: () => void) => {
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
    expect(emitPartialSyncStatus).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'completed' })
    );
  });

  it('carries forward managedChainPath and stagingRootPath even when only stagingRootPath is in the marker', async () => {
    readMithrilPartialSyncMarker.mockResolvedValue({
      state: 'installed-awaiting-node-start',
      updatedAt: '2026-06-01T00:00:00.000Z',
      stagingRootPath: '/vol/mithril-partial-sync',
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
