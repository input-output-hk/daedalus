import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import { MithrilPartialSyncService } from './MithrilPartialSyncService';

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/daedalus-state',
}));

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

const createContext = (): PartialSyncPreflightContext => ({
  layoutResult: {
    managedChainPath: '/tmp/chain',
    isRecoveryFallback: false,
  },
  mithrilWorkDir: '/tmp/mithril-workdir',
});

describe('MithrilPartialSyncService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('falls back to snapshot list when latest show lookup fails', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'showSnapshot')
      .mockRejectedValueOnce(new Error('show failed'));
    jest.spyOn(service, 'listSnapshots').mockResolvedValueOnce([
      {
        digest: 'older-digest',
        createdAt: '2026-05-19T00:00:00Z',
        size: 1,
      },
      {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
    ]);

    await expect(service.resolveLatestSnapshotMetadata()).resolves.toEqual(
      expect.objectContaining({
        digest: 'latest-digest',
      })
    );
  });

  it('fails in preparing with the partial sync log path when the skeleton stops after latest resolution', async () => {
    const service = new MithrilPartialSyncService();

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      digest: 'latest-digest',
      createdAt: '2026-05-20T00:00:00Z',
      size: 2,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync range derivation is not implemented yet.'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        logPath: '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
        error: expect.objectContaining({
          stage: 'preparing',
          code: 'PARTIAL_SYNC_NOT_READY',
          logPath: '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
        }),
      })
    );
  });

  it('uses a distinct partial sync log file when running Mithril commands', async () => {
    const service = new MithrilPartialSyncService();

    const runCommandSpy = jest.spyOn(
      require('./mithrilCommandRunner'),
      'runCommand'
    );
    runCommandSpy.mockResolvedValueOnce({
      stdout: '[]',
      stderr: '',
      exitCode: 0,
    });

    await service.listSnapshots();

    expect(runCommandSpy).toHaveBeenCalledWith(
      ['cardano-db', 'snapshot', 'list', '--json'],
      expect.any(String),
      expect.objectContaining({
        logFileName: 'mithril-partial-sync.log',
      }),
      expect.any(Object)
    );
  });
});
