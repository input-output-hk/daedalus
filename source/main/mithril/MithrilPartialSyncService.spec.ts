import fs from 'fs-extra';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import { MithrilPartialSyncService } from './MithrilPartialSyncService';

jest.mock('fs-extra', () => ({
  constants: require('fs').constants,
  stat: jest.fn(),
  access: jest.fn(),
  readdir: jest.fn(),
  remove: jest.fn(),
  ensureDir: jest.fn(),
}));

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
  const statMock = fs.stat as jest.Mock;
  const accessMock = fs.access as jest.Mock;
  const readdirMock = fs.readdir as jest.Mock;
  const removeMock = fs.remove as jest.Mock;
  const ensureDirMock = fs.ensureDir as jest.Mock;

  const mockDirectoryStats = () => ({
    isDirectory: () => true,
    isFile: () => false,
  });

  const mockFileStats = () => ({
    isDirectory: () => false,
    isFile: () => true,
  });

  beforeEach(() => {
    jest.clearAllMocks();

    statMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/protocolMagicId')) {
        return mockFileStats();
      }

      return mockDirectoryStats();
    });
    accessMock.mockResolvedValue(undefined);
    readdirMock.mockResolvedValue([
      '00010.chunk',
      '00011.primary',
      'not-an-immutable-entry',
    ]);
    removeMock.mockResolvedValue(undefined);
    ensureDirMock.mockResolvedValue(undefined);
  });

  it('fails latest metadata resolution when no certified immutable number is present', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, '_showSnapshotRaw')
      .mockRejectedValueOnce(new Error('show failed'));
    jest.spyOn(service, '_listSnapshotsRaw').mockResolvedValueOnce([]);

    await expect(service.resolveLatestSnapshotMetadata()).rejects.toThrow(
      'Unable to resolve the latest Mithril snapshot metadata.'
    );
  });

  it('resolves the latest certified immutable number from show latest metadata', async () => {
    const service = new MithrilPartialSyncService();

    jest.spyOn(service, '_runCommand').mockResolvedValueOnce({
      stdout: JSON.stringify({
        digest: 'latest-digest',
        created_at: '2026-05-20T00:00:00Z',
        beacon: {
          immutable_file_number: 25,
        },
      }),
      stderr: '',
      exitCode: 0,
    });

    await expect(service.resolveLatestSnapshotMetadata()).resolves.toEqual({
      snapshot: expect.objectContaining({
        digest: 'latest-digest',
      }),
      latestCertifiedImmutableNumber: 25,
    });
  });

  it('falls back to snapshot list metadata when show latest fails', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, '_runCommand')
      .mockRejectedValueOnce(new Error('show failed'))
      .mockResolvedValueOnce({
        stdout: JSON.stringify([
          {
            digest: 'older-digest',
            created_at: '2026-05-19T00:00:00Z',
            cardano_db_beacon: {
              immutable_file_number: 20,
            },
          },
          {
            digest: 'latest-digest',
            created_at: '2026-05-20T00:00:00Z',
            cardano_db_beacon: {
              immutable_file_number: 25,
            },
          },
        ]),
        stderr: '',
        exitCode: 0,
      });

    await expect(service.resolveLatestSnapshotMetadata()).resolves.toEqual({
      snapshot: expect.objectContaining({
        digest: 'latest-digest',
      }),
      latestCertifiedImmutableNumber: 25,
    });
  });

  it('fails in preparing with the partial sync log path after preflight and staging complete', async () => {
    const service = new MithrilPartialSyncService();

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync download execution is not implemented yet.'
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

    expect(removeMock).toHaveBeenCalledWith('/tmp/daedalus-state/mithril-partial-sync');
    expect(ensureDirMock).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync/download'
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

  it('fails when the managed chain does not expose a readable immutable directory', async () => {
    const service = new MithrilPartialSyncService();

    statMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/immutable')) {
        throw new Error('missing immutable');
      }

      if (targetPath.endsWith('/protocolMagicId')) {
        return mockFileStats();
      }

      return mockDirectoryStats();
    });

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );
  });

  it('fails when the managed chain does not expose a readable protocolMagicId file', async () => {
    const service = new MithrilPartialSyncService();

    statMock.mockImplementation(async () => mockDirectoryStats());

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'The managed chain protocolMagicId path is not a file.'
    );
  });

  it('fails when the managed chain immutable directory is not readable', async () => {
    const service = new MithrilPartialSyncService();

    accessMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/immutable')) {
        throw new Error('permission denied');
      }
    });

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );
  });

  it('fails when protocolMagicId is not readable', async () => {
    const service = new MithrilPartialSyncService();

    accessMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/protocolMagicId')) {
        throw new Error('permission denied');
      }
    });

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to read protocolMagicId for Mithril partial sync preflight.'
    );
  });

  it('fails when no parseable immutable filename is available', async () => {
    const service = new MithrilPartialSyncService();

    readdirMock.mockResolvedValue(['volatile', 'CURRENT', 'primary']);

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to determine the local immutable position from the managed chain immutable directory.'
    );
  });

  it('fails when there is no certified immutable range to download', async () => {
    const service = new MithrilPartialSyncService();

    readdirMock.mockResolvedValue(['00025.chunk']);

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 25,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'The managed chain is not missing any certified immutable files for Mithril partial sync.'
    );
  });

  it('prepares staging outside the managed chain subtree for custom storage too', async () => {
    const service = new MithrilPartialSyncService();

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 12,
    });

    await expect(
      service.start({
        layoutResult: {
          managedChainPath: '/mnt/custom-storage/chain',
          isRecoveryFallback: false,
        },
        mithrilWorkDir: '/mnt/custom-storage/chain',
      })
    ).rejects.toThrow('Mithril partial sync download execution is not implemented yet.');

    expect(removeMock).toHaveBeenCalledWith('/tmp/daedalus-state/mithril-partial-sync');
    expect(ensureDirMock).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync/download'
    );
  });

  it('rejects staging paths that resolve inside the managed chain subtree', async () => {
    const service = new MithrilPartialSyncService();

    jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValueOnce({
      snapshot: {
        digest: 'latest-digest',
        createdAt: '2026-05-20T00:00:00Z',
        size: 2,
      },
      latestCertifiedImmutableNumber: 12,
    });

    await expect(
      service.start({
        layoutResult: {
          managedChainPath: '/tmp',
          isRecoveryFallback: false,
        },
        mithrilWorkDir: '/tmp',
      })
    ).rejects.toThrow(
      'The partial sync staging directory must be outside the managed chain path.'
    );
  });
});
