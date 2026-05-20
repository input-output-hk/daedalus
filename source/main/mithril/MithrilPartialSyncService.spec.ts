import fs from 'fs-extra';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import { MithrilPartialSyncService } from './MithrilPartialSyncService';
import type { MithrilPartialSyncStatusUpdate } from '../../common/types/mithril-partial-sync.types';

jest.mock('fs-extra', () => ({
  constants: require('fs').constants,
  pathExists: jest.fn(),
  readJson: jest.fn(),
  writeJson: jest.fn(),
  move: jest.fn(),
  stat: jest.fn(),
  access: jest.fn(),
  readdir: jest.fn(),
  remove: jest.fn(),
  ensureDir: jest.fn(),
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/daedalus-state',
  launcherConfig: {
    nodeConfig: {
      network: {
        configFile: '/config/config.yaml',
      },
    },
  },
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

const createLatestSnapshot = (latestCertifiedImmutableNumber = 25) => ({
  snapshot: {
    digest: 'latest-digest',
    createdAt: '2026-05-20T00:00:00Z',
    size: 2,
  },
  latestCertifiedImmutableNumber,
});

describe('MithrilPartialSyncService', () => {
  const statMock = fs.stat as jest.Mock;
  const accessMock = fs.access as jest.Mock;
  const readdirMock = fs.readdir as jest.Mock;
  const removeMock = fs.remove as jest.Mock;
  const ensureDirMock = fs.ensureDir as jest.Mock;
  const writeJsonMock = fs.writeJson as jest.Mock;
  const moveMock = fs.move as jest.Mock;

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
    writeJsonMock.mockResolvedValue(undefined);
    moveMock.mockResolvedValue(undefined);
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

  it('runs the staged partial restore command through conversion and validated cutover', async () => {
    const service = new MithrilPartialSyncService();
    jest
      .spyOn(service._chainStorageManager, 'installValidatedPartialSyncSnapshot')
      .mockResolvedValue(undefined);

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    const runCommandSpy = jest.spyOn(service, '_runCommand').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    jest.spyOn(service, '_runBinary').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    readdirMock.mockImplementation(async (targetPath: string) => {
      if (targetPath === '/tmp/chain/immutable') {
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db') {
        return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
      }

      return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
    });

    await expect(service.start(createContext())).resolves.toBeUndefined();

    expect(runCommandSpy).toHaveBeenCalledWith(
      [
        '--json',
        'cardano-db',
        'download',
        'latest',
        '--download-dir',
        '/tmp/daedalus-state/mithril-partial-sync/download',
        '--start',
        '12',
        '--end',
        '25',
        '--include-ancillary',
        '--allow-override',
      ],
      expect.any(Object),
      '/tmp/daedalus-state/mithril-partial-sync/download'
    );

    expect(service._chainStorageManager.installValidatedPartialSyncSnapshot).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync/download/db',
      {
        expectedTopLevelEntries: ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'],
      }
    );
    expect(writeJsonMock).toHaveBeenCalledTimes(2);
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'finalizing',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        logPath: '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
        error: null,
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

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );
  });

  it('fails when the managed chain does not expose a readable protocolMagicId file', async () => {
    const service = new MithrilPartialSyncService();

    statMock.mockImplementation(async () => mockDirectoryStats());

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

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

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

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

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to read protocolMagicId for Mithril partial sync preflight.'
    );
  });

  it('fails when no parseable immutable filename is available', async () => {
    const service = new MithrilPartialSyncService();

    readdirMock.mockResolvedValue(['volatile', 'CURRENT', 'primary']);

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

    await expect(service.start(createContext())).rejects.toThrow(
      'Unable to determine the local immutable position from the managed chain immutable directory.'
    );
  });

  it('fails when there is no certified immutable range to download', async () => {
    const service = new MithrilPartialSyncService();

    readdirMock.mockResolvedValue(['00025.chunk']);

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25));

    await expect(service.start(createContext())).rejects.toThrow(
      'The managed chain is not missing any certified immutable files for Mithril partial sync.'
    );
  });

  it('prepares staging outside the managed chain subtree for custom storage too', async () => {
    const service = new MithrilPartialSyncService();
    jest
      .spyOn(service._chainStorageManager, 'installValidatedPartialSyncSnapshot')
      .mockResolvedValue(undefined);

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(12));
    jest.spyOn(service, '_runCommand').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    jest.spyOn(service, '_runBinary').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    readdirMock.mockImplementation(async (targetPath: string) => {
      if (targetPath === '/mnt/custom-storage/chain/immutable') {
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db') {
        return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
      }

      return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
    });

    await expect(
      service.start({
        layoutResult: {
          managedChainPath: '/mnt/custom-storage/chain',
          isRecoveryFallback: false,
        },
        mithrilWorkDir: '/mnt/custom-storage/chain',
      })
    ).resolves.toBeUndefined();

    expect(removeMock).toHaveBeenCalledWith('/tmp/daedalus-state/mithril-partial-sync');
    expect(ensureDirMock).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync/download'
    );
  });

  it('rejects staging paths that resolve inside the managed chain subtree', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(12));

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

  it('rejects latest snapshot drift before command execution', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(25))
      .mockResolvedValueOnce(createLatestSnapshot(26));

    await expect(service.start(createContext())).rejects.toThrow(
      'The latest certified Mithril snapshot changed during partial sync preparation. Please retry with the refreshed range.'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        error: expect.objectContaining({
          stage: 'preparing',
          code: 'PARTIAL_SYNC_LATEST_DRIFT',
        }),
      })
    );
  });

  it('maps Mithril progress into downloading and verifying status updates', async () => {
    const service = new MithrilPartialSyncService();
    const statusUpdates: Array<MithrilPartialSyncStatusUpdate> = [];
    jest
      .spyOn(service._chainStorageManager, 'installValidatedPartialSyncSnapshot')
      .mockResolvedValue(undefined);

    service.onStatus((update) => {
      statusUpdates.push(update);
    });

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    jest.spyOn(service, '_runBinary').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    readdirMock.mockImplementation(async (targetPath: string) => {
      if (targetPath === '/tmp/chain/immutable') {
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db') {
        return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
      }

      return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
    });
    jest.spyOn(service, '_runCommand').mockImplementation(async (_args, options) => {
      options.onStdout?.(
        '{"step_num":1,"total_steps":7,"label":"Files","files_downloaded":2,"files_total":10,"seconds_elapsed":3}\n'
      );
      options.onStdout?.(
        '{"step_num":4,"total_steps":7,"message":"Verifying download"}\n'
      );
      options.onStdout?.(
        '{"label":"Ancillary","bytes_downloaded":100,"bytes_total":200,"seconds_elapsed":5}\n'
      );

      return {
        stdout: '',
        stderr: '',
        exitCode: 0,
      };
    });

    await expect(service.start(createContext())).resolves.toBeUndefined();

    expect(statusUpdates).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          status: 'downloading',
          filesDownloaded: 2,
          filesTotal: 10,
          elapsedSeconds: 3,
        }),
        expect.objectContaining({
          status: 'verifying',
          filesDownloaded: 2,
          filesTotal: 10,
          ancillaryBytesDownloaded: 100,
          ancillaryBytesTotal: 200,
          elapsedSeconds: 5,
        }),
      ])
    );
  });

  it('fails in downloading when the Mithril command exits before verification starts', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    jest.spyOn(service, '_runCommand').mockResolvedValue({
      stdout: '',
      stderr: 'download failed',
      exitCode: 1,
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync download failed with exit code 1'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        error: expect.objectContaining({
          stage: 'downloading',
          code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
        }),
      })
    );
  });

  it('fails in verifying when the Mithril command exits after verification starts', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    jest.spyOn(service, '_runCommand').mockImplementation(async (_args, options) => {
      options.onStdout?.('{"step_num":4,"total_steps":7,"message":"Verifying download"}\n');
      return {
        stdout: '',
        stderr: 'verification failed',
        exitCode: 1,
      };
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync verification failed with exit code 1'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        error: expect.objectContaining({
          stage: 'verifying',
          code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
        }),
      })
    );
  });

  it('fails in verifying when the staged db output is incomplete', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    jest.spyOn(service, '_runCommand').mockImplementation(async (_args, options) => {
      options.onStdout?.('{"step_num":4,"total_steps":7,"message":"Verifying download"}\n');
      return {
        stdout: '',
        stderr: '',
        exitCode: 0,
      };
    });
    statMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/ledger')) {
        throw new Error('missing ledger');
      }

      if (targetPath.endsWith('/protocolMagicId')) {
        return mockFileStats();
      }

      return mockDirectoryStats();
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync staged output is missing ledger.'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        error: expect.objectContaining({
          stage: 'verifying',
          code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
        }),
      })
    );
  });

  it('fails in installing when converted staged output includes volatile', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValue(createLatestSnapshot(25));
    jest.spyOn(service, '_runCommand').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    jest.spyOn(service, '_runBinary').mockResolvedValue({
      stdout: '',
      stderr: '',
      exitCode: 0,
    });
    readdirMock.mockImplementation(async (targetPath: string) => {
      if (targetPath === '/tmp/chain/immutable') {
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/daedalus-state/mithril-partial-sync/download/db') {
        return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId', 'volatile'];
      }

      return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
    });

    await expect(service.start(createContext())).rejects.toThrow(
      'Mithril partial sync staged output must contain exactly clean, immutable, ledger, lsm, protocolMagicId.'
    );

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        error: expect.objectContaining({
          stage: 'installing',
          code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
        }),
      })
    );
  });

  it('rejects cancellation once live cutover has started', async () => {
    const service = new MithrilPartialSyncService();

    service._activeWorkDir = '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'installing',
      allowedRecoveryActions: [],
      error: null,
    };

    await expect(service.cancel()).rejects.toThrow(
      'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
    );
  });
});
