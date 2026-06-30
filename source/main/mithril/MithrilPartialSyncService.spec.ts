import fs from 'fs-extra';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import { MithrilPartialSyncService } from './MithrilPartialSyncService';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';

jest.mock('./mithrilPartialSyncMarker', () => ({
  clearMithrilPartialSyncMarker: jest.fn().mockResolvedValue(undefined),
  writeMithrilPartialSyncMarker: jest.fn().mockResolvedValue(undefined),
  readMithrilPartialSyncMarker: jest.fn().mockResolvedValue(null),
}));

jest.mock('check-disk-space', () => jest.fn());

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
  DISK_SPACE_REQUIRED: 1024,
  launcherConfig: {
    mithrilPartialSyncEnabled: true,
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

jest.mock('./mithrilCommandRunner', () => ({
  runCommand: jest.fn(),
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
  const runCommandMock = require('./mithrilCommandRunner')
    .runCommand as jest.Mock;
  const writeMithrilPartialSyncMarkerMock =
    require('./mithrilPartialSyncMarker')
      .writeMithrilPartialSyncMarker as jest.Mock;
  const clearMithrilPartialSyncMarkerMock =
    require('./mithrilPartialSyncMarker')
      .clearMithrilPartialSyncMarker as jest.Mock;
  const readMithrilPartialSyncMarkerMockTop =
    require('./mithrilPartialSyncMarker')
      .readMithrilPartialSyncMarker as jest.Mock;

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

    require('check-disk-space').mockResolvedValue({ free: 1_000_000_000_000, size: 2_000_000_000_000 });

    statMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/clean')) {
        return mockFileStats();
      }
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
    runCommandMock.mockReset();
    writeMithrilPartialSyncMarkerMock.mockResolvedValue(undefined);
    clearMithrilPartialSyncMarkerMock.mockResolvedValue(undefined);
    readMithrilPartialSyncMarkerMockTop.mockResolvedValue(null);
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
      .spyOn(
        service._chainStorageManager,
        'installValidatedPartialSyncSnapshot'
      )
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
      if (
        targetPath ===
        '/tmp/mithril-partial-sync/download/db/ledger'
      ) {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (
        targetPath === '/tmp/mithril-partial-sync/download/db'
      ) {
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
        '/tmp/mithril-partial-sync/download',
        '--start',
        '12',
        '--end',
        '25',
        '--include-ancillary',
        '--allow-override',
      ],
      expect.any(Object),
      '/tmp/mithril-partial-sync/download'
    );

    expect(
      service._chainStorageManager.installValidatedPartialSyncSnapshot
    ).toHaveBeenCalledWith(
      '/tmp/mithril-partial-sync/download/db',
      {
        expectedTopLevelEntries: [
          'clean',
          'immutable',
          'ledger',
          'lsm',
          'protocolMagicId',
        ],
      }
    );
    expect(writeMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(2);
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'finalizing',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        logPath: '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
        error: null,
      })
    );

    expect(removeMock).toHaveBeenCalledWith(
      '/tmp/mithril-partial-sync'
    );
    expect(ensureDirMock).toHaveBeenCalledWith(
      '/tmp/mithril-partial-sync/download'
    );
  });

  it('uses a distinct partial sync log file when running Mithril commands', async () => {
    const service = new MithrilPartialSyncService();

    runCommandMock.mockResolvedValueOnce({
      stdout: '[]',
      stderr: '',
      exitCode: 0,
    });

    await service.listSnapshots();

    expect(runCommandMock).toHaveBeenCalledWith(
      ['cardano-db', 'snapshot', 'list', '--json'],
      expect.any(String),
      expect.objectContaining({
        requireKeys: false,
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
      .spyOn(
        service._chainStorageManager,
        'installValidatedPartialSyncSnapshot'
      )
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
      if (
        targetPath ===
        '/mnt/custom-storage/mithril-partial-sync/download/db/ledger'
      ) {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (
        targetPath === '/mnt/custom-storage/mithril-partial-sync/download/db'
      ) {
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

    expect(removeMock).toHaveBeenCalledWith(
      '/mnt/custom-storage/mithril-partial-sync'
    );
    expect(ensureDirMock).toHaveBeenCalledWith(
      '/mnt/custom-storage/mithril-partial-sync/download'
    );
    // Cutover colocation assertion: staging parent dir equals chain parent dir (intra-volume)
    expect(
      require('path').dirname('/mnt/custom-storage/mithril-partial-sync')
    ).toBe(
      require('path').dirname('/mnt/custom-storage/chain')
    );
  });

  it('rejects staging paths that resolve inside the managed chain subtree', async () => {
    const service = new MithrilPartialSyncService();

    jest
      .spyOn(service, 'resolveLatestSnapshotMetadata')
      .mockResolvedValueOnce(createLatestSnapshot(12));

    // mithrilWorkDir '/something' → dirname '/' → staging '/mithril-partial-sync'
    // managedChainPath '/' → isPathWithin('/', '/mithril-partial-sync') = true → guard fires
    await expect(
      service.start({
        layoutResult: {
          managedChainPath: '/',
          isRecoveryFallback: false,
        },
        mithrilWorkDir: '/something',
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
    const statusUpdates: Array<MithrilPartialSyncStatusSnapshot> = [];
    jest
      .spyOn(
        service._chainStorageManager,
        'installValidatedPartialSyncSnapshot'
      )
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
      if (
        targetPath ===
        '/tmp/mithril-partial-sync/download/db/ledger'
      ) {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (
        targetPath === '/tmp/mithril-partial-sync/download/db'
      ) {
        return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
      }

      return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
    });
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_args, options) => {
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
          transferProgress: expect.objectContaining({
            filesDownloaded: 2,
            filesTotal: 10,
            elapsedSeconds: 3,
          }),
        }),
        expect.objectContaining({
          status: 'verifying',
          transferProgress: expect.objectContaining({
            filesDownloaded: 2,
            filesTotal: 10,
            ancillaryBytesDownloaded: 100,
            ancillaryBytesTotal: 200,
            elapsedSeconds: 5,
          }),
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
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_args, options) => {
        options.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying download"}\n'
        );
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
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_args, options) => {
        options.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying download"}\n'
        );
        return {
          stdout: '',
          stderr: '',
          exitCode: 0,
        };
      });
    statMock.mockImplementation(async (targetPath: string) => {
      if (targetPath.endsWith('/clean')) {
        return mockFileStats();
      }
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
      if (
        targetPath ===
        '/tmp/mithril-partial-sync/download/db/ledger'
      ) {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (
        targetPath === '/tmp/mithril-partial-sync/download/db'
      ) {
        return [
          'clean',
          'immutable',
          'ledger',
          'lsm',
          'protocolMagicId',
          'volatile',
        ];
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

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'installing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.cancel()).rejects.toThrow(
      'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
    );
  });

  it('re-emits the current status when cancel is requested during the node-stop window', async () => {
    const service = new MithrilPartialSyncService();

    // node-stop window: nothing active, both work refs null.
    service._activeWorkDir = null;
    service._currentProcess = null;
    service._status = {
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
    service.onStatus((update) => emissions.push(update));

    await expect(service.cancel()).resolves.toBeUndefined();

    // A status emission happened (renderer can resync off it) ...
    expect(emissions).toHaveLength(1);
    // ... and it is the TRUE current status, not a fabricated `cancelled`/`failed`.
    expect(emissions[0]).toEqual(
      expect.objectContaining({
        status: 'completed',
        allowedRecoveryActions: [],
      })
    );
    expect(service.status.status).toBe('completed');
  });

  it('does not emit a status when post-cutover cancel hard-rejects (lock #6 regression guard)', async () => {
    const service = new MithrilPartialSyncService();

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'installing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
    service.onStatus((update) => emissions.push(update));

    await expect(service.cancel()).rejects.toThrow(
      'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
    );
    // The throw path emits nothing; the early-return re-emit did not leak into it.
    expect(emissions).toHaveLength(0);
  });

  it('cleans staging artifacts and clears the marker when cancellation succeeds before cutover', async () => {
    const service = new MithrilPartialSyncService();

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._stagedDbPath =
      '/tmp/daedalus-state/mithril-partial-sync/download/db';
    service._status = {
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.cancel()).resolves.toBeUndefined();

    expect(removeMock).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync'
    );
    expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(1);
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'cancelled',
        allowedRecoveryActions: ['retry', 'restart-normal'],
      })
    );
  });

  it('surfaces a boundary-a failure when cancellation cleanup fails', async () => {
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

    await expect(service.cancel()).rejects.toThrow('cleanup failed');

    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        error: expect.objectContaining({
          message: 'cleanup failed',
          stage: 'downloading',
        }),
      })
    );
  });

  it('resets to idle after restart-normal cleanup when that recovery is allowed', async () => {
    const service = new MithrilPartialSyncService();

    service._status = {
      status: 'failed',
      allowedRecoveryActions: ['restart-normal', 'wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: {
        message: 'node handoff failed',
        stage: 'starting-node',
      },
    };

    await expect(service.restartNormal()).resolves.toBeUndefined();

    expect(removeMock).toHaveBeenCalledWith(
      '/tmp/daedalus-state/mithril-partial-sync'
    );
    expect(service.status).toEqual({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      error: null,
      logPath: undefined,
      progressItems: [],
    });
  });

  it('retains the marker during wipe-and-full-sync cleanup until finalization runs', async () => {
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
      '/tmp/daedalus-state/mithril-partial-sync'
    );
    expect(clearMithrilPartialSyncMarkerMock).not.toHaveBeenCalled();

    await expect(service.finalizeWipeAndFullSync()).resolves.toBeUndefined();
    expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(1);
  });

  it('rejects start reuse from wipe-only failed boundaries', () => {
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

    expect(() => service.assertStartAllowed()).toThrow(
      'Mithril partial sync cannot retry from the current recovery boundary.'
    );
  });

  describe('disk-space preflight', () => {
    const setupStartMocks = (service: MithrilPartialSyncService) => {
      jest
        .spyOn(
          service._chainStorageManager,
          'installValidatedPartialSyncSnapshot'
        )
        .mockResolvedValue(undefined);
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
        if (targetPath === '/tmp/mithril-partial-sync/download/db/ledger') {
          return [{ name: '12345', isDirectory: () => true }];
        }
        if (targetPath === '/tmp/mithril-partial-sync/download/db') {
          return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
        }
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      });
    };

    it('fails closed with PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE when free space is insufficient', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);

      // Inject a large snapshot size so the required threshold exceeds the free space
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue({
          snapshot: {
            digest: 'latest-digest',
            createdAt: '2026-05-20T00:00:00Z',
            size: 10_000_000_000,
          },
          latestCertifiedImmutableNumber: 25,
        });

      const runCommandSpy = jest.spyOn(service, '_runCommand');

      require('check-disk-space').mockResolvedValue({ free: 1, size: 2 });

      await expect(service.start(createContext())).rejects.toThrow(
        'Not enough free disk space'
      );

      expect(service.status).toEqual(
        expect.objectContaining({
          status: 'failed',
          error: expect.objectContaining({
            code: 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE',
            stage: 'preparing',
          }),
          allowedRecoveryActions: expect.arrayContaining(['retry']),
        })
      );

      // Download command must never have been called (preflight precedes download)
      expect(runCommandSpy).not.toHaveBeenCalledWith(
        expect.arrayContaining(['cardano-db', 'download']),
        expect.anything(),
        expect.anything()
      );
    });

    it('proceeds when disk measurement throws (fail-open on measurement error)', async () => {
      const service = new MithrilPartialSyncService();
      setupStartMocks(service);

      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue({
          snapshot: {
            digest: 'latest-digest',
            createdAt: '2026-05-20T00:00:00Z',
            size: 10_000_000_000,
          },
          latestCertifiedImmutableNumber: 25,
        });

      require('check-disk-space').mockRejectedValueOnce(new Error('no measure'));

      await expect(service.start(createContext())).resolves.toBeUndefined();
    });
  });

  describe('finalizeCompletedPartialSync', () => {
    const readMithrilPartialSyncMarkerMock =
      require('./mithrilPartialSyncMarker')
        .readMithrilPartialSyncMarker as jest.Mock;

    beforeEach(() => {
      readMithrilPartialSyncMarkerMock.mockResolvedValue(null);
    });

    it('resets to idle, removes marker-persisted stagingRoot, and clears the marker exactly once', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValue({
        state: 'node-start-verified',
        updatedAt: '2026-06-01T00:00:00.000Z',
        stagingRootPath: '/vol/mithril-partial-sync',
      });
      const service = new MithrilPartialSyncService();
      // Prime status to something non-idle to confirm the reset
      service._status = {
        status: 'completed',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      };

      await expect(service.finalizeCompletedPartialSync()).resolves.toBeUndefined();

      expect(removeMock).toHaveBeenCalledWith('/vol/mithril-partial-sync');
      expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(1);
      expect(service.status).toEqual(
        expect.objectContaining({
          status: 'idle',
          allowedRecoveryActions: [],
          progressItems: [],
          error: null,
        })
      );
    });

    it('falls back to _getStagingRootPath() when the marker carries no stagingRootPath', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValue({
        state: 'node-start-verified',
        updatedAt: '2026-06-01T00:00:00.000Z',
        // no stagingRootPath
      });
      const service = new MithrilPartialSyncService();

      await expect(service.finalizeCompletedPartialSync()).resolves.toBeUndefined();

      // Falls back to stateDirectoryPath-based default path (stateDirectoryPath = /tmp/daedalus-state)
      expect(removeMock).toHaveBeenCalledWith('/tmp/daedalus-state/mithril-partial-sync');
    });

    it('is idempotent from idle (no throw, safe no-ops on fs.remove and clearMarker)', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValue(null);
      const service = new MithrilPartialSyncService();

      await expect(service.finalizeCompletedPartialSync()).resolves.toBeUndefined();
      await expect(service.finalizeCompletedPartialSync()).resolves.toBeUndefined();

      expect(service.status.status).toBe('idle');
      expect(removeMock).toHaveBeenCalledTimes(2);
      expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(2);
    });
  });

  describe('cutover marker writes persist stagingRootPath', () => {
    it('passes stagingRootPath at both cutover marker writes during start()', async () => {
      const service = new MithrilPartialSyncService();
      jest
        .spyOn(
          service._chainStorageManager,
          'installValidatedPartialSyncSnapshot'
        )
        .mockResolvedValue(undefined);

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
        if (targetPath === '/tmp/mithril-partial-sync/download/db/ledger') {
          return [{ name: '12345', isDirectory: () => true }];
        }
        if (targetPath === '/tmp/mithril-partial-sync/download/db') {
          return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
        }
        return ['00010.chunk', '00011.primary', 'not-an-immutable-entry'];
      });

      await expect(service.start(createContext())).resolves.toBeUndefined();

      // Both cutover writes must include stagingRootPath
      const cutoverCall = writeMithrilPartialSyncMarkerMock.mock.calls.find(
        ([state]: [string]) => state === 'cutover-in-progress'
      );
      const awaitingCall = writeMithrilPartialSyncMarkerMock.mock.calls.find(
        ([state]: [string]) => state === 'installed-awaiting-node-start'
      );

      expect(cutoverCall).toBeDefined();
      expect(cutoverCall[1]).toMatchObject({
        managedChainPath: '/tmp/chain',
        stagingRootPath: '/tmp/mithril-partial-sync',
      });

      expect(awaitingCall).toBeDefined();
      expect(awaitingCall[1]).toMatchObject({
        managedChainPath: '/tmp/chain',
        stagingRootPath: '/tmp/mithril-partial-sync',
      });
    });
  });

  describe('getPartialSyncBehindness', () => {
    // Stub the local immutable read deterministically. The fs-extra mock lacks lstat/realpath, so
    // mock getManagedChainPath directly and drive `local` through the immutable directory readdir.
    const stubLocalImmutableNumber = (
      service: MithrilPartialSyncService,
      localImmutableNumber: number
    ) => {
      jest
        .spyOn(service._chainStorageManager, 'getManagedChainPath')
        .mockResolvedValue('/tmp/chain');
      readdirMock.mockResolvedValue([
        `${String(localImmutableNumber).padStart(5, '0')}.chunk`,
        'not-an-immutable-entry',
      ]);
    };

    it('reports significantly behind when the gap meets the threshold', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
      });
    });

    it('reports not significantly behind but sets the gap when below the threshold', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 20);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
        behindByImmutables: 5,
      });
    });

    it('reports not behind without throwing when the local position is at or beyond latest', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 25);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
      });
    });

    it('degrades to not behind without throwing when the latest snapshot lookup rejects', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockRejectedValue(new Error('aggregator unreachable'));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
      });
    });

    it('caches the aggregator query within the TTL and re-queries after it expires', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      const resolveSpy = jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const nowSpy = jest.spyOn(Date, 'now');

      nowSpy.mockReturnValue(1_000);
      await service.getPartialSyncBehindness();
      nowSpy.mockReturnValue(1_000 + 60_000); // within the 5 min TTL
      await service.getPartialSyncBehindness();

      expect(resolveSpy).toHaveBeenCalledTimes(1);

      nowSpy.mockReturnValue(1_000 + 6 * 60_000); // past the 5 min TTL
      await service.getPartialSyncBehindness();

      expect(resolveSpy).toHaveBeenCalledTimes(2);

      nowSpy.mockRestore();
    });
  });
});
