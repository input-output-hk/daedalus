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
  lstat: jest.fn(),
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
  runBinary: jest.fn(),
}));

// The three interactive kill sites route through killProcessTree; mock it so
// no test ever issues a real process-group kill against a fake pid.
jest.mock('./killProcessTree', () => ({
  killProcessTree: jest.fn(),
}));

const createContext = (): PartialSyncPreflightContext => ({
  layoutResult: {
    managedChainPath: '/tmp/chain',
    isRecoveryFallback: false,
  },
  mithrilWorkDir: '/tmp/mithril-workdir',
});

const createLatestSnapshot = (
  latestCertifiedImmutableNumber = 25,
  certifiedEpoch: number | null = null
) => ({
  snapshot: {
    digest: 'latest-digest',
    createdAt: '2026-05-20T00:00:00Z',
    size: 2,
  },
  latestCertifiedImmutableNumber,
  certifiedEpoch,
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
  const runBinaryMock = require('./mithrilCommandRunner')
    .runBinary as jest.Mock;
  const killProcessTreeMock = require('./killProcessTree')
    .killProcessTree as jest.Mock;
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

    require('check-disk-space').mockResolvedValue({
      free: 1_000_000_000_000,
      size: 2_000_000_000_000,
    });

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
      certifiedEpoch: null,
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
      certifiedEpoch: null,
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
      if (targetPath === '/tmp/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/mithril-partial-sync/download/db') {
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
    ).toHaveBeenCalledWith('/tmp/mithril-partial-sync/download/db', {
      expectedTopLevelEntries: [
        'clean',
        'immutable',
        'ledger',
        'lsm',
        'protocolMagicId',
      ],
    });
    expect(writeMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(2);
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'finalizing',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        logPath: '/tmp/daedalus-state/Logs/mithril-partial-sync.log',
        error: null,
      })
    );

    expect(removeMock).toHaveBeenCalledWith('/tmp/mithril-partial-sync');
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

  it('serves snapshot list and show reads from the shared metadata pipeline', async () => {
    const service = new MithrilPartialSyncService();

    runCommandMock.mockResolvedValueOnce({
      stdout: JSON.stringify([
        {
          digest: 'list-digest',
          created_at: '2026-07-01T00:00:00Z',
          size: 10,
          beacon: { immutable_file_number: 1200, epoch: 320 },
        },
      ]),
      stderr: '',
      exitCode: 0,
    });

    await expect(service.listSnapshots()).resolves.toEqual([
      expect.objectContaining({ digest: 'list-digest', size: 10 }),
    ]);

    runCommandMock.mockResolvedValueOnce({
      stdout: JSON.stringify({
        digest: 'show-digest',
        created_at: '2026-07-02T00:00:00Z',
        size: 20,
        beacon: { immutable_file_number: 1300, epoch: 321 },
      }),
      stderr: '',
      exitCode: 0,
    });

    await expect(service.showSnapshot('show-digest')).resolves.toEqual(
      expect.objectContaining({ digest: 'show-digest', size: 20 })
    );

    await expect(service.showSnapshot('   ')).resolves.toBeNull();
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
    ).toBe(require('path').dirname('/mnt/custom-storage/chain'));
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
      if (targetPath === '/tmp/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/mithril-partial-sync/download/db') {
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
      if (targetPath === '/tmp/mithril-partial-sync/download/db/ledger') {
        return [{ name: '12345', isDirectory: () => true }];
      }
      if (targetPath === '/tmp/mithril-partial-sync/download/db') {
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
      'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'
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

  it('does not emit a status when post-cutover cancel hard-rejects', async () => {
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
      'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'
    );
    // The throw path emits nothing; the early-return re-emit did not leak into it.
    expect(emissions).toHaveLength(0);
  });

  it('emits cancelling and defers cleanup until finalizeCancel when cancellation succeeds before cutover', async () => {
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

    const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
    service.onStatus((update) => emissions.push(update));

    await expect(service.cancel()).resolves.toBeUndefined();

    expect(removeMock).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarkerMock).not.toHaveBeenCalled();
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'cancelling',
        allowedRecoveryActions: [],
      })
    );
    expect(emissions).toEqual([
      expect.objectContaining({
        status: 'cancelling',
        allowedRecoveryActions: [],
      }),
    ]);

    await expect(service.finalizeCancel()).resolves.toBeUndefined();

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

  it('emits a non-retryable failed state from abandonCancel without cleanup', async () => {
    const service = new MithrilPartialSyncService();

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'cancelling',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.abandonCancel()).resolves.toBeUndefined();

    expect(removeMock).not.toHaveBeenCalled();
    expect(clearMithrilPartialSyncMarkerMock).not.toHaveBeenCalled();
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'failed',
        allowedRecoveryActions: [],
      })
    );
  });

  it('logs an info event at the real-cancel branch before killing the tracked process (observability)', async () => {
    const { info: infoLog } = require('../utils/logging').logger;
    const service = new MithrilPartialSyncService();

    const fakeChild = { pid: 999, kill: jest.fn() } as any;
    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._currentProcess = fakeChild;
    service._status = {
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.cancel()).resolves.toBeUndefined();

    // cancel() now routes through killProcessTree (group/tree SIGTERM), not a
    // direct child.kill().
    expect(killProcessTreeMock).toHaveBeenCalledWith(fakeChild, 'SIGTERM');
    expect(fakeChild.kill).not.toHaveBeenCalled();
    expect(infoLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: cancelling active partial sync process',
      expect.objectContaining({
        status: 'cancelling',
        pid: 999,
        hadChild: true,
      })
    );
    // The UNCONDITIONAL cancel-entry line fires alongside the in-branch line,
    // carrying the tracked child's real pid.
    expect(infoLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: cancel entry',
      expect.objectContaining({
        status: 'cancelling',
        pid: 999,
        hadChild: true,
      })
    );
  });

  it('emits the unconditional cancel-entry log with an empty slot ({ hadChild: false, pid: null }) and issues no kill', async () => {
    const { info: infoLog } = require('../utils/logging').logger;
    const service = new MithrilPartialSyncService();

    // Empty cancelable slot but an ACTIVE work dir: the real cancel branch must be reachable —
    // an empty slot AND empty workDir short-circuits on the no-active-sync early return, which
    // never emits the entry line.
    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._currentProcess = null;
    service._status = {
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.cancel()).resolves.toBeUndefined();

    // The entry line is UNCONDITIONAL — it fires even with no child to kill (the case the
    // in-branch line can never log), directly observing the slot state at cancel.
    expect(infoLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: cancel entry',
      expect.objectContaining({
        status: 'cancelling',
        pid: null,
        hadChild: false,
      })
    );
    // No child: neither the in-branch line nor any kill routing fires.
    expect(killProcessTreeMock).not.toHaveBeenCalled();
    expect(infoLog).not.toHaveBeenCalledWith(
      'MithrilPartialSyncService: cancelling active partial sync process',
      expect.anything()
    );
  });

  it('forceKillForShutdown() issues the sync-mode SIGKILL through killProcessTree (shutdown reap)', () => {
    const service = new MithrilPartialSyncService();
    const fakeChild = { pid: 999, kill: jest.fn() } as any;
    service._currentProcess = fakeChild;

    service.forceKillForShutdown();

    // The shutdown reap MUST stay sync: true — safeExitWithCode reaches process.exit()
    // inside a stream-end callback, so an async Windows taskkill issued that late would
    // never launch. Dropping { sync: true } or downgrading SIGKILL here is the exact
    // regression this pin exists to catch.
    expect(killProcessTreeMock).toHaveBeenCalledWith(
      fakeChild,
      'SIGKILL',
      expect.objectContaining({ sync: true })
    );
    expect(fakeChild.kill).not.toHaveBeenCalled();
  });

  it('forceKillForShutdown() swallows a throwing kill and warns — it must never throw into safeExit (shutdown reap)', () => {
    const { warn: warnLog } = require('../utils/logging').logger;
    const service = new MithrilPartialSyncService();
    const fakeChild = { pid: 999, kill: jest.fn() } as any;
    service._currentProcess = fakeChild;
    killProcessTreeMock.mockImplementationOnce(() => {
      throw new Error('taskkill exploded');
    });

    expect(() => service.forceKillForShutdown()).not.toThrow();

    expect(warnLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: failed to force kill process on shutdown',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('logs finalize entry and cleanup-success on the finalizeCancel happy path (observability)', async () => {
    const { info: infoLog } = require('../utils/logging').logger;
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

    await service.cancel();
    await expect(service.finalizeCancel()).resolves.toBeUndefined();

    expect(infoLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: finalizing cancel',
      expect.objectContaining({ status: 'cancelling' })
    );
    expect(infoLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: cancel finalized; partial sync artifacts cleaned up',
      null
    );
  });

  it('logs a warn when finalizeCancel cleanup fails and cancel proceeds (observability)', async () => {
    const { warn: warnLog } = require('../utils/logging').logger;
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

    await service.cancel();
    await expect(service.finalizeCancel()).resolves.toBeUndefined();

    expect(warnLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: finalizeCancel cleanup failed; landing on cancelled and leaving staging for the next start to reclaim',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('logs a warn at the abandonCancel restart floor including the captured fallback stage (observability)', async () => {
    const { warn: warnLog } = require('../utils/logging').logger;
    const service = new MithrilPartialSyncService();

    service._activeWorkDir =
      '/tmp/daedalus-state/mithril-partial-sync/download';
    service._status = {
      status: 'cancelling',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    };

    await expect(service.abandonCancel()).resolves.toBeUndefined();

    expect(warnLog).toHaveBeenCalledWith(
      'MithrilPartialSyncService: abandoning cancel; cleanup could not be completed — user must restart Daedalus',
      expect.objectContaining({ cancelFallbackErrorStage: 'preparing' })
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
      'PARTIAL_SYNC_START_NOT_ALLOWED'
    );
  });

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
        'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'
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

  describe('cancel correctness — interruptible stage machine', () => {
    // Drive the real start() stage machine with the stage helpers stubbed to succeed (mirroring the
    // happy-path / disk-space-preflight setup). Each test injects a concurrent cancel by flipping
    // _isCancelled inside a chosen stage stub, then asserts the checkpoints (CP-A..CP-D) unwind
    // start() without re-spawning the download, reaching cutover, or emitting `failed`.
    const primeStart = (service: MithrilPartialSyncService) => {
      jest
        .spyOn(
          service._chainStorageManager,
          'installValidatedPartialSyncSnapshot'
        )
        .mockResolvedValue(undefined);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const runCommandSpy = jest
        .spyOn(service, '_runCommand')
        .mockResolvedValue({ stdout: '', stderr: '', exitCode: 0 });
      jest
        .spyOn(service, '_runBinary')
        .mockResolvedValue({ stdout: '', stderr: '', exitCode: 0 });
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
      return { runCommandSpy };
    };

    it('_throwIfCancelled throws MithrilPartialSyncCancelledError only when _isCancelled is set', () => {
      const service = new MithrilPartialSyncService();

      expect(() => service._throwIfCancelled()).not.toThrow();

      service._isCancelled = true;
      let thrown: Error | undefined;
      try {
        service._throwIfCancelled();
      } catch (error) {
        thrown = error as Error;
      }
      expect(thrown).toBeInstanceOf(Error);
      expect(thrown?.name).toBe('MithrilPartialSyncCancelledError');
      expect(thrown?.message).toBe('Mithril partial sync was cancelled.');
    });

    it('a cancel during preparing unwinds at a checkpoint — no download re-spawn, no cutover, no failed emit', async () => {
      const service = new MithrilPartialSyncService();
      const { runCommandSpy } = primeStart(service);
      const downloadSpy = jest.spyOn(
        service,
        '_downloadAndVerifyPartialSnapshot'
      );
      const installSpy = jest.spyOn(service, '_installValidatedStagedSnapshot');

      // Simulate a concurrent cancel() landing during `preparing`: the disk-space preflight is the
      // last await before CP-A, so flip the flag there (cancel() has no live _currentProcess yet).
      jest
        .spyOn(service, '_assertSufficientDiskSpace')
        .mockImplementation(async () => {
          service._isCancelled = true;
        });

      const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
      service.onStatus((update) => emissions.push(update));

      await expect(service.start(createContext())).resolves.toBeUndefined();

      // CP-A unwound the stage machine before the download re-spawn: the flag flips in the
      // disk-space stub that runs immediately before CP-A, so start() throws there — the download
      // stage (and CP-B) is never entered.
      expect(downloadSpy).not.toHaveBeenCalled();
      expect(runCommandSpy).not.toHaveBeenCalled();
      // Never reached cutover.
      expect(installSpy).not.toHaveBeenCalled();
      expect(writeMithrilPartialSyncMarkerMock).not.toHaveBeenCalledWith(
        'cutover-in-progress',
        expect.anything()
      );
      // The _isCancelled catch short-circuits: no `failed`, and no re-emitted `downloading` frame.
      const statuses = emissions.map((emission) => emission.status);
      expect(statuses).not.toContain('failed');
      expect(statuses).not.toContain('downloading');
    });

    it('a cancel during converting unwinds at CP-D before install — no cutover marker, no install', async () => {
      const service = new MithrilPartialSyncService();
      primeStart(service);
      const installSpy = jest.spyOn(service, '_installValidatedStagedSnapshot');

      // Concurrent cancel lands during `converting`; the (stubbed) conversion returns normally, then
      // CP-D fires before the cutover marker/install (Boundary-A-only cancel invariant).
      jest
        .spyOn(service, '_convertStagedSnapshot')
        .mockImplementation(async () => {
          service._isCancelled = true;
        });

      await expect(service.start(createContext())).resolves.toBeUndefined();

      expect(installSpy).not.toHaveBeenCalled();
      expect(writeMithrilPartialSyncMarkerMock).not.toHaveBeenCalledWith(
        'cutover-in-progress',
        expect.anything()
      );
    });

    it('a cancel during converting that kills the converter (stage error) is swallowed — no failed emit, no cutover', async () => {
      const service = new MithrilPartialSyncService();
      primeStart(service);
      const installSpy = jest.spyOn(service, '_installValidatedStagedSnapshot');

      // Model the killed converter: cancel flips _isCancelled AND the conversion throws a `converting`
      // stage error (the tracked child exits non-zero). start()'s existing `if (this._isCancelled)
      // return;` catch must swallow it — no `failed` emit, no cutover — so the run settles quickly and
      // the coordinator join reaches finalizeCancel rather than the abandonCancel floor.
      jest
        .spyOn(service, '_convertStagedSnapshot')
        .mockImplementation(async () => {
          service._isCancelled = true;
          throw new Error(
            'Mithril partial sync conversion failed with exit code null (converter killed)'
          );
        });

      const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
      service.onStatus((update) => emissions.push(update));

      await expect(service.start(createContext())).resolves.toBeUndefined();

      expect(installSpy).not.toHaveBeenCalled();
      expect(writeMithrilPartialSyncMarkerMock).not.toHaveBeenCalledWith(
        'cutover-in-progress',
        expect.anything()
      );
      expect(emissions.map((emission) => emission.status)).not.toContain(
        'failed'
      );
    });

    it('_runBinary tracks the conversion child in the cancelable slot via the runner onProcess callback', async () => {
      const service = new MithrilPartialSyncService();
      const fakeChild = { pid: 4242 } as any;

      let capturedCallbacks: any;
      runBinaryMock.mockImplementation(
        async (
          _binaryName: string,
          _args: Array<string>,
          _workDir: string,
          _options: unknown,
          callbacks: any
        ) => {
          capturedCallbacks = callbacks;
          return { stdout: '', stderr: '', exitCode: 0 };
        }
      );

      await service._runBinary('snapshot-converter', ['--convert']);

      // _runBinary now passes the fifth `callbacks` arg with onProcess, mirroring _runCommand, so an
      // in-flight conversion lands in the cancelable slot that cancel()/forceKill() kill.
      expect(runBinaryMock).toHaveBeenCalledWith(
        'snapshot-converter',
        ['--convert'],
        expect.any(String),
        expect.objectContaining({ logFileName: 'mithril-partial-sync.log' }),
        expect.objectContaining({ onProcess: expect.any(Function) })
      );
      expect(capturedCallbacks).toBeDefined();

      // onProcess(child) populates the cancelable slot; onProcess(null) clears it on close/error.
      capturedCallbacks.onProcess(fakeChild);
      expect(service._currentProcess).toBe(fakeChild);
      capturedCallbacks.onProcess(null);
      expect(service._currentProcess).toBeNull();
    });

    it('_runCommand kills a late-spawned download child immediately when cancel already won the race', async () => {
      const service = new MithrilPartialSyncService();
      const fakeChild = { pid: 4343, kill: jest.fn() } as any;

      let capturedCallbacks: any;
      runCommandMock.mockImplementation(
        async (
          _args: Array<string>,
          _workDir: string,
          _options: unknown,
          callbacks: any
        ) => {
          capturedCallbacks = callbacks;
          return { stdout: '', stderr: '', exitCode: 0 };
        }
      );

      // The download call site threads trackAsCancelable: true; without it the
      // runner callbacks now (correctly) omit onProcess entirely.
      await service._runCommand(['cardano-db', 'download', 'latest'], {
        trackAsCancelable: true,
      });

      expect(capturedCallbacks).toBeDefined();

      service._isCancelled = true;
      capturedCallbacks.onProcess(fakeChild);

      expect(service._currentProcess).toBe(fakeChild);
      // The late-kill routes through killProcessTree (group/tree SIGTERM).
      expect(killProcessTreeMock).toHaveBeenCalledWith(fakeChild, 'SIGTERM');
      expect(fakeChild.kill).not.toHaveBeenCalled();

      const { info: infoLog } = require('../utils/logging').logger;
      expect(infoLog).toHaveBeenCalledWith(
        'MithrilPartialSyncService: killing late-arriving child after cancel',
        expect.objectContaining({ pid: 4343 })
      );
    });

    it('_runBinary kills a late-spawned converter child immediately when cancel already won the race', async () => {
      const service = new MithrilPartialSyncService();
      const fakeChild = { pid: 4444, kill: jest.fn() } as any;

      let capturedCallbacks: any;
      runBinaryMock.mockImplementation(
        async (
          _binaryName: string,
          _args: Array<string>,
          _workDir: string,
          _options: unknown,
          callbacks: any
        ) => {
          capturedCallbacks = callbacks;
          return { stdout: '', stderr: '', exitCode: 0 };
        }
      );

      await service._runBinary('snapshot-converter', ['--convert']);

      expect(capturedCallbacks).toBeDefined();

      service._isCancelled = true;
      capturedCallbacks.onProcess(fakeChild);

      expect(service._currentProcess).toBe(fakeChild);
      // The late-kill routes through killProcessTree (group/tree SIGTERM).
      expect(killProcessTreeMock).toHaveBeenCalledWith(fakeChild, 'SIGTERM');
      expect(fakeChild.kill).not.toHaveBeenCalled();

      const { info: infoLog } = require('../utils/logging').logger;
      expect(infoLog).toHaveBeenCalledWith(
        'MithrilPartialSyncService: killing late-arriving child after cancel',
        expect.objectContaining({ pid: 4444 })
      );
    });

    it('an untracked behind-ness probe mid-download receives NO onProcess and cannot clobber the durable download slot (slot-clobber regression)', async () => {
      const service = new MithrilPartialSyncService();
      const downloadChild = { pid: 5151, kill: jest.fn() } as any;

      const invocations: Array<{
        args: Array<string>;
        options: any;
        callbacks: any;
      }> = [];
      let releaseDownload: any;
      runCommandMock.mockImplementation(
        (
          args: Array<string>,
          _workDir: string,
          options: any,
          callbacks: any
        ) => {
          invocations.push({ args, options, callbacks });
          if (args.includes('download')) {
            // Hold the download in flight while the probe runs to completion.
            return new Promise((resolve) => {
              releaseDownload = resolve;
            });
          }
          // The untracked metadata read (snapshot show latest) resolves immediately.
          return Promise.resolve({
            stdout: JSON.stringify({
              digest: 'latest-digest',
              created_at: '2026-05-20T00:00:00Z',
              beacon: { immutable_file_number: 25 },
            }),
            stderr: '',
            exitCode: 0,
          });
        }
      );

      // The tracked download registers its child into the durable cancelable slot.
      const downloadPromise = service._runCommand(
        ['cardano-db', 'download', 'latest'],
        { trackAsCancelable: true }
      );
      expect(invocations).toHaveLength(1);
      invocations[0].callbacks.onProcess(downloadChild);
      expect(service._currentProcess).toBe(downloadChild);

      // Drive the REAL 30s behind-ness probe to completion mid-download (untracked default).
      jest
        .spyOn(service._chainStorageManager, 'getManagedChainPath')
        .mockResolvedValue('/tmp/chain');
      readdirMock.mockResolvedValue(['00005.chunk', 'not-an-immutable-entry']);
      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
      });

      // The metadata invocation carried NO onProcess callback AT ALL — it can neither
      // overwrite the slot on spawn nor null it on its own clean close (the confirmed
      // slot-clobber) — while it KEEPS onLogStream, and trackAsCancelable never leaks
      // into the runner options.
      expect(invocations).toHaveLength(2);
      const metadataInvocation = invocations[1];
      expect(metadataInvocation.args).toEqual([
        'cardano-db',
        'snapshot',
        'show',
        'latest',
        '--json',
      ]);
      expect(metadataInvocation.callbacks.onProcess).toBeUndefined();
      expect(metadataInvocation.callbacks.onLogStream).toEqual(
        expect.any(Function)
      );
      expect(metadataInvocation.options).toEqual({
        requireKeys: false,
        logFileName: 'mithril-partial-sync.log',
      });

      // The durable slot still names the download child ...
      expect(service._currentProcess).toBe(downloadChild);

      // ... so cancel() and forceKill() still target IT — not a no-op on a nulled slot, not
      // a metadata child (none was ever surfaced).
      service._activeWorkDir =
        '/tmp/daedalus-state/mithril-partial-sync/download';
      service._status = {
        status: 'downloading',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      };
      await expect(service.cancel()).resolves.toBeUndefined();
      expect(killProcessTreeMock).toHaveBeenNthCalledWith(
        1,
        downloadChild,
        'SIGTERM'
      );

      service.forceKill();
      expect(killProcessTreeMock).toHaveBeenNthCalledWith(
        2,
        downloadChild,
        'SIGKILL'
      );
      expect(killProcessTreeMock).toHaveBeenCalledTimes(2);
      expect(downloadChild.kill).not.toHaveBeenCalled();

      releaseDownload({ stdout: '', stderr: '', exitCode: 0 });
      await expect(downloadPromise).resolves.toEqual(
        expect.objectContaining({ exitCode: 0 })
      );
    });

    it('an untracked metadata read mid-conversion leaves the converter child tracked and killable', async () => {
      const service = new MithrilPartialSyncService();
      const converterChild = { pid: 5252, kill: jest.fn() } as any;

      let converterCallbacks: any;
      let releaseConversion: any;
      runBinaryMock.mockImplementation(
        (
          _binaryName: string,
          _args: Array<string>,
          _workDir: string,
          _options: unknown,
          callbacks: any
        ) => {
          converterCallbacks = callbacks;
          return new Promise((resolve) => {
            releaseConversion = resolve;
          });
        }
      );
      const metadataInvocations: Array<{ options: any; callbacks: any }> = [];
      runCommandMock.mockImplementation(
        (
          _args: Array<string>,
          _workDir: string,
          options: any,
          callbacks: any
        ) => {
          metadataInvocations.push({ options, callbacks });
          return Promise.resolve({ stdout: '[]', stderr: '', exitCode: 0 });
        }
      );

      // Hold the conversion in flight with its child in the durable slot (_runBinary's
      // unconditional onProcess — opt-in by construction).
      const conversionPromise = service._runBinary('snapshot-converter', [
        '--convert',
      ]);
      while (!converterCallbacks) {
        // No setImmediate in this jest environment; a zero timeout flushes the dynamic-import
        // microtasks ahead of the runner invocation.
        await new Promise((resolve) => {
          setTimeout(resolve, 0);
        });
      }
      converterCallbacks.onProcess(converterChild);
      expect(service._currentProcess).toBe(converterChild);

      // An ad-hoc untracked list read completes mid-conversion without touching the slot.
      await expect(service._listSnapshotsRaw()).resolves.toEqual([]);
      expect(metadataInvocations).toHaveLength(1);
      expect(metadataInvocations[0].callbacks.onProcess).toBeUndefined();
      expect(metadataInvocations[0].options).toEqual({
        requireKeys: false,
        logFileName: 'mithril-partial-sync.log',
      });
      expect(service._currentProcess).toBe(converterChild);

      // The conversion child registered via _runBinary stays killable.
      service.forceKill();
      expect(killProcessTreeMock).toHaveBeenCalledWith(
        converterChild,
        'SIGKILL'
      );
      expect(converterChild.kill).not.toHaveBeenCalled();

      releaseConversion({ stdout: '', stderr: '', exitCode: 0 });
      await conversionPromise;
    });

    it('a start()-phase metadata read registers into the cancelable slot so a cancel during preparing kills the in-flight metadata child', async () => {
      const service = new MithrilPartialSyncService();
      const metaChild = { pid: 5353, kill: jest.fn() } as any;

      const invocations: Array<{
        args: Array<string>;
        options: any;
        callbacks: any;
      }> = [];
      let releaseShowLatest: any;
      runCommandMock.mockImplementation(
        (
          args: Array<string>,
          _workDir: string,
          options: any,
          callbacks: any
        ) => {
          invocations.push({ args, options, callbacks });
          return new Promise((resolve) => {
            releaseShowLatest = () =>
              resolve({
                stdout: JSON.stringify({
                  digest: 'latest-digest',
                  created_at: '2026-05-20T00:00:00Z',
                  beacon: { immutable_file_number: 25 },
                }),
                stderr: '',
                exitCode: 0,
              });
          });
        }
      );

      // Drive the REAL start() so the preparing-phase resolveLatestSnapshotMetadata threads
      // trackAsCancelable: true down to _runCommand (no metadata/spawn stubs in the way).
      const startPromise = service.start(createContext());
      while (invocations.length === 0) {
        await new Promise((resolve) => {
          setTimeout(resolve, 0);
        });
      }

      // Pins that the start()-phase read DOES register — onProcess is passed to the runner —
      // while the service-local flag is stripped from the runner options.
      expect(invocations[0].args).toEqual([
        'cardano-db',
        'snapshot',
        'show',
        'latest',
        '--json',
      ]);
      expect(invocations[0].callbacks.onProcess).toEqual(expect.any(Function));
      expect(invocations[0].options).toEqual({
        requireKeys: false,
        logFileName: 'mithril-partial-sync.log',
      });

      invocations[0].callbacks.onProcess(metaChild);
      expect(service._currentProcess).toBe(metaChild);
      expect(service.status.status).toBe('preparing');

      // A cancel landing during `preparing` kills the in-flight metadata child.
      await expect(service.cancel()).resolves.toBeUndefined();
      expect(killProcessTreeMock).toHaveBeenCalledWith(metaChild, 'SIGTERM');
      expect(service.status.status).toBe('cancelling');

      // The runner closes out (slot cleared, stdout settles); start() unwinds via the
      // cancelled checkpoints without ever re-spawning a download or metadata re-resolve.
      invocations[0].callbacks.onProcess(null);
      releaseShowLatest();
      await expect(startPromise).resolves.toBeUndefined();
      expect(invocations).toHaveLength(1);
      expect(service.status.status).toBe('cancelling');
    });
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
      jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValue({
        snapshot: {
          digest: 'latest-digest',
          createdAt: '2026-05-20T00:00:00Z',
          size: 10_000_000_000,
        },
        latestCertifiedImmutableNumber: 25,
        certifiedEpoch: null,
      });

      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(0);
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

      jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockResolvedValue({
        snapshot: {
          digest: 'latest-digest',
          createdAt: '2026-05-20T00:00:00Z',
          size: 10_000_000_000,
        },
        latestCertifiedImmutableNumber: 25,
        certifiedEpoch: null,
      });

      jest
        .spyOn(service._chainStorageManager, '_getPathSizeBytes')
        .mockResolvedValue(0);
      require('check-disk-space').mockRejectedValueOnce(
        new Error('no measure')
      );

      await expect(service.start(createContext())).resolves.toBeUndefined();
    });

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

      await expect(
        service.finalizeCompletedPartialSync()
      ).resolves.toBeUndefined();

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

      await expect(
        service.finalizeCompletedPartialSync()
      ).resolves.toBeUndefined();

      // Falls back to stateDirectoryPath-based default path (stateDirectoryPath = /tmp/daedalus-state)
      expect(removeMock).toHaveBeenCalledWith(
        '/tmp/daedalus-state/mithril-partial-sync'
      );
    });

    it('is idempotent from idle (no throw, safe no-ops on fs.remove and clearMarker)', async () => {
      readMithrilPartialSyncMarkerMock.mockResolvedValue(null);
      const service = new MithrilPartialSyncService();

      await expect(
        service.finalizeCompletedPartialSync()
      ).resolves.toBeUndefined();
      await expect(
        service.finalizeCompletedPartialSync()
      ).resolves.toBeUndefined();

      expect(service.status.status).toBe('idle');
      expect(removeMock).toHaveBeenCalledTimes(2);
      expect(clearMithrilPartialSyncMarkerMock).toHaveBeenCalledTimes(2);
    });
  });

  describe('staging root resolution from the durable marker', () => {
    const readMithrilPartialSyncMarkerMock = require('./mithrilPartialSyncMarker')
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
        isProbeFailed: true,
      });
    });

    it('flags the probe as failed when the local immutable read rejects', async () => {
      const service = new MithrilPartialSyncService();
      jest
        .spyOn(service._chainStorageManager, 'getManagedChainPath')
        .mockRejectedValue(new Error('no chain dir yet'));
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
        isProbeFailed: true,
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

    it('caches the local immutable read within the TTL and re-resolves after it expires', async () => {
      // A cache hit must skip getManagedChainPath (which forks checkDiskSpace via
      // getConfig) AND the immutable/ readdir — count getManagedChainPath as the proxy for both.
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );
      const nowSpy = jest.spyOn(Date, 'now');

      nowSpy.mockReturnValue(1_000);
      await service.getPartialSyncBehindness();
      nowSpy.mockReturnValue(1_000 + 60_000); // within the 5 min TTL
      await service.getPartialSyncBehindness();

      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);

      nowSpy.mockReturnValue(1_000 + 6 * 60_000); // past the 5 min TTL
      await service.getPartialSyncBehindness();

      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(2);

      nowSpy.mockRestore();
    });

    it('invalidates the cached local read on a lifecycle reset so the next probe re-resolves it', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );
      const nowSpy = jest.spyOn(Date, 'now');
      nowSpy.mockReturnValue(1_000);

      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);

      // A lifecycle reset (finalize-completed → _resetToIdleStatus) drops both behind-ness caches.
      await service.finalizeCompletedPartialSync();

      // No Date.now advance: without invalidation the within-TTL cache would serve the stale value.
      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(2);

      nowSpy.mockRestore();
    });

    it('invalidates both cached reads on a chain-directory change so the next probe re-resolves them', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      const resolveSpy = jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );
      const nowSpy = jest.spyOn(Date, 'now');
      nowSpy.mockReturnValue(1_000);

      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);
      expect(resolveSpy).toHaveBeenCalledTimes(1);

      service.onChainDirectoryChanged();

      // No Date.now advance: without invalidation the within-TTL caches would
      // serve the stale values.
      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(2);
      expect(resolveSpy).toHaveBeenCalledTimes(2);

      nowSpy.mockRestore();
    });

    it('starts the aggregator query and the local read concurrently', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      let resolveLatest;
      jest.spyOn(service, 'resolveLatestSnapshotMetadata').mockImplementation(
        () =>
          new Promise((resolve) => {
            resolveLatest = resolve;
          })
      );
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );

      const probe = service.getPartialSyncBehindness();
      // Flush a few microtask turns so both joined reads have started while
      // the aggregator query is still pending; a sequential await would not
      // have touched the local read yet.
      await Promise.resolve();
      await Promise.resolve();
      await Promise.resolve();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);

      resolveLatest(createLatestSnapshot(25));
      await expect(probe).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
      });
    });

    it('returns the certified epoch on the success result when the beacon carries one', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25, 320));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
        certifiedEpoch: 320,
      });
    });

    it('returns the certified epoch on the not-behind (gap <= 0) result too', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 25);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25, 320));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
        certifiedEpoch: 320,
      });
    });

    it('omits the certified epoch when the beacon has none, leaving the verdict unchanged (safe-degrade)', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25, null));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
      });
    });
  });
});
