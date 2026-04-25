import { parseMithrilProgressUpdate } from './mithrilProgress';
import { MithrilBootstrapService } from './MithrilBootstrapService';

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/daedalus-state',
}));

jest.mock('../environment', () => ({
  environment: {
    network: 'mainnet',
    nodeVersion: '1.35.0',
  },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

describe('MithrilBootstrapService parsing', () => {
  it('preserves raw file progress counters', () => {
    expect(
      parseMithrilProgressUpdate('{"files_downloaded": 50, "files_total": 200}')
    ).toEqual({ filesDownloaded: 50, filesTotal: 200, progress: 25 });
  });

  it('parses step announcement fields', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"step_num": 3, "total_steps": 7, "message": "Downloading and unpacking the cardano db snapshot"}'
      )
    ).toEqual({
      stepNum: 3,
      totalSteps: 7,
      message: 'Downloading and unpacking the cardano db snapshot',
    });
  });

  it('step announcement is not null', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"step_num": 3, "total_steps": 7, "message": "Downloading..."}'
      )
    ).not.toBeNull();
  });

  it('parses Files label alongside file counts', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"label": "Files", "files_downloaded": 1200, "files_total": 5457, "seconds_elapsed": 20}'
      )
    ).toEqual({
      label: 'Files',
      filesDownloaded: 1200,
      filesTotal: 5457,
      progress: (1200 / 5457) * 100,
      elapsedSeconds: 20,
    });
  });

  it('parses Ancillary label with byte fields', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"label": "Ancillary", "bytes_downloaded": 154304512, "bytes_total": 683052510, "seconds_elapsed": 8.3}'
      )
    ).toEqual({
      label: 'Ancillary',
      bytesDownloaded: 154304512,
      bytesTotal: 683052510,
      elapsedSeconds: 8.3,
    });
  });

  it('ancillary-only bytes line is not null', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"label": "Ancillary", "bytes_downloaded": 100, "bytes_total": 500}'
      )
    ).not.toBeNull();
  });

  it('ignores unknown label values', () => {
    const result = parseMithrilProgressUpdate(
      '{"label": "Unknown", "files_downloaded": 1}'
    );
    expect(result).not.toBeNull();
    expect(result?.label).toBeUndefined();
  });

  it('graceful degradation — no label field still parses file counts', () => {
    expect(
      parseMithrilProgressUpdate('{"files_downloaded": 50, "files_total": 200}')
    ).toEqual({ filesDownloaded: 50, filesTotal: 200, progress: 25 });
  });

  it('returns null for message-only JSON', () => {
    expect(parseMithrilProgressUpdate('{"message": "hello"}')).toBeNull();
  });
});

describe('MithrilBootstrapService progress and error stages', () => {
  it('propagates raw file counters in download status updates', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const originalUpdateStatus = service._updateStatus.bind(service);
    const statusSpy = jest
      .spyOn(service, '_updateStatus')
      .mockImplementation((update) => originalUpdateStatus(update));

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"files_downloaded": 1, "files_total": 4, "seconds_elapsed": 2, "seconds_left": 6}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        filesDownloaded: 1,
        filesTotal: 4,
      })
    );
  });

  it('keeps download stage when startBootstrap reports failure', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const originalUpdateStatus = service._updateStatus.bind(service);
    const statusSpy = jest
      .spyOn(service, '_updateStatus')
      .mockImplementation((update) => originalUpdateStatus(update));

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_downloadSnapshot')
      .mockRejectedValue(
        service._createStageError('download', 'Mithril download failed', 'ERR')
      );

    await expect(service.startBootstrap('latest')).rejects.toThrow(
      'Mithril download failed'
    );

    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'failed',
        error: expect.objectContaining({
          stage: 'download',
          code: 'ERR',
          logPath: '/tmp/daedalus-state/Logs/mithril-bootstrap.log',
        }),
      })
    );
  });

  it('collapses post-download work into finalizing after download completes', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const originalUpdateStatus = service._updateStatus.bind(service);
    const statusSpy = jest
      .spyOn(service, '_updateStatus')
      .mockImplementation((update) => originalUpdateStatus(update));

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      service._updateStatus({
        status: 'downloading',
        filesDownloaded: 4,
        filesTotal: 4,
        elapsedSeconds: 12,
      });
    });
    jest.spyOn(service, '_convertSnapshot').mockResolvedValue(undefined);
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(service, '_installSnapshot').mockResolvedValue(undefined);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);

    await service.startBootstrap('latest');

    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'unpacking',
        filesDownloaded: undefined,
        filesTotal: undefined,
      })
    );
    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'finalizing',
        filesDownloaded: undefined,
        filesTotal: undefined,
      })
    );
    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'finalizing',
        filesDownloaded: undefined,
        filesTotal: undefined,
      })
    );
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'completed',
        filesDownloaded: undefined,
        filesTotal: undefined,
      })
    );
  });

  it('tracks total elapsed time through post-download local processing', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    service._bootstrapStartedAt = Date.now() - 4500;

    service._updateStatus({
      status: 'unpacking',
    });

    expect(service.status.elapsedSeconds).toBeGreaterThanOrEqual(4);
    expect(service.status.elapsedSeconds).toBeLessThan(6);
  });

  it('annotates conversion failures with convert stage', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const fse = require('fs-extra');
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest
      .spyOn(fse, 'readdir')
      .mockResolvedValue([{ name: '12345', isDirectory: () => true }]);
    jest.spyOn(service, '_runBinary').mockResolvedValue({
      stdout: '',
      stderr: 'conversion failed',
      exitCode: 1,
    });

    let conversionError: unknown;
    try {
      await service._convertSnapshot(null);
    } catch (error) {
      conversionError = error;
    }

    expect(service._buildError(conversionError)).toEqual(
      expect.objectContaining({
        stage: 'convert',
      })
    );
  });

  it('calls snapshot-converter with paths derived from the most recent ledger slot', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const fse = require('fs-extra');
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(fse, 'readdir').mockResolvedValue([
      { name: '10000', isDirectory: () => true },
      { name: '99999', isDirectory: () => true },
      { name: '50000', isDirectory: () => true },
      { name: 'not-a-slot', isDirectory: () => true },
    ]);
    const runBinarySpy = jest
      .spyOn(service, '_runBinary')
      .mockResolvedValue({ stdout: '', stderr: '', exitCode: 0 });

    await service._convertSnapshot(null);

    expect(runBinarySpy).toHaveBeenCalledWith(
      'snapshot-converter',
      expect.arrayContaining([
        '--input-mem',
        '/tmp/db/ledger/99999',
        '--output-lsm-snapshot',
        '/tmp/db/tmp/snapshots/99999_lsm',
        '--output-lsm-database',
        '/tmp/db/tmp/snapshots/lsm',
        '--config',
        '/tmp/db/tmp/cardano-node-distribution/share/mainnet/config.json',
      ])
    );
  });

  it('throws a convert-stage error when no ledger slots are found', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const fse = require('fs-extra');
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(fse, 'readdir').mockResolvedValue([
      { name: 'not-a-slot', isDirectory: () => true },
    ]);

    let conversionError: unknown;
    try {
      await service._convertSnapshot(null);
    } catch (error) {
      conversionError = error;
    }

    expect(service._buildError(conversionError)).toEqual(
      expect.objectContaining({ stage: 'convert' })
    );
  });

  it('keeps fallback stage for generic errors', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    expect(service._buildError(new Error('generic failure'), 'verify')).toEqual(
      expect.objectContaining({
        message: 'generic failure',
        stage: 'verify',
        logPath: '/tmp/daedalus-state/Logs/mithril-bootstrap.log',
      })
    );
  });

  it('preserves explicit stage when normalizing stage errors', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const stageError = service._createStageError('download', 'failed', 'E1');

    expect(service._buildError(stageError, 'verify')).toEqual({
      message: 'failed',
      code: 'E1',
      stage: 'download',
      logPath: '/tmp/daedalus-state/Logs/mithril-bootstrap.log',
    });
  });
});

describe('MithrilBootstrapService hardening fixes', () => {
  it('ignores cancel requests when no bootstrap is active', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const cleanupSpy = jest.spyOn(service, '_cleanupSnapshotArtifacts');
    const clearLockSpy = jest.spyOn(service, 'clearLockFile');

    await service.cancel();

    expect(service.status.status).toBe('idle');
    expect(cleanupSpy).not.toHaveBeenCalled();
    expect(clearLockSpy).not.toHaveBeenCalled();
  });

  // Gap 1: cancel must not fall through to failed
  it('status stays cancelled after process is killed — never transitions to failed', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);

    // Simulate _downloadSnapshot calling cancel() mid-download then raising an error
    // (mirrors the real scenario: user cancels → process killed → non-zero exit)
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      await service.cancel(); // sets _isCancelled = true and status = 'cancelled'
      throw service._createStageError('download', 'Process killed', 'SIGTERM');
    });

    await service.startBootstrap('latest').catch(() => {});

    expect(service.status.status).toBe('cancelled');
  });

  it('ignores buffered progress updates that arrive after cancel', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<string> = [];

    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      if (update.status) {
        statusUpdates.push(update.status);
      }
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest.spyOn(service, 'cancel').mockImplementation(async () => {
      service._isCancelled = true;
      service._progressItems = [];
      service._updateStatus({
        status: 'cancelled',
        progressItems: [],
      });
    });
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        await service.cancel();
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":100,"files_total":500,"seconds_elapsed":5}\n'
        );
        return { stdout: '', stderr: 'killed', exitCode: 1 };
      });

    await expect(service._downloadSnapshot('latest', null)).rejects.toThrow();

    expect(service.status.status).toBe('cancelled');
    expect(statusUpdates).toEqual(['downloading', 'cancelled']);
    expect(service.status.filesDownloaded).toBeUndefined();
    expect(service.status.filesTotal).toBeUndefined();
  });

  // Gap 2: step-only updates preserve last-known file counts
  it('step-only progress updates preserve last-known filesDownloaded/filesTotal', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');

    let capturedFilesUpdate: {
      filesDownloaded: any;
      filesTotal: any;
    } | null = null;
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      if (
        'filesDownloaded' in update &&
        update.status === 'downloading' &&
        capturedFilesUpdate !== null
      ) {
        // Only capture after the first files update
        capturedFilesUpdate = {
          filesDownloaded: update.filesDownloaded,
          filesTotal: update.filesTotal,
        };
      }
      if (update.filesDownloaded != null && capturedFilesUpdate === null) {
        capturedFilesUpdate = {
          filesDownloaded: update.filesDownloaded,
          filesTotal: update.filesTotal,
        };
      }
    });

    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        // First: a Files progress line
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":100,"files_total":500,"seconds_elapsed":5,"seconds_left":20}\n'
        );
        // Then: a step-only announcement (no file counts)
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    // After step 4, synthesis sets both bars to 100% (filesDownloaded = filesTotal)
    expect(service.status.filesDownloaded).toBe(500);
    expect(service.status.filesTotal).toBe(500);
  });

  // Gap 3a: ancillary metrics cleared when leaving downloading phase
  it('ancillary metrics are cleared when transitioning to unpacking', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<Partial<typeof service._status>> = [];

    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update } as any);
    });

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(service, '_convertSnapshot').mockResolvedValue(undefined);
    jest.spyOn(service, '_installSnapshot').mockResolvedValue(undefined);

    // _downloadSnapshot sets ancillary data then completes
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      service._updateStatus({
        status: 'downloading',
        ancillaryBytesDownloaded: 1000,
        ancillaryBytesTotal: 5000,
      });
    });

    await service.startBootstrap('latest');

    const unpackingUpdate = statusUpdates.find((u) => u.status === 'unpacking');
    expect(unpackingUpdate).toBeDefined();
    expect(unpackingUpdate?.ancillaryBytesDownloaded).toBeUndefined();
    expect(unpackingUpdate?.ancillaryBytesTotal).toBeUndefined();
  });

  // Gap 3b: ancillary metrics cleared when starting a new bootstrap run after failure
  it('ancillary metrics are cleared in preparing status when retrying after failed', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const preparingUpdates: Array<any> = [];

    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      if (update.status === 'preparing') {
        preparingUpdates.push({ ...update });
      }
    });

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);

    // Simulate a failed run that leaves ancillary data in status
    service._status = {
      ...service._status,
      status: 'failed',
      ancillaryBytesDownloaded: 9999,
      ancillaryBytesTotal: 10000,
    } as any;

    // Fail the next run immediately to keep test simple
    jest
      .spyOn(service, '_downloadSnapshot')
      .mockRejectedValue(new Error('network error'));

    await service.startBootstrap('latest').catch(() => {});

    const preparingUpdate = preparingUpdates[0];
    expect(preparingUpdate).toBeDefined();
    expect(preparingUpdate.ancillaryBytesDownloaded).toBeUndefined();
    expect(preparingUpdate.ancillaryBytesTotal).toBeUndefined();
  });

  // Gap 4: early process crash before any step leaves a downloadng progress item with error state
  it('progress has a downloading item with error state if process crashes before any steps', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');

    // Process exits immediately with no output (no step announcements)
    jest
      .spyOn(service, '_runCommand')
      .mockResolvedValue({ stdout: '', stderr: 'fatal error', exitCode: 1 });

    await expect(service.startBootstrap('latest')).rejects.toThrow();

    const progressItems = service._progressItems;
    const downloadingItem = progressItems.find(
      (item) => item.id === 'downloading'
    );
    expect(downloadingItem).toBeDefined();
    expect(downloadingItem?.state).toBe('error');
  });

  it('clears ancillary metrics on download failure', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    let failedStatus: any = null;

    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      if (update.status === 'failed') {
        failedStatus = { ...update };
      }
    });

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);

    // _downloadSnapshot sets ancillary data then fails
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      service._updateStatus({
        status: 'downloading',
        ancillaryBytesDownloaded: 1000,
        ancillaryBytesTotal: 5000,
      });
      throw new Error('download failed');
    });

    await service.startBootstrap('latest').catch(() => {});

    expect(failedStatus).toBeDefined();
    expect(failedStatus.ancillaryBytesDownloaded).toBeUndefined();
    expect(failedStatus.ancillaryBytesTotal).toBeUndefined();
  });
});

describe('MithrilBootstrapService verification transition (T2/T3/T3b)', () => {
  it('synthesizes both bars to 100% and emits verifying when step 4 arrives', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<any> = [];
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update });
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":500,"files_total":500,"seconds_elapsed":30,"seconds_left":0}\n'
        );
        options?.onStdout?.(
          '{"label":"Ancillary","bytes_downloaded":5000,"bytes_total":5000,"seconds_elapsed":10,"seconds_left":0}\n'
        );
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    const verifyingUpdate = statusUpdates.find((u) => u.status === 'verifying');
    expect(verifyingUpdate).toBeDefined();
    expect(verifyingUpdate.filesDownloaded).toBe(500);
    expect(verifyingUpdate.filesTotal).toBe(500);
    expect(verifyingUpdate.ancillaryBytesDownloaded).toBe(5000);
    expect(verifyingUpdate.ancillaryBytesTotal).toBe(5000);
  });

  it('subsequent steps 5-7 emit verifying status', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<any> = [];
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update });
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        options?.onStdout?.(
          '{"step_num":5,"total_steps":7,"message":"Verifying database"}\n'
        );
        options?.onStdout?.(
          '{"step_num":6,"total_steps":7,"message":"Computing message"}\n'
        );
        options?.onStdout?.(
          '{"step_num":7,"total_steps":7,"message":"Verifying signature"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    const verifyingUpdates = statusUpdates.filter(
      (u) => u.status === 'verifying'
    );
    // step 4 (synthesis) + steps 5, 6, 7 + final post-download = 5 verifying updates
    expect(verifyingUpdates.length).toBe(5);
    // No downloading updates after step 4
    const downloadingAfterVerify = statusUpdates.filter(
      (u, i) =>
        u.status === 'downloading' &&
        i > statusUpdates.indexOf(verifyingUpdates[0])
    );
    expect(downloadingAfterVerify.length).toBe(0);
  });

  it('drops late Files and Ancillary updates after step 4', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<any> = [];
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update });
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":400,"files_total":500,"seconds_elapsed":20,"seconds_left":5}\n'
        );
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        // Late arrivals after verification started
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":500,"files_total":500,"seconds_elapsed":25,"seconds_left":0}\n'
        );
        options?.onStdout?.(
          '{"label":"Ancillary","bytes_downloaded":3000,"bytes_total":3000,"seconds_elapsed":15,"seconds_left":0}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    // After verifying synthesis, no downloading updates should appear
    const verifyIdx = statusUpdates.findIndex((u) => u.status === 'verifying');
    const downloadingAfter = statusUpdates.filter(
      (u, i) => i > verifyIdx && u.status === 'downloading'
    );
    expect(downloadingAfter.length).toBe(0);
  });

  it('synthesis fires only once per download', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<any> = [];
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update });
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"label":"Files","files_downloaded":200,"files_total":500,"seconds_elapsed":10,"seconds_left":15}\n'
        );
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        // Second step 4 line (shouldn't happen but guard against it)
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    // Only one synthesis update with filesDownloaded/filesTotal
    const synthUpdates = statusUpdates.filter(
      (u) =>
        u.status === 'verifying' &&
        u.filesDownloaded != null &&
        u.filesTotal != null
    );
    expect(synthUpdates.length).toBe(1);
    expect(synthUpdates[0].filesDownloaded).toBe(500);
    expect(synthUpdates[0].filesTotal).toBe(500);
  });

  it('step progress items still appear in the waterfall for steps 4-7', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"step_num":3,"total_steps":7,"message":"Downloading"}\n'
        );
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        options?.onStdout?.(
          '{"step_num":5,"total_steps":7,"message":"Verifying database"}\n'
        );
        options?.onStdout?.(
          '{"step_num":6,"total_steps":7,"message":"Computing message"}\n'
        );
        options?.onStdout?.(
          '{"step_num":7,"total_steps":7,"message":"Verifying signature"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    const stepIds = service._progressItems.map((item) => item.id);
    expect(stepIds).toContain('step-3');
    expect(stepIds).toContain('step-4');
    expect(stepIds).toContain('step-5');
    expect(stepIds).toContain('step-6');
    expect(stepIds).toContain('step-7');
  });

  it('verification-stage errors carry verify stage', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        return { stdout: '', stderr: 'verify error', exitCode: 1 };
      });

    let thrownError: unknown;
    try {
      await service._downloadSnapshot('latest', null);
    } catch (error) {
      thrownError = error;
    }

    expect(service._buildError(thrownError)).toEqual(
      expect.objectContaining({
        stage: 'verify',
      })
    );
  });

  it('download-stage errors still carry download stage when failing before step 4', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"step_num":3,"total_steps":7,"message":"Downloading"}\n'
        );
        return { stdout: '', stderr: 'download error', exitCode: 1 };
      });

    let thrownError: unknown;
    try {
      await service._downloadSnapshot('latest', null);
    } catch (error) {
      thrownError = error;
    }

    expect(service._buildError(thrownError)).toEqual(
      expect.objectContaining({
        stage: 'download',
      })
    );
  });

  it('extracts the real stderr summary for download failures with JSON preamble', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        options?.onStdout?.(
          '{"step_num":3,"total_steps":7,"message":"Downloading"}\n'
        );
        return {
          stdout: '',
          stderr:
            '{"mithril_client_cli_version":"0.12.38"}\n' +
            '{"caution":"Ancillary verification does not use the Mithril certification."}\n' +
            "Error: Can not download and unpack cardano db snapshot for hash: 'latest'\n\n" +
            'Caused by:\n' +
            '    All locations failed for immutable_file_00120, tried locations: https://example.com/file.tar.zst\n',
          exitCode: 1,
        };
      });

    let thrownError: unknown;
    try {
      await service._downloadSnapshot('latest', null);
    } catch (error) {
      thrownError = error;
    }

    expect(service._buildError(thrownError)).toEqual(
      expect.objectContaining({
        stage: 'download',
        message:
          "Can not download and unpack cardano db snapshot for hash: 'latest'",
        code: expect.stringContaining(
          'All locations failed for immutable_file_00120'
        ),
      })
    );
  });

  it('_verifyingSynthesized resets on each bootstrap run', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(service, '_convertSnapshot').mockResolvedValue(undefined);
    jest.spyOn(service, '_installSnapshot').mockResolvedValue(undefined);
    jest.spyOn(service, '_downloadSnapshot').mockResolvedValue(undefined);

    // First run — manually set flag as if verification happened
    service._verifyingSynthesized = true;

    // Second run should reset the flag
    await service.startBootstrap('latest');
    expect(service._verifyingSynthesized).toBe(false);
  });

  it('inferErrorStageFromStatus maps verifying to verify', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    expect(service._inferErrorStageFromStatus('verifying')).toBe('verify');
  });

  it('startBootstrap error during verification carries verify stage', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    let failedUpdate: any = null;
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      if (update.status === 'failed') failedUpdate = { ...update };
    });

    jest.spyOn(service, '_createLockFile').mockResolvedValue(undefined);
    jest.spyOn(service, 'showSnapshot').mockResolvedValue(null);
    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest.spyOn(service, 'clearLockFile').mockResolvedValue(undefined);

    // Simulate download that reaches verification then fails
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      service._updateStatus({ status: 'verifying' });
      throw service._createStageError(
        'verify',
        'Mithril verification failed',
        'ERR'
      );
    });

    await service.startBootstrap('latest').catch(() => {});

    expect(failedUpdate).toBeDefined();
    expect(failedUpdate.error).toEqual(
      expect.objectContaining({
        stage: 'verify',
        code: 'ERR',
      })
    );
  });

  it('step 4 before any Files or Ancillary totals emits verifying without crashing', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    const statusUpdates: Array<any> = [];
    const originalUpdate = service._updateStatus.bind(service);
    jest.spyOn(service, '_updateStatus').mockImplementation((update) => {
      originalUpdate(update);
      statusUpdates.push({ ...update });
    });

    jest
      .spyOn(service, '_cleanupSnapshotArtifacts')
      .mockResolvedValue(undefined);
    jest
      .spyOn(service, '_resolveCardanoDbDownloadMode')
      .mockResolvedValue('download');
    jest
      .spyOn(service, '_runCommand')
      .mockImplementation(async (_, options) => {
        // Step 4 arrives with no prior Files or Ancillary lines
        options?.onStdout?.(
          '{"step_num":4,"total_steps":7,"message":"Verifying digests"}\n'
        );
        return { stdout: '', stderr: '', exitCode: 0 };
      });

    await service._downloadSnapshot('latest', null);

    const verifyingUpdate = statusUpdates.find((u) => u.status === 'verifying');
    expect(verifyingUpdate).toBeDefined();
    // Undefined totals should not be forcefully set to 100%
    expect(verifyingUpdate.filesDownloaded).toBeUndefined();
    expect(verifyingUpdate.filesTotal).toBeUndefined();
    expect(verifyingUpdate.ancillaryBytesDownloaded).toBeUndefined();
    expect(verifyingUpdate.ancillaryBytesTotal).toBeUndefined();
  });
});
