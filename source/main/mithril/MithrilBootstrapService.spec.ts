import {
  parseMithrilProgressLine,
  parseMithrilProgressUpdate,
} from './mithrilProgress';
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
  it('returns null for non-JSON lines', () => {
    expect(parseMithrilProgressLine('Download started')).toBeNull();
    expect(parseMithrilProgressLine('')).toBeNull();
  });

  it('returns number for numeric progress', () => {
    expect(parseMithrilProgressLine('{"progress": 42}')).toBe(42);
  });

  it('returns number for numeric string progress', () => {
    expect(parseMithrilProgressLine('{"progress": "15"}')).toBe(15);
  });

  it('returns number for percent alias', () => {
    expect(parseMithrilProgressLine('{"percent": 65}')).toBe(65);
    expect(parseMithrilProgressLine('{"percentage": 80}')).toBe(80);
  });

  it('returns null when progress is not numeric', () => {
    expect(parseMithrilProgressLine('{"progress": "foo"}')).toBeNull();
  });

  it('parses file progress totals', () => {
    expect(
      parseMithrilProgressLine('{"files_downloaded": 50, "files_total": 200}')
    ).toBe(25);
  });

  it('preserves raw file progress counters', () => {
    expect(
      parseMithrilProgressUpdate('{"files_downloaded": 50, "files_total": 200}')
    ).toEqual({ filesDownloaded: 50, filesTotal: 200, progress: 25 });
  });

  it('parses elapsed and remaining times', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"seconds_elapsed": 120.5, "seconds_left": 42}'
      )
    ).toEqual({ elapsedSeconds: 120.5, remainingSeconds: 42 });
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
        '{"label": "Files", "files_downloaded": 1200, "files_total": 5457, "seconds_elapsed": 20, "seconds_left": 72}'
      )
    ).toEqual({
      label: 'Files',
      filesDownloaded: 1200,
      filesTotal: 5457,
      progress: (1200 / 5457) * 100,
      elapsedSeconds: 20,
      remainingSeconds: 72,
    });
  });

  it('parses Ancillary label with byte fields', () => {
    expect(
      parseMithrilProgressUpdate(
        '{"label": "Ancillary", "bytes_downloaded": 154304512, "bytes_total": 683052510, "seconds_elapsed": 8.3, "seconds_left": 29.9}'
      )
    ).toEqual({
      label: 'Ancillary',
      bytesDownloaded: 154304512,
      bytesTotal: 683052510,
      elapsedSeconds: 8.3,
      remainingSeconds: 29.9,
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
        progress: 90,
        filesDownloaded: 4,
        filesTotal: 4,
        elapsedSeconds: 12,
        remainingSeconds: 0,
      });
    });
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
        progress: 92.5,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      })
    );
    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'finalizing',
        progress: 95,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      })
    );
    expect(statusSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        status: 'finalizing',
        progress: 97.5,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      })
    );
    expect(service.status).toEqual(
      expect.objectContaining({
        status: 'completed',
        progress: 100,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      })
    );
  });

  it('tracks total elapsed time through post-download local processing', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    service._bootstrapStartedAt = Date.now() - 4500;

    service._updateStatus({
      status: 'unpacking',
      progress: 92.5,
      remainingSeconds: undefined,
    });

    expect(service.status.elapsedSeconds).toBeGreaterThanOrEqual(4);
    expect(service.status.elapsedSeconds).toBeLessThan(6);
  });

  it('annotates conversion failures with convert stage', async () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');
    jest.spyOn(service, '_resolveDbDirectory').mockResolvedValue('/tmp/db');
    jest.spyOn(service, '_runCommand').mockResolvedValue({
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

  it('keeps fallback stage for generic errors', () => {
    const service = new MithrilBootstrapService('/tmp/mithril-test');

    expect(service._buildError(new Error('generic failure'), 'verify')).toEqual(
      expect.objectContaining({
        message: 'generic failure',
        stage: 'verify',
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
    });
  });
});

describe('MithrilBootstrapService hardening fixes', () => {
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

    // After the step-only update, the stored status should still have the file counts
    expect(service.status.filesDownloaded).toBe(100);
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
    jest.spyOn(service, '_installSnapshot').mockResolvedValue(undefined);

    // _downloadSnapshot sets ancillary data then completes
    jest.spyOn(service, '_downloadSnapshot').mockImplementation(async () => {
      service._updateStatus({
        status: 'downloading',
        ancillaryBytesDownloaded: 1000,
        ancillaryBytesTotal: 5000,
        ancillaryElapsedSeconds: 3,
        ancillaryRemainingSeconds: 12,
      });
    });

    await service.startBootstrap('latest');

    const unpackingUpdate = statusUpdates.find((u) => u.status === 'unpacking');
    expect(unpackingUpdate).toBeDefined();
    expect(unpackingUpdate?.ancillaryBytesDownloaded).toBeUndefined();
    expect(unpackingUpdate?.ancillaryBytesTotal).toBeUndefined();
    expect(unpackingUpdate?.ancillaryElapsedSeconds).toBeUndefined();
    expect(unpackingUpdate?.ancillaryRemainingSeconds).toBeUndefined();
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
      ancillaryElapsedSeconds: 30,
      ancillaryRemainingSeconds: 0,
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
    expect(preparingUpdate.ancillaryElapsedSeconds).toBeUndefined();
    expect(preparingUpdate.ancillaryRemainingSeconds).toBeUndefined();
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
        ancillaryElapsedSeconds: 3,
        ancillaryRemainingSeconds: 12,
      });
      throw new Error('download failed');
    });

    await service.startBootstrap('latest').catch(() => {});

    expect(failedStatus).toBeDefined();
    expect(failedStatus.ancillaryBytesDownloaded).toBeUndefined();
    expect(failedStatus.ancillaryBytesTotal).toBeUndefined();
    expect(failedStatus.ancillaryElapsedSeconds).toBeUndefined();
    expect(failedStatus.ancillaryRemainingSeconds).toBeUndefined();
  });
});
