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
