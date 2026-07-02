import { resolvePartialSyncErrorCopy } from './partialSyncErrorCopy';

const err = (code?: string, stage?: any) =>
  ({ message: 'raw backend string', code, stage }) as any;

describe('resolvePartialSyncErrorCopy', () => {
  it.each([
    [
      'PARTIAL_SYNC_NO_CERTIFIED_RANGE',
      'loading.mithrilPartialSync.error.noCertifiedRange.title',
    ],
    [
      'PARTIAL_SYNC_LATEST_DRIFT',
      'loading.mithrilPartialSync.error.latestDrift.title',
    ],
    [
      'PARTIAL_SYNC_STAGED_DB_INVALID',
      'loading.mithrilPartialSync.error.stagedDbInvalid.title',
    ],
    [
      'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
      'loading.mithrilPartialSync.error.downloadFailed.title',
    ],
    [
      'PARTIAL_SYNC_CONVERSION_FAILED',
      'loading.mithrilPartialSync.error.conversionFailed.title',
    ],
  ])('maps code %s to bespoke copy', (code, titleId) => {
    expect(resolvePartialSyncErrorCopy('failed', err(code)).title.id).toBe(
      titleId
    );
  });

  it('latest-drift hint is the retriable message', () => {
    expect(
      resolvePartialSyncErrorCopy('failed', err('PARTIAL_SYNC_LATEST_DRIFT'))
        .hint.id
    ).toBe('loading.mithrilPartialSync.error.latestDrift.hint');
  });

  it('falls back to generic failed copy for an unknown code', () => {
    expect(
      resolvePartialSyncErrorCopy('failed', err('SOMETHING_ELSE')).title.id
    ).toBe('loading.mithrilPartialSync.error.failed.title');
  });

  it('uses the stage tier when no code matches', () => {
    expect(
      resolvePartialSyncErrorCopy('failed', err(undefined, 'converting')).title
        .id
    ).toBe('loading.mithrilPartialSync.error.conversionFailed.title');
  });

  it('code wins over stage (staged-db-invalid emitted at verifying or installing)', () => {
    expect(
      resolvePartialSyncErrorCopy(
        'failed',
        err('PARTIAL_SYNC_STAGED_DB_INVALID', 'installing')
      ).title.id
    ).toBe('loading.mithrilPartialSync.error.stagedDbInvalid.title');
  });

  it('returns calmer cancelled copy for cancelled status (distinct from failed)', () => {
    const cancelled = resolvePartialSyncErrorCopy('cancelled', null);
    const failed = resolvePartialSyncErrorCopy('failed', null);
    expect(cancelled.title.id).toBe(
      'loading.mithrilPartialSync.error.cancelled.title'
    );
    expect(cancelled.hint.id).not.toBe(failed.hint.id);
  });

  it('cancelled status short-circuits even when an error rode along', () => {
    const copy = resolvePartialSyncErrorCopy(
      'cancelled',
      err('PARTIAL_SYNC_LATEST_DRIFT')
    );
    expect(copy.title.id).toBe(
      'loading.mithrilPartialSync.error.cancelled.title'
    );
  });

  it('never returns error.message as the copy (no raw JSON in primary copy)', () => {
    const copy = resolvePartialSyncErrorCopy(
      'failed',
      err('PARTIAL_SYNC_LATEST_DRIFT')
    );
    expect(copy.title.defaultMessage).not.toContain('raw backend string');
    expect(copy.hint.defaultMessage).not.toContain('raw backend string');
  });
});
