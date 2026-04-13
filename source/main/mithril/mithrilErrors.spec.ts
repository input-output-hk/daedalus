import {
  MithrilBootstrapStageError,
  buildMithrilError,
  createStageError,
  inferErrorStageFromStatus,
} from './mithrilErrors';

describe('MithrilBootstrapStageError', () => {
  it('has the correct name, stage, and code', () => {
    const error = new MithrilBootstrapStageError('oops', 'download', 'E01');
    expect(error.name).toBe('MithrilBootstrapStageError');
    expect(error.message).toBe('oops');
    expect(error.stage).toBe('download');
    expect(error.code).toBe('E01');
  });

  it('code is optional', () => {
    const error = new MithrilBootstrapStageError('fail', 'verify');
    expect(error.code).toBeUndefined();
  });
});

describe('buildMithrilError', () => {
  it('preserves stage and code from MithrilBootstrapStageError', () => {
    const stageError = new MithrilBootstrapStageError(
      'failed',
      'download',
      'E1'
    );
    expect(buildMithrilError(stageError, 'verify')).toEqual({
      message: 'failed',
      code: 'E1',
      stage: 'download',
    });
  });

  it('uses fallback stage for generic Error', () => {
    expect(buildMithrilError(new Error('generic failure'), 'verify')).toEqual({
      message: 'generic failure',
      stage: 'verify',
    });
  });

  it('returns default message for unknown error type', () => {
    expect(buildMithrilError(null, 'download')).toEqual({
      message: 'Mithril bootstrap failed',
      stage: 'download',
    });
  });

  it('returns undefined stage when no fallback provided and error is generic', () => {
    const result = buildMithrilError(new Error('err'));
    expect(result.stage).toBeUndefined();
  });
});

describe('createStageError', () => {
  it('creates a MithrilBootstrapStageError with all fields', () => {
    const error = createStageError('convert', 'conversion failed', 'E2');
    expect(error).toBeInstanceOf(MithrilBootstrapStageError);
    expect(error.stage).toBe('convert');
    expect(error.message).toBe('conversion failed');
    expect(error.code).toBe('E2');
  });
});

describe('inferErrorStageFromStatus', () => {
  it('maps downloading to download', () => {
    expect(inferErrorStageFromStatus('downloading')).toBe('download');
  });

  it('maps unpacking to convert', () => {
    expect(inferErrorStageFromStatus('unpacking')).toBe('convert');
  });

  it('maps converting to convert', () => {
    expect(inferErrorStageFromStatus('converting')).toBe('convert');
  });

  it('maps finalizing to convert', () => {
    expect(inferErrorStageFromStatus('finalizing')).toBe('convert');
  });

  it('returns undefined for idle', () => {
    expect(inferErrorStageFromStatus('idle')).toBeUndefined();
  });

  it('returns undefined for completed', () => {
    expect(inferErrorStageFromStatus('completed')).toBeUndefined();
  });
});
