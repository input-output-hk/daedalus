import type { Intl } from '../types/i18nTypes';
import {
  extractMithrilErrorMessage,
  getMithrilStartErrorMessage,
  toMithrilStartError,
} from './mithrilErrorMessage';

const makeIntl = () => {
  const formatMessage = jest.fn(
    (message: { id: string }) => `formatted:${message.id}`
  );
  return { intl: ({ formatMessage } as unknown) as Intl, formatMessage };
};

describe('extractMithrilErrorMessage', () => {
  it('returns the message of a real Error', () => {
    expect(extractMithrilErrorMessage(new Error('boom'))).toBe('boom');
  });

  it('returns null for an Error with an empty or blank message', () => {
    expect(extractMithrilErrorMessage(new Error(''))).toBeNull();
    expect(extractMithrilErrorMessage(new Error('   '))).toBeNull();
  });

  it('returns the message carried by a plain object rejection', () => {
    expect(extractMithrilErrorMessage({ message: 'from-ipc' })).toBe(
      'from-ipc'
    );
  });

  it('returns null for values without a usable message', () => {
    expect(extractMithrilErrorMessage(undefined)).toBeNull();
    expect(extractMithrilErrorMessage(null)).toBeNull();
    expect(extractMithrilErrorMessage('plain string')).toBeNull();
    expect(extractMithrilErrorMessage({ message: 42 })).toBeNull();
    expect(extractMithrilErrorMessage({ message: '' })).toBeNull();
  });
});

describe('getMithrilStartErrorMessage', () => {
  it('maps a rejection whose message is a known backend code to its copy', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(new Error('PARTIAL_SYNC_DISABLED'), intl)
    ).toBe('formatted:loading.mithrilPartialSync.error.failed.title');
  });

  it('maps a coded plain-object rejection the same way', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(
        { message: 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE' },
        intl
      )
    ).toBe(
      'formatted:loading.mithrilPartialSync.error.insufficientDiskSpace.title'
    );
  });

  it('formats the shared fallback for prose rejections instead of the raw message', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(new Error('raw backend prose'), intl)
    ).toBe('formatted:loading.mithrilPartialSync.error.startFailure');
  });

  it('formats the shared fallback when no message is extractable', () => {
    const { intl, formatMessage } = makeIntl();
    expect(getMithrilStartErrorMessage(undefined, intl)).toBe(
      'formatted:loading.mithrilPartialSync.error.startFailure'
    );
    expect(formatMessage).toHaveBeenCalledTimes(1);
  });
});

describe('toMithrilStartError', () => {
  it('returns a real Error unchanged', () => {
    const error = new Error('boom');
    expect(toMithrilStartError(error)).toBe(error);
  });

  it('wraps an object rejection preserving its message', () => {
    expect(toMithrilStartError({ message: 'from-ipc' }).message).toBe(
      'from-ipc'
    );
  });

  it('wraps unusable rejections with an empty message so the UI can apply its fallback', () => {
    expect(toMithrilStartError(42).message).toBe('');
    expect(toMithrilStartError(undefined).message).toBe('');
  });
});
