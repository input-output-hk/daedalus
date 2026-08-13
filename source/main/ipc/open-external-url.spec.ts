import { shell } from 'electron';
import { normalizeExternalUrl, openExternalUrl } from './open-external-url';

jest.mock('electron', () => ({
  ipcMain: { on: jest.fn(), once: jest.fn() },
  shell: { openExternal: jest.fn() },
}));

const mockOpenExternal = shell.openExternal as jest.Mock;

describe('external URL policy', () => {
  beforeEach(() => mockOpenExternal.mockReset());

  it('accepts and canonicalizes credential-free HTTPS URLs', () => {
    expect(normalizeExternalUrl('https://EXAMPLE.test/path?q=1')).toBe(
      'https://example.test/path?q=1'
    );
  });

  it.each([
    undefined,
    42,
    'not a URL',
    'http://example.test/',
    'file:///tmp/a',
    'https:///',
    'https://user:secret@example.test/private?token=secret',
  ])(
    'rejects disallowed input without leaking it or opening the shell',
    (value) => {
      let error;
      try {
        normalizeExternalUrl(value);
      } catch (caughtError) {
        error = caughtError;
      }
      expect(error.message).toBe('External URL is not allowed');
      expect(error.message).not.toContain('secret');
      expect(mockOpenExternal).not.toHaveBeenCalled();
    }
  );

  it('awaits shell completion and replaces shell errors', async () => {
    let rejectShell;
    mockOpenExternal.mockReturnValue(
      new Promise((_resolve, reject) => {
        rejectShell = reject;
      })
    );

    const result = openExternalUrl('https://example.test/?secret=value');
    let settled = false;
    result.catch(() => {
      settled = true;
    });
    await Promise.resolve();
    expect(settled).toBe(false);

    rejectShell(new Error('secret=value'));
    await expect(result).rejects.toEqual(
      new Error('Unable to open external URL')
    );
    expect(mockOpenExternal).toHaveBeenCalledWith(
      'https://example.test/?secret=value'
    );
  });

  it('awaits successful shell completion', async () => {
    let resolveShell;
    mockOpenExternal.mockReturnValue(
      new Promise((resolve) => {
        resolveShell = resolve;
      })
    );
    const result = openExternalUrl('https://example.test/');
    let settled = false;
    result.then(() => {
      settled = true;
    });

    await Promise.resolve();
    expect(settled).toBe(false);
    resolveShell();
    await expect(result).resolves.toBeUndefined();
  });
});
