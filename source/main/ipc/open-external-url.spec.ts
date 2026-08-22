/**
 * @jest-environment node
 */
/* eslint-disable no-script-url */
import type {} from './open-external-url';

const mockChannels: Array<{ onReceive: jest.Mock }> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = { onReceive: jest.fn() };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('electron', () => ({
  shell: { openExternal: jest.fn(() => Promise.resolve()) },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

const { shell } = jest.requireMock('electron');
const { logger } = jest.requireMock('../utils/logging');

const loadModule = () => {
  mockChannels.length = 0;
  let moduleExports;
  jest.isolateModules(() => {
    moduleExports = require('./open-external-url');
  });
  return moduleExports as typeof import('./open-external-url');
};

describe('open-external-url', () => {
  it('opens an https url', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('https://example.com/anchor.jsonld')
    ).resolves.toBeUndefined();
    expect(shell.openExternal).toHaveBeenCalledWith(
      'https://example.com/anchor.jsonld'
    );
  });

  it('opens an https url written with an uppercase scheme', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('HTTPS://example.com/')
    ).resolves.toBeUndefined();
    expect(shell.openExternal).toHaveBeenCalledTimes(1);
  });

  it.each([
    ['javascript', 'javascript:alert(document.cookie)'],
    ['file', 'file:///etc/passwd'],
    ['data', 'data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg=='],
    ['http', 'http://example.com/anchor.jsonld'],
    ['mixed-case javascript', 'JavaScript:alert(1)'],
    ['unparseable input', 'not a url'],
  ])('rejects %s without reaching the shell', async (_name, url) => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(handleOpenExternalUrl(url)).rejects.toThrow(
      'Rejected non-https external URL'
    );
    expect(shell.openExternal).not.toHaveBeenCalled();
  });

  it('logs the rejected scheme and nothing else', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('javascript:alert(1)')
    ).rejects.toThrow();
    expect(logger.warn).toHaveBeenCalledWith(
      'Open external URL: rejected non-https scheme',
      {
        scheme: 'javascript:',
      }
    );
  });

  it('logs an unparseable marker when the input is not a url', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(handleOpenExternalUrl('not a url')).rejects.toThrow();
    expect(logger.warn).toHaveBeenCalledWith(
      'Open external URL: rejected non-https scheme',
      {
        scheme: 'unparseable',
      }
    );
  });

  it('omits the rejected url from the log payload', async () => {
    const { handleOpenExternalUrl } = loadModule();
    await expect(
      handleOpenExternalUrl('http://user:pw@internal.example/secret')
    ).rejects.toThrow();
    const payload = JSON.stringify(logger.warn.mock.calls);
    expect(payload).not.toContain('internal.example');
    expect(payload).not.toContain('secret');
  });

  it('registers the hardened handler on the channel', () => {
    const moduleExports = loadModule();
    expect(mockChannels).toHaveLength(1);
    expect(mockChannels[0].onReceive).toHaveBeenCalledWith(
      moduleExports.handleOpenExternalUrl
    );
  });

  it('accepts only the https scheme', () => {
    const { isAllowedExternalUrl } = loadModule();
    expect(isAllowedExternalUrl('https://example.com')).toBe(true);
    expect(isAllowedExternalUrl('http://example.com')).toBe(false);
    expect(isAllowedExternalUrl('ipfs://QmHash')).toBe(false);
    expect(isAllowedExternalUrl('')).toBe(false);
  });
});
