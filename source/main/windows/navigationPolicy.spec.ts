import {
  bindWindowRecovery,
  ERR_ABORTED,
  getTrustedRendererUrl,
  installGlobalPopupPolicy,
  installFailedLoadRecovery,
  installTrustedWindowNavigationPolicy,
  isTrustedDocumentUrl,
  loadTrustedRenderer,
  shouldRecoverFailedLoad,
} from './navigationPolicy';

describe('trusted window navigation policy', () => {
  const trustedUrl = new URL('http://127.0.0.1:8080/');

  it('matches only the canonical trusted document and ignores its hash', () => {
    expect(
      isTrustedDocumentUrl(`${trustedUrl.href}#/wallets`, trustedUrl)
    ).toBe(true);
    expect(
      isTrustedDocumentUrl('http://127.0.0.1:8080.evil.test/', trustedUrl)
    ).toBe(false);
    expect(isTrustedDocumentUrl('http://127.0.0.1:8081/', trustedUrl)).toBe(
      false
    );
    expect(isTrustedDocumentUrl('https://127.0.0.1:8080/', trustedUrl)).toBe(
      false
    );
    expect(isTrustedDocumentUrl('not a URL', trustedUrl)).toBe(false);
  });

  it('constructs normalized development and packaged URLs', () => {
    expect(getTrustedRendererUrl(true, '/app/main').href).toBe(
      'http://127.0.0.1:8080/'
    );
    expect(getTrustedRendererUrl(false, '/app/main').href).toBe(
      'file:///app/renderer/index.html'
    );
  });

  it('denies untrusted main-frame navigation, redirects, and every subframe', () => {
    const handlers = {};
    const webContents = {
      on: jest.fn((name, handler) => {
        handlers[name] = handler;
      }),
    };
    installTrustedWindowNavigationPolicy(webContents as any, trustedUrl);

    const navigationEvent = { preventDefault: jest.fn() };
    handlers['will-navigate']({
      ...navigationEvent,
      url: 'https://example.test/',
    });
    expect(navigationEvent.preventDefault).toHaveBeenCalledTimes(1);

    const redirectEvent = { preventDefault: jest.fn() };
    handlers['will-redirect']({
      ...redirectEvent,
      url: 'https://example.test/',
    });
    expect(redirectEvent.preventDefault).toHaveBeenCalledTimes(1);

    const frameEvent = { preventDefault: jest.fn() };
    handlers['will-frame-navigate']({
      ...frameEvent,
      url: trustedUrl.href,
      isMainFrame: false,
    });
    expect(frameEvent.preventDefault).toHaveBeenCalledTimes(1);

    const trustedEvent = { preventDefault: jest.fn() };
    handlers['will-navigate']({
      ...trustedEvent,
      url: `${trustedUrl.href}#route`,
    });
    expect(trustedEvent.preventDefault).not.toHaveBeenCalled();
  });

  it('keeps aborted and subframe failures out of recovery', () => {
    expect(shouldRecoverFailedLoad(ERR_ABORTED, true)).toBe(false);
    expect(shouldRecoverFailedLoad(-2, false)).toBe(false);
    expect(shouldRecoverFailedLoad(-2, true)).toBe(true);
  });

  it('installs policy before loading the canonical document', async () => {
    const calls = [];
    const window = {
      webContents: { on: jest.fn(() => calls.push('policy')) },
      loadURL: jest.fn(() => {
        calls.push('load');
        return Promise.resolve();
      }),
    };

    await loadTrustedRenderer(window as any, trustedUrl);

    expect(calls.slice(0, 2)).toEqual(['policy', 'policy']);
    expect(calls[calls.length - 1]).toBe('load');
    expect(window.loadURL).toHaveBeenCalledWith(trustedUrl.href);
  });

  it('routes only recoverable load failures to recovery', () => {
    let handler;
    const webContents = {
      on: jest.fn((_name, listener) => {
        handler = listener;
      }),
    };
    const recover = jest.fn();
    installFailedLoadRecovery(webContents as any, recover);

    handler({}, ERR_ABORTED, '', '', true);
    handler({}, -2, '', '', false);
    handler({ type: 'failure' }, -2, '', '', true);

    expect(recover).toHaveBeenCalledTimes(1);
    expect(recover).toHaveBeenCalledWith({ type: 'failure' });
  });

  it('binds locale and bounds provider for window recovery', () => {
    const createWindow = jest.fn();
    const getBounds = jest.fn();
    const recover = bindWindowRecovery(createWindow, 'ja-JP', getBounds);

    recover();

    expect(createWindow).toHaveBeenCalledWith('ja-JP', getBounds);
  });

  it('installs side-effect-free popup denial', () => {
    let createdHandler;
    const app = {
      on: jest.fn((_name, handler) => {
        createdHandler = handler;
      }),
    };
    const contents = { setWindowOpenHandler: jest.fn() };

    installGlobalPopupPolicy(app as any);
    createdHandler({}, contents);

    expect(contents.setWindowOpenHandler).toHaveBeenCalledTimes(1);
    expect(contents.setWindowOpenHandler.mock.calls[0][0]()).toEqual({
      action: 'deny',
    });
  });
});
