import { EventEmitter } from 'events';
import {
  authorizeTrustedRenderer,
  bindTrustedRenderer,
  clearTrustedRendererForTests,
} from './trustedRendererIpcAuthority';

const createWindow = () => {
  const window = new EventEmitter() as any;
  const webContents = new EventEmitter() as any;
  const frame = {
    processId: 1,
    routingId: 2,
    url: 'http://127.0.0.1:8080/',
    origin: 'http://127.0.0.1:8080',
    detached: false,
    isDestroyed: () => false,
  };
  webContents.mainFrame = frame;
  webContents.isDestroyed = () => false;
  window.webContents = webContents;
  return { window, webContents, frame };
};

const activate = (webContents: any) => {
  webContents.emit(
    'did-frame-navigate',
    {},
    webContents.mainFrame.url,
    200,
    'OK',
    true,
    1,
    2
  );
  webContents.emit('did-frame-finish-load', {}, true, 1, 2);
};

describe('trustedRendererIpcAuthority', () => {
  afterEach(clearTrustedRendererForTests);

  it('rejects until the canonical main frame finishes loading', () => {
    const { window, webContents, frame } = createWindow();
    bindTrustedRenderer(window, new URL('http://127.0.0.1:8080/'));
    const event = { sender: webContents, senderFrame: frame } as any;
    expect(authorizeTrustedRenderer(event)).toBeNull();

    activate(webContents);
    expect(authorizeTrustedRenderer(event)).not.toBeNull();
  });

  it('accepts Electron file frame origins for packaged documents', () => {
    const { window, webContents, frame } = createWindow();
    frame.url = 'file:///opt/daedalus/renderer/index.html';
    frame.origin = 'file://';
    bindTrustedRenderer(
      window,
      new URL('file:///opt/daedalus/renderer/index.html') as any
    );
    activate(webContents);

    expect(
      authorizeTrustedRenderer({
        sender: webContents,
        senderFrame: frame,
      } as any)
    ).not.toBeNull();
  });

  it('does not activate a failed canonical HTTP commit', () => {
    const { window, webContents, frame } = createWindow();
    bindTrustedRenderer(window, new URL('http://127.0.0.1:8080/'));
    webContents.emit(
      'did-frame-navigate',
      {},
      frame.url,
      500,
      'Failed',
      true,
      1,
      2
    );
    webContents.emit('did-frame-finish-load', {}, true, 1, 2);

    expect(
      authorizeTrustedRenderer({
        sender: webContents,
        senderFrame: frame,
      } as any)
    ).toBeNull();
  });

  it('invalidates authority when the renderer process exits', () => {
    const { window, webContents, frame } = createWindow();
    bindTrustedRenderer(window, new URL('http://127.0.0.1:8080/'));
    activate(webContents);
    const authorization = authorizeTrustedRenderer({
      sender: webContents,
      senderFrame: frame,
    } as any);

    webContents.emit('render-process-gone');
    expect(authorization.isCurrent()).toBe(false);
  });

  it('rejects wrong senders, subframes, origins, and destroyed frames', () => {
    const { window, webContents, frame } = createWindow();
    bindTrustedRenderer(window, new URL('http://127.0.0.1:8080/'));
    activate(webContents);

    expect(
      authorizeTrustedRenderer({ sender: {}, senderFrame: frame } as any)
    ).toBeNull();
    expect(
      authorizeTrustedRenderer({ sender: webContents, senderFrame: {} } as any)
    ).toBeNull();
    frame.origin = 'https://attacker.test';
    expect(
      authorizeTrustedRenderer({
        sender: webContents,
        senderFrame: frame,
      } as any)
    ).toBeNull();
    frame.origin = 'http://127.0.0.1:8080';
    frame.isDestroyed = () => true;
    expect(
      authorizeTrustedRenderer({
        sender: webContents,
        senderFrame: frame,
      } as any)
    ).toBeNull();
  });

  it('revokes before navigation and does not let stale destruction clear replacement', () => {
    const first = createWindow();
    bindTrustedRenderer(first.window, new URL('http://127.0.0.1:8080/'));
    activate(first.webContents);
    const firstEvent = {
      sender: first.webContents,
      senderFrame: first.frame,
    } as any;
    const authorization = authorizeTrustedRenderer(firstEvent);
    expect(authorization).not.toBeNull();
    first.webContents.emit('did-start-navigation', {
      isMainFrame: true,
      isSameDocument: false,
    });
    expect(authorization.isCurrent()).toBe(false);

    const second = createWindow();
    bindTrustedRenderer(second.window, new URL('http://127.0.0.1:8080/'));
    activate(second.webContents);
    first.window.emit('closed');
    expect(
      authorizeTrustedRenderer({
        sender: second.webContents,
        senderFrame: second.frame,
      } as any)
    ).not.toBeNull();
  });
});
