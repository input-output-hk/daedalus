import { EventEmitter } from 'events';
import { BrowserWindow, screen } from 'electron';
import type { IpcMainInvokeEvent } from 'electron';
import { requireDappSandboxAvailable } from '../sandbox/dappSandboxAvailability';
import { logDappConsole } from '../utils/logging';
import {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installGuestDenialHandlers,
} from './DappSessionPolicy';
import { DappBrowserManager } from './DappBrowserManager';
import type { DappCatalogEntry } from './dappCatalog';
import {
  restoreSavedWindowBounds,
  saveWindowBoundsOnSizeAndPositionChange,
} from '../windows/windowBounds';
import { requestElectronStore } from '../ipc/electronStoreConversation';

jest.mock('electron', () => ({ BrowserWindow: jest.fn(), screen: {} }));
jest.mock('../sandbox/dappSandboxAvailability', () => ({
  requireDappSandboxAvailable: jest.fn(),
}));
jest.mock('../utils/logging', () => ({
  logDappConsole: jest.fn(),
}));
jest.mock('../ipc/electronStoreConversation', () => ({
  requestElectronStore: jest.fn(),
}));
jest.mock('../windows/windowBounds', () => ({
  restoreSavedWindowBounds: jest.fn(),
  saveWindowBoundsOnSizeAndPositionChange: jest.fn(),
}));
jest.mock('./DappSessionPolicy', () => ({
  clearDappSession: jest.fn(() => Promise.resolve()),
  createDappSession: jest.fn(),
  installDappSessionPolicy: jest.fn(),
  installGuestDenialHandlers: jest.fn(),
}));

const entry: DappCatalogEntry = {
  id: 'example',
  availableIn: ['preprod'],
  nameMessageId: 'dapp.example.name',
  descriptionMessageId: 'dapp.example.description',
  iconAsset: 'example.svg',
  entryUrlByNetworkGenesis: { genesis: 'https://example.com/app' },
  canonicalOrigin: 'https://example.com',
  allowedResourceOrigins: ['https://cdn.example.com'],
  supportedWalletKinds: ['shelley'],
  supportedExtensions: [],
};

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((resolvePromise) => {
    resolve = resolvePromise;
  });
  return { promise, resolve };
};
const egressPolicy = { close: jest.fn(() => Promise.resolve()) };

const makeWindow = (load = Promise.resolve()) => {
  const frame = {
    url: 'https://example.com/app',
    origin: 'https://example.com',
    detached: false,
    isDestroyed: jest.fn(() => false),
  };
  const webContents = Object.assign(new EventEmitter(), {
    id: 17,
    mainFrame: frame,
    getURL: jest.fn(() => 'https://example.com/app'),
    isDestroyed: jest.fn(() => false),
    stop: jest.fn(),
  });
  let destroyed = false;
  const window = Object.assign(new EventEmitter(), {
    webContents,
    loadURL: jest.fn(() => load),
    show: jest.fn(),
    hide: jest.fn(),
    setTitle: jest.fn(),
    isDestroyed: jest.fn(() => destroyed),
    destroy: jest.fn(() => {
      destroyed = true;
    }),
  });
  return { window, webContents, frame };
};

const flush = () =>
  new Promise<void>((resolve) => {
    setTimeout(resolve, 0);
  });

describe('DappBrowserManager', () => {
  beforeEach(() => {
    egressPolicy.close.mockClear();
    jest.clearAllMocks();
    (restoreSavedWindowBounds as jest.Mock).mockReturnValue(undefined);
    (createDappSession as jest.Mock).mockReturnValue({ id: 'session' });
    (installDappSessionPolicy as jest.Mock).mockResolvedValue(egressPolicy);
  });

  test('uses the exact hidden secure window and only shows after origin verification', async () => {
    const load = deferred();
    const { window } = makeWindow(load.promise);
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();

    const launched = manager.launch(entry, 'genesis', 'Example', false);
    await flush();
    expect(requireDappSandboxAvailable).toHaveBeenCalled();
    expect(BrowserWindow).toHaveBeenCalledWith({
      show: false,
      title: expect.stringMatching(/^https:\/\/example\.com — /u),
      frame: true,
      fullscreenable: false,
      autoHideMenuBar: true,
      webPreferences: {
        session: { id: 'session' },
        preload: expect.stringMatching(/dapp\.js$/u),
        nodeIntegration: false,
        nodeIntegrationInWorker: false,
        nodeIntegrationInSubFrames: false,
        contextIsolation: true,
        sandbox: true,
        webSecurity: true,
        allowRunningInsecureContent: false,
        webviewTag: false,
        devTools: false,
        plugins: false,
        spellcheck: false,
        enableWebSQL: false,
        navigateOnDragDrop: false,
        disableDialogs: true,
        autoplayPolicy: 'document-user-activation-required',
        disableBlinkFeatures: 'DirectSockets,WebTransport',
      },
    });
    expect(installDappSessionPolicy).toHaveBeenCalledWith(
      { id: 'session' },
      new Set(['https://cdn.example.com', 'https://example.com'])
    );
    expect(
      (installDappSessionPolicy as jest.Mock).mock.invocationCallOrder[0]
    ).toBeLessThan(
      ((BrowserWindow as unknown) as jest.Mock).mock.invocationCallOrder[0]
    );
    expect(installGuestDenialHandlers).toHaveBeenCalledWith(window.webContents);
    expect(window.loadURL).toHaveBeenCalledWith('https://example.com/app');
    expect(window.show).not.toHaveBeenCalled();

    load.resolve();
    await launched;
    expect(window.show).toHaveBeenCalledTimes(1);
    expect(manager.isOpen).toBe(true);
  });

  test('uses the same isolated lifecycle and diagnostics grant identity', async () => {
    const { window, webContents, frame } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    const policy = { allowHttpLoopback: false };

    await manager.launchDiagnostics(
      'https://example.com/app',
      'https://example.com',
      'Untrusted dApp',
      policy,
      false
    );

    expect(installDappSessionPolicy).toHaveBeenCalledWith(
      { id: 'session' },
      undefined,
      policy
    );
    expect(
      manager.authenticate(({
        sender: webContents,
        senderFrame: frame,
      } as unknown) as IpcMainInvokeEvent)
    ).toMatchObject({
      origin: 'https://example.com',
      launch: { kind: 'diagnostics' },
    });
  });
  test('captures raw dApp warnings and errors only when enabled', async () => {
    const { window, webContents } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example', true);

    webContents.emit('console-message', {
      level: 'error',
      message: 'failed at https://secret.example/path?token=private',
    });
    for (let index = 0; index < 10; index += 1)
      webContents.emit('console-message', {
        level: 'warning',
        message: `warning ${index}`,
      });

    expect(logDappConsole).toHaveBeenCalledTimes(10);
    expect(logDappConsole).toHaveBeenNthCalledWith(
      1,
      'dapp-console:error failed at https://secret.example/path?token=private'
    );
  });

  test('authenticates only the live exact guest top frame and origin', async () => {
    const { window, webContents, frame } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example', false);
    const event = ({
      sender: webContents,
      senderFrame: frame,
    } as unknown) as IpcMainInvokeEvent;

    const authority = manager.authenticate(event);
    expect(authority).toMatchObject({
      guestWebContentsId: 17,
      documentGeneration: 1,
      origin: 'https://example.com',
      launch: {
        kind: 'catalog',
        catalogEntryId: 'example',
      },
    });
    expect(authority?.isCurrent()).toBe(true);
    expect(
      manager.authenticate(({
        sender: webContents,
        senderFrame: { ...frame },
      } as unknown) as IpcMainInvokeEvent)
    ).toBeNull();
    expect(
      manager.authenticate(({
        sender: { id: 18 },
        senderFrame: frame,
      } as unknown) as IpcMainInvokeEvent)
    ).toBeNull();

    frame.origin = 'https://evil.test';
    expect(authority?.isCurrent()).toBe(false);
  });

  test('keeps an origin mismatch hidden and clears the guest', async () => {
    const { window, webContents } = makeWindow();
    webContents.getURL.mockReturnValue('https://evil.test/');
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);

    await expect(
      manager.launch(entry, 'genesis', 'Example', false)
    ).rejects.toThrow('DApp guest failed to load');
    expect(window.show).not.toHaveBeenCalled();
    expect(onRevoke).toHaveBeenCalledWith('origin-mismatch', 17, false);
    expect(window.destroy).toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(manager.isOpen).toBe(false);
  });

  test('keeps exact initial and same-document canonical-origin navigation open', async () => {
    const load = deferred();
    const { window, webContents } = makeWindow(load.promise);
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);
    const launched = manager.launch(entry, 'genesis', 'Example', false);
    await flush();

    const preventDefault = jest.fn();
    webContents.emit('will-navigate', {
      url: 'https://example.com/app',
      preventDefault,
    });
    expect(preventDefault).not.toHaveBeenCalled();
    expect(manager.isOpen).toBe(true);

    load.resolve();
    await launched;
    webContents.emit(
      'did-start-navigation',
      {},
      'https://example.com/markets',
      true,
      true
    );
    webContents.emit(
      'did-navigate-in-page',
      {},
      'https://example.com/markets',
      true
    );
    await flush();
    expect(onRevoke).not.toHaveBeenCalled();
    expect(manager.isOpen).toBe(true);

    webContents.emit(
      'did-navigate-in-page',
      {},
      'https://evil.test/markets',
      true
    );
    await flush();
    expect(onRevoke).toHaveBeenCalledWith('navigation', 17, false);
    expect(manager.isOpen).toBe(false);
  });
  test('keeps same-origin full navigation open and rotates document authority', async () => {
    const { window, webContents, frame } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);
    await manager.launch(entry, 'genesis', 'Example', false);
    const event = ({
      sender: webContents,
      senderFrame: frame,
    } as unknown) as IpcMainInvokeEvent;
    const previous = manager.authenticate(event);
    const url = 'https://example.com/swaps/complete';
    const preventDefault = jest.fn();

    webContents.emit('will-navigate', {
      url,
      isMainFrame: true,
      preventDefault,
    });
    frame.url = url;
    webContents.emit('did-start-navigation', {}, url, false, true);

    const current = manager.authenticate(event);
    expect(preventDefault).not.toHaveBeenCalled();
    expect(onRevoke).toHaveBeenCalledWith('navigation', 17, true);
    expect(previous?.isCurrent()).toBe(false);
    expect(current?.documentGeneration).toBeGreaterThan(
      previous?.documentGeneration || 0
    );
    expect(window.destroy).not.toHaveBeenCalled();
    expect(manager.isOpen).toBe(true);
  });

  test('revokes route authority without closing the guest window', async () => {
    const { window } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);
    await manager.launch(entry, 'genesis', 'Example', false);

    manager.revoke('route-changed');

    expect(onRevoke).toHaveBeenCalledWith('route-changed', 17, true);
    expect(window.destroy).not.toHaveBeenCalled();
    expect(manager.isOpen).toBe(true);
  });

  test.each([
    [
      'close',
      'closed',
      (window) => window.emit('close', { preventDefault: jest.fn() }),
    ],
    [
      'navigation',
      'navigation',
      (window) =>
        window.webContents.emit('will-navigate', { preventDefault: jest.fn() }),
    ],
    [
      'crash',
      'crashed',
      (window) => window.webContents.emit('render-process-gone', {}, {}),
    ],
    [
      'preload failure',
      'preload-failed',
      (window) =>
        window.webContents.emit('preload-error', {}, 'dapp.js', new Error()),
    ],
  ])(
    'revokes before destroy and cleanup on %s',
    async (_name, reason, trigger) => {
      const { window } = makeWindow();
      ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
      const onRevoke = jest.fn();
      const manager = new DappBrowserManager(onRevoke);
      await manager.launch(entry, 'genesis', 'Example', false);

      trigger(window);
      await flush();

      expect(onRevoke).toHaveBeenCalledWith(reason, 17, false);
      expect(onRevoke.mock.invocationCallOrder[0]).toBeLessThan(
        window.destroy.mock.invocationCallOrder[0]
      );
      expect(window.destroy.mock.invocationCallOrder[0]).toBeLessThan(
        (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
      );
      expect(egressPolicy.close.mock.invocationCallOrder[0]).toBeLessThan(
        (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
      );
      expect(manager.isOpen).toBe(false);
    }
  );

  test('keeps sibling guests open when one closes', async () => {
    const first = makeWindow();
    const second = makeWindow();
    second.webContents.id = 18;
    ((BrowserWindow as unknown) as jest.Mock)
      .mockReturnValueOnce(first.window)
      .mockReturnValueOnce(second.window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);

    await manager.launch(entry, 'genesis', 'Example', false);
    await manager.launch(entry, 'genesis', 'Example', false);
    first.window.emit('close', { preventDefault: jest.fn() });
    await flush();

    expect(first.window.destroy).toHaveBeenCalled();
    expect(second.window.destroy).not.toHaveBeenCalled();
    expect(onRevoke).toHaveBeenCalledWith('closed', 17, true);
    expect(manager.isOpen).toBe(true);
  });

  test('restores and saves bounds separately for each catalog entry', async () => {
    const { window } = makeWindow();
    const bounds = { x: 10, y: 20, width: 900, height: 700 };
    (restoreSavedWindowBounds as jest.Mock).mockReturnValue(bounds);
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();

    await manager.launch(entry, 'genesis', 'Example', false);

    const storeWindowState = (restoreSavedWindowBounds as jest.Mock).mock
      .calls[0][1];
    expect(restoreSavedWindowBounds).toHaveBeenCalledWith(
      screen,
      storeWindowState
    );
    expect(BrowserWindow).toHaveBeenCalledWith(expect.objectContaining(bounds));
    expect(saveWindowBoundsOnSizeAndPositionChange).toHaveBeenCalledWith(
      window,
      storeWindowState
    );
    storeWindowState({ type: 'get', key: 'WINDOW-BOUNDS' });
    expect(requestElectronStore).toHaveBeenCalledWith(
      expect.objectContaining({
        type: 'get',
        key: 'DAPP-WINDOW-BOUNDS',
        id: entry.id,
      })
    );
  });

  test('rejects before creating a guest when egress setup fails', async () => {
    (installDappSessionPolicy as jest.Mock).mockRejectedValue(
      new Error('proxy unavailable')
    );
    const manager = new DappBrowserManager();

    await expect(
      manager.launch(entry, 'genesis', 'Example', false)
    ).rejects.toThrow('DApp guest failed to load');
    expect(BrowserWindow).not.toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(manager.isOpen).toBe(false);
  });
  test('clears egress state when guest construction fails', async () => {
    ((BrowserWindow as unknown) as jest.Mock).mockImplementation(() => {
      throw new Error('window unavailable');
    });
    const manager = new DappBrowserManager();

    await expect(
      manager.launch(entry, 'genesis', 'Example', false)
    ).rejects.toThrow('DApp guest failed to load');
    expect(egressPolicy.close).toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(egressPolicy.close.mock.invocationCallOrder[0]).toBeLessThan(
      (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
    );
    expect(manager.isOpen).toBe(false);
  });

  test('keeps the validated origin visible when the page attempts to spoof the title', async () => {
    const { window, webContents } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example', false);
    const event = { preventDefault: jest.fn() };

    webContents.emit(
      'page-title-updated',
      event,
      'https://attacker.test',
      true
    );

    expect(event.preventDefault).toHaveBeenCalled();
    const restoredTitle = window.setTitle.mock.calls[0][0];
    expect(restoredTitle.startsWith('https://example.com — ')).toBe(true);
    expect(restoredTitle).not.toContain('attacker.test');
  });
});
