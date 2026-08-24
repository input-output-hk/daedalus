import { EventEmitter } from 'events';
import { BrowserWindow } from 'electron';
import { requireDappSandboxAvailable } from '../sandbox/dappSandboxAvailability';
import {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installGuestDenialHandlers,
} from './DappSessionPolicy';
import { DappBrowserManager } from './DappBrowserManager';
import type { DappCatalogEntry } from './dappCatalog';

jest.mock('electron', () => ({ BrowserWindow: jest.fn() }));
jest.mock('../sandbox/dappSandboxAvailability', () => ({
  requireDappSandboxAvailable: jest.fn(),
}));
jest.mock('./DappSessionPolicy', () => ({
  clearDappSession: jest.fn(() => Promise.resolve()),
  createDappSession: jest.fn(),
  installDappSessionPolicy: jest.fn(),
  installGuestDenialHandlers: jest.fn(),
}));

const entry: DappCatalogEntry = {
  id: 'example',
  nameMessageId: 'dapp.example.name',
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

const makeWindow = (load = Promise.resolve()) => {
  const webContents = Object.assign(new EventEmitter(), {
    getURL: jest.fn(() => 'https://example.com/app'),
    stop: jest.fn(),
  });
  let destroyed = false;
  const window = Object.assign(new EventEmitter(), {
    webContents,
    loadURL: jest.fn(() => load),
    show: jest.fn(),
    setTitle: jest.fn(),
    isDestroyed: jest.fn(() => destroyed),
    destroy: jest.fn(() => {
      destroyed = true;
    }),
  });
  return { window, webContents };
};

const flush = () =>
  new Promise<void>((resolve) => {
    setTimeout(resolve, 0);
  });

describe('DappBrowserManager', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    (requireDappSandboxAvailable as jest.Mock).mockResolvedValue(undefined);
    (createDappSession as jest.Mock).mockReturnValue({ id: 'session' });
  });

  test('uses the exact hidden secure window and only shows after origin verification', async () => {
    const load = deferred();
    const { window } = makeWindow(load.promise);
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();

    const launched = manager.launch(entry, 'genesis', 'Example');
    await flush();
    expect(requireDappSandboxAvailable).toHaveBeenCalled();
    expect(BrowserWindow).toHaveBeenCalledWith({
      show: false,
      title: 'Example — Daedalus',
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
    expect(installGuestDenialHandlers).toHaveBeenCalledWith(window.webContents);
    expect(window.loadURL).toHaveBeenCalledWith('https://example.com/app');
    expect(window.show).not.toHaveBeenCalled();

    load.resolve();
    await launched;
    expect(window.show).toHaveBeenCalledTimes(1);
    expect(manager.isOpen).toBe(true);
  });

  test('keeps an origin mismatch hidden and clears the guest', async () => {
    const { window, webContents } = makeWindow();
    webContents.getURL.mockReturnValue('https://evil.test/');
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const onRevoke = jest.fn();
    const manager = new DappBrowserManager(onRevoke);

    await expect(manager.launch(entry, 'genesis', 'Example')).rejects.toThrow(
      'DApp guest failed to load'
    );
    expect(window.show).not.toHaveBeenCalled();
    expect(onRevoke).toHaveBeenCalledWith('origin-mismatch');
    expect(window.destroy).toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(manager.isOpen).toBe(false);
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
      await manager.launch(entry, 'genesis', 'Example');

      trigger(window);
      await flush();

      expect(onRevoke).toHaveBeenCalledWith(reason);
      expect(onRevoke.mock.invocationCallOrder[0]).toBeLessThan(
        window.destroy.mock.invocationCallOrder[0]
      );
      expect(window.destroy.mock.invocationCallOrder[0]).toBeLessThan(
        (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
      );
      expect(manager.isOpen).toBe(false);
    }
  );

  test('suppresses page titles in favor of the local catalog title', async () => {
    const { window, webContents } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example');
    const event = { preventDefault: jest.fn() };

    webContents.emit('page-title-updated', event, 'Hostile title', true);

    expect(event.preventDefault).toHaveBeenCalled();
    expect(window.setTitle).toHaveBeenCalledWith('Example — Daedalus');
  });
});
