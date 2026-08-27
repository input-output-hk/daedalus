import { EventEmitter } from 'events';
import { BrowserWindow } from 'electron';
import type { IpcMainInvokeEvent } from 'electron';
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
    (requireDappSandboxAvailable as jest.Mock).mockResolvedValue(undefined);
    (createDappSession as jest.Mock).mockReturnValue({ id: 'session' });
    (installDappSessionPolicy as jest.Mock).mockResolvedValue(egressPolicy);
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
      policy
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

  test('hides and restores the active guest for trusted consent', async () => {
    const { window } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example');
    window.show.mockClear();

    manager.setHidden(true);
    manager.setHidden(false);

    expect(window.hide).toHaveBeenCalledTimes(1);
    expect(window.show).toHaveBeenCalledTimes(1);
  });

  test('authenticates only the live exact guest top frame and origin', async () => {
    const { window, webContents, frame } = makeWindow();
    ((BrowserWindow as unknown) as jest.Mock).mockReturnValue(window);
    const manager = new DappBrowserManager();
    await manager.launch(entry, 'genesis', 'Example');
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
      expect(egressPolicy.close.mock.invocationCallOrder[0]).toBeLessThan(
        (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
      );
      expect(manager.isOpen).toBe(false);
    }
  );

  test('rejects before creating a guest when egress setup fails', async () => {
    (installDappSessionPolicy as jest.Mock).mockRejectedValue(
      new Error('proxy unavailable')
    );
    const manager = new DappBrowserManager();

    await expect(manager.launch(entry, 'genesis', 'Example')).rejects.toThrow(
      'DApp guest failed to load'
    );
    expect(BrowserWindow).not.toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(manager.isOpen).toBe(false);
  });
  test('clears egress state when guest construction fails', async () => {
    ((BrowserWindow as unknown) as jest.Mock).mockImplementation(() => {
      throw new Error('window unavailable');
    });
    const manager = new DappBrowserManager();

    await expect(manager.launch(entry, 'genesis', 'Example')).rejects.toThrow(
      'DApp guest failed to load'
    );
    expect(egressPolicy.close).toHaveBeenCalled();
    expect(clearDappSession).toHaveBeenCalledWith({ id: 'session' });
    expect(egressPolicy.close.mock.invocationCallOrder[0]).toBeLessThan(
      (clearDappSession as jest.Mock).mock.invocationCallOrder[0]
    );
    expect(manager.isOpen).toBe(false);
  });

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
