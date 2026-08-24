import path from 'path';
import { BrowserWindow } from 'electron';
import type { Session } from 'electron';
import type { DappEgressPolicy } from './DappEgressPolicy';
import { requireDappSandboxAvailable } from '../sandbox/dappSandboxAvailability';
import {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installGuestDenialHandlers,
} from './DappSessionPolicy';
import { resolveCatalogLaunch } from './dappCatalog';
import type { DappCatalogEntry, ResolvedCatalogLaunch } from './dappCatalog';
import { parseDappUrl } from './urlPolicy';

export type DappGuestRevocationReason =
  | 'closed'
  | 'replaced'
  | 'navigation'
  | 'load-failed'
  | 'crashed'
  | 'unresponsive'
  | 'preload-failed'
  | 'origin-mismatch'
  | 'route-changed';

type ActiveGuest = {
  readonly window: BrowserWindow;
  readonly session: Session;
  readonly launch: ResolvedCatalogLaunch;
  readonly egressPolicy: DappEgressPolicy;
  initialLoad: boolean;
  teardown?: Promise<void>;
};

export class DappBrowserManager {
  private activeGuest?: ActiveGuest;

  readonly onRevoke: (_reason: DappGuestRevocationReason) => void;

  constructor(
    onRevoke: (_reason: DappGuestRevocationReason) => void = () => undefined
  ) {
    this.onRevoke = onRevoke;
  }

  get isOpen(): boolean {
    return this.activeGuest !== undefined;
  }

  async launch(
    entry: DappCatalogEntry,
    networkGenesis: string,
    localName: string
  ): Promise<void> {
    await requireDappSandboxAvailable();
    const launch = resolveCatalogLaunch(entry, networkGenesis, localName);
    await this.close('replaced');

    const guestSession = createDappSession();
    let egressPolicy: DappEgressPolicy;
    try {
      egressPolicy = await installDappSessionPolicy(
        guestSession,
        launch.allowedResourceOrigins
      );
    } catch {
      await clearDappSession(guestSession);
      throw new Error('DApp guest failed to load');
    }

    let guestWindow: BrowserWindow;
    try {
      guestWindow = new BrowserWindow({
        show: false,
        title: launch.windowTitle,
        frame: true,
        fullscreenable: false,
        autoHideMenuBar: true,
        webPreferences: {
          session: guestSession,
          preload: path.join(__dirname, 'dapp.js'),
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
    } catch {
      await egressPolicy.close();
      await clearDappSession(guestSession);
      throw new Error('DApp guest failed to load');
    }
    const guest: ActiveGuest = {
      window: guestWindow,
      session: guestSession,
      launch,
      egressPolicy,
      initialLoad: true,
    };
    this.activeGuest = guest;
    this.installLifecyclePolicy(guest);

    try {
      await guestWindow.loadURL(launch.entryUrl);
      guest.initialLoad = false;
      if (this.activeGuest !== guest || guestWindow.isDestroyed())
        throw new Error('DApp guest closed during load');
      if (
        parseDappUrl(guestWindow.webContents.getURL()).origin !==
        launch.canonicalOrigin
      ) {
        await this.teardown(guest, 'origin-mismatch');
        throw new Error('DApp origin verification failed');
      }
      guestWindow.show();
    } catch {
      await this.teardown(guest, 'load-failed');
      throw new Error('DApp guest failed to load');
    }
  }

  async close(reason: DappGuestRevocationReason = 'closed'): Promise<void> {
    if (this.activeGuest) await this.teardown(this.activeGuest, reason);
  }

  private installLifecyclePolicy(guest: ActiveGuest): void {
    const { window, launch } = guest;
    const { webContents } = window;
    const revoke = (reason: DappGuestRevocationReason) => {
      this.teardown(guest, reason).catch(() => undefined);
    };
    const denyNavigation = (event: Electron.Event) => {
      event.preventDefault();
      revoke('navigation');
    };

    installGuestDenialHandlers(webContents);
    webContents.on('will-navigate', denyNavigation);
    webContents.on('will-frame-navigate', denyNavigation);
    webContents.on('will-redirect', denyNavigation);
    webContents.on(
      'did-start-navigation',
      (_event, url, _isInPlace, isMainFrame) => {
        let isExpectedInitialLoad = false;
        try {
          isExpectedInitialLoad =
            guest.initialLoad &&
            isMainFrame &&
            parseDappUrl(url).href === launch.entryUrl;
        } catch {
          // Invalid navigation is revoked below.
        }
        if (!isExpectedInitialLoad) revoke('navigation');
      }
    );
    webContents.on('did-navigate-in-page', (_event, _url, isMainFrame) => {
      if (isMainFrame) revoke('navigation');
    });
    webContents.on(
      'did-fail-load',
      (_event, _code, _description, _url, isMainFrame) => {
        if (isMainFrame) revoke('load-failed');
      }
    );
    webContents.on('render-process-gone', () => revoke('crashed'));
    webContents.on('unresponsive', () => revoke('unresponsive'));
    webContents.on('preload-error', () => revoke('preload-failed'));
    webContents.on('page-title-updated', (event) => {
      event.preventDefault();
      if (!window.isDestroyed()) window.setTitle(launch.windowTitle);
    });
    window.on('close', (event) => {
      if (!guest.teardown) {
        event.preventDefault();
        revoke('closed');
      }
    });
    window.on('closed', () => revoke('closed'));
  }

  private teardown(
    guest: ActiveGuest,
    reason: DappGuestRevocationReason
  ): Promise<void> {
    if (guest.teardown) return guest.teardown;

    guest.teardown = (async () => {
      if (this.activeGuest === guest) this.activeGuest = undefined;
      try {
        this.onRevoke(reason);
      } catch {
        // Revocation state is already inactive; cleanup must still complete.
      }
      if (!guest.window.isDestroyed()) {
        guest.window.webContents.stop();
        guest.window.destroy();
      }
      await guest.egressPolicy.close();
      await clearDappSession(guest.session);
    })();
    return guest.teardown;
  }
}
