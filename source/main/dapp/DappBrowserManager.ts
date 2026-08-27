import path from 'path';
import { BrowserWindow } from 'electron';
import type {
  IpcMainInvokeEvent,
  Session,
  WebFrameMain,
  WebPreferences,
} from 'electron';
import type { DappEgressPolicy } from './DappEgressPolicy';
import { requireDappSandboxAvailable } from '../sandbox/dappSandboxAvailability';
import {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installGuestDenialHandlers,
} from './DappSessionPolicy';
import { localDappWindowTitle, resolveCatalogLaunch } from './dappCatalog';
import type { DappCatalogEntry, ResolvedCatalogLaunch } from './dappCatalog';
import { parseDappUrl, parseDiagnosticsDappUrl } from './urlPolicy';
import type { DappUrlPolicy, ParsedDappUrl } from './urlPolicy';
import type { DappGrantLaunch } from '../../common/types/dapp.types';

type ResolvedDappLaunch = Readonly<
  Pick<
    ResolvedCatalogLaunch,
    'entryUrl' | 'canonicalOrigin' | 'allowedResourceOrigins' | 'windowTitle'
  >
>;

const parseLaunchUrl = (
  value: string,
  diagnosticsPolicy?: DappUrlPolicy
): ParsedDappUrl =>
  diagnosticsPolicy
    ? parseDiagnosticsDappUrl(value, diagnosticsPolicy)
    : parseDappUrl(value);

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

export type DappGuestAuthority = Readonly<{
  guestWebContentsId: number;
  documentGeneration: number;
  origin: string;
  launch: DappGrantLaunch;
  isCurrent: () => boolean;
}>;

type ActiveGuest = {
  readonly window: BrowserWindow;
  readonly session: Session;
  readonly launch: ResolvedDappLaunch;
  readonly egressPolicy: DappEgressPolicy;
  initialLoad: boolean;
  readonly documentGeneration: number;
  readonly grantLaunch: DappGrantLaunch;
  teardown?: Promise<void>;
  readonly diagnosticsPolicy?: DappUrlPolicy;
};
export const createDappGuestWebPreferences = (
  guestSession: Session,
  preload = path.join(__dirname, 'dapp.js')
): WebPreferences => ({
  session: guestSession,
  preload,
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
});
export const installDappGuestLifecyclePolicy = (
  window: BrowserWindow,
  entryUrl: string,
  windowTitle: string,
  isInitialLoad: () => boolean,
  isTearingDown: () => boolean,
  revoke: (reason: DappGuestRevocationReason) => void,
  diagnosticsPolicy?: DappUrlPolicy
): void => {
  const { webContents } = window;
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
          isInitialLoad() &&
          isMainFrame &&
          parseLaunchUrl(url, diagnosticsPolicy).href === entryUrl;
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
    if (!window.isDestroyed()) window.setTitle(windowTitle);
  });
  window.on('close', (event) => {
    if (!isTearingDown()) {
      event.preventDefault();
      revoke('closed');
    }
  });
  window.on('closed', () => revoke('closed'));
};

export class DappBrowserManager {
  private activeGuest?: ActiveGuest;
  private nextDocumentGeneration = 0;

  readonly onRevoke: (_reason: DappGuestRevocationReason) => void;

  constructor(
    onRevoke: (_reason: DappGuestRevocationReason) => void = () => undefined
  ) {
    this.onRevoke = onRevoke;
  }

  get isOpen(): boolean {
    return this.activeGuest !== undefined;
  }

  setHidden(hidden: boolean): void {
    const guestWindow = this.activeGuest?.window;
    if (!guestWindow || guestWindow.isDestroyed()) return;
    if (hidden) guestWindow.hide();
    else guestWindow.show();
  }

  authenticate(event: IpcMainInvokeEvent): DappGuestAuthority | null {
    const guest = this.activeGuest;
    const frame = event.senderFrame;
    if (
      !guest ||
      !frame ||
      event.sender !== guest.window.webContents ||
      !this.isCurrentFrame(guest, frame, event.sender.id)
    )
      return null;
    return Object.freeze({
      guestWebContentsId: guest.window.webContents.id,
      documentGeneration: guest.documentGeneration,
      origin: guest.launch.canonicalOrigin,
      launch: guest.grantLaunch,
      isCurrent: () =>
        this.isCurrentFrame(guest, frame, guest.window.webContents.id),
    });
  }

  private isCurrentFrame(
    guest: ActiveGuest,
    frame: WebFrameMain,
    senderId: number
  ): boolean {
    if (
      this.activeGuest !== guest ||
      guest.teardown !== undefined ||
      guest.window.isDestroyed() ||
      guest.window.webContents.isDestroyed() ||
      guest.window.webContents.id !== senderId ||
      frame !== guest.window.webContents.mainFrame ||
      frame.detached ||
      frame.isDestroyed() ||
      frame.origin !== guest.launch.canonicalOrigin
    )
      return false;
    try {
      return (
        parseLaunchUrl(frame.url, guest.diagnosticsPolicy).origin ===
        guest.launch.canonicalOrigin
      );
    } catch {
      return false;
    }
  }

  async launch(
    entry: DappCatalogEntry,
    networkGenesis: string,
    localName: string
  ): Promise<void> {
    const launch = resolveCatalogLaunch(entry, networkGenesis, localName);
    return this.launchResolved(
      launch,
      Object.freeze({
        kind: 'catalog',
        catalogEntryId: launch.catalogId,
        catalogEntryIdentity: launch.catalogIdentity,
      })
    );
  }

  async launchDiagnostics(
    entryUrl: string,
    canonicalOrigin: string,
    localName: string,
    policy: DappUrlPolicy
  ): Promise<void> {
    return this.launchResolved(
      Object.freeze({
        entryUrl,
        canonicalOrigin,
        allowedResourceOrigins: new Set<string>(),
        windowTitle: localDappWindowTitle(localName),
      }),
      Object.freeze({ kind: 'diagnostics' }),
      policy
    );
  }

  private async launchResolved(
    launch: ResolvedDappLaunch,
    grantLaunch: DappGrantLaunch,
    diagnosticsPolicy?: DappUrlPolicy
  ): Promise<void> {
    await requireDappSandboxAvailable();
    await this.close('replaced');

    const guestSession = createDappSession();
    let egressPolicy: DappEgressPolicy;
    try {
      egressPolicy = diagnosticsPolicy
        ? await installDappSessionPolicy(
            guestSession,
            undefined,
            diagnosticsPolicy
          )
        : await installDappSessionPolicy(
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
        webPreferences: createDappGuestWebPreferences(guestSession),
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
      diagnosticsPolicy,
      documentGeneration: ++this.nextDocumentGeneration,
      grantLaunch,
    };
    this.activeGuest = guest;
    installDappGuestLifecyclePolicy(
      guest.window,
      guest.launch.entryUrl,
      guest.launch.windowTitle,
      () => guest.initialLoad,
      () => guest.teardown !== undefined,
      (reason) => this.teardown(guest, reason).catch(() => undefined),
      diagnosticsPolicy
    );

    try {
      await guestWindow.loadURL(launch.entryUrl);
      guest.initialLoad = false;
      if (this.activeGuest !== guest || guestWindow.isDestroyed())
        throw new Error('DApp guest closed during load');
      if (
        parseLaunchUrl(guestWindow.webContents.getURL(), diagnosticsPolicy)
          .origin !== launch.canonicalOrigin
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
