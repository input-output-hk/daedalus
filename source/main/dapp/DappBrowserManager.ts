import path from 'path';
import { BrowserWindow, screen } from 'electron';
import type {
  IpcMainInvokeEvent,
  Session,
  WebFrameMain,
  WebPreferences,
} from 'electron';
import type { DappEgressPolicy } from './DappEgressPolicy';
import { STORAGE_KEYS } from '../../common/config/electron-store.config';
import type { StoreMessage } from '../../common/types/electron-store.types';
import { requestElectronStore } from '../ipc/electronStoreConversation';
import {
  restoreSavedWindowBounds,
  saveWindowBoundsOnSizeAndPositionChange,
} from '../windows/windowBounds';
import { requireDappSandboxAvailable } from '../sandbox/dappSandboxAvailability';
import { logDappConsole } from '../utils/logging';
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

const DAPP_CONSOLE_MESSAGE_LIMIT = 100;
const DAPP_CONSOLE_MESSAGE_RATE_LIMIT = 10;
const DAPP_CONSOLE_MESSAGE_LENGTH_LIMIT = 2048;

const installDappConsoleCapture = (
  webContents: BrowserWindow['webContents']
): void => {
  let captured = 0;
  let capturedThisSecond = 0;
  let secondStartedAt = Date.now();
  webContents.on('console-message', (details) => {
    if (details.level !== 'warning' && details.level !== 'error') return;
    if (captured >= DAPP_CONSOLE_MESSAGE_LIMIT) return;
    const now = Date.now();
    if (now - secondStartedAt >= 1000) {
      secondStartedAt = now;
      capturedThisSecond = 0;
    }
    if (capturedThisSecond >= DAPP_CONSOLE_MESSAGE_RATE_LIMIT) return;
    captured += 1;
    capturedThisSecond += 1;
    logDappConsole(
      `dapp-console:${details.level} ${details.message.slice(
        0,
        DAPP_CONSOLE_MESSAGE_LENGTH_LIMIT
      )}`
    );
  });
};
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
  documentGeneration: number;
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
  refreshDocument: () => void,
  revoke: (reason: DappGuestRevocationReason) => void,
  diagnosticsPolicy?: DappUrlPolicy
): void => {
  const { webContents } = window;
  const launchOrigin = parseLaunchUrl(entryUrl, diagnosticsPolicy).origin;
  const denyNavigation = (event: {
    url?: string;
    isMainFrame?: boolean;
    preventDefault: () => void;
  }) => {
    try {
      const parsed = event.url && parseLaunchUrl(event.url, diagnosticsPolicy);
      if (
        event.isMainFrame !== false &&
        parsed &&
        ((isInitialLoad() && parsed.href === entryUrl) ||
          (!isInitialLoad() && parsed.origin === launchOrigin))
      )
        return;
    } catch {
      // Invalid navigation is revoked below.
    }
    event.preventDefault();
    revoke('navigation');
  };

  installGuestDenialHandlers(webContents);
  webContents.on('will-navigate', denyNavigation);
  webContents.on('will-frame-navigate', denyNavigation);
  webContents.on('will-redirect', denyNavigation);
  webContents.on(
    'did-start-navigation',
    (_event, url, isInPlace, isMainFrame) => {
      let isAllowedNavigation = false;
      try {
        const parsed = parseLaunchUrl(url, diagnosticsPolicy);
        isAllowedNavigation =
          isMainFrame &&
          ((isInitialLoad() && parsed.href === entryUrl) ||
            (!isInitialLoad() && parsed.origin === launchOrigin));
        if (isAllowedNavigation && !isInitialLoad() && !isInPlace)
          refreshDocument();
      } catch {
        // Invalid navigation is revoked below.
      }
      if (!isAllowedNavigation) revoke('navigation');
    }
  );
  webContents.on('did-navigate-in-page', (_event, url, isMainFrame) => {
    if (!isMainFrame) return;
    try {
      if (parseLaunchUrl(url, diagnosticsPolicy).origin === launchOrigin)
        return;
    } catch {
      // Invalid navigation is revoked below.
    }
    revoke('navigation');
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
  private readonly activeGuests = new Set<ActiveGuest>();
  private nextDocumentGeneration = 0;

  readonly onRevoke: (
    _reason: DappGuestRevocationReason,
    _guestWebContentsId: number,
    _isOpen: boolean
  ) => void;

  constructor(
    onRevoke: (
      _reason: DappGuestRevocationReason,
      _guestWebContentsId: number,
      _isOpen: boolean
    ) => void = () => undefined
  ) {
    this.onRevoke = onRevoke;
  }

  get isOpen(): boolean {
    return this.activeGuests.size > 0;
  }

  authenticate(event: IpcMainInvokeEvent): DappGuestAuthority | null {
    const guest = [...this.activeGuests].find(
      ({ window }) => window.webContents === event.sender
    );
    const frame = event.senderFrame;
    if (
      !guest ||
      !frame ||
      event.sender !== guest.window.webContents ||
      !this.isCurrentFrame(guest, frame, event.sender.id)
    )
      return null;
    const documentGeneration = guest.documentGeneration;
    return Object.freeze({
      guestWebContentsId: guest.window.webContents.id,
      documentGeneration,
      origin: guest.launch.canonicalOrigin,
      launch: guest.grantLaunch,
      isCurrent: () =>
        guest.documentGeneration === documentGeneration &&
        this.isCurrentFrame(guest, frame, guest.window.webContents.id),
    });
  }

  private isCurrentFrame(
    guest: ActiveGuest,
    frame: WebFrameMain,
    senderId: number
  ): boolean {
    if (
      !this.activeGuests.has(guest) ||
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
    localName: string,
    captureConsole: boolean
  ): Promise<void> {
    const launch = resolveCatalogLaunch(entry, networkGenesis, localName);
    return this.launchResolved(
      launch,
      Object.freeze({
        kind: 'catalog',
        catalogEntryId: launch.catalogId,
        catalogEntryIdentity: launch.catalogIdentity,
      }),
      undefined,
      captureConsole
    );
  }

  async launchDiagnostics(
    entryUrl: string,
    canonicalOrigin: string,
    localName: string,
    policy: DappUrlPolicy,
    captureConsole: boolean
  ): Promise<void> {
    return this.launchResolved(
      Object.freeze({
        entryUrl,
        canonicalOrigin,
        allowedResourceOrigins: new Set<string>(),
        windowTitle: localDappWindowTitle(localName),
      }),
      Object.freeze({ kind: 'diagnostics' }),
      policy,
      captureConsole
    );
  }

  private async launchResolved(
    launch: ResolvedDappLaunch,
    grantLaunch: DappGrantLaunch,
    diagnosticsPolicy: DappUrlPolicy | undefined,
    captureConsole: boolean
  ): Promise<void> {
    await requireDappSandboxAvailable();
    const windowTitle = `${
      parseLaunchUrl(launch.entryUrl, diagnosticsPolicy).origin
    } — ${launch.windowTitle}`;
    const windowStateId =
      grantLaunch.kind === 'catalog'
        ? grantLaunch.catalogEntryId
        : Buffer.from(launch.canonicalOrigin).toString('base64url');
    const storeWindowState = (request: StoreMessage) =>
      requestElectronStore({
        ...request,
        key: STORAGE_KEYS.DAPP_WINDOW_BOUNDS,
        id: windowStateId,
      });
    const savedWindowBounds = restoreSavedWindowBounds(
      screen,
      storeWindowState
    );

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
        title: windowTitle,
        frame: true,
        fullscreenable: false,
        autoHideMenuBar: true,
        ...(savedWindowBounds ?? {}),
        webPreferences: createDappGuestWebPreferences(guestSession),
      });
    } catch {
      await egressPolicy.close();
      await clearDappSession(guestSession);
      throw new Error('DApp guest failed to load');
    }
    if (captureConsole) installDappConsoleCapture(guestWindow.webContents);
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
    this.activeGuests.add(guest);
    installDappGuestLifecyclePolicy(
      guest.window,
      guest.launch.entryUrl,
      windowTitle,
      () => guest.initialLoad,
      () => guest.teardown !== undefined,
      () => this.refreshDocument(guest),
      (reason) => this.teardown(guest, reason).catch(() => undefined),
      diagnosticsPolicy
    );
    saveWindowBoundsOnSizeAndPositionChange(guestWindow, storeWindowState);

    try {
      await guestWindow.loadURL(launch.entryUrl);
      guest.initialLoad = false;
      if (!this.activeGuests.has(guest) || guestWindow.isDestroyed())
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

  revoke(reason: DappGuestRevocationReason): void {
    this.activeGuests.forEach((guest) => {
      try {
        this.onRevoke(reason, guest.window.webContents.id, true);
      } catch {
        // Authority is already revoked; the guest remains open.
      }
    });
  }

  private refreshDocument(guest: ActiveGuest): void {
    if (!this.activeGuests.has(guest) || guest.teardown) return;
    guest.documentGeneration = ++this.nextDocumentGeneration;
    try {
      this.onRevoke('navigation', guest.window.webContents.id, true);
    } catch {
      // The new same-origin document remains isolated and open.
    }
  }

  async close(reason: DappGuestRevocationReason = 'closed'): Promise<void> {
    await Promise.all(
      [...this.activeGuests].map((guest) => this.teardown(guest, reason))
    );
  }

  private teardown(
    guest: ActiveGuest,
    reason: DappGuestRevocationReason
  ): Promise<void> {
    if (guest.teardown) return guest.teardown;

    guest.teardown = (async () => {
      this.activeGuests.delete(guest);
      try {
        this.onRevoke(
          reason,
          guest.window.webContents.id,
          this.activeGuests.size > 0
        );
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
