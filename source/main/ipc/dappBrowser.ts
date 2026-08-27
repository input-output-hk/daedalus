import type { BrowserWindow, IpcMainInvokeEvent } from 'electron';
import {
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_OPEN_CHANNEL,
  DAPP_BROWSER_STATE_CHANNEL,
  DAPP_BROWSER_STATUS_CHANNEL,
} from '../../common/ipc/api';
import type {
  DappBrowserCatalogOpenRendererRequest,
  DappBrowserDiagnosticsOpenRendererRequest,
  DappBrowserCloseMainResponse,
  DappBrowserCloseRendererRequest,
  DappBrowserOpenMainResponse,
  DappBrowserOpenRendererRequest,
  DappBrowserStateMainRequest,
  DappBrowserStateRendererResponse,
  DappBrowserStatusMainResponse,
  DappBrowserStatusRendererRequest,
} from '../../common/ipc/api';
import {
  dappCatalog,
  findDappCatalogEntry,
} from '../../common/config/dappCatalog';
import type { DappCatalogEntry } from '../../common/types/dapp.types';
import { dappLaunchPolicy, launcherConfig } from '../config';
import { environment } from '../environment';
import { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappGuestRevocationReason } from '../dapp/DappBrowserManager';
import { DappRouteLeaseService } from '../dapp/DappRouteLease';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import { parseDiagnosticsDappUrl } from '../dapp/urlPolicy';
import type { ParsedDappUrl } from '../dapp/urlPolicy';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  consumeIpcResponse,
  currentWindowSender,
} from './lib/currentWindowSender';

type PendingDiagnosticsLaunch = Readonly<{
  url: ParsedDappUrl;
  walletId: string;
  localName: string;
}>;

const isCatalogOpenRequest = (
  value: DappBrowserOpenRendererRequest
): value is DappBrowserCatalogOpenRendererRequest => {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Record<string, unknown>;
  return (
    Object.keys(candidate).sort().join('\0') === 'catalogId\0localName' &&
    typeof candidate.catalogId === 'string' &&
    typeof candidate.localName === 'string'
  );
};

const isDiagnosticsOpenRequest = (
  value: DappBrowserOpenRendererRequest
): value is DappBrowserDiagnosticsOpenRendererRequest => {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Record<string, unknown>;
  return (
    Object.keys(candidate).sort().join('\0') === 'localName\0url\0walletId' &&
    typeof candidate.url === 'string' &&
    typeof candidate.walletId === 'string' &&
    typeof candidate.localName === 'string'
  );
};

export class DappBrowserController {
  readonly routeLease: DappRouteLeaseService;
  private pendingDiagnosticsLaunch?: PendingDiagnosticsLaunch;
  private readonly manager: DappBrowserManager;
  private readonly policy: typeof dappLaunchPolicy;
  private readonly catalog: readonly DappCatalogEntry[];
  private readonly onState: (isOpen: boolean) => void;

  constructor(
    manager: DappBrowserManager,
    networkGenesis: string,
    policy = dappLaunchPolicy,
    catalog: readonly DappCatalogEntry[] = dappCatalog,
    onState: (isOpen: boolean) => void = () => undefined
  ) {
    this.manager = manager;
    this.policy = policy;
    this.catalog = catalog;
    this.onState = onState;
    this.routeLease = new DappRouteLeaseService(networkGenesis, () => {
      this.manager.close('route-changed').catch(() => undefined);
    });
  }

  observeWindow(window: BrowserWindow): void {
    const observe = (url: string) => {
      const lease = this.routeLease.observeTrustedRoute(url);
      if (this.pendingDiagnosticsLaunch)
        this.consumeDiagnosticsLaunch(lease).catch(() => undefined);
    };
    window.webContents.on(
      'did-navigate-in-page',
      (_event, url, isMainFrame) => {
        if (isMainFrame) observe(url);
      }
    );
    window.webContents.on('did-frame-finish-load', (_event, isMainFrame) => {
      if (isMainFrame) observe(window.webContents.getURL());
    });
    window.webContents.once('destroyed', () => {
      this.pendingDiagnosticsLaunch = undefined;
      this.routeLease.revoke();
    });
  }

  get status(): DappBrowserStatusMainResponse {
    return Object.freeze({
      isOpen: this.manager.isOpen,
      catalogAvailable: this.policy.allows('preferred'),
      diagnosticsAvailable: this.policy.allows('diagnostics'),
    });
  }

  async open(request: DappBrowserOpenRendererRequest): Promise<void> {
    if (isCatalogOpenRequest(request)) {
      if (!this.policy.allows('preferred'))
        throw new Error('DApp launch is disabled');
      const lease = this.routeLease.current;
      if (!lease) throw new Error('DApp route lease is stale');
      const entry = findDappCatalogEntry(this.catalog, request.catalogId);
      this.routeLease.requireCurrent(lease);
      if (!this.policy.allows('preferred'))
        throw new Error('DApp launch is disabled');
      await this.manager.launch(entry, lease.networkGenesis, request.localName);
      if (!this.routeLease.isCurrent(lease)) {
        await this.manager.close('route-changed');
        throw new Error('DApp route lease is stale');
      }
      this.onState(true);
      return;
    }
    if (!isDiagnosticsOpenRequest(request))
      throw new Error('Invalid dApp browser request');
    if (!this.policy.allows('diagnostics'))
      throw new Error('DApp launch is disabled');
    if (request.walletId === '')
      throw new Error('Invalid dApp browser request');
    const url = parseDiagnosticsDappUrl(request.url, {
      allowHttpLoopback: environment.isDev,
    });
    this.pendingDiagnosticsLaunch = Object.freeze({
      url,
      walletId: request.walletId,
      localName: request.localName,
    });
    const lease = this.routeLease.current;
    if (lease?.walletId === request.walletId)
      await this.consumeDiagnosticsLaunch(lease);
  }

  private async consumeDiagnosticsLaunch(
    lease: DappRouteLease | null
  ): Promise<void> {
    const launch = this.pendingDiagnosticsLaunch;
    this.pendingDiagnosticsLaunch = undefined;
    if (
      !launch ||
      !lease ||
      launch.walletId !== lease.walletId ||
      !this.policy.allows('diagnostics')
    )
      return;

    await this.manager.launchDiagnostics(
      launch.url.href,
      launch.url.origin,
      launch.localName,
      { allowHttpLoopback: environment.isDev }
    );
    if (!this.routeLease.isCurrent(lease)) {
      await this.manager.close('route-changed');
      throw new Error('DApp route lease is stale');
    }
    this.onState(true);
  }

  setConsentPending(pending: boolean): void {
    this.manager.setHidden(pending);
  }

  async close(): Promise<void> {
    this.pendingDiagnosticsLaunch = undefined;
    const wasOpen = this.manager.isOpen;
    await this.manager.close();
    if (!wasOpen) this.onState(false);
  }

  authenticate(event: IpcMainInvokeEvent) {
    return this.manager.authenticate(event);
  }
}

let onDappConsentLifecycleRevoked = (
  _reason: DappGuestRevocationReason
): void => undefined;
let onDappBrokerLifecycleRevoked = (): void => undefined;
let publishDappBrowserState = (_isOpen: boolean): void => undefined;

export const setDappConsentLifecycleRevoker = (
  revoke: (reason: DappGuestRevocationReason) => void
): void => {
  onDappConsentLifecycleRevoked = revoke;
};

export const setDappBrokerLifecycleRevoker = (revoke: () => void): void => {
  onDappBrokerLifecycleRevoked = revoke;
};

const browserController = new DappBrowserController(
  new DappBrowserManager((reason) => {
    onDappConsentLifecycleRevoked(reason);
    onDappBrokerLifecycleRevoked();
    publishDappBrowserState(false);
  }),
  launcherConfig.nodeConfig.network.genesisHash,
  dappLaunchPolicy,
  dappCatalog,
  (isOpen) => publishDappBrowserState(isOpen)
);
const openChannel = new MainIpcChannel<
  DappBrowserOpenRendererRequest,
  DappBrowserOpenMainResponse
>(DAPP_BROWSER_OPEN_CHANNEL);
const closeChannel = new MainIpcChannel<
  DappBrowserCloseRendererRequest,
  DappBrowserCloseMainResponse
>(DAPP_BROWSER_CLOSE_CHANNEL);
const statusChannel = new MainIpcChannel<
  DappBrowserStatusRendererRequest,
  DappBrowserStatusMainResponse
>(DAPP_BROWSER_STATUS_CHANNEL);
const stateChannel = new MainIpcChannel<
  DappBrowserStateRendererResponse,
  DappBrowserStateMainRequest
>(DAPP_BROWSER_STATE_CHANNEL);
publishDappBrowserState = (isOpen) =>
  consumeIpcResponse(
    stateChannel.send(isOpen, currentWindowSender.sender),
    DAPP_BROWSER_STATE_CHANNEL
  );
let registered = false;

export const handleDappBrowserRequests = (window: BrowserWindow): void => {
  browserController.observeWindow(window);
  if (registered) return;
  registered = true;
  openChannel.onRequest((request) => browserController.open(request));
  closeChannel.onRequest(() => browserController.close());
  statusChannel.onRequest(async () => browserController.status);
};

export const closeDappBrowser = (): Promise<void> => browserController.close();
export const setDappBrowserConsentPending = (pending: boolean): void =>
  browserController.setConsentPending(pending);
export const authenticateDappGuest = (event: IpcMainInvokeEvent) =>
  browserController.authenticate(event);
export const getCurrentDappRouteLease = (): DappRouteLease | null =>
  browserController.routeLease.current;
