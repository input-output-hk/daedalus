import { randomUUID } from 'crypto';
import type { BrowserWindow, IpcMainInvokeEvent } from 'electron';
import {
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_OPEN_CHANNEL,
  DAPP_BROWSER_STATE_CHANNEL,
  DAPP_BROWSER_STATUS_CHANNEL,
} from '../../common/ipc/api';
import type {
  DappBrowserCatalogOpenRendererRequest,
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
import { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappGuestRevocationReason } from '../dapp/DappBrowserManager';
import type { DappLaunchMode } from '../dapp/DappLaunchPolicy';
import { DappRouteLeaseService } from '../dapp/DappRouteLease';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  consumeIpcResponse,
  currentWindowSender,
} from './lib/currentWindowSender';

export type StagedDappLaunch = Readonly<{
  lease: DappRouteLease;
  mode: DappLaunchMode;
  entry: DappCatalogEntry;
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

export class DappBrowserController {
  readonly routeLease: DappRouteLeaseService;
  private readonly stagedLaunches = new Map<string, StagedDappLaunch>();
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
      this.stagedLaunches.clear();
      this.manager.close('route-changed').catch(() => undefined);
    });
  }

  observeWindow(window: BrowserWindow): void {
    const observe = (url: string) => this.routeLease.observeTrustedRoute(url);
    window.webContents.on(
      'did-navigate-in-page',
      (_event, url, isMainFrame) => {
        if (isMainFrame) observe(url);
      }
    );
    window.webContents.on('did-frame-finish-load', (_event, isMainFrame) => {
      if (isMainFrame) observe(window.webContents.getURL());
    });
    window.webContents.once('destroyed', () => this.routeLease.revoke());
  }

  stageLaunch(launch: StagedDappLaunch): string {
    this.routeLease.requireCurrent(launch.lease);
    const launchId = randomUUID();
    this.stagedLaunches.set(launchId, Object.freeze(launch));
    return launchId;
  }

  get status(): DappBrowserStatusMainResponse {
    return Object.freeze({
      isOpen: this.manager.isOpen,
      catalogAvailable: this.policy.allows('preferred'),
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
    if (!request || typeof request.launchId !== 'string' || !request.lease)
      throw new Error('Invalid dApp browser request');
    const launch = this.stagedLaunches.get(request.launchId);
    if (!launch) throw new Error('Unknown dApp launch');
    this.stagedLaunches.delete(request.launchId);

    this.routeLease.requireCurrent(request.lease);
    this.routeLease.requireCurrent(launch.lease);
    if (!this.policy.allows(launch.mode))
      throw new Error('DApp launch is disabled');

    if (launch.mode === 'diagnostics') {
      await this.manager.launch(
        launch.entry,
        launch.lease.networkGenesis,
        launch.localName,
        { kind: 'diagnostics' }
      );
    } else {
      await this.manager.launch(
        launch.entry,
        launch.lease.networkGenesis,
        launch.localName
      );
    }
    if (!this.routeLease.isCurrent(launch.lease)) {
      await this.manager.close('route-changed');
      throw new Error('DApp route lease is stale');
    }
    this.onState(true);
  }

  setConsentPending(pending: boolean): void {
    this.manager.setHidden(pending);
  }

  async close(): Promise<void> {
    this.stagedLaunches.clear();
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
export const stageDappLaunch = (launch: StagedDappLaunch): string =>
  browserController.stageLaunch(launch);
export const setDappBrowserConsentPending = (pending: boolean): void =>
  browserController.setConsentPending(pending);
export const authenticateDappGuest = (event: IpcMainInvokeEvent) =>
  browserController.authenticate(event);
export const getCurrentDappRouteLease = (): DappRouteLease | null =>
  browserController.routeLease.current;
