import { randomUUID } from 'crypto';
import type { BrowserWindow } from 'electron';
import {
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_OPEN_CHANNEL,
} from '../../common/ipc/api';
import type {
  DappBrowserCloseMainResponse,
  DappBrowserCloseRendererRequest,
  DappBrowserOpenMainResponse,
  DappBrowserOpenRendererRequest,
} from '../../common/ipc/api';
import { dappLaunchPolicy, launcherConfig } from '../config';
import { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappCatalogEntry } from '../dapp/dappCatalog';
import type { DappLaunchMode } from '../dapp/DappLaunchPolicy';
import { DappRouteLeaseService } from '../dapp/DappRouteLease';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import { MainIpcChannel } from './lib/MainIpcChannel';

export type StagedDappLaunch = Readonly<{
  lease: DappRouteLease;
  mode: DappLaunchMode;
  entry: DappCatalogEntry;
  localName: string;
}>;

export class DappBrowserController {
  readonly routeLease: DappRouteLeaseService;
  private readonly stagedLaunches = new Map<string, StagedDappLaunch>();
  private readonly manager: DappBrowserManager;
  private readonly policy: typeof dappLaunchPolicy;

  constructor(
    manager: DappBrowserManager,
    networkGenesis: string,
    policy = dappLaunchPolicy
  ) {
    this.manager = manager;
    this.policy = policy;
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

  async open(request: DappBrowserOpenRendererRequest): Promise<void> {
    if (!request || typeof request.launchId !== 'string')
      throw new Error('Invalid dApp browser request');
    const launch = this.stagedLaunches.get(request.launchId);
    if (!launch) throw new Error('Unknown dApp launch');
    this.stagedLaunches.delete(request.launchId);

    this.routeLease.requireCurrent(request.lease);
    this.routeLease.requireCurrent(launch.lease);
    if (!this.policy.allows(launch.mode))
      throw new Error('DApp launch is disabled');

    await this.manager.launch(
      launch.entry,
      launch.lease.networkGenesis,
      launch.localName
    );
    if (!this.routeLease.isCurrent(launch.lease)) {
      await this.manager.close('route-changed');
      throw new Error('DApp route lease is stale');
    }
  }

  close(): Promise<void> {
    this.stagedLaunches.clear();
    return this.manager.close();
  }
}

const browserController = new DappBrowserController(
  new DappBrowserManager(),
  launcherConfig.nodeConfig.network.genesisHash
);
const openChannel = new MainIpcChannel<
  DappBrowserOpenRendererRequest,
  DappBrowserOpenMainResponse
>(DAPP_BROWSER_OPEN_CHANNEL);
const closeChannel = new MainIpcChannel<
  DappBrowserCloseRendererRequest,
  DappBrowserCloseMainResponse
>(DAPP_BROWSER_CLOSE_CHANNEL);
let registered = false;

export const handleDappBrowserRequests = (window: BrowserWindow): void => {
  browserController.observeWindow(window);
  if (registered) return;
  registered = true;
  openChannel.onRequest((request) => browserController.open(request));
  closeChannel.onRequest(() => browserController.close());
};

export const closeDappBrowser = (): Promise<void> => browserController.close();
export const stageDappLaunch = (launch: StagedDappLaunch): string =>
  browserController.stageLaunch(launch);
