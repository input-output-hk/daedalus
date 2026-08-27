import { action, computed, observable, runInAction } from 'mobx';
import {
  bindDappBrowserState,
  closeDappBrowserChannel,
  dappBrowserStatusChannel,
  openDappBrowserChannel,
} from '../ipc/dappBrowser';
import { dappConnectionsChannel } from '../ipc/dappConnections';
import type {
  DappConnectionIdentity,
  DappConnectionScope,
  DappConnectionsRendererRequest,
} from '../../../common/ipc/api';
import type { DappGrant } from '../../../common/types/dapp.types';
import Store from './lib/Store';

export default class DappStore extends Store {
  @observable catalogAvailable = false;
  @observable diagnosticsAvailable = false;
  @observable guestOpen = false;
  @observable isLaunching = false;
  @observable connections: readonly DappGrant[] = [];
  @observable connectionsCorrupt = false;
  @observable isManagingConnections = false;
  @observable connectionActionFailed = false;
  private connectionRequestGeneration = 0;
  private prunedWalletIds?: string;
  private generation = 0;
  private unbind?: () => void;

  @computed
  get ready(): boolean {
    return (
      this.catalogAvailable &&
      !!this.stores.wallets.activeDappWallet &&
      this.stores.networkStatus.isSynced
    );
  }

  @computed
  get diagnosticsReady(): boolean {
    return (
      this.diagnosticsAvailable &&
      this.stores.wallets.eligibleDappWallets.length > 0 &&
      this.stores.networkStatus.isSynced
    );
  }

  setup(): void {
    const generation = ++this.generation;
    this.unbind = bindDappBrowserState((isOpen) => {
      if (generation === this.generation)
        runInAction('DappStore::receiveState', () => {
          this.guestOpen = isOpen;
        });
    });
    dappBrowserStatusChannel.request(undefined).then(
      (status) => {
        if (generation !== this.generation) return;
        runInAction('DappStore::receiveStatus', () => {
          this.catalogAvailable = status.catalogAvailable;
          this.diagnosticsAvailable = status.diagnosticsAvailable;
          this.guestOpen = status.isOpen;
        });
      },
      () => undefined
    );
  }

  teardown(): void {
    ++this.generation;
    ++this.connectionRequestGeneration;
    this.unbind?.();
    this.unbind = undefined;
    this.isLaunching = false;
    this.isManagingConnections = false;
    this.prunedWalletIds = undefined;
    super.teardown();
  }

  @action.bound
  async launch(catalogId: string, localName: string): Promise<void> {
    if (!this.ready || this.isLaunching) return;
    const generation = this.generation;
    this.isLaunching = true;
    try {
      await openDappBrowserChannel.request({ catalogId, localName });
    } finally {
      if (generation === this.generation)
        runInAction('DappStore::finishLaunch', () => {
          this.isLaunching = false;
        });
    }
  }

  @action.bound
  async launchDiagnostics(
    url: string,
    walletId: string,
    localName: string
  ): Promise<void> {
    if (
      !this.diagnosticsReady ||
      this.isLaunching ||
      !this.stores.wallets.eligibleDappWallets.some(
        (wallet) => wallet.id === walletId
      )
    )
      return;
    const generation = this.generation;
    this.isLaunching = true;
    try {
      await openDappBrowserChannel.request({ url, walletId, localName });
      this.actions.router.goToRoute.trigger({
        route: this.stores.wallets.getWalletRoute(walletId, 'dapps'),
      });
    } finally {
      if (generation === this.generation)
        runInAction('DappStore::finishDiagnosticsLaunch', () => {
          this.isLaunching = false;
        });
    }
  }

  @action.bound
  async close(): Promise<void> {
    const generation = this.generation;
    await closeDappBrowserChannel.request(undefined);
    if (generation === this.generation) this.guestOpen = false;
  }

  refreshConnections(): Promise<boolean> {
    return this.manageConnections({ type: 'list' });
  }

  disconnectConnection(grant: DappGrant): Promise<boolean> {
    return this.manageConnections({
      type: 'disconnect',
      identity: this.identity(grant),
    });
  }

  forgetConnection(grant: DappGrant): Promise<boolean> {
    return this.manageConnections({
      type: 'forget',
      identity: this.identity(grant),
    });
  }

  revokeConnectionScope(
    grant: DappGrant,
    scope: DappConnectionScope
  ): Promise<boolean> {
    return this.manageConnections({
      type: 'revoke-scope',
      identity: this.identity(grant),
      scope,
    });
  }

  repairConnections(): Promise<boolean> {
    return this.manageConnections({ type: 'repair' });
  }

  removeWalletConnections(walletId: string): Promise<boolean> {
    this.prunedWalletIds = undefined;
    return this.manageConnections({ type: 'remove-wallet', walletId });
  }

  async pruneWalletConnections(walletIds: readonly string[]): Promise<void> {
    const fingerprint = [...walletIds].sort().join('\0');
    if (this.prunedWalletIds === fingerprint) return;
    const succeeded = await this.manageConnections({
      type: 'prune-wallets',
      walletIds,
    });
    if (succeeded) this.prunedWalletIds = fingerprint;
  }

  private identity(grant: DappGrant): DappConnectionIdentity {
    return {
      origin: grant.origin,
      walletId: grant.walletId,
      networkGenesis: grant.networkGenesis,
      launch: grant.launch,
    };
  }

  private async manageConnections(
    request: DappConnectionsRendererRequest
  ): Promise<boolean> {
    const requestGeneration = ++this.connectionRequestGeneration;
    runInAction('DappStore::startConnectionAction', () => {
      this.isManagingConnections = true;
      this.connectionActionFailed = false;
    });
    try {
      const snapshot = await dappConnectionsChannel.request(request);
      if (requestGeneration !== this.connectionRequestGeneration) return false;
      runInAction('DappStore::receiveConnections', () => {
        this.connections = snapshot.grants;
        this.connectionsCorrupt = snapshot.corrupt;
      });
      return true;
    } catch {
      if (requestGeneration === this.connectionRequestGeneration)
        runInAction('DappStore::connectionActionFailed', () => {
          this.connectionActionFailed = true;
        });
      return false;
    } finally {
      if (requestGeneration === this.connectionRequestGeneration)
        runInAction('DappStore::finishConnectionAction', () => {
          this.isManagingConnections = false;
        });
    }
  }
}
