// @flow
import { observable, action, computed, runInAction } from 'mobx';
import Log from 'electron-log';
import Store from './lib/Store';
import Request from './lib/Request';
import { ROUTES } from '../Routes';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedDifficulties = null;

export default class NetworkStatusStore extends Store {

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isLoadingWallets = true;
  @observable networkDifficultyRequest = new Request(this.api, 'getSyncProgress');
  @observable _localDifficultyStartedWith = null;

  @action initialize() {
    super.initialize();
    if (cachedDifficulties !== null) Object.assign(this, cachedDifficulties);
  }

  setup() {
    this.registerReactions([
      this._redirectToWalletAfterSync,
      this._redirectToLoadingWhenDisconnected,
    ]);
    this._listenToServerStatusNotifications();
  }

  teardown() {
    super.teardown();
    cachedDifficulties = {
      isConnected: this.isConnected,
      hasBeenConnected: this.hasBeenConnected,
      localDifficulty: this.localDifficulty,
      networkDifficulty: this.networkDifficulty,
      isLoadingWallets: true,
    };
  }

  @computed get isConnecting(): bool {
    // until we start receiving network difficulty messages we are not connected to node and
    // we should be on the blue connecting screen instead of displaying "Loading wallet data"
    return !this.isConnected || this.networkDifficulty <= 1;
  }

  @computed get hasBlockSyncingStarted(): bool {
    // until we start receiving network difficulty messages we are not connected to node and
    // we should be on the blue connecting screen instead of displaying "Loading wallet data"
    return this.networkDifficulty >= 1;
  }

  @computed get relativeSyncPercentage(): number {
    if (this.networkDifficulty > 0 && this._localDifficultyStartedWith !== null) {
      const relativeLocal = this.localDifficulty - this._localDifficultyStartedWith;
      const relativeNetwork = this.networkDifficulty - this._localDifficultyStartedWith;
      // In case node is in sync after first local difficulty messages
      // local and network difficulty will be the same (0)
      Log.debug('Network difficulty: ', this.networkDifficulty);
      Log.debug('Local difficulty: ', this.localDifficulty);
      Log.debug('Relative local difficulty: ', relativeLocal);
      Log.debug('Relative network difficulty: ', relativeNetwork);

      if (relativeLocal >= relativeNetwork) return 100;
      return relativeLocal / relativeNetwork * 100;
    }
    return 0;
  }

  @computed get syncPercentage(): number {
    if (this.networkDifficulty > 0) {
      if (this.localDifficulty >= this.networkDifficulty) return 100;
      return this.localDifficulty / this.networkDifficulty * 100;
    }
    return 0;
  }

  @computed get isSyncing(): bool {
    return !this.isConnecting && this.hasBlockSyncingStarted && !this.isSynced;
  }

  @computed get isSynced(): bool {
    return !this.isConnecting && this.syncPercentage >= 100 && this.hasBlockSyncingStarted;
  }

  @action _setInitialDifficulty = async () => {
    this._localDifficultyStartedWith = null;
    const initialDifficulty = await this.networkDifficultyRequest.execute().promise;
    if (initialDifficulty) {
      runInAction('set initial difficulty', () => {
        this._localDifficultyStartedWith = initialDifficulty.localDifficulty;
        this.localDifficulty = initialDifficulty.localDifficulty;
        this.networkDifficulty = initialDifficulty.networkDifficulty;
        Log.debug('Initial difficulty: ', initialDifficulty);
      });
    }
  };

  _listenToServerStatusNotifications() {
    this.api.notify(action((message) => {
      if (message === 'ConnectionClosed') {
        this.isConnected = false;
        return;
      }
      switch (message.tag) {
        case 'ConnectionOpened':
          this._setInitialDifficulty();
          this.isConnected = true;
          break;
        case 'NetworkDifficultyChanged':
          if (message.contents.getChainDifficulty) this.networkDifficulty = message.contents.getChainDifficulty;
          this.isConnected = true;
          this.hasBeenConnected = true;
          break;
        case 'LocalDifficultyChanged':
          if (message.contents.getChainDifficulty) this.localDifficulty = message.contents.getChainDifficulty;
          break;
        case 'ConnectionClosedReconnecting':
          this.isConnected = false;
          break;
        default:
          Log.warn('Unknown server notification received:', message);
      }
    }));
  }

  _redirectToWalletAfterSync = () => {
    const { app, wallets } = this.stores;
    if (app.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION) return;
    // TODO: introduce smarter way to bootsrap initial screens
    if (this.isConnected && this.isSynced && wallets.hasLoadedWallets && app.currentRoute === '/') {
      runInAction(() => { this.isLoadingWallets = false; });
      if (wallets.first) {
        this.actions.router.goToRoute({
          route: ROUTES.WALLETS.SUMMARY,
          params: { id: wallets.first.id }
        });
      } else {
        this.actions.router.goToRoute({ route: ROUTES.NO_WALLETS });
      }
      this.actions.networkStatus.isSyncedAndReady();
    }
  };

  _redirectToLoadingWhenDisconnected = () => {
    if (this.stores.app.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION) return;
    if (!this.isConnected) {
      this._setInitialDifficulty();
      this.actions.router.goToRoute({ route: ROUTES.ROOT });
    }
  };

}
