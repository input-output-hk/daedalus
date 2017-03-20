// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';

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
    return !this.isConnected;
  }

  @computed get relativeSyncPercentage(): number {
    if (this.networkDifficulty > 0 && this._localDifficultyStartedWith !== null) {
      const relativeLocal = this.localDifficulty - this._localDifficultyStartedWith;
      const relativeNetwork = this.networkDifficulty - this._localDifficultyStartedWith;
      // In case node is in sync after first local difficulty messages
      // local and network difficulty will be the same (0)
      console.log('networkDifficulty', this.networkDifficulty);
      console.log('localDifficulty', this.localDifficulty);
      console.log('relativeLocal', relativeLocal);
      console.log('relativeNetwork', relativeNetwork);

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
    return !this.isConnecting && this.networkDifficulty > 0 && !this.isSynced;
  }

  @computed get isSynced(): bool {
    return !this.isConnecting && this.syncPercentage >= 100;
  }

  @action _setInitialDifficulty = async () => {
    const initialDifficulty = await this.networkDifficultyRequest.execute();
    this._localDifficultyStartedWith = initialDifficulty.localDifficulty;
    this.localDifficulty = initialDifficulty.localDifficulty;
    this.networkDifficulty = initialDifficulty.networkDifficulty;
    console.log('INITIAL', initialDifficulty);
  };

  @action _listenToServerStatusNotifications() {
    this.api.notify((message) => {
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
          this.networkDifficulty = message.contents.getChainDifficulty;
          this.isConnected = true;
          this.hasBeenConnected = true;
          break;
        case 'LocalDifficultyChanged':
          this.localDifficulty = message.contents.getChainDifficulty;
          break;
        case 'ConnectionClosedReconnecting':
          this.isConnected = false;
          break;
        default:
          console.log('Unknown server notification received:', message);
      }
    });
  }

  _redirectToWalletAfterSync = () => {
    const { app, wallets } = this.stores;
    if (this.isConnected && this.isSynced && wallets.hasLoadedWallets && app.currentRoute === '/') {
      this.isLoadingWallets = false;
      if (wallets.first) {
        this.actions.router.goToRoute({ route: wallets.getWalletRoute(wallets.first.id) });
      } else {
        this.actions.router.goToRoute({ route: '/no-wallets' });
      }
      this.actions.networkStatus.isSyncedAndReady();
    }
  };

  _redirectToLoadingWhenDisconnected = () => {
    if (!this.isConnected) {
      this._localDifficultyStartedWith = null;
      this._setInitialDifficulty();
      this.actions.router.goToRoute({ route: '/' });
    }
  };

}
