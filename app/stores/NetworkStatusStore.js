// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';

export default class NetworkStatusStore extends Store {

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isLoadingWallets = true;

  _localDifficultyStartedWith = null;

  setup() {
    this.registerReactions([
      this._redirectToWalletAfterSync,
      this._redirectToLoadingWhenDisconnected,
    ]);
    this._listenToServerStatusNotifications();
  }

  @computed get isConnecting(): bool {
    return !this.isConnected;
  }

  @computed get syncPercentage(): number {
    if (this.networkDifficulty > 0 && this._localDifficultyStartedWith != null) {
      const relativeLocal = this.localDifficulty - this._localDifficultyStartedWith;
      const relativeNetwork = this.networkDifficulty - this._localDifficultyStartedWith;
      return relativeLocal / relativeNetwork * 100;
    }
    return 0;
  }

  @computed get isSyncing(): bool {
    return !this.isConnecting && this.networkDifficulty > 0 && !this.isSynced;
  }

  @computed get isSynced(): bool {
    return !this.isConnecting && this.syncPercentage >= 100;
  }

  @action _listenToServerStatusNotifications() {
    this.api.notify((message) => {
      if (message === 'ConnectionClosed') {
        this.isConnected = false;
        return;
      }
      switch (message.tag) {
        case 'ConnectionOpened':
          this.isConnected = true; break;
        case 'NetworkDifficultyChanged':
          this.networkDifficulty = message.contents.getChainDifficulty;
          this.isConnected = true;
          this.hasBeenConnected = true;
          break;
        case "LocalDifficultyChanged":
          const difficulty = message.contents.getChainDifficulty;
          if (this._localDifficultyStartedWith == null) {
            this._localDifficultyStartedWith = difficulty;
          }
          this.localDifficulty = difficulty;
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
    }
  };

  _redirectToLoadingWhenDisconnected = () => {
    if (!this.isConnected) {
      this.actions.router.goToRoute({ route: '/' });
    }
  };

}
