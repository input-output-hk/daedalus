// @flow
import { observable, action, computed} from 'mobx';
import Store from './lib/Store';

export default class NetworkStatusStore extends Store {

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isSyncedAfterLaunch = false;
  @observable isLoadingWallets = true;

  constructor(...args) {
    super(...args);
    this.registerReactions([
      this._redirectToWalletAfterSync,
      this._computeSyncStatus,
      this._redirectToLoadingWhenDisconnected,
    ]);
    this._listenToServerStatusNotifications();
  }

  @computed get isConnecting() {
    return !this.isConnected;
  }

  @computed get syncPercentage() {
    if (this.networkDifficulty > 0) {
      return (this.localDifficulty / this.networkDifficulty * 100);
    }
    return 0;
  }

  @computed get isSyncing() {
    return !this.isConnecting && this.networkDifficulty > 0 && !this.isSynced;
  }

  @computed get isSynced() {
    return !this.isConnecting && this.syncPercentage >= 100;
  }

  @action _listenToServerStatusNotifications() {
    this.api.notify((message) => {
      if (message === "ConnectionClosed") {
        return this.isConnected = false;
      }
      switch (message.tag) {
        case "ConnectionOpened":
          this.isConnected = true; break;
        case "NetworkDifficultyChanged":
          this.networkDifficulty = message.contents.getChainDifficulty;
          this.isConnected = true;
          this.hasBeenConnected = true;
          break;
        case "LocalDifficultyChanged":
          this.localDifficulty = message.contents.getChainDifficulty;
          break;
        case "ConnectionClosedReconnecting":
          this.isConnected = false;
          break;
        default:
          console.log("Unknown server notification received:", message);
      }
    });
  };

  _computeSyncStatus = () => {
    if (this.syncPercentage === 100) this.isSyncedAfterLaunch = true;
  };

  _redirectToWalletAfterSync = () => {
    const { app, wallets } = this.stores;
    if (this.isConnected && this.isSyncedAfterLaunch && wallets.hasLoadedWallets && app.currentRoute === '/') {
      this.isLoadingWallets = false;
      if (wallets.first) {
        this.actions.goToRoute({ route: wallets.getWalletRoute(wallets.first.id) });
      } else {
        this.actions.goToRoute({ route: '/no-wallets'});
      }
    }
  };

  _redirectToLoadingWhenDisconnected = () => {
    if (!this.isConnected) {
      this.actions.goToRoute({ route: '/' });
    }
  };

}
