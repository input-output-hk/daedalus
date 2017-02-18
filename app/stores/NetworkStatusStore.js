// @flow
import { observable, action, computed} from 'mobx';
import Store from './lib/Store';

export default class NetworkStatusStore extends Store {

  @observable isConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isSyncedAfterLaunch = false;
  @observable isLoadingWallets = true;

  constructor(...args) {
    super(...args);
    this.registerReactions([
      this._redirectToWalletAfterSync,
      this._computeSyncStatus,
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
        // case "ConnectionOpened":
        //   this.isConnected = true; break;
        case "NetworkDifficultyChanged":
          this.networkDifficulty = message.contents.getChainDifficulty;
          this.isConnected = true;
          break;
        case "LocalDifficultyChanged":
          this.localDifficulty = message.contents.getChainDifficulty;
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
    const { router, wallets } = this.stores;
    if (this.isSyncedAfterLaunch && wallets.hasLoadedWallets && router.location.pathname === '/') {
      this.isLoadingWallets = false;
      if (wallets.first) {
        router.push(wallets.getWalletRoute(wallets.first.id)); // just pick the first for now
      } else {
        router.push('/no-wallets');
      }
    }
  };

}
