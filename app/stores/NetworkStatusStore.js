// @flow
import { observable, action, computed, runInAction } from 'mobx';
import Log from 'electron-log';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../Routes';
import type { GetSyncProgressResponse } from '../api';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedDifficulties = null;

// Maximum number of out-of-sync blocks above which we consider to be out-of-sync
const OUT_OF_SYNC_BLOCKS_LIMIT = 10;

const STARTUP_STAGES = {
  CONNECTING: 0,
  SYNCING: 1,
  LOADING: 2,
  RUNNING: 3,
};

export default class NetworkStatusStore extends Store {

  _startTime = Date.now();
  _startupStage = STARTUP_STAGES.CONNECTING;

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isLoadingWallets = true;
  @observable networkDifficultyRequest: Request<GetSyncProgressResponse> = new Request(
    this.api.getSyncProgress
  );
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

  @computed get isConnecting(): boolean {
    // until we start receiving network difficulty messages we are not connected to node and
    // we should be on the blue connecting screen instead of displaying "Loading wallet data"
    return !this.isConnected || this.networkDifficulty <= 1;
  }

  @computed get hasBlockSyncingStarted(): boolean {
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

  @computed get relativeSyncBlocksDifference(): number {
    if (this.networkDifficulty > 0 && this._localDifficultyStartedWith !== null) {
      const relativeLocal = this.localDifficulty - this._localDifficultyStartedWith;
      const relativeNetwork = this.networkDifficulty - this._localDifficultyStartedWith;
      // In case node is in sync after first local difficulty messages
      // local and network difficulty will be the same (0)
      Log.debug('Network difficulty: ', this.networkDifficulty);
      Log.debug('Local difficulty: ', this.localDifficulty);
      Log.debug('Relative local difficulty: ', relativeLocal);
      Log.debug('Relative network difficulty: ', relativeNetwork);

      if (relativeLocal >= relativeNetwork) return 0;
      return relativeNetwork - relativeLocal;
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

  @computed get isSyncing(): boolean {
    return !this.isConnecting && this.hasBlockSyncingStarted && !this.isSynced;
  }

  @computed get isSynced(): boolean {
    return (
      !this.isConnecting &&
      this.hasBlockSyncingStarted &&
      this.relativeSyncBlocksDifference <= OUT_OF_SYNC_BLOCKS_LIMIT
    );
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
    this.api.notify(action('NetworkStatusStore::_listenToServerStatusNotifications', (message) => {
      if (message === 'ConnectionClosed') {
        Log.debug('ServerStatusNotification: ConnectionClosed');
        this.isConnected = false;
        return;
      }
      switch (message.tag) {
        case 'ConnectionOpened':
          Log.debug('ServerStatusNotification: ConnectionOpened');
          this._setInitialDifficulty();
          this.isConnected = true;
          if (this._startupStage === STARTUP_STAGES.CONNECTING) {
            Log.info(
              `========== Connected after ${this._getStartupTimeDelta()} milliseconds ==========`
            );
            this._startupStage = STARTUP_STAGES.SYNCING;
          }
          break;
        case 'NetworkDifficultyChanged':
          if (message.contents.getChainDifficulty) {
            this.networkDifficulty = message.contents.getChainDifficulty;
          }
          Log.debug('ServerStatusNotification: NetworkDifficultyChanged: ', this.networkDifficulty);
          this.isConnected = true;
          this.hasBeenConnected = true;
          break;
        case 'LocalDifficultyChanged':
          if (message.contents.getChainDifficulty) {
            this.localDifficulty = message.contents.getChainDifficulty;
          }
          Log.debug('ServerStatusNotification: LocalDifficultyChanged: ', this.localDifficulty);
          break;
        case 'ConnectionClosedReconnecting':
          Log.debug('ServerStatusNotification: ConnectionClosedReconnecting');
          this.isConnected = false;
          break;
        default:
          Log.warn('ServerStatusNotification: Unknown server notification received: ', message);
      }
    }));
  }

  _redirectToWalletAfterSync = () => {
    const { app, wallets } = this.stores;
    if (this._startupStage === STARTUP_STAGES.SYNCING && this.isSynced) {
      Log.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
      this._startupStage = STARTUP_STAGES.LOADING;
    }
    // TODO: introduce smarter way to bootsrap initial screens
    if (this.isConnected && this.isSynced && wallets.hasLoadedWallets) {
      if (this._startupStage === STARTUP_STAGES.LOADING) {
        Log.info(`========== Loaded after ${this._getStartupTimeDelta()} milliseconds ==========`);
        this._startupStage = STARTUP_STAGES.RUNNING;
      }
      runInAction('NetworkStatusStore::_redirectToWalletAfterSync', () => { this.isLoadingWallets = false; });
      if (app.currentRoute === ROUTES.ROOT) {
        if (wallets.first) {
          this.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.SUMMARY,
            params: { id: wallets.first.id }
          });
        } else {
          this.actions.router.goToRoute.trigger({ route: ROUTES.NO_WALLETS });
        }
      }
      this.actions.networkStatus.isSyncedAndReady.trigger();
    }
  };

  _redirectToLoadingWhenDisconnected = () => {
    if (this.stores.app.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION) return;
    if (!this.isConnected) {
      this._setInitialDifficulty();
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

}
