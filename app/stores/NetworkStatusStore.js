// @flow
import { observable, action, computed, runInAction } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import { Logger } from '../utils/logging';
import type { GetSyncProgressResponse } from '../api/common';
import environment from '../environment';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedDifficulties = null;

// Maximum number of out-of-sync blocks above which we consider to be out-of-sync
const OUT_OF_SYNC_BLOCKS_LIMIT = 10;
const SYNC_PROGRESS_INTERVAL = 2000;

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
  @observable syncProgressRequest: Request<GetSyncProgressResponse> = new Request(
    // Use the sync progress for target API
    this.api[environment.API].getSyncProgress
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
    this._pollSyncProgress();
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
    // we should be on the blue connecting screen instead of displaying 'Loading wallet data'
    return !this.isConnected || this.networkDifficulty <= 1;
  }

  @computed get hasBlockSyncingStarted(): boolean {
    // until we start receiving network difficulty messages we are not connected to node and
    // we should be on the blue connecting screen instead of displaying 'Loading wallet data'
    return this.networkDifficulty >= 1;
  }

  @computed get relativeSyncPercentage(): number {
    if (this.networkDifficulty > 0 && this._localDifficultyStartedWith !== null) {
      const relativeLocal = this.localDifficulty - this._localDifficultyStartedWith;
      const relativeNetwork = this.networkDifficulty - this._localDifficultyStartedWith;
      // In case node is in sync after first local difficulty messages
      // local and network difficulty will be the same (0)
      Logger.debug('Network difficulty: ' + this.networkDifficulty);
      Logger.debug('Local difficulty: ' + this.localDifficulty);
      Logger.debug('Relative local difficulty: ' + relativeLocal);
      Logger.debug('Relative network difficulty: ' + relativeNetwork);

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
      Logger.debug('Network difficulty: ' + this.networkDifficulty);
      Logger.debug('Local difficulty: ' + this.localDifficulty);
      Logger.debug('Relative local difficulty: ' + relativeLocal);
      Logger.debug('Relative network difficulty: ' + relativeNetwork);

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

  @action _updateSyncProgress = async () => {
    try {
      const difficulty = await this.syncProgressRequest.execute().promise;
      runInAction('update difficulties', () => {
        this.isConnected = true;
        // We are connected, move on to syncing stage
        if (this._startupStage === STARTUP_STAGES.CONNECTING) {
          Logger.info(
            `========== Connected after ${this._getStartupTimeDelta()} milliseconds ==========`
          );
          this._startupStage = STARTUP_STAGES.SYNCING;
        }
        // If we haven't set local difficulty before, mark the first
        // result as 'start' difficulty for the sync progress
        if (this._localDifficultyStartedWith === null) {
          this._localDifficultyStartedWith = difficulty.localDifficulty;
          Logger.debug('Initial difficulty: ' + JSON.stringify(difficulty));
        }
        // Update the local and network difficulties on each request
        this.localDifficulty = difficulty.localDifficulty;
        Logger.debug('Local difficulty changed: ' + this.localDifficulty);
        this.networkDifficulty = difficulty.networkDifficulty;
        Logger.debug('Network difficulty changed: ' + this.networkDifficulty);
      });
    } catch (error) {
      // If the sync progress request fails, switch to disconnected state
      runInAction('update connected status', () => (this.isConnected = false));
      Logger.debug('Connection Lost. Reconnecting...');
    }
  };

  _pollSyncProgress() {
    setInterval(this._updateSyncProgress, SYNC_PROGRESS_INTERVAL);
    this._updateSyncProgress();
  }

  _redirectToWalletAfterSync = () => {
    const { app } = this.stores;
    const { wallets } = this.stores[environment.API];
    if (this._startupStage === STARTUP_STAGES.SYNCING && this.isSynced) {
      Logger.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
      this._startupStage = STARTUP_STAGES.LOADING;
    }
    // TODO: introduce smarter way to bootsrap initial screens
    if (this.isConnected && this.isSynced && wallets.hasLoadedWallets) {
      if (this._startupStage === STARTUP_STAGES.LOADING) {
        Logger.info(`========== Loaded after ${this._getStartupTimeDelta()} milliseconds ==========`);
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
      this._updateSyncProgress();
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

}
