// @flow
import { observable, action, computed, runInAction } from 'mobx';
import moment from 'moment';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import { Logger } from '../../../common/logging';
import type { GetSyncProgressResponse, GetLocalTimeDifferenceResponse } from '../api/common';
import environment from '../../../common/environment';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedDifficulties = null;

// Maximum number of out-of-sync blocks above which we consider to be out-of-sync
const OUT_OF_SYNC_BLOCKS_LIMIT = 6;
const SYNC_PROGRESS_INTERVAL = 2000;
const TIME_DIFF_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes
const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds
const ALLOWED_NETWORK_DIFFICULTY_STALL = 2 * 60 * 1000; // 2 minutes

const STARTUP_STAGES = {
  CONNECTING: 0,
  SYNCING: 1,
  LOADING: 2,
  RUNNING: 3,
};

export default class NetworkStatusStore extends Store {

  _startTime = Date.now();
  _startupStage = STARTUP_STAGES.CONNECTING;
  _lastNetworkDifficultyChange = 0;

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable isLoadingWallets = true;
  @observable localTimeDifference = 0;
  @observable syncProgressRequest: Request<GetSyncProgressResponse> = new Request(
    // Use the sync progress for target API
    this.api[environment.API].getSyncProgress
  );
  @observable localTimeDifferenceRequest: Request<GetLocalTimeDifferenceResponse> = new Request(
    this.api.ada.getLocalTimeDifference
  );
  @observable _localDifficultyStartedWith = null;

  _timeDifferencePollInterval: ?number = null;

  @action initialize() {
    super.initialize();
    if (cachedDifficulties !== null) Object.assign(this, cachedDifficulties);
  }

  setup() {
    this.registerReactions([
      this._redirectToWalletAfterSync,
      this._redirectToLoadingWhenDisconnected,
      this._redirectToSyncingWhenOutOfSync,
      this._pollTimeDifferenceWhenConnected,
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

  @computed get isSystemTimeCorrect(): boolean {
    if (!environment.isAdaApi()) return true;
    return (this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE);
  }

  @computed get isSyncing(): boolean {
    return !this.isConnecting && this.hasBlockSyncingStarted && !this.isSynced;
  }

  @computed get isSynced(): boolean {
    return (
      !this.isConnecting &&
      this.hasBlockSyncingStarted &&
      this.relativeSyncBlocksDifference <= OUT_OF_SYNC_BLOCKS_LIMIT &&
      this.isSystemTimeCorrect
    );
  }

  @computed get isSetupPage(): boolean {
    return (
      this.stores.app.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION ||
      this.stores.app.currentRoute === ROUTES.PROFILE.TERMS_OF_USE
    );
  }

  @action _updateSyncProgress = async () => {
    try {
      const difficulty = await this.syncProgressRequest.execute().promise;
      runInAction('update difficulties', () => {
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
        // Update the local difficulty on each request
        this.localDifficulty = difficulty.localDifficulty;
        Logger.debug('Local difficulty changed: ' + this.localDifficulty);
        // Check if network difficulty is stalled (e.g. unchanged for more than 2 minutes)
        // e.g. in case there is no Internet connection Api will send the last known value
        if (this.networkDifficulty !== difficulty.networkDifficulty) {
          if (!this.isConnected) this.isConnected = true;
          this._lastNetworkDifficultyChange = Date.now();
        } else if (this.isConnected) {
          const currentNetworkDifficultyStall = moment(Date.now()).diff(
            moment(this._lastNetworkDifficultyChange)
          );
          if (currentNetworkDifficultyStall > ALLOWED_NETWORK_DIFFICULTY_STALL) {
            this.isConnected = false;
            if (!this.hasBeenConnected) this.hasBeenConnected = true;
          }
        }
        // Update the network difficulty on each request
        this.networkDifficulty = difficulty.networkDifficulty;
      });
      Logger.debug('Network difficulty changed: ' + this.networkDifficulty);
    } catch (error) {
      // If the sync progress request fails, switch to disconnected state
      runInAction('update connected status', () => {
        if (this.isConnected) {
          this.isConnected = false;
          if (!this.hasBeenConnected) this.hasBeenConnected = true;
        }
      });
      Logger.debug('Connection Lost. Reconnecting...');
    }
  };

  @action _updateLocalTimeDifference = async () => {
    if (!this.isConnected) return;
    try {
      const response = await this.localTimeDifferenceRequest.execute().promise;
      runInAction('update time difference', () => (this.localTimeDifference = response));
    } catch (error) {
      runInAction('update time difference', () => (this.localTimeDifference = 0));
    }
  };

  _pollLocalTimeDifference() {
    Logger.debug('Started polling local time difference');
    if (this._timeDifferencePollInterval) clearInterval(this._timeDifferencePollInterval);
    this._timeDifferencePollInterval = setInterval(
      this._updateLocalTimeDifference, TIME_DIFF_POLL_INTERVAL
    );
    this._updateLocalTimeDifference();
  }

  _stopPollingLocalTimeDifference() {
    Logger.debug('Stopped polling local time difference');
    if (this._timeDifferencePollInterval) clearInterval(this._timeDifferencePollInterval);
  }

  _pollSyncProgress() {
    console.log("setting interval");
    var id = setInterval(this._updateSyncProgress, SYNC_PROGRESS_INTERVAL);
    console.log("its id is", id);
    this._updateSyncProgress();
  }

  _redirectToWalletAfterSync = () => {
    const { app } = this.stores;
    const { wallets } = this.stores[environment.API];
    if (this._startupStage === STARTUP_STAGES.SYNCING && this.isSynced) {
      Logger.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
      this._startupStage = STARTUP_STAGES.LOADING;
      // close reportIssue dialog if is opened and app synced in meanwhile
      this.actions.dialogs.closeActiveDialog.trigger();
    }
    // TODO: introduce smarter way to bootsrap initial screens
    if (this.isConnected && this.isSynced && wallets.hasLoadedWallets) {
      if (this._startupStage === STARTUP_STAGES.LOADING) {
        Logger.info(`========== Loaded after ${this._getStartupTimeDelta()} milliseconds ==========`);
        this._startupStage = STARTUP_STAGES.RUNNING;
      }
      runInAction('NetworkStatusStore::_redirectToWalletAfterSync', () => (this.isLoadingWallets = false));
      if (app.currentRoute === ROUTES.ROOT) {
        if (wallets.first) {
          this.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.SUMMARY,
            params: { id: wallets.first.id }
          });
        } else {
          this.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
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

  _redirectToSyncingWhenOutOfSync = () => {
    if (!this.isSynced && !this.isSetupPage) {
      this._updateSyncProgress();
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  _pollTimeDifferenceWhenConnected = () => {
    if (!environment.isAdaApi()) return;
    if (this.isConnected) {
      this._pollLocalTimeDifference();
    } else {
      this._stopPollingLocalTimeDifference();
    }
  };

}
