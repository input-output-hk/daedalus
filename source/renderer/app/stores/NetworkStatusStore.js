// @flow
import { observable, action, computed, runInAction } from 'mobx';
import moment from 'moment';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { Logger } from '../../../common/logging';
import type { GetSyncProgressResponse, GetLocalTimeDifferenceResponse } from '../api/common';
import environment from '../../../common/environment';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedState = null;

// Maximum number of out-of-sync blocks above which we consider to be out-of-sync
const OUT_OF_SYNC_BLOCKS_LIMIT = 6;
const SYNC_PROGRESS_INTERVAL = 2000;
const TIME_DIFF_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes
const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds
const ALLOWED_NETWORK_DIFFICULTY_STALL = 2 * 60 * 1000; // 2 minutes

const STARTUP_STAGES = {
  CONNECTING: 0,
  SYNCING: 1,
  RUNNING: 2,
};

export default class NetworkStatusStore extends Store {

  _startTime = Date.now();
  _startupStage = STARTUP_STAGES.CONNECTING;
  _lastNetworkDifficultyChange = 0;
  _syncProgressPollInterval: ?number = null;
  _updateLocalTimeDifferencePollInterval: ?number = null;

  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable localDifficulty = 0;
  @observable networkDifficulty = 0;
  @observable syncProgress = 0;
  @observable localTimeDifference = 0;
  @observable syncProgressRequest: Request<GetSyncProgressResponse> = new Request(
    // Use the sync progress for target API
    this.api[environment.API].getSyncProgress
  );
  @observable localTimeDifferenceRequest: Request<GetLocalTimeDifferenceResponse> = new Request(
    this.api.ada.getLocalTimeDifference
  );
  @observable _localDifficultyStartedWith = null;

  @action initialize() {
    super.initialize();
    if (cachedState !== null) Object.assign(this, cachedState);
  }

  setup() {
    this.registerReactions([
      this._updateSyncProgressWhenDisconnected,
      this._updateLocalTimeDifferenceWhenConnected,
    ]);

    // Setup polling intervals
    this._syncProgressPollInterval = setInterval(
      this._updateSyncProgress, SYNC_PROGRESS_INTERVAL
    );
    if (environment.isAdaApi()) {
      this._updateLocalTimeDifferencePollInterval = setInterval(
        this._updateLocalTimeDifference, TIME_DIFF_POLL_INTERVAL
      );
    }
  }

  teardown() {
    super.teardown();

    // Teardown polling intervals
    if (this._syncProgressPollInterval) {
      clearInterval(this._syncProgressPollInterval);
    }
    if (this._updateLocalTimeDifferencePollInterval) {
      clearInterval(this._updateLocalTimeDifferencePollInterval);
    }
    // Save current state into the cache
    cachedState = {
      isConnected: this.isConnected,
      hasBeenConnected: this.hasBeenConnected,
      localDifficulty: this.localDifficulty,
      networkDifficulty: this.networkDifficulty,
    };
  }

  checkTheTimeAgain = () => {
    this.api.ada.getLocalTimeDifference({
      force_ntp_check: true,
    });
  }

  @computed get isConnecting(): boolean {
    // until we start receiving network difficulty messages we are not connected to node
    return !this.isConnected;
  }

  @computed get hasBlockSyncingStarted(): boolean {
    return this.syncProgress > 0;
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
    return this.syncProgress;
  }

  @computed get isSystemTimeCorrect(): boolean {
    if (!environment.isAdaApi()) return true;
    // We assume that system time is correct by default
    if (!this.localTimeDifferenceRequest.wasExecuted) return true;
    // Compare time difference if we have a result
    return this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE;
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
      const {
        localBlockchainHeight,
        blockchainHeight,
        syncProgress
      } = await this.syncProgressRequest.execute().promise;

      runInAction('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });

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
          this._localDifficultyStartedWith = localBlockchainHeight;
          Logger.debug('Initial difficulty: ' + JSON.stringify({ localBlockchainHeight, blockchainHeight }));
        }

        // Update the local difficulty on each request
        this.localDifficulty = localBlockchainHeight;
        Logger.debug('Local difficulty changed: ' + this.localDifficulty);

        // Check if network difficulty is stalled (e.g. unchanged for more than 2 minutes)
        // e.g. in case there is no Internet connection Api will send the last known value
        if (this.networkDifficulty !== blockchainHeight) {
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
        this.networkDifficulty = blockchainHeight;
      });
      Logger.debug('Network difficulty changed: ' + this.networkDifficulty);

      if (this._startupStage === STARTUP_STAGES.SYNCING && this.isSynced) {
        Logger.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
        this._startupStage = STARTUP_STAGES.RUNNING;
        this.actions.networkStatus.isSyncedAndReady.trigger();
      }
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

  _updateLocalTimeDifferenceWhenConnected = async () => {
    if (this.isConnected) await this._updateLocalTimeDifference();
  };

  _updateSyncProgressWhenDisconnected = async () => {
    if (!this.isConnected) await this._updateSyncProgress();
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }
}
