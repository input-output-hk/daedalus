// @flow
import { observable, action, computed, runInAction } from 'mobx';
import moment from 'moment';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { Logger } from '../../../common/logging';
import type { NodeQueryParams } from '../api/ada/types';
import type { GetNetworkStatusResponse } from '../api/common';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedState = null;

// DEFINE CONSTANTS ----------------------------
const TIME_DIFF_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes (milliseconds)
const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds (microseconds)
const MAX_ALLOWED_STALL_DURATION = 2 * 60 * 1000; // 2 minutes (milliseconds)
const NETWORK_POLL_INTERVAL = 2000; // 2 seconds (milliseconds)
// Maximum number of out-of-sync blocks above which we consider to be out-of-sync
const UNSYNCED_BLOCKS_ALLOWED = 6;

const NODE_STATUS = {
  CONNECTING: 0,
  SYNCING: 1,
  RUNNING: 2,
};
// END CONSTANTS ----------------------------

export default class NetworkStatusStore extends Store {

  // Initialize store properties
  _startTime = Date.now();
  _nodeStatus = NODE_STATUS.CONNECTING;
  _mostRecentBlockTimestamp = 0;
  _networkStatusPollingInterval: ?number = null;
  _updateLocalTimeDifferencePollInterval: ?number = null;

  // Initialize store observables
  @observable isConnected = false;
  @observable hasBeenConnected = false;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable localTimeDifference = 0; // microseconds
  @observable syncProgress = null;
  @observable getNetworkStatusRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );
  @observable getLocalTimeDifferenceRequest: Request<number> = new Request(
    this.api.ada.getLocalTimeDifference
  );

  // DEFINE STORE METHODS
  setup() {
    this.registerReactions([
      this._updateNetworkStatusWhenDisconnected,
      this._updateLocalTimeDifferenceWhenConnected,
    ]);

    // Setup network status polling interval
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus, NETWORK_POLL_INTERVAL
    );

    // Setup time difference polling interval
    this._updateLocalTimeDifferencePollInterval = setInterval(
      this._updateLocalTimeDifference, TIME_DIFF_POLL_INTERVAL
    );
  }

  teardown() {
    super.teardown();
    // Teardown polling intervals
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
    }
    if (this._updateLocalTimeDifferencePollInterval) {
      clearInterval(this._updateLocalTimeDifferencePollInterval);
    }
    // Save current state into the cache
    cachedState = {
      isConnected: this.isConnected,
      hasBeenConnected: this.hasBeenConnected,
      localBlockHeight: this.localBlockHeight,
      networkBlockHeight: this.networkBlockHeight,
    };
  }

  _updateLocalTimeDifferenceWhenConnected = async () => {
    if (this.isConnected) await this._updateLocalTimeDifference();
  };

  _updateNetworkStatusWhenDisconnected = async () => {
    if (!this.isConnected) await this._updateNetworkStatus();
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  // DEFINE ACTIONS
  @action initialize() {
    super.initialize();
    if (cachedState !== null) Object.assign(this, cachedState);
  }

  @action _updateNetworkStatus = async () => {
    try {
      const {
        subscriptionStatus,
        syncProgress,
        blockchainHeight,
        localBlockchainHeight
      } = await this.getNetworkStatusRequest.execute().promise;

      // Update sync progress
      runInAction('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });

      // Update both local and network block heights
      runInAction('update block heights', () => {
        const nodeIPs = Object.values(subscriptionStatus || {});
        const isSubscribed = nodeIPs.includes('subscribed');

        // We are connected, move on to syncing stage
        if (this._nodeStatus === NODE_STATUS.CONNECTING && isSubscribed) {
          Logger.info(
            `========== Connected after ${this._getStartupTimeDelta()} milliseconds ==========`
          );
          this._nodeStatus = NODE_STATUS.SYNCING;
        }

        // If initial local block height isn't set, mark the first
        // result as the 'starting' height for the sync progress
        if (this.initialLocalHeight === null) {
          runInAction('update initial local block height', () => {
            this.initialLocalHeight = localBlockchainHeight;
          });
          Logger.debug('Initial local block height: ' + JSON.stringify(localBlockchainHeight));
        }

        // Update the local block height on each request
        runInAction('update local block height', () => {
          this.localBlockHeight = localBlockchainHeight;
        });
        Logger.debug('Local blockchain height updated: ' + localBlockchainHeight);

        // Check if the network's block height has ceased to change
        // If unchanged for > 2 minutes, it indicates the node has stalled
        // w/o internet connection, the node will send its last known network block height
        if (
          this.networkBlockHeight !== blockchainHeight || // New block detected
          this._mostRecentBlockTimestamp > Date.now() // Guard against future (machine time altered)
        ) {
          // Record latest block timestamp
          this._mostRecentBlockTimestamp = Date.now();
        }

        // Received block is the same as the last one - check if block syncing has stalled
        const timeSinceLastBlock = moment(Date.now()).diff(
          moment(this._mostRecentBlockTimestamp)
        );
        // Check if elapsed time exceeds maximum allowance
        const isBlockSyncingStalled = timeSinceLastBlock > MAX_ALLOWED_STALL_DURATION;
        const wasConnected = this.isConnected;
        const isConnected = isSubscribed && !isBlockSyncingStalled;
        if (isConnected !== wasConnected) {
          runInAction('update connected status', () => {
            this.isConnected = isConnected;
            if (!this.isConnected) {
              if (!this.hasBeenConnected) {
                runInAction('update hasBeenConnected', () => this.hasBeenConnected = true);
              }
              Logger.debug('Connection Lost. Reconnecting...');
            } else if (this.hasBeenConnected) {
              Logger.debug('Connection Restored.');
            }
          });
        }

        // Update latest block height on each request
        runInAction('update network blockchain height', () => {
          this.networkBlockHeight = blockchainHeight;
        });
        Logger.debug('Network blockchain height updated: ' + blockchainHeight);
      });

      if (this._nodeStatus === NODE_STATUS.SYNCING && this.isSynced) {
        Logger.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
        this._nodeStatus = NODE_STATUS.RUNNING;
        this.actions.networkStatus.isSyncedAndReady.trigger();
      }
    } catch (error) {
      // If the node info request fails, switch to disconnected state
      runInAction('update connected status', () => {
        if (this.isConnected) {
          this.isConnected = false;
          if (!this.hasBeenConnected) {
            runInAction('update hasBeenConnected', () => this.hasBeenConnected = true);
          }
          Logger.debug('Connection Lost. Reconnecting...');
        }
      });
    }
  };

  forceCheckLocalTimeDifference = () => {
    this._updateLocalTimeDifference({
      force_ntp_check: true,
    });
  };

  @action _updateLocalTimeDifference = async (queryParams?: NodeQueryParams) => {
    if (!this.isConnected) return;
    try {
      const timeDifference: number =
        await this.getLocalTimeDifferenceRequest.execute(queryParams).promise;
      runInAction('update time difference', () => (this.localTimeDifference = timeDifference));
    } catch (error) {
      runInAction('update time difference', () => (this.localTimeDifference = 0));
    }
  };

  // DEFINE COMPUTED VALUES
  @computed get isConnecting(): boolean {
    // Until we start receiving network difficulty messages we are not connected to node and
    // we should be on the blue connecting screen instead of displaying 'Loading wallet data'
    return !this.isConnected || this.networkBlockHeight < 1;
  }

  @computed get hasBlockSyncingStarted(): boolean {
    const { initialLocalHeight, syncProgress, networkBlockHeight } = this;
    return initialLocalHeight !== null && syncProgress !== null && networkBlockHeight >= 1;
  }

  @computed get remainingUnsyncedBlocks(): number {
    if (this.networkBlockHeight >= 1 && this.initialLocalHeight !== null) {
      const blocksSyncedSinceStart = this.localBlockHeight - this.initialLocalHeight;
      const totalUnsyncedBlocksAtStart = this.networkBlockHeight - this.initialLocalHeight;
      // In case node is in sync after first local block height changes
      // local and network heights will be the same (0)
      Logger.debug('Network block height: ' + this.networkBlockHeight);
      Logger.debug('Local block height: ' + this.localBlockHeight);
      Logger.debug('Total unsynced blocks at node start: ' + totalUnsyncedBlocksAtStart);
      Logger.debug('Blocks synced since node start: ' + blocksSyncedSinceStart);

      if (blocksSyncedSinceStart >= totalUnsyncedBlocksAtStart) return 0;
      return totalUnsyncedBlocksAtStart - blocksSyncedSinceStart;
    }
    return 0;
  }

  @computed get syncPercentage(): number {
    const { networkBlockHeight, localBlockHeight } = this;
    if (networkBlockHeight >= 1) {
      if (localBlockHeight >= networkBlockHeight) { return 100; }
      return localBlockHeight / networkBlockHeight * 100;
    }
    return 0;
  }

  @computed get isSystemTimeCorrect(): boolean {
    // We assume that system time is correct by default
    if (!this.getLocalTimeDifferenceRequest.wasExecuted) return true;
    // Compare time difference if we have a result
    return this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE;
  }

  @computed get isSyncing(): boolean {
    return (
      !this.isSynced &&
      !this.isConnecting &&
      this.hasBlockSyncingStarted
    );
  }

  @computed get isSynced(): boolean {
    return (
      !this.isConnecting &&
      this.hasBlockSyncingStarted &&
      this.remainingUnsyncedBlocks <= UNSYNCED_BLOCKS_ALLOWED
    );
  }
}
