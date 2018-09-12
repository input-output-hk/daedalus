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

  // Initialize store observables
  @observable isNodeResponding = false;
  @observable isNodeSubscribed = false;
  @observable isNodeSyncing = false;
  @observable isNodeTimeCorrect = false;
  @observable isNodeInSync = false;
  @observable hasBeenConnected = false;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable localTimeDifference = 0; // microseconds
  @observable syncProgress = null;
  @observable getNetworkStatusRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );

  // DEFINE STORE METHODS
  setup() {
    this.registerReactions([
      this._updateNetworkStatusWhenDisconnected,
    ]);

    // Setup network status polling interval
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus, NETWORK_POLL_INTERVAL
    );
  }

  teardown() {
    super.teardown();

    // Teardown polling intervals
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
    }

    // Save current state into the cache
    cachedState = {
      hasBeenConnected: this.hasBeenConnected,
      localBlockHeight: this.localBlockHeight,
      networkBlockHeight: this.networkBlockHeight,
    };
  }

  _updateNetworkStatusWhenDisconnected = async () => {
    if (!this.isConnected) await this._updateNetworkStatus({ force_ntp_check: true });
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  // DEFINE ACTIONS
  @action initialize() {
    super.initialize();
    if (cachedState !== null) Object.assign(this, cachedState);
  }

  @action _updateNetworkStatus = async (queryParams?: NodeQueryParams) => {
    const wasConnected = this.isConnected;
    try {
      const {
        subscriptionStatus,
        syncProgress,
        blockchainHeight,
        localBlockchainHeight,
        localTimeDifference,
      } = await this.getNetworkStatusRequest.execute(queryParams).promise;

      // We got response which means node is responding
      runInAction('update isNodeResponding', () => {
        this.isNodeResponding = true;
      });

      // Node is subscribed in case it is subscribed to at least one other node in the network
      runInAction('update isNodeSubscribed', () => {
        const nodeIPs = Object.values(subscriptionStatus || {});
        this.isNodeSubscribed = nodeIPs.includes('subscribed');
      });

      // System time is correct if local time difference is below allowed threshold
      runInAction('update localTimeDifference and isSystemTimeCorrect', () => {
        this.localTimeDifference = localTimeDifference;
        this.isNodeTimeCorrect = (
          this.localTimeDifference !== null && // If we receive 'null' it means NTP check failed
          this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE
        );
      });

      if (this._nodeStatus === NODE_STATUS.CONNECTING && this.isNodeSubscribed) {
        // We are connected for the first time, move on to syncing stage
        this._nodeStatus = NODE_STATUS.SYNCING;
        Logger.info(
          `========== Connected after ${this._getStartupTimeDelta()} milliseconds ==========`
        );
      }

      // Update sync progress
      runInAction('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });

      runInAction('update block heights', () => {
        if (this.initialLocalHeight === null) {
          // If initial local block height isn't set, mark the first
          // result as the 'starting' height for the sync progress
          this.initialLocalHeight = localBlockchainHeight;
          Logger.debug('Initial local block height: ' + JSON.stringify(localBlockchainHeight));
        }

        // Update the local block height on each request
        this.localBlockHeight = localBlockchainHeight;
        Logger.debug('Local blockchain height: ' + localBlockchainHeight);

        // Update the network block height on each request
        const lastBlockchainHeight = this.networkBlockHeight;
        this.networkBlockHeight = blockchainHeight;
        Logger.debug('Network blockchain height: ' + blockchainHeight);

        // Check if the network's block height has ceased to change
        const isBlockchainHeightIncreasing = this.networkBlockHeight > lastBlockchainHeight;
        if (
          isBlockchainHeightIncreasing || // New block detected
          this._mostRecentBlockTimestamp > Date.now() || // Guard against future timestamps
          !this.isSystemTimeCorrect // Guard against incorrect system time
        ) {
          this._mostRecentBlockTimestamp = Date.now(); // Record latest block timestamp
        }
        const timeSinceLastBlock = moment(Date.now()).diff(moment(this._mostRecentBlockTimestamp));
        const isBlockchainHeightStalling = timeSinceLastBlock > MAX_ALLOWED_STALL_DURATION;

        // Node is syncing in case we are receiving blocks and they are not stalling
        runInAction('update isNodeSyncing', () => {
          this.isNodeSyncing = (
            this.networkBlockHeight > 0 &&
            (isBlockchainHeightIncreasing || !isBlockchainHeightStalling)
          );
        });

        runInAction('update isNodeInSync', () => {
          const remainingUnsyncedBlocks = this.networkBlockHeight - this.localBlockHeight;
          this.isNodeInSync = (
            this.isNodeSyncing &&
            remainingUnsyncedBlocks <= UNSYNCED_BLOCKS_ALLOWED
          );
        });
      });

      const initialLocalHeight = this.initialLocalHeight || 0;
      const blocksSyncedSinceStart = this.localBlockHeight - initialLocalHeight;
      const totalUnsyncedBlocksAtStart = this.networkBlockHeight - initialLocalHeight;
      Logger.debug('Total unsynced blocks at node start: ' + totalUnsyncedBlocksAtStart);
      Logger.debug('Blocks synced since node start: ' + blocksSyncedSinceStart);

      if (this._nodeStatus === NODE_STATUS.SYNCING && this.isNodeInSync) {
        // We are synced for the first time, move on to running stage
        this._nodeStatus = NODE_STATUS.RUNNING;
        this.actions.networkStatus.isSyncedAndReady.trigger();
        Logger.info(`========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`);
      }

      if (wasConnected !== this.isConnected) {
        if (!this.isConnected) {
          if (!this.hasBeenConnected) {
            runInAction('update hasBeenConnected', () => this.hasBeenConnected = true);
          }
          Logger.debug('Connection Lost. Reconnecting...');
        } else if (this.hasBeenConnected) {
          Logger.debug('Connection Restored.');
        }
      }
    } catch (error) {
      // Node is not responding, switch to disconnected state
      runInAction('update connected status', () => {
        this.isNodeResponding = false;
        this.isNodeSubscribed = false;
        this.isNodeSyncing = false;
        this.isNodeInSync = false;
        if (wasConnected) {
          if (!this.hasBeenConnected) {
            runInAction('update hasBeenConnected', () => this.hasBeenConnected = true);
          }
          Logger.debug('Connection Lost. Reconnecting...');
        }
      });
    }
  };

  forceCheckLocalTimeDifference = () => {
    this._updateNetworkStatus({ force_ntp_check: true });
  };

  // DEFINE COMPUTED VALUES
  @computed get isConnected(): boolean {
    return this.isNodeResponding && this.isNodeSubscribed && this.isNodeSyncing;
  }

  @computed get isSystemTimeCorrect(): boolean {
    return this.isNodeTimeCorrect;
  }

  @computed get isSynced(): boolean {
    return this.isConnected && this.isNodeInSync && this.isNodeTimeCorrect;
  }

  @computed get syncPercentage(): number {
    const { networkBlockHeight, localBlockHeight } = this;
    if (networkBlockHeight >= 1) {
      if (localBlockHeight >= networkBlockHeight) { return 100; }
      return localBlockHeight / networkBlockHeight * 100;
    }
    return 0;
  }

}
