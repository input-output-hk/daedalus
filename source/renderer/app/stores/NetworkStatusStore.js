// @flow
import { observable, action, computed, runInAction } from 'mobx';
import moment from 'moment';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  ALLOWED_TIME_DIFFERENCE,
  MAX_ALLOWED_STALL_DURATION,
  NETWORK_STATUS_REQUEST_TIMEOUT,
  NETWORK_STATUS_POLL_INTERVAL,
  SYSTEM_TIME_POLL_INTERVAL,
} from '../config/timingConfig';
import { Logger } from '../../../common/logging';
import type { NodeQueryParams } from '../api/ada/types';
import type { GetNetworkStatusResponse } from '../api/common';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedState = null;

// DEFINE CONSTANTS ----------------------------
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
  _systemTime = Date.now();
  _nodeStatus = NODE_STATUS.CONNECTING;
  _mostRecentBlockTimestamp = 0;
  _networkStatusPollingInterval: ?number = null;
  _systemTimeChangeCheckPollingInterval: ?number = null;

  // Initialize store observables
  @observable isForceCheckingNodeTime = false; // Tracks active force time difference check
  @observable isSystemTimeChanged = false; // Tracks system time change event

  // Internal Node states
  /* eslint-disable indent */
  @observable isNodeResponding = false; // Is 'true' as long we are receiving node Api responses
  @observable isNodeSubscribed = false; // Is 'true' in case node is subscribed to the network
  @observable isNodeSyncing = false; // Is 'true' in case we are receiving blocks and not stalling
  @observable isNodeTimeCorrect = true; // Is 'true' in case local and global time are in sync
  @observable isNodeInSync = false; // Is 'true' if node is syncing and local/network block height
                                    // difference is within the allowed limit
  /* eslint-enabme indent */

  @observable hasBeenConnected = false;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable localTimeDifference: ?number = 0; // microseconds
  @observable syncProgress = null;
  @observable getNetworkStatusRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );

  // DEFINE STORE METHODS
  setup() {
    this.registerReactions([
      this._updateNetworkStatusWhenDisconnected,
      this._updateLocalTimeDifferenceWhenSystemTimeChanged,
    ]);

    // Setup network status polling interval
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus, NETWORK_STATUS_POLL_INTERVAL
    );

    // Setup system time change polling interval
    this._systemTimeChangeCheckPollingInterval = setInterval(
      this._updateSystemTime, SYSTEM_TIME_POLL_INTERVAL
    );
  }

  teardown() {
    super.teardown();

    // Teardown polling intervals
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
    }
    if (this._systemTimeChangeCheckPollingInterval) {
      clearInterval(this._systemTimeChangeCheckPollingInterval);
    }

    // Save current state into the cache
    cachedState = {
      hasBeenConnected: this.hasBeenConnected,
      localBlockHeight: this.localBlockHeight,
      networkBlockHeight: this.networkBlockHeight,
    };
  }

  _updateNetworkStatusWhenDisconnected = async () => {
    if (!this.isConnected) await this._updateNetworkStatus();
  };

  _updateLocalTimeDifferenceWhenSystemTimeChanged = async () => {
    if (this.isSystemTimeChanged) await this._updateNetworkStatus({ force_ntp_check: true });
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  // DEFINE ACTIONS
  @action initialize() {
    super.initialize();
    if (cachedState !== null) Object.assign(this, cachedState);
  }

  @action _updateSystemTime = () => {
    // In order to detect system time changes (e.g. user updates machine's date/time)
    // we are checking current system time and comparing the difference to the last known value.
    // If the difference is larger than the polling interval plus a safety margin of 100%
    // we can be sure the user has update the time.
    const systemTimeDifference = Math.abs(moment(Date.now()).diff(moment(this._systemTime)));
    this.isSystemTimeChanged = Math.floor(systemTimeDifference / SYSTEM_TIME_POLL_INTERVAL) > 1;
    this._systemTime = Date.now();
  };

  @action _updateNetworkStatus = async (queryParams?: NodeQueryParams) => {
    const { getNetworkStatusRequest } = this;
    const isForcedTimeDifferenceCheck = !!queryParams;

    // Prevent network status requests in case there is an already executing request
    // unless we are trying to run a forced time difference check
    if (
      getNetworkStatusRequest.isExecuting &&
      (!isForcedTimeDifferenceCheck || this.isForceCheckingNodeTime)
    ) {
      return;
    }

    // Keep track of active forced time difference checks
    runInAction('update isForceCheckingNodeTime', () => {
      this.isForceCheckingNodeTime = isForcedTimeDifferenceCheck;
    });

    if (isForcedTimeDifferenceCheck) {
      // Set most recent block timestamp into the future as a guard
      // against system time changes - e.g. if system time was set into the past
      this._mostRecentBlockTimestamp = Date.now() + NETWORK_STATUS_REQUEST_TIMEOUT;
    }

    // Record connection status before running network status call
    const wasConnected = this.isConnected;

    try {
      const {
        subscriptionStatus,
        syncProgress,
        blockchainHeight,
        localBlockchainHeight,
        localTimeDifference,
      } = await getNetworkStatusRequest.execute(queryParams).promise;

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
      runInAction('update localTimeDifference and isNodeTimeCorrect', () => {
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
          !this.isNodeTimeCorrect // Guard against incorrect system time
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
    } finally {
      // In case the executed request was a forced time difference check
      // clear active forced time difference check flag
      if (this.isForceCheckingNodeTime) {
        runInAction('update isForceCheckingNodeTime', () => {
          this.isForceCheckingNodeTime = false;
        });
      }
    }
  };

  forceCheckLocalTimeDifference = async () => {
    await this._updateNetworkStatus({ force_ntp_check: true });
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
