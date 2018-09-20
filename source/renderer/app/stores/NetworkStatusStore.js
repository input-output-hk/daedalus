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
  REQUEST_TLS_CONFIG_RETRY_INTERVAL,
} from '../config/timingConfig';
import { UNSYNCED_BLOCKS_ALLOWED } from '../config/numbersConfig';
import { Logger } from '../../../common/logging';
import {
  cardanoStateChangeChannel,
  tlsConfigChannel,
  restartCardanoNodeChannel
} from '../ipc/cardano.ipc';
import { CardanoNodeStates } from '../../../common/types/cardanoNode.types';
import type { GetNetworkStatusResponse } from '../api/nodes/types';
import type { CardanoNodeState, TlsConfig } from '../../../common/types/cardanoNode.types';
import type { NodeQueryParams } from '../api/nodes/requests/getNodeInfo';

// To avoid slow reconnecting on store reset, we cache the most important props
let cachedState = null;

// DEFINE CONSTANTS -------------------------
const NODE_STATUS = {
  CONNECTING: 0,
  SYNCING: 1,
  RUNNING: 2,
};
// END CONSTANTS ----------------------------

export default class NetworkStatusStore extends Store {

  // Initialize store properties
  _startTime = Date.now();
  _hasReceivedTlsConfig = false;
  _systemTime = Date.now();
  _nodeStatus = NODE_STATUS.CONNECTING;
  _networkStatusPollingInterval: ?number = null;
  _systemTimeChangeCheckPollingInterval: ?number = null;

  // Initialize store observables

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
  @observable syncProgress = null;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable mostRecentBlockTimestamp = 0; // milliseconds
  @observable localTimeDifference: ?number = 0; // microseconds
  @observable isSystemTimeChanged = false; // Tracks system time change event
  @observable getNetworkStatusRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );
  @observable forceCheckTimeDifferenceRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );

  // DEFINE STORE METHODS
  setup() {
    // ========== IPC CHANNELS =========== //

    // Actively ask the main process to send tls config to this renderer process
    this._requestTlsConfig();
    // Passively receive broadcasted tls config changes (which can happen without requesting it)
    // E.g if the cardano-node restarted for some reason
    tlsConfigChannel.onReceive(this._updateTlsConfig);
    // Passively receive state changes of the cardano-node
    cardanoStateChangeChannel.onReceive(this._handleCardanoNodeStateChange);

    // ========== MOBX REACTIONS =========== //

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

  async restartNode() {
    try {
      Logger.info('NetwortStatusStore: Requesting a restart of cardano-node.');
      await restartCardanoNodeChannel.send();
    } catch (error) {
      Logger.info(`NetwortStatusStore: Restart of cardano-node failed with ${error}`);
    }
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
    if (this.isSystemTimeChanged) {
      Logger.debug('System time change detected');
      await this._updateNetworkStatus({ force_ntp_check: true });
    }
  };

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  _requestTlsConfig = async () => {
    try {
      const tlsConfig = await tlsConfigChannel.send();
      await this._updateTlsConfig(tlsConfig);
    } catch (error) {
      Logger.info(`NetworkStatusStore: error while requesting tls config ${error}. Retrying …`);
      setTimeout(this._requestTlsConfig, REQUEST_TLS_CONFIG_RETRY_INTERVAL);
    }
  };

  _updateTlsConfig = (config: TlsConfig): Promise<void> => {
    Logger.info('NetworkStatusStore: received tls config from main process.');
    this.api.ada.setRequestConfig(config);
    this._hasReceivedTlsConfig = true;
    return Promise.resolve();
  };

  _handleCardanoNodeStateChange = (state: CardanoNodeState): Promise<void> => {
    Logger.info(`NetworkStatusStore: handling cardano-node state change to <${state}>`);
    const wasConnected = this.isConnected;
    switch (state) {
      case CardanoNodeStates.STOPPED:
      case CardanoNodeStates.UPDATING:
      case CardanoNodeStates.UPDATED:
      case CardanoNodeStates.CRASHED:
        this._setDisconnected(wasConnected);
        this._hasReceivedTlsConfig = false;
        break;
      default:
    }
    return Promise.resolve();
  };

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
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this._hasReceivedTlsConfig) return;

    const isForcedTimeDifferenceCheck = !!queryParams;

    // Prevent network status requests in case there is an already executing
    // forced time difference check unless we are trying to run another
    // forced time difference check in which case we need to wait for it to finish
    if (this.forceCheckTimeDifferenceRequest.isExecuting) {
      if (isForcedTimeDifferenceCheck) {
        await this.forceCheckTimeDifferenceRequest;
      } else {
        return;
      }
    }

    if (isForcedTimeDifferenceCheck) {
      // Set most recent block timestamp into the future as a guard
      // against system time changes - e.g. if system time was set into the past
      this.mostRecentBlockTimestamp = Date.now() + NETWORK_STATUS_REQUEST_TIMEOUT;
    }

    // Record connection status before running network status call
    const wasConnected = this.isConnected;

    try {
      let networkStatus: GetNetworkStatusResponse;
      if (isForcedTimeDifferenceCheck) {
        networkStatus = await this.forceCheckTimeDifferenceRequest.execute(queryParams).promise;
      } else {
        networkStatus = await this.getNetworkStatusRequest.execute().promise;
      }

      const {
        subscriptionStatus,
        syncProgress,
        blockchainHeight,
        localBlockchainHeight,
        localTimeDifference,
      } = networkStatus;

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
        const hasStartedReceivingBlocks = blockchainHeight > 0;
        const lastBlockchainHeight = this.networkBlockHeight;
        this.networkBlockHeight = blockchainHeight;
        if (hasStartedReceivingBlocks) {
          Logger.debug('Network blockchain height: ' + blockchainHeight);
        }

        // Check if the network's block height has ceased to change
        const isBlockchainHeightIncreasing = (
          hasStartedReceivingBlocks &&
          this.networkBlockHeight > lastBlockchainHeight
        );
        if (
          isBlockchainHeightIncreasing || // New block detected
          this.mostRecentBlockTimestamp > Date.now() || // Guard against future timestamps
          !this.isNodeTimeCorrect // Guard against incorrect system time
        ) {
          this.mostRecentBlockTimestamp = Date.now(); // Record latest block timestamp
        }
        const timeSinceLastBlock = moment(Date.now()).diff(moment(this.mostRecentBlockTimestamp));
        const isBlockchainHeightStalling = timeSinceLastBlock > MAX_ALLOWED_STALL_DURATION;

        // Node is syncing in case we are receiving blocks and they are not stalling
        runInAction('update isNodeSyncing', () => {
          this.isNodeSyncing = isBlockchainHeightIncreasing || !isBlockchainHeightStalling;
        });

        runInAction('update isNodeInSync', () => {
          const remainingUnsyncedBlocks = this.networkBlockHeight - this.localBlockHeight;
          this.isNodeInSync = (
            this.isNodeSyncing &&
            remainingUnsyncedBlocks <= UNSYNCED_BLOCKS_ALLOWED
          );
        });

        if (hasStartedReceivingBlocks) {
          const initialLocalHeight = this.initialLocalHeight || 0;
          const blocksSyncedSinceStart = this.localBlockHeight - initialLocalHeight;
          const totalUnsyncedBlocksAtStart = this.networkBlockHeight - initialLocalHeight;
          Logger.debug('Total unsynced blocks at node start: ' + totalUnsyncedBlocksAtStart);
          Logger.debug('Blocks synced since node start: ' + blocksSyncedSinceStart);
        }
      });

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
          Logger.debug('Connection Restored');
        }
      }
    } catch (error) {
      // Node is not responding, switch to disconnected state
      this._setDisconnected(wasConnected);
    }
  };

  @action _setDisconnected = (wasConnected: boolean) => {
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
