// @flow
import { observable, action, computed, runInAction } from 'mobx';
import moment from 'moment';
import { isEqual, includes } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  ALLOWED_TIME_DIFFERENCE,
  MAX_ALLOWED_STALL_DURATION,
  NETWORK_STATUS_REQUEST_TIMEOUT,
  NETWORK_STATUS_POLL_INTERVAL,
  NTP_IGNORE_CHECKS_GRACE_PERIOD,
} from '../config/timingConfig';
import { UNSYNCED_BLOCKS_ALLOWED } from '../config/numbersConfig';
import { Logger } from '../utils/logging';
import {
  cardanoStateChangeChannel,
  cardanoTlsConfigChannel,
  restartCardanoNodeChannel,
  getCachedCardanoStatusChannel,
  setCachedCardanoStatusChannel,
} from '../ipc/cardano.ipc';
import { CardanoNodeStates } from '../../../common/types/cardano-node.types';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidatedChannel';
import { getDiskSpaceStatusChannel } from '../ipc/getDiskSpaceChannel.js';
import type { GetNetworkStatusResponse } from '../api/nodes/types';
import type {
  CardanoNodeState,
  CardanoStatus,
  TlsConfig,
} from '../../../common/types/cardano-node.types';
import type { NodeInfoQueryParams } from '../api/nodes/requests/getNodeInfo';
import type { GetConsolidatedEpochsCountResponse } from '../../../common/ipc/api';
import type { CheckDiskSpaceResponse } from '../../../common/types/no-disk-space.types';
import { TlsCertificateNotValidError } from '../api/nodes/errors';

// DEFINE CONSTANTS -------------------------
const NETWORK_STATUS = {
  CONNECTING: 0,
  SYNCING: 1,
  RUNNING: 2,
};

const NODE_STOPPING_STATES = [
  CardanoNodeStates.EXITING,
  CardanoNodeStates.STOPPING,
  CardanoNodeStates.UPDATING,
];

const NODE_STOPPED_STATES = [
  CardanoNodeStates.CRASHED,
  CardanoNodeStates.ERRORED,
  CardanoNodeStates.STOPPED,
  CardanoNodeStates.UPDATED,
  CardanoNodeStates.UNRECOVERABLE,
];
// END CONSTANTS ----------------------------

export default class NetworkStatusStore extends Store {
  // Initialize store properties
  _startTime = Date.now();
  _tlsConfig: ?TlsConfig = null;
  _networkStatus = NETWORK_STATUS.CONNECTING;
  _networkStatusPollingInterval: ?IntervalID = null;

  // Initialize store observables

  // Internal Node states
  @observable cardanoNodeState: ?CardanoNodeState = null;
  @observable isNodeResponding = false; // Is 'true' as long we are receiving node Api responses
  @observable isNodeSubscribed = false; // Is 'true' in case node is subscribed to the network
  @observable isNodeSyncing = false; // Is 'true' in case we are receiving blocks and not stalling
  @observable isNodeTimeCorrect = true; // Is 'true' in case local and global time are in sync
  @observable isNodeInSync = false; // 'true' if syncing & local/network blocks diff within limit
  @observable isNodeStopping = false; // 'true' if node is in `NODE_STOPPING_STATES` states
  @observable isNodeStopped = false; // 'true' if node is in `NODE_STOPPED_STATES` states

  @observable hasBeenConnected = false;
  @observable syncProgress = null;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable epochsConsolidated: number = 0;
  @observable currentEpoch: number = 0;
  @observable latestLocalBlockTimestamp = 0; // milliseconds
  @observable latestNetworkBlockTimestamp = 0; // milliseconds
  @observable localTimeDifference: ?number = 0; // microseconds
  @observable isSystemTimeIgnored = false; // Tracks if NTP time checks are ignored
  @observable
  getNetworkStatusRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );
  @observable
  getCurrentEpochFallbackRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getCurrentEpochFallback
  );
  @observable
  forceCheckTimeDifferenceRequest: Request<GetNetworkStatusResponse> = new Request(
    this.api.ada.getNetworkStatus
  );

  @observable isNotEnoughDiskSpace: boolean = false;
  @observable diskSpaceRequired: string = '';
  @observable diskSpaceMissing: string = '';
  @observable diskSpaceRecommended: string = '';
  @observable isTlsCertInvalid: boolean = false;
  @observable currentEpochFallbackRequested: boolean = false;

  // DEFINE STORE METHODS
  setup() {
    const actions = this.actions.networkStatus;
    actions.getEpochsData.listen(this._getEpochsData);
    // ========== IPC CHANNELS =========== //

    // Request node state
    this._requestCardanoState();

    // Request cached node status for fast bootstrapping of frontend
    this._requestCardanoStatus();

    // Passively receive broadcasted tls config changes (which can happen without requesting it)
    // E.g if the cardano-node restarted for some reason
    cardanoTlsConfigChannel.onReceive(this._updateTlsConfig);

    // Passively receive state changes of the cardano-node
    cardanoStateChangeChannel.onReceive(this._handleCardanoNodeStateChange);

    // ========== MOBX REACTIONS =========== //

    this.registerReactions([
      this._updateNetworkStatusWhenConnected,
      this._updateNetworkStatusWhenDisconnected,
      this._updateNodeStatus,
    ]);

    // Setup polling interval
    this._setNetworkStatusPollingInterval();

    // Ignore system time checks for the first 35 seconds:
    this.ignoreSystemTimeChecks();
    setTimeout(
      () => this.ignoreSystemTimeChecks(false),
      NTP_IGNORE_CHECKS_GRACE_PERIOD
    );

    // Setup disk space checks
    getDiskSpaceStatusChannel.onReceive(this._onCheckDiskSpace);
    this._checkDiskSpace();
  }

  // Setup network status polling interval
  _setNetworkStatusPollingInterval = () => {
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus,
      NETWORK_STATUS_POLL_INTERVAL
    );
  };

  async restartNode() {
    try {
      Logger.info('NetwortStatusStore: Requesting a restart of cardano-node');
      await restartCardanoNodeChannel.send();
    } catch (error) {
      Logger.error('NetwortStatusStore: Restart of cardano-node failed', {
        error,
      });
    }
  }

  teardown() {
    super.teardown();

    // Teardown polling intervals
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
    }
  }

  // ================= REACTIONS ==================

  _updateNetworkStatusWhenDisconnected = () => {
    if (!this.isConnected) this._updateNetworkStatus();
  };

  _updateNetworkStatusWhenConnected = () => {
    if (this.isConnected) {
      Logger.info('NetworkStatusStore: Connected, forcing NTP check now...');
      this._updateNetworkStatus({ force_ntp_check: true });
    }
  };

  _updateNodeStatus = async () => {
    if (!this.isConnected) return;
    try {
      Logger.info('NetworkStatusStore: Updating node status');
      await setCachedCardanoStatusChannel.send(this._extractNodeStatus(this));
    } catch (error) {
      Logger.error('NetworkStatusStore: Error while updating node status', {
        error,
      });
    }
  };

  // =============== PRIVATE ===============

  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  _checkDiskSpace(diskSpaceRequired?: number) {
    getDiskSpaceStatusChannel.send(diskSpaceRequired);
  }

  _requestCardanoState = async () => {
    Logger.info('NetworkStatusStore: requesting node state');
    const state = await cardanoStateChangeChannel.request();
    Logger.info(`NetworkStatusStore: handling node state <${state}>`, {
      state,
    });
    await this._handleCardanoNodeStateChange(state);
  };

  _requestCardanoStatus = async () => {
    try {
      Logger.info('NetworkStatusStore: requesting node status');
      const status = await getCachedCardanoStatusChannel.request();
      Logger.info('NetworkStatusStore: received cached node status', {
        status,
      });
      if (status)
        runInAction('assigning node status', () => Object.assign(this, status));
    } catch (error) {
      Logger.error('NetworkStatusStore: error while requesting node state', {
        error,
      });
    }
  };

  _requestTlsConfig = async () => {
    try {
      Logger.info(
        'NetworkStatusStore: requesting tls config from main process'
      );
      const tlsConfig = await cardanoTlsConfigChannel.request();
      await this._updateTlsConfig(tlsConfig);
    } catch (error) {
      Logger.error('NetworkStatusStore: error while requesting tls config', {
        error,
      });
    }
  };

  _updateTlsConfig = (config: ?TlsConfig): Promise<void> => {
    if (config == null || isEqual(config, this._tlsConfig))
      return Promise.resolve();
    Logger.info('NetworkStatusStore: received tls config from main process');
    this.api.ada.setRequestConfig(config);
    runInAction('update _tlsConfig', () => {
      this._tlsConfig = config;
    });
    return Promise.resolve();
  };

  _handleCardanoNodeStateChange = async (state: CardanoNodeState) => {
    if (state === this.cardanoNodeState) return Promise.resolve();
    Logger.info(`NetworkStatusStore: handling cardano-node state <${state}>`, {
      state,
    });
    const wasConnected = this.isConnected;
    switch (state) {
      case CardanoNodeStates.STARTING:
        break;
      case CardanoNodeStates.RUNNING:
        await this._requestTlsConfig();
        break;
      case CardanoNodeStates.STOPPING:
      case CardanoNodeStates.EXITING:
      case CardanoNodeStates.UPDATING:
        runInAction('reset _tlsConfig', () => {
          this._tlsConfig = null;
        });
        this._setDisconnected(wasConnected);
        break;
      default:
        this._setDisconnected(wasConnected);
    }
    runInAction('setting cardanoNodeState', () => {
      this.cardanoNodeState = state;
      this.isNodeStopping = includes(NODE_STOPPING_STATES, state);
      this.isNodeStopped = includes(NODE_STOPPED_STATES, state);
    });
    return Promise.resolve();
  };

  _extractNodeStatus = (from: Object & CardanoStatus): CardanoStatus => {
    const {
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
    } = from;

    return {
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
    };
  };

  _getEpochsData = async () => {
    this._onReceiveEpochsData(
      await getNumberOfEpochsConsolidatedChannel.request()
    );
  };

  // DEFINE ACTIONS

  @action _onReceiveEpochsData = (
    epochsConsolidated: GetConsolidatedEpochsCountResponse
  ) => {
    this.epochsConsolidated = epochsConsolidated;
  };

  @action _updateNetworkStatus = async (
    queryInfoParams?: NodeInfoQueryParams
  ) => {
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this._tlsConfig) return;

    const isForcedTimeDifferenceCheck = !!queryInfoParams;

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
      // Set latest block timestamps into the future as a guard
      // against system time changes - e.g. if system time was set into the past
      runInAction('update latest block timestamps', () => {
        const futureTimestamp = Date.now() + NETWORK_STATUS_REQUEST_TIMEOUT;
        this.latestLocalBlockTimestamp = futureTimestamp;
        this.latestNetworkBlockTimestamp = futureTimestamp;
      });
    }

    // Record connection status before running network status call
    const wasConnected = this.isConnected;

    try {
      const networkStatus: GetNetworkStatusResponse = isForcedTimeDifferenceCheck
        ? await this.forceCheckTimeDifferenceRequest.execute(queryInfoParams)
            .promise
        : await this.getNetworkStatusRequest.execute().promise;

      // In case we no longer have TLS config we ignore all API call responses
      // as this means we are in the Cardano shutdown (stopping|exiting|updating) sequence
      if (!this._tlsConfig) {
        Logger.debug(
          'NetworkStatusStore: Ignoring NetworkStatusRequest result during Cardano shutdown sequence...'
        );
        return;
      }

      const {
        subscriptionStatus,
        syncProgress,
        slotId,
        blockchainHeight,
        localBlockchainHeight,
        localTimeInformation,
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
        // Update localTimeDifference only in case NTP check status is not still pending
        if (localTimeInformation.status !== 'pending') {
          this.localTimeDifference = localTimeInformation.difference;
          this.isNodeTimeCorrect =
            this.localTimeDifference != null && // If we receive 'null' it means NTP check failed
            this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE;
        }
      });

      if (
        this._networkStatus === NETWORK_STATUS.CONNECTING &&
        this.isNodeSubscribed
      ) {
        // We are connected for the first time, move on to syncing stage
        runInAction('update _networkStatus', () => {
          this._networkStatus = NETWORK_STATUS.SYNCING;
        });
        Logger.info(
          `========== Connected after ${this._getStartupTimeDelta()} milliseconds ==========`
        );
      }

      // Update sync progress
      runInAction('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });

      // Update current epoch
      if (slotId && slotId.epoch) {
        runInAction('current epoch', () => {
          this.currentEpoch = slotId.epoch;
        });
      } else {
        this._getCurrentEpochFallback();
      }

      runInAction('update block heights', () => {
        if (this.initialLocalHeight === null) {
          // If initial local block height isn't set, mark the first
          // result as the 'starting' height for the sync progress
          this.initialLocalHeight = localBlockchainHeight;
          Logger.debug('NetworkStatusStore: Initial local block height', {
            localBlockchainHeight,
          });
        }

        // Update the local block height on each request
        const lastLocalBlockHeight = this.localBlockHeight;
        this.localBlockHeight = localBlockchainHeight;
        Logger.debug('NetworkStatusStore: Local blockchain height', {
          localBlockchainHeight,
        });

        // Update the network block height on each request
        const hasStartedReceivingBlocks = blockchainHeight > 0;
        const lastNetworkBlockHeight = this.networkBlockHeight;
        this.networkBlockHeight = blockchainHeight;
        if (hasStartedReceivingBlocks) {
          Logger.debug('NetworkStatusStore: Network blockchain height', {
            blockchainHeight,
          });
        }

        // Check if the local block height has ceased to change
        const isLocalBlockHeightIncreasing =
          this.localBlockHeight > lastLocalBlockHeight;
        if (
          isLocalBlockHeightIncreasing || // New local block detected
          this.latestLocalBlockTimestamp > Date.now() || // Guard against future timestamps // Guard against incorrect system time
          (!this.isNodeTimeCorrect && !this.isSystemTimeIgnored)
        ) {
          this.latestLocalBlockTimestamp = Date.now(); // Record latest local block timestamp
        }
        const latestLocalBlockAge = moment(Date.now()).diff(
          moment(this.latestLocalBlockTimestamp)
        );
        const isLocalBlockHeightStalling =
          latestLocalBlockAge > MAX_ALLOWED_STALL_DURATION;
        const isLocalBlockHeightSyncing =
          isLocalBlockHeightIncreasing || !isLocalBlockHeightStalling;

        // Check if the network's block height has ceased to change
        const isNetworkBlockHeightIncreasing =
          hasStartedReceivingBlocks &&
          this.networkBlockHeight > lastNetworkBlockHeight;
        if (
          isNetworkBlockHeightIncreasing || // New network block detected
          this.latestNetworkBlockTimestamp > Date.now() || // Guard against future timestamps // Guard against incorrect system time
          (!this.isNodeTimeCorrect && !this.isSystemTimeIgnored)
        ) {
          this.latestNetworkBlockTimestamp = Date.now(); // Record latest network block timestamp
        }
        const latestNetworkBlockAge = moment(Date.now()).diff(
          moment(this.latestNetworkBlockTimestamp)
        );
        const isNetworkBlockHeightStalling =
          latestNetworkBlockAge > MAX_ALLOWED_STALL_DURATION;
        const isNetworkBlockHeightSyncing =
          isNetworkBlockHeightIncreasing || !isNetworkBlockHeightStalling;

        // Node is syncing in case we are receiving blocks and they are not stalling
        runInAction('update isNodeSyncing', () => {
          this.isNodeSyncing =
            hasStartedReceivingBlocks &&
            (isLocalBlockHeightSyncing || isNetworkBlockHeightSyncing);
        });

        runInAction('update isNodeInSync', () => {
          const remainingUnsyncedBlocks =
            this.networkBlockHeight - this.localBlockHeight;
          this.isNodeInSync =
            this.isNodeSyncing &&
            remainingUnsyncedBlocks <= UNSYNCED_BLOCKS_ALLOWED;
        });

        if (hasStartedReceivingBlocks) {
          const initialLocalHeight = this.initialLocalHeight || 0;
          const blocksSyncedSinceStart =
            this.localBlockHeight - initialLocalHeight;
          const totalUnsyncedBlocksAtStart =
            this.networkBlockHeight - initialLocalHeight;
          Logger.debug(
            'NetworkStatusStore: Total unsynced blocks at node start',
            { totalUnsyncedBlocksAtStart }
          );
          Logger.debug('NetworkStatusStore: Blocks synced since node start', {
            blocksSyncedSinceStart,
          });
        }
      });

      if (this._networkStatus === NETWORK_STATUS.SYNCING && this.isNodeInSync) {
        // We are synced for the first time, move on to running stage
        runInAction('update _networkStatus', () => {
          this._networkStatus = NETWORK_STATUS.RUNNING;
        });
        this.actions.networkStatus.isSyncedAndReady.trigger();
        Logger.info(
          `========== Synced after ${this._getStartupTimeDelta()} milliseconds ==========`
        );
      }

      if (wasConnected !== this.isConnected) {
        if (!this.isConnected) {
          if (!this.hasBeenConnected) {
            runInAction('update hasBeenConnected', () => {
              this.hasBeenConnected = true;
            });
          }
          Logger.debug('NetworkStatusStore: Connection Lost. Reconnecting...');
        } else if (this.hasBeenConnected) {
          // Make sure all wallets data is fully reloaded after the connection is re-established
          this.stores.wallets.resetWalletsData();
          Logger.debug('NetworkStatusStore: Connection Restored');
        }
        if (this.isTlsCertInvalid) {
          runInAction('set isTlsCertInvalid = false', () => {
            this.isTlsCertInvalid = false;
          });
        }
      }
    } catch (error) {
      // Node is not responding, switch to disconnected state
      this._getCurrentEpochFallback();
      this._setDisconnected(wasConnected);
      if (error instanceof TlsCertificateNotValidError) {
        runInAction('set isTlsCertInvalid = true', () => {
          this.isTlsCertInvalid = true;
        });
      }
    }
  };

  @action _getCurrentEpochFallback = async () => {
    if (this.currentEpochFallbackRequested) return;
    this.currentEpochFallbackRequested = true;
    const currentEpoch = await this.getCurrentEpochFallbackRequest.execute()
      .promise;
    runInAction(() => {
      this.currentEpoch = currentEpoch;
    });
  };

  @action _setDisconnected = (wasConnected: boolean) => {
    this.isNodeResponding = false;
    this.isNodeSubscribed = false;
    this.isNodeSyncing = false;
    this.isNodeInSync = false;
    if (wasConnected) {
      if (!this.hasBeenConnected) {
        runInAction('update hasBeenConnected', () => {
          this.hasBeenConnected = true;
        });
      }
      Logger.debug('NetworkStatusStore: Connection Lost. Reconnecting...');
    }
  };

  @action ignoreSystemTimeChecks = (flag: boolean = true) => {
    this.isSystemTimeIgnored = flag;
  };

  forceCheckLocalTimeDifference = () => {
    if (this.isConnected) this._updateNetworkStatus({ force_ntp_check: true });
  };

  @action _onCheckDiskSpace = ({
    isNotEnoughDiskSpace,
    diskSpaceRequired,
    diskSpaceMissing,
    diskSpaceRecommended,
  }: CheckDiskSpaceResponse): Promise<void> => {
    this.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
    this.diskSpaceRequired = diskSpaceRequired;
    this.diskSpaceMissing = diskSpaceMissing;
    this.diskSpaceRecommended = diskSpaceRecommended;

    if (this.isNotEnoughDiskSpace) {
      if (this._networkStatusPollingInterval) {
        clearInterval(this._networkStatusPollingInterval);
        this._networkStatusPollingInterval = null;
      }
    } else if (!this._networkStatusPollingInterval) {
      this._setNetworkStatusPollingInterval();
    }

    return Promise.resolve();
  };

  // DEFINE COMPUTED VALUES

  @computed get isConnected(): boolean {
    return this.isNodeResponding && this.isNodeSubscribed && this.isNodeSyncing;
  }

  @computed get isSystemTimeCorrect(): boolean {
    return this.isNodeTimeCorrect || this.isSystemTimeIgnored;
  }

  @computed get isSynced(): boolean {
    return this.isConnected && this.isNodeInSync && this.isSystemTimeCorrect;
  }

  @computed get syncPercentage(): number {
    const { networkBlockHeight, localBlockHeight } = this;
    if (networkBlockHeight >= 1) {
      if (localBlockHeight >= networkBlockHeight) {
        return 100;
      }
      return (localBlockHeight / networkBlockHeight) * 100;
    }
    return 0;
  }
}
