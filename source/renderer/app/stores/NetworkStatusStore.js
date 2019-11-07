// @flow
import { observable, action, computed, runInAction } from 'mobx';
// import moment from 'moment';
import { isEqual, includes } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  ALLOWED_TIME_DIFFERENCE,
  // MAX_ALLOWED_STALL_DURATION,
  NETWORK_STATUS_REQUEST_TIMEOUT,
  NETWORK_STATUS_POLL_INTERVAL,
  NTP_IGNORE_CHECKS_GRACE_PERIOD,
  NTP_RECHECK_TIMEOUT,
} from '../config/timingConfig';
import {
  // UNSYNCED_BLOCKS_ALLOWED,
  MAX_NTP_RECHECKS,
} from '../config/numbersConfig';
import { Logger } from '../utils/logging';
import {
  cardanoStateChangeChannel,
  cardanoNodeImplementationChannel,
  cardanoTlsConfigChannel,
  restartCardanoNodeChannel,
  getCachedCardanoStatusChannel,
  setCachedCardanoStatusChannel,
} from '../ipc/cardano.ipc';
import { CardanoNodeStates } from '../../../common/types/cardano-node.types';
import { getDiskSpaceStatusChannel } from '../ipc/getDiskSpaceChannel.js';
import { getStateDirectoryPathChannel } from '../ipc/getStateDirectoryPathChannel';
import type { GetNetworkInfoResponse, TipInfo } from '../api/network/types';
import type {
  CardanoNodeState,
  CardanoStatus,
  TlsConfig,
  CardanoNodeImplementation,
} from '../../../common/types/cardano-node.types';
import type { NetworkInfoQueryParams } from '../api/network/requests/getNetworkInfo';
import type { CheckDiskSpaceResponse } from '../../../common/types/no-disk-space.types';
import { TlsCertificateNotValidError } from '../api/nodes/errors';
import { openLocalDirectoryChannel } from '../ipc/open-local-directory';

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

const NODE_IMPLEMENTATIONS: { [key: string]: CardanoNodeImplementation } = {
  jormungandr: 'jormungandr',
  cardanoNode: 'cardano-node',
};

// END CONSTANTS ----------------------------

export default class NetworkStatusStore extends Store {
  // Initialize store properties
  _startTime = Date.now();
  _networkStatus = NETWORK_STATUS.CONNECTING;
  _networkStatusPollingInterval: ?IntervalID = null;
  _ntpRecheckTimeout: ?TimeoutID = null;
  _numberOfNTPRechecks = 0;

  // Initialize store observables

  // Internal Node states
  @observable tlsConfig: ?TlsConfig = null;
  @observable cardanoNodeState: ?CardanoNodeState = null;
  @observable cardanoNodeID: number = 0;
  @observable isNodeResponding = false; // Is 'true' as long we are receiving node Api responses
  @observable isNodeSubscribed = false; // Is 'true' in case node is subscribed to the network
  @observable isNodeSyncing = false; // Is 'true' in case we are receiving blocks and not stalling
  @observable isNodeInSync = false; // 'true' if syncing & local/network blocks diff within limit
  @observable isNodeStopping = false; // 'true' if node is in `NODE_STOPPING_STATES` states
  @observable isNodeStopped = false; // 'true' if node is in `NODE_STOPPED_STATES` states
  @observable isNodeTimeCorrect = true; // Is 'true' in case local and global time are in sync
  @observable isSystemTimeIgnored = false; // Tracks if NTP time checks are ignored

  @observable hasBeenConnected = false;
  @observable syncProgress = null;
  @observable localTip: ?TipInfo = null;
  @observable networkTip: ?TipInfo = null;
  @observable initialLocalHeight = null;
  @observable localBlockHeight = 0;
  @observable networkBlockHeight = 0;
  @observable latestLocalBlockTimestamp = 0; // milliseconds
  @observable latestNetworkBlockTimestamp = 0; // milliseconds
  @observable localTimeDifference: ?number = 0; // microseconds
  @observable
  getNetworkInfoRequest: Request<GetNetworkInfoResponse> = new Request(
    this.api.ada.getNetworkInfo
  );
  @observable
  forceCheckTimeDifferenceRequest: Request<GetNetworkInfoResponse> = new Request(
    this.api.ada.getNetworkInfo
  );

  @observable isNotEnoughDiskSpace: boolean = false;
  @observable diskSpaceRequired: string = '';
  @observable diskSpaceMissing: string = '';
  @observable diskSpaceRecommended: string = '';
  @observable diskSpaceAvailable: string = '';
  @observable isTlsCertInvalid: boolean = false;
  @observable stateDirectoryPath: string = '';
  @observable nodeImplementation: CardanoNodeImplementation =
    NODE_IMPLEMENTATIONS.cardanoNode;

  // DEFINE STORE METHODS
  setup() {
    // ========== IPC CHANNELS =========== //

    this.actions.networkStatus.restartNode.listen(this._restartNode);

    // Request node state
    this._requestCardanoState();

    // Request cached node status for fast bootstrapping of frontend
    this._requestCardanoStatus();

    // Request node implementation
    this._requestCardanoNodeImplementation();

    // Passively receive broadcasted tls config changes (which can happen without requesting it)
    // E.g if the cardano-node restarted for some reason
    cardanoTlsConfigChannel.onReceive(this._updateTlsConfig);

    // Passively receive state changes of the cardano-node
    cardanoStateChangeChannel.onReceive(this._handleCardanoNodeStateChange);

    // Passively receive the node implementation from the main process
    cardanoNodeImplementationChannel.onReceive(this._handleNodeImplementation);
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

    this._getStateDirectoryPath();
  }

  // Setup network status polling interval
  _setNetworkStatusPollingInterval = () => {
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus,
      NETWORK_STATUS_POLL_INTERVAL
    );
  };

  _restartNode = async () => {
    try {
      Logger.info('NetworkStatusStore: Requesting a restart of cardano-node');
      await restartCardanoNodeChannel.send();
    } catch (error) {
      Logger.error('NetworkStatusStore: Restart of cardano-node failed', {
        error,
      });
    }
  };

  teardown() {
    super.teardown();

    // Teardown polling intervals
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
      this._resetNTPRechecks();
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
    if (this.environment.isTest && !this.isConnected) return;
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

  _getStateDirectoryPath = async () => {
    this._onReceiveStateDirectoryPath(
      await getStateDirectoryPathChannel.request()
    );
  };

  _requestCardanoState = async () => {
    Logger.info('NetworkStatusStore: requesting node state');
    const state = await cardanoStateChangeChannel.request();
    Logger.info(`NetworkStatusStore: handling node state <${state}>`, {
      state,
    });
    await this._handleCardanoNodeStateChange(state);
  };

  _requestCardanoNodeImplementation = async () => {
    Logger.info('NetworkStatusStore: requesting node implementation');
    const impl = await cardanoNodeImplementationChannel.request();
    Logger.info(`NetworkStatusStore: handling node implementation <${impl}>`, {
      nodeImplementation: impl,
    });

    await this._handleNodeImplementation(impl);
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
    if (config == null || isEqual(config, this.tlsConfig))
      return Promise.resolve();
    Logger.info('NetworkStatusStore: received tls config from main process');
    this.api.ada.setRequestConfig(config);
    runInAction('updating tlsConfig', () => {
      this.tlsConfig = config;
    });
    this.actions.networkStatus.tlsConfigIsReady.trigger();
    return Promise.resolve();
  };

  _handleNodeImplementation = (
    nodeImplementation: CardanoNodeImplementation
  ) => {
    runInAction('updating nodeImplementation', () => {
      this.nodeImplementation = nodeImplementation;
      this.actions.networkStatus.nodeImplementationUpdate.trigger();
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
        await this._requestCardanoStatus();
        break;
      case CardanoNodeStates.STOPPING:
      case CardanoNodeStates.EXITING:
      case CardanoNodeStates.UPDATING:
        runInAction('updating tlsConfig', () => {
          this.tlsConfig = null;
        });
        this._setDisconnected(wasConnected);
        this.stores.nodeUpdate.hideUpdateDialog();
        this.stores.app._closeActiveDialog();
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
      cardanoNodeID,
    } = from;

    return {
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
      cardanoNodeID,
    };
  };

  // DEFINE ACTIONS

  @action _updateNetworkStatus = async (
    queryInfoParams?: NetworkInfoQueryParams
  ) => {
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this.tlsConfig) return;

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
      const networkStatus: GetNetworkInfoResponse = isForcedTimeDifferenceCheck
        ? await this.forceCheckTimeDifferenceRequest.execute(queryInfoParams)
            .promise
        : await this.getNetworkInfoRequest.execute().promise;

      // In case we no longer have TLS config we ignore all API call responses
      // as this means we are in the Cardano shutdown (stopping|exiting|updating) sequence
      if (!this.tlsConfig) {
        Logger.debug(
          'NetworkStatusStore: Ignoring NetworkStatusRequest result during Cardano shutdown sequence...'
        );
        return;
      }

      const {
        // subscriptionStatus,
        syncProgress,
        localTip,
        networkTip,
        localTimeInformation,
      } = networkStatus;

      // We got response which means node is responding
      runInAction('update isNodeResponding', () => {
        this.isNodeResponding = true;
      });

      // Node is subscribed in case it is subscribed to at least one other node in the network
      runInAction('update isNodeSubscribed', () => {
        // TODO: Resolve once mock is removed
        // const nodeIPs = Object.values(subscriptionStatus || {});
        // this.isNodeSubscribed = nodeIPs.includes('subscribed');
        this.isNodeSubscribed = true;
      });

      runInAction('update localTip and networkTip', () => {
        this.localTip = localTip;
        this.networkTip = networkTip;
      });

      // System time is correct if local time difference is below allowed threshold
      runInAction('update localTimeDifference and isNodeTimeCorrect', () => {
        // Update localTimeDifference only in case NTP check status is not still pending
        if (localTimeInformation.status !== 'pending') {
          this.localTimeDifference = localTimeInformation.difference;
          const { isNodeTimeCorrect: isNodeTimeCorrectPrev } = this;
          const isNodeTimeCorrectNext =
            this.localTimeDifference != null && // If we receive 'null' it means NTP check failed
            this.localTimeDifference <= ALLOWED_TIME_DIFFERENCE;

          if (
            !isNodeTimeCorrectNext &&
            isNodeTimeCorrectPrev &&
            this._numberOfNTPRechecks < MAX_NTP_RECHECKS
          ) {
            this._numberOfNTPRechecks++;
            this._ntpRecheckTimeout = setTimeout(
              this.forceCheckLocalTimeDifference,
              NTP_RECHECK_TIMEOUT
            );
          } else {
            this._resetNTPRechecks();
            this.isNodeTimeCorrect = isNodeTimeCorrectNext;
          }
        }
      });

      if (
        this._networkStatus === NETWORK_STATUS.CONNECTING &&
        this.isNodeSubscribed
      ) {
        // We are connected for the first time, move on to syncing stage
        this._networkStatus = NETWORK_STATUS.SYNCING;
        const connectingTimeDelta = this._getStartupTimeDelta();
        Logger.info(`Connected after ${connectingTimeDelta} milliseconds`, {
          connectingTimeDelta,
        });
      }

      // Update sync progress
      runInAction('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });

      runInAction('update block heights', () => {
        /*
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
        */

        // Node is syncing in case we are receiving blocks and they are not stalling
        runInAction('update isNodeSyncing', () => {
          // TODO: revert once mocks removed
          // this.isNodeSyncing =
          //   hasStartedReceivingBlocks &&
          //   (isLocalBlockHeightSyncing || isNetworkBlockHeightSyncing);
          this.isNodeSyncing = true;
        });

        runInAction('update isNodeInSync', () => {
          // TODO: revert once mocks removed
          // const remainingUnsyncedBlocks =
          //   this.networkBlockHeight - this.localBlockHeight;
          // this.isNodeInSync =
          //   this.isNodeSyncing &&
          //   remainingUnsyncedBlocks <= UNSYNCED_BLOCKS_ALLOWED;
          this.isNodeInSync = true;
        });

        /*
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
        */
      });

      if (this._networkStatus === NETWORK_STATUS.SYNCING && this.isNodeInSync) {
        // We are synced for the first time, move on to running stage
        this._networkStatus = NETWORK_STATUS.RUNNING;
        this.actions.networkStatus.isSyncedAndReady.trigger();
        const syncingTimeDelta = this._getStartupTimeDelta();
        Logger.info(`Synced after ${syncingTimeDelta} milliseconds`, {
          syncingTimeDelta,
        });
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
      this._setDisconnected(wasConnected);
      if (error instanceof TlsCertificateNotValidError) {
        runInAction('set isTlsCertInvalid = true', () => {
          this.isTlsCertInvalid = true;
        });
      }
    }
  };

  @action _resetNTPRechecks = () => {
    clearTimeout(this._ntpRecheckTimeout);
    this._ntpRecheckTimeout = null;
    this._numberOfNTPRechecks = 0;
  };

  @action _setDisconnected = (wasConnected: boolean) => {
    this.isNodeResponding = false;
    this.isNodeSubscribed = false;
    this.isNodeSyncing = false;
    this.isNodeInSync = false;
    this._resetNTPRechecks();
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

  openStateDirectory(path: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openLocalDirectoryChannel.send(path);
  }

  @action _onCheckDiskSpace = ({
    isNotEnoughDiskSpace,
    diskSpaceRequired,
    diskSpaceMissing,
    diskSpaceRecommended,
    diskSpaceAvailable,
  }: CheckDiskSpaceResponse): Promise<void> => {
    this.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
    this.diskSpaceRequired = diskSpaceRequired;
    this.diskSpaceMissing = diskSpaceMissing;
    this.diskSpaceRecommended = diskSpaceRecommended;
    this.diskSpaceAvailable = diskSpaceAvailable;

    if (this.isNotEnoughDiskSpace) {
      if (this._networkStatusPollingInterval) {
        clearInterval(this._networkStatusPollingInterval);
        this._networkStatusPollingInterval = null;
        this._resetNTPRechecks();
      }
    } else if (!this._networkStatusPollingInterval) {
      this._setNetworkStatusPollingInterval();
    }

    return Promise.resolve();
  };

  @action _onReceiveStateDirectoryPath = (stateDirectoryPath: string) => {
    this.stateDirectoryPath = stateDirectoryPath;
  };

  // DEFINE COMPUTED VALUES
  @computed get isIncentivizedTestnet(): boolean {
    return this.nodeImplementation === NODE_IMPLEMENTATIONS.jormungandr;
  }

  @computed get isConnected(): boolean {
    return this.isNodeResponding && this.isNodeSubscribed && this.isNodeSyncing;
  }

  // @API TODO - uncomment checking once api v2 is integrated
  @computed get isSystemTimeCorrect(): boolean {
    // return this.isNodeTimeCorrect || this.isSystemTimeIgnored;
    return true;
  }

  @computed get isSynced(): boolean {
    return this.isConnected && this.isNodeInSync && this.isSystemTimeCorrect;
  }

  @computed get syncPercentage(): number {
    if (this.isIncentivizedTestnet) {
      return this.syncProgress || 0;
    }

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
