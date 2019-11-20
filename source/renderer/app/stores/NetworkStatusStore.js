// @flow
import { observable, action, computed, runInAction } from 'mobx';
import { isEqual, includes } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { NETWORK_STATUS_POLL_INTERVAL } from '../config/timingConfig';
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

  // Initialize store observables

  // Internal Node states
  @observable tlsConfig: ?TlsConfig = null;
  @observable cardanoNodeState: ?CardanoNodeState = null;
  @observable cardanoNodeID: number = 0;
  @observable isNodeResponding = false; // Is 'true' as long we are receiving node Api responses
  @observable isNodeSyncing = false; // Is 'true' in case we are receiving blocks and not stalling
  @observable isNodeInSync = false; // 'true' if syncing & local/network blocks diff within limit
  @observable isNodeStopping = false; // 'true' if node is in `NODE_STOPPING_STATES` states
  @observable isNodeStopped = false; // 'true' if node is in `NODE_STOPPED_STATES` states
  @observable isSplashShown = true; // Visibility of splash screen

  @observable hasBeenConnected = false;
  @observable syncProgress = null;
  @observable localTip: ?TipInfo = null;
  @observable networkTip: ?TipInfo = null;
  @observable
  getNetworkInfoRequest: Request<GetNetworkInfoResponse> = new Request(
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
    this.actions.networkStatus.toggleSplash.listen(this._toggleSplash);

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
    }
  }

  // ================= REACTIONS ==================

  _updateNetworkStatusWhenDisconnected = () => {
    if (!this.isConnected) this._updateNetworkStatus();
  };

  _updateNetworkStatusWhenConnected = () => {
    if (this.isConnected) {
      Logger.info('NetworkStatusStore: Connected');
      this._updateNetworkStatus();
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
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
      cardanoNodeID,
    } = from;

    return {
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
      cardanoNodeID,
    };
  };

  // DEFINE ACTIONS

  @action _updateNetworkStatus = async () => {
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this.tlsConfig) return;

    // Record connection status before running network status call
    const wasConnected = this.isConnected;

    try {
      const networkStatus: GetNetworkInfoResponse = await this.getNetworkInfoRequest.execute()
        .promise;

      // In case we no longer have TLS config we ignore all API call responses
      // as this means we are in the Cardano shutdown (stopping|exiting|updating) sequence
      if (!this.tlsConfig) {
        Logger.debug(
          'NetworkStatusStore: Ignoring NetworkStatusRequest result during Cardano shutdown sequence...'
        );
        return;
      }

      const { syncProgress, localTip, networkTip } = networkStatus;

      // We got response which means node is responding
      runInAction('update isNodeResponding', () => {
        this.isNodeResponding = true;
      });

      runInAction('update localTip and networkTip', () => {
        this.localTip = localTip;
        this.networkTip = networkTip;
      });

      if (this._networkStatus === NETWORK_STATUS.CONNECTING) {
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

      runInAction('update isNodeSyncing', () => {
        this.isNodeSyncing = true;
      });

      runInAction('update isNodeInSync', () => {
        this.isNodeInSync = this.syncProgress === 100;
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

  @action _setDisconnected = (wasConnected: boolean) => {
    this.isNodeResponding = false;
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
      }
    } else if (!this._networkStatusPollingInterval) {
      this._setNetworkStatusPollingInterval();
    }

    return Promise.resolve();
  };

  @action _onReceiveStateDirectoryPath = (stateDirectoryPath: string) => {
    this.stateDirectoryPath = stateDirectoryPath;
  };

  @action _toggleSplash = () => {
    runInAction('Toggle splash visibility', () => {
      this.isSplashShown = !this.isSplashShown;
    });
  };

  // DEFINE COMPUTED VALUES
  @computed get isIncentivizedTestnet(): boolean {
    return this.environment.isIncentivizedTestnet;
    //  ||
    // this.nodeImplementation === NODE_IMPLEMENTATIONS.jormungandr
  }

  @computed get isConnected(): boolean {
    return this.isNodeResponding && this.isNodeSyncing;
  }

  @computed get isSynced(): boolean {
    return this.isConnected && this.isNodeInSync;
  }

  @computed get syncPercentage(): number {
    return this.syncProgress || 0;
  }
}
