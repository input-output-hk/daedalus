import {
  action,
  computed,
  makeObservable,
  observable,
  runInAction,
} from 'mobx';
import moment from 'moment';
import { get, includes, isEqual } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  ALLOWED_TIME_DIFFERENCE,
  DECENTRALIZATION_LEVEL_POLLING_INTERVAL,
  MAX_ALLOWED_STALL_DURATION,
  NETWORK_CLOCK_POLL_INTERVAL,
  NETWORK_STATUS_POLL_INTERVAL,
} from '../config/timingConfig';
import { INITIAL_DESIRED_POOLS_NUMBER } from '../config/stakingConfig';
import {
  cardanoStateChangeChannel,
  cardanoTlsConfigChannel,
  getCachedCardanoStatusChannel,
  restartCardanoNodeChannel,
  setCachedCardanoStatusChannel,
} from '../ipc/cardano.ipc';
import {
  BlockSyncType,
  CardanoNodeState,
  CardanoNodeStates,
  CardanoStatus,
  TlsConfig,
} from '../../../common/types/cardano-node.types';
import { getDiskSpaceStatusChannel } from '../ipc/getDiskSpaceChannel';
import { getBlockSyncProgressChannel } from '../ipc/getBlockSyncChannel';
import { getStateDirectoryPathChannel } from '../ipc/getStateDirectoryPathChannel';
import type {
  FutureEpoch,
  GetNetworkClockResponse,
  GetNetworkInfoResponse,
  GetNetworkParametersResponse,
  NextEpoch,
  TipInfo,
} from '../api/network/types';
import type { GetBlockSyncProgressMainResponse } from '../../../common/ipc/api';
import type { CheckDiskSpaceResponse } from '../../../common/types/no-disk-space.types';
import { TlsCertificateNotValidError } from '../api/nodes/errors';
import { openLocalDirectoryChannel } from '../ipc/open-local-directory';
import { toggleRTSFlagsModeChannel } from '../ipc/toggleRTSFlagsModeChannel';
import { AnalyticsTracker, EventCategories } from '../analytics';
import { Api } from '../api';
import { ActionsMap } from '../actions';

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
const { isFlight } = global;
export default class NetworkStatusStore extends Store {
  // Initialize store properties
  _startTime = Date.now();
  _networkStatus = NETWORK_STATUS.CONNECTING;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkStatusPollingInterval: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkClockPollingInterval: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkParametersPollingInterval: IntervalID | null | undefined = null;
  // Initialize store observables
  // Internal Node states
  tlsConfig: TlsConfig | null | undefined = null;
  cardanoNodeState: CardanoNodeState = 'unknown';
  cardanoNodePID = 0;
  cardanoWalletPID = 0;
  isRTSFlagsModeEnabled = false;
  isNodeResponding = false; // Is 'true' as long we are receiving node Api responses

  isNodeSyncing = false; // Is 'true' in case we are receiving blocks and not stalling

  isNodeInSync = false; // Is 'true' if syncing & local/network blocks diff within limit

  isNodeStopping = false; // Is 'true' if node is in `NODE_STOPPING_STATES` states

  isNodeStopped = false; // Is 'true' if node is in `NODE_STOPPED_STATES` states

  isNodeTimeCorrect = true; // Is 'true' in case local and global time are in sync

  isSystemTimeIgnored = false; // Tracks if NTP time checks are ignored

  isSplashShown = isFlight; // Visibility of splash screen

  isSyncProgressStalling = false; // Is 'true' in case sync progress doesn't change within limit

  hasBeenConnected = false;
  syncProgress = null;
  localTip: TipInfo | null | undefined = null;
  networkTip: TipInfo | null | undefined = null;
  nextEpoch: NextEpoch | null | undefined = null;
  futureEpoch: FutureEpoch | null | undefined = null;
  lastSyncProgressChangeTimestamp = 0; // milliseconds

  localTimeDifference: number | null | undefined = 0; // microseconds

  decentralizationProgress = 0; // percentage

  desiredPoolNumber: number = INITIAL_DESIRED_POOLS_NUMBER;
  getNetworkInfoRequest: Request<GetNetworkInfoResponse> = new Request(
    this.api.ada.getNetworkInfo
  );
  getNetworkClockRequest: Request<GetNetworkClockResponse> = new Request(
    this.api.ada.getNetworkClock
  );
  getNetworkParametersRequest: Request<GetNetworkParametersResponse> =
    new Request(this.api.ada.getNetworkParameters);
  isNotEnoughDiskSpace = false;
  diskSpaceRequired = '';
  diskSpaceMissing = '';
  diskSpaceRecommended = '';
  diskSpaceAvailable = '';
  isTlsCertInvalid = false;
  stateDirectoryPath = '';
  isShelleyActivated = false;
  isShelleyPending = false;
  isAlonzoActivated = false;
  shelleyActivationTime = '';
  isAlonzoPending = false;
  alonzoActivationTime = '';
  blockSyncProgress: Record<BlockSyncType, number> = {
    [BlockSyncType.validatingChunk]: 0,
    [BlockSyncType.replayedBlock]: 0,
    [BlockSyncType.pushingLedger]: 0,
  };
  epochLength: number | null | undefined = null; // unit: 1 slot

  slotLength: number | null | undefined = null; // unit: 1 second

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      tlsConfig: observable,
      cardanoNodeState: observable,
      cardanoNodePID: observable,
      cardanoWalletPID: observable,
      isRTSFlagsModeEnabled: observable,
      isNodeResponding: observable,
      isNodeSyncing: observable,
      isNodeInSync: observable,
      isNodeStopping: observable,
      isNodeStopped: observable,
      isNodeTimeCorrect: observable,
      isSystemTimeIgnored: observable,
      isSplashShown: observable,
      isSyncProgressStalling: observable,
      hasBeenConnected: observable,
      syncProgress: observable,
      localTip: observable,
      networkTip: observable,
      nextEpoch: observable,
      futureEpoch: observable,
      lastSyncProgressChangeTimestamp: observable,
      localTimeDifference: observable,
      decentralizationProgress: observable,
      desiredPoolNumber: observable,
      getNetworkInfoRequest: observable,
      getNetworkClockRequest: observable,
      getNetworkParametersRequest: observable,
      isNotEnoughDiskSpace: observable,
      diskSpaceRequired: observable,
      diskSpaceMissing: observable,
      diskSpaceRecommended: observable,
      diskSpaceAvailable: observable,
      isTlsCertInvalid: observable,
      stateDirectoryPath: observable,
      isShelleyActivated: observable,
      isShelleyPending: observable,
      isAlonzoActivated: observable,
      shelleyActivationTime: observable,
      isAlonzoPending: observable,
      alonzoActivationTime: observable,
      blockSyncProgress: observable,
      epochLength: observable,
      slotLength: observable,
      _toggleRTSFlagsMode: action,
      _setNetworkStatusPollingInterval: action,
      _setNetworkClockPollingInterval: action,
      _setNetworkParametersPollingInterval: action,
      _clearNetworkStatusPollingInterval: action,
      _clearNetworkClockPollingInterval: action,
      _clearNetworkParametersPollingInterval: action,
      ignoreSystemTimeChecks: action,
      _forceCheckNetworkClock: action,
      _updateNetworkClock: action,
      _updateNetworkStatus: action,
      _setDisconnected: action,
      _getNetworkParameters: action,
      _onCheckDiskSpace: action,
      _onBlockSyncProgressUpdate: action,
      _onReceiveStateDirectoryPath: action,
      _toggleSplash: action,
      isConnected: computed,
      isSystemTimeCorrect: computed,
      isSynced: computed,
      syncPercentage: computed,
      absoluteSlotNumber: computed,
      isEpochsInfoAvailable: computed,
      isVerifyingBlockchain: computed,
    });
  }

  // DEFINE STORE METHODS
  setup() {
    // ========== IPC CHANNELS =========== //
    const { networkStatus: networkStatusActions } = this.actions;
    networkStatusActions.restartNode.listen(this._restartNode);
    networkStatusActions.toggleSplash.listen(this._toggleSplash);
    networkStatusActions.forceCheckNetworkClock.listen(
      this._forceCheckNetworkClock
    );
    networkStatusActions.toggleRTSFlagsMode.listen(this._toggleRTSFlagsMode);

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

    this._setNetworkClockPollingInterval();

    this._setNetworkParametersPollingInterval();

    // Setup disk space checks
    getDiskSpaceStatusChannel.onReceive(this._onCheckDiskSpace);
    this._checkDiskSpace();

    this._getStateDirectoryPath();

    // Blockchain verification checking
    getBlockSyncProgressChannel.onReceive(this._onBlockSyncProgressUpdate);
  }

  _restartNode = async () => {
    this._resetSystemTime();

    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: Requesting a restart of cardano-node');
      await restartCardanoNodeChannel.send();
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.error('NetworkStatusStore: Restart of cardano-node failed', {
        error,
      });
    }
  };

  teardown() {
    super.teardown();

    // Teardown polling intervals
    this._clearNetworkStatusPollingInterval();

    this._clearNetworkClockPollingInterval();

    this._clearNetworkParametersPollingInterval();
  }

  // ================= REACTIONS ==================
  _updateNetworkStatusWhenDisconnected = () => {
    if (!this.isConnected) {
      this._updateNetworkStatus();

      if (!this._networkClockPollingInterval) {
        this._setNetworkClockPollingInterval();
      }
    }
  };
  _updateNetworkStatusWhenConnected = () => {
    if (this.isConnected) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: Connected');

      this._updateNetworkStatus();

      // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
      this.actions.walletMigration.startMigration.trigger();
    }
  };
  _updateNodeStatus = async () => {
    if (this.environment.isTest && !this.isConnected) return;

    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: Updating node status');
      await setCachedCardanoStatusChannel.send(this._extractNodeStatus(this));
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.error('NetworkStatusStore: Error while updating node status', {
        error,
      });
    }
  };

  // =============== PRIVATE ===============
  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }

  _checkDiskSpace() {
    getDiskSpaceStatusChannel.send();
  }

  _getStateDirectoryPath = async () => {
    this._onReceiveStateDirectoryPath(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
      await getStateDirectoryPathChannel.request()
    );
  };
  _requestCardanoState = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    console.info('NetworkStatusStore: requesting node state');
    const state = await cardanoStateChangeChannel.request();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    await this._handleCardanoNodeStateChange(state);
  };
  _requestCardanoStatus = async () => {
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: requesting node status');
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
      const status = await getCachedCardanoStatusChannel.request();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: received cached node status', {
        status,
      });
      if (status) runInAction(() => Object.assign(this, status));
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.error('NetworkStatusStore: error while requesting node state', {
        error,
      });
    }
  };
  _requestTlsConfig = async () => {
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info(
        'NetworkStatusStore: requesting tls config from main process'
      );
      const tlsConfig = await cardanoTlsConfigChannel.request();
      await this._updateTlsConfig(tlsConfig);
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.error('NetworkStatusStore: error while requesting tls config', {
        error,
      });
    }
  };
  // @ts-ignore
  _updateTlsConfig = (config: TlsConfig | null | undefined): Promise<void> => {
    if (config == null || isEqual(config, this.tlsConfig))
      return Promise.resolve();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    console.info('NetworkStatusStore: received tls config from main process');
    this.api.ada.setRequestConfig(config);
    runInAction(() => {
      this.tlsConfig = config;
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.networkStatus.tlsConfigIsReady.trigger();
    return Promise.resolve();
  };
  _handleCardanoNodeStateChange = async (state: CardanoNodeState) => {
    if (state === this.cardanoNodeState) return Promise.resolve();

    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
        runInAction(() => {
          this.tlsConfig = null;
        });

        this._setDisconnected(wasConnected);

        this.stores.app._closeActiveDialog();

        break;

      default:
        this._setDisconnected(wasConnected);
    }

    runInAction(() => {
      this.cardanoNodeState = state;
      this.isNodeStopping = includes(NODE_STOPPING_STATES, state);
      this.isNodeStopped = includes(NODE_STOPPED_STATES, state);
    });
    return Promise.resolve();
  };
  _extractNodeStatus = (
    from: Record<string, any> & CardanoStatus
  ): CardanoStatus => {
    const {
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
      cardanoNodePID,
      cardanoWalletPID,
      isRTSFlagsModeEnabled,
    } = from;
    return {
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      hasBeenConnected,
      cardanoNodePID,
      cardanoWalletPID,
      isRTSFlagsModeEnabled,
    };
  };

  // DEFINE ACTIONS
  _toggleRTSFlagsMode = async () => {
    this.analytics.sendEvent(
      EventCategories.SETTINGS,
      `RTS flags ${this.isRTSFlagsModeEnabled ? 'disabled' : 'enabled'}`
    );
    await toggleRTSFlagsModeChannel.send();
  };

  _setNetworkStatusPollingInterval = () => {
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus,
      NETWORK_STATUS_POLL_INTERVAL
    );
  };
  _setNetworkClockPollingInterval = () => {
    this._networkClockPollingInterval = setInterval(
      this._updateNetworkClock,
      NETWORK_CLOCK_POLL_INTERVAL
    );
  };
  _setNetworkParametersPollingInterval = () => {
    this._networkParametersPollingInterval = setInterval(
      this._getNetworkParameters,
      DECENTRALIZATION_LEVEL_POLLING_INTERVAL
    );
  };
  _clearNetworkStatusPollingInterval = () => {
    if (this._networkStatusPollingInterval) {
      clearInterval(this._networkStatusPollingInterval);
      this._networkStatusPollingInterval = null;
    }
  };
  _clearNetworkClockPollingInterval = () => {
    if (this._networkClockPollingInterval) {
      clearInterval(this._networkClockPollingInterval);
      this._networkClockPollingInterval = null;
    }
  };
  _clearNetworkParametersPollingInterval = () => {
    if (this._networkParametersPollingInterval) {
      clearInterval(this._networkParametersPollingInterval);
      this._networkParametersPollingInterval = null;
    }
  };
  ignoreSystemTimeChecks = (flag = true) => {
    this.isSystemTimeIgnored = flag;
  };
  _forceCheckNetworkClock = () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    console.info('NetworkStatusStore: Force checking network clock...');

    this._updateNetworkClock(true);
  };
  _updateNetworkClock = async (isForceCheck = false) => {
    // Skip checking network clock if we are not connected or system time is ignored
    if (!this.isNodeResponding || (this.isSystemTimeIgnored && !isForceCheck))
      return;

    // Cancel check if one is already running, unless we are trying to execute a force
    // check in which case we need to wait for previous request to be executed
    if (this.getNetworkClockRequest.isExecuting) {
      if (isForceCheck) {
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        await this.getNetworkClockRequest;
      } else {
        return;
      }
    }

    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    console.info('NetworkStatusStore: Checking network clock...', {
      isForceCheck,
    });

    try {
      const networkClock: GetNetworkClockResponse =
        await this.getNetworkClockRequest.execute({
          isForceCheck,
        }).promise;
      // System time is correct if local time difference is below allowed threshold
      runInAction(() => {
        // Update localTimeDifference only in case NTP check status is not still pending
        if (networkClock.status !== 'pending') {
          this.localTimeDifference = networkClock.offset;
          this.isNodeTimeCorrect =
            this.localTimeDifference != null && // If we receive 'null' it means NTP check failed
            Math.abs(this.localTimeDifference) <= ALLOWED_TIME_DIFFERENCE;

          this._clearNetworkClockPollingInterval();
        }
      });
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.info('NetworkStatusStore: Network clock response received', {
        localTimeDifference: this.localTimeDifference,
        isNodeTimeCorrect: this.isNodeTimeCorrect,
        allowedDifference: ALLOWED_TIME_DIFFERENCE,
        isForceCheck,
      });
    } catch (error) {} // eslint-disable-line
  };
  _updateNetworkStatus = async () => {
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this.tlsConfig) return;
    // Record connection status before running network status call
    const wasConnected = this.isConnected;

    try {
      const networkStatus: GetNetworkInfoResponse =
        await this.getNetworkInfoRequest.execute().promise;

      // In case we no longer have TLS config we ignore all API call responses
      // as this means we are in the Cardano shutdown (stopping|exiting|updating) sequence
      if (!this.tlsConfig) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        console.debug(
          'NetworkStatusStore: Ignoring NetworkStatusRequest result during Cardano shutdown sequence...'
        );
        return;
      }

      const { syncProgress, localTip, networkTip, nextEpoch } = networkStatus;
      let futureEpoch = null;

      if (nextEpoch && this.epochLength && this.slotLength) {
        const startDelta = this.epochLength * this.slotLength;
        futureEpoch = {
          epochNumber: nextEpoch.epochNumber ? nextEpoch.epochNumber + 1 : null,
          epochStart: nextEpoch.epochStart
            ? moment(nextEpoch.epochStart)
                .add(startDelta, 'seconds')
                .toISOString()
            : '',
        };
      }

      // We got response which means node is responding
      runInAction(() => {
        this.isNodeResponding = true;
      });
      runInAction(() => {
        this.localTip = localTip;
        this.networkTip = networkTip;
        this.nextEpoch = nextEpoch;
        this.futureEpoch = futureEpoch;
      });

      if (this._networkStatus === NETWORK_STATUS.CONNECTING) {
        // We are connected for the first time, move on to syncing stage
        this._networkStatus = NETWORK_STATUS.SYNCING;

        const connectingTimeDelta = this._getStartupTimeDelta();

        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        console.info(`Connected after ${connectingTimeDelta} milliseconds`, {
          connectingTimeDelta,
        });
      }

      // Update sync progress
      const lastSyncProgress = this.syncProgress;
      runInAction(() => {
        this.syncProgress = syncProgress;
      });
      runInAction(() => {
        this.isNodeInSync = this.syncProgress === 100;
      });
      runInAction(() => {
        // Check if sync progress is stalling
        const hasSyncProgressChanged = this.syncProgress !== lastSyncProgress;

        if (
          this.isNodeInSync || // Update last sync progress change timestamp if node is in sync
          hasSyncProgressChanged || // Sync progress change detected
          (!this.isSyncProgressStalling && // Guard against future timestamps / incorrect system time
            (this.lastSyncProgressChangeTimestamp > Date.now() ||
              (!this.isNodeTimeCorrect && !this.isSystemTimeIgnored)))
        ) {
          this.lastSyncProgressChangeTimestamp = Date.now(); // Record last sync progress change timestamp
        }

        const lastSyncProgressChangeStall = moment(Date.now()).diff(
          moment(this.lastSyncProgressChangeTimestamp)
        );
        this.isSyncProgressStalling =
          lastSyncProgressChangeStall > MAX_ALLOWED_STALL_DURATION;
      });
      runInAction(() => {
        this.isNodeSyncing = !this.isSyncProgressStalling;
      });

      if (this._networkStatus === NETWORK_STATUS.SYNCING && this.isNodeInSync) {
        // We are synced for the first time, move on to running stage
        this._networkStatus = NETWORK_STATUS.RUNNING;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.networkStatus.isSyncedAndReady.trigger();

        const syncingTimeDelta = this._getStartupTimeDelta();

        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        console.info(`Synced after ${syncingTimeDelta} milliseconds`, {
          syncingTimeDelta,
        });
      }

      // this._requestCardanoState();
      if (wasConnected !== this.isConnected) {
        if (!this.isConnected) {
          if (!this.hasBeenConnected) {
            runInAction(() => {
              this.hasBeenConnected = true;
            });
          }

          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          console.debug('NetworkStatusStore: Connection Lost. Reconnecting...');
        } else if (this.hasBeenConnected) {
          // Make sure all wallets data is fully reloaded after the connection is re-established
          this.stores.wallets.resetWalletsData();
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          console.debug('NetworkStatusStore: Connection Restored');
        }

        if (this.isTlsCertInvalid) {
          runInAction(() => {
            this.isTlsCertInvalid = false;
          });
        }
      }

      // Reset request errors since we've received a valid response
      if (this.getNetworkInfoRequest.error !== null) {
        this.getNetworkInfoRequest.reset();
      }
    } catch (error) {
      // Node is not responding, switch to disconnected state
      this._setDisconnected(wasConnected);

      if (error instanceof TlsCertificateNotValidError) {
        runInAction(() => {
          this.isTlsCertInvalid = true;
        });
      }
    }
  };
  _setDisconnected = (wasConnected: boolean) => {
    this.isNodeResponding = false;
    this.isNodeSyncing = false;
    this.isNodeInSync = false;

    this._resetSystemTime();

    if (wasConnected) {
      if (!this.hasBeenConnected) {
        runInAction(() => {
          this.hasBeenConnected = true;
        });
      }

      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      console.debug('NetworkStatusStore: Connection Lost. Reconnecting...');
    }
  };
  _getNetworkParameters = async () => {
    // Skip checking network parameters if we are not connected
    if (!this.isNodeResponding) return;

    try {
      const networkParameters: GetNetworkParametersResponse =
        await this.getNetworkParametersRequest.execute().promise;
      let {
        isShelleyActivated,
        isShelleyPending,
        shelleyActivationTime,
        isAlonzoActivated,
        isAlonzoPending,
        alonzoActivationTime,
      } = this;
      const {
        decentralizationLevel,
        desiredPoolNumber,
        slotLength,
        epochLength,
        eras,
      } = networkParameters;

      if (eras) {
        const currentTimeStamp = new Date().getTime();
        shelleyActivationTime = get(eras, 'shelley.epoch_start_time', '');

        if (shelleyActivationTime !== '') {
          const shelleyActivationTimeStamp = new Date(
            shelleyActivationTime
          ).getTime();
          isShelleyActivated = currentTimeStamp >= shelleyActivationTimeStamp;
          isShelleyPending = currentTimeStamp < shelleyActivationTimeStamp;
        }

        alonzoActivationTime = get(eras, 'alonzo.epoch_start_time', '');

        if (alonzoActivationTime !== '') {
          const alonzoActivationTimeStamp = new Date(
            alonzoActivationTime
          ).getTime();
          isAlonzoActivated = currentTimeStamp >= alonzoActivationTimeStamp;
          isAlonzoPending = currentTimeStamp < alonzoActivationTimeStamp;
        }
      }

      runInAction(() => {
        this.decentralizationProgress = decentralizationLevel.quantity;
        this.isShelleyActivated = isShelleyActivated;
        this.isShelleyPending = isShelleyPending;
        this.shelleyActivationTime = shelleyActivationTime;
        this.isAlonzoActivated = isAlonzoActivated;
        this.isAlonzoPending = isAlonzoPending;
        this.alonzoActivationTime = alonzoActivationTime;
      });
      runInAction(() => {
        this.desiredPoolNumber =
          desiredPoolNumber || INITIAL_DESIRED_POOLS_NUMBER;
      });
      runInAction(() => {
        this.slotLength = slotLength.quantity;
        this.epochLength = epochLength.quantity;
      });
    } catch (e) {
      runInAction(() => {
        this.decentralizationProgress = 0;
      });
    }
  };

  openStateDirectory(path: string, event?: MouseEvent): void {
    if (event) event.preventDefault();
    openLocalDirectoryChannel.send(path);
  }

  _onCheckDiskSpace = ({
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

  _onBlockSyncProgressUpdate = async (
    blockSyncProgress: GetBlockSyncProgressMainResponse
  ) => {
    this.blockSyncProgress = blockSyncProgress;
  };

  _onReceiveStateDirectoryPath = (stateDirectoryPath: string) => {
    this.stateDirectoryPath = stateDirectoryPath;
  };
  _toggleSplash = () => {
    runInAction(() => {
      this.isSplashShown = !this.isSplashShown;
    });
  };
  _resetSystemTime = () => {
    runInAction(() => {
      this.getNetworkClockRequest.reset();
      this.localTimeDifference = null;
      this.isNodeTimeCorrect = true;
      this.isSystemTimeIgnored = false;
    });
  };

  get isConnected(): boolean {
    return this.isNodeResponding && this.isNodeSyncing;
  }

  get isSystemTimeCorrect(): boolean {
    return this.isNodeTimeCorrect || this.isSystemTimeIgnored;
  }

  get isSynced(): boolean {
    return this.isConnected && this.isNodeInSync && this.isSystemTimeCorrect;
  }

  get syncPercentage(): number {
    return this.syncProgress || 0;
  }

  get absoluteSlotNumber(): number {
    const { networkTip } = this;
    return get(networkTip, 'absoluteSlotNumber', 0);
  }

  get isEpochsInfoAvailable(): boolean {
    const { networkTip, nextEpoch } = this;
    return (
      get(nextEpoch, 'epochNumber', null) !== null &&
      get(nextEpoch, 'epochStart', null) !== null &&
      get(networkTip, 'epoch', null) !== null &&
      get(networkTip, 'slot', null) !== null
    );
  }

  get isVerifyingBlockchain(): boolean {
    const isNotReady = Object.values(this.blockSyncProgress).some(
      (value) => value < 100
    );
    return !this.isConnected && isNotReady;
  }
}
