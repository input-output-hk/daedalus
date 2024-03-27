'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const moment_1 = __importDefault(require('moment'));
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const timingConfig_1 = require('../config/timingConfig');
const stakingConfig_1 = require('../config/stakingConfig');
const logging_1 = require('../utils/logging');
const cardano_ipc_1 = require('../ipc/cardano.ipc');
const cardano_node_types_1 = require('../../../common/types/cardano-node.types');
const getDiskSpaceChannel_1 = require('../ipc/getDiskSpaceChannel');
const getBlockSyncChannel_1 = require('../ipc/getBlockSyncChannel');
const getStateDirectoryPathChannel_1 = require('../ipc/getStateDirectoryPathChannel');
const errors_1 = require('../api/nodes/errors');
const open_local_directory_1 = require('../ipc/open-local-directory');
const toggleRTSFlagsModeChannel_1 = require('../ipc/toggleRTSFlagsModeChannel');
const analytics_1 = require('../analytics');
// DEFINE CONSTANTS -------------------------
const NETWORK_STATUS = {
  CONNECTING: 0,
  SYNCING: 1,
  RUNNING: 2,
};
const NODE_STOPPING_STATES = [
  cardano_node_types_1.CardanoNodeStates.EXITING,
  cardano_node_types_1.CardanoNodeStates.STOPPING,
  cardano_node_types_1.CardanoNodeStates.UPDATING,
];
const NODE_STOPPED_STATES = [
  cardano_node_types_1.CardanoNodeStates.CRASHED,
  cardano_node_types_1.CardanoNodeStates.ERRORED,
  cardano_node_types_1.CardanoNodeStates.STOPPED,
  cardano_node_types_1.CardanoNodeStates.UPDATED,
  cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE,
];
// END CONSTANTS ----------------------------
const { isFlight } = global;
class NetworkStatusStore extends Store_1.default {
  // Initialize store properties
  _startTime = Date.now();
  _networkStatus = NETWORK_STATUS.CONNECTING;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkStatusPollingInterval = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkClockPollingInterval = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _networkParametersPollingInterval = null;
  // Initialize store observables
  // Internal Node states
  tlsConfig = null;
  cardanoNodeState = 'unknown';
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
  localTip = null;
  networkTip = null;
  nextEpoch = null;
  futureEpoch = null;
  lastSyncProgressChangeTimestamp = 0; // milliseconds
  localTimeDifference = 0; // microseconds
  decentralizationProgress = 0; // percentage
  desiredPoolNumber = stakingConfig_1.INITIAL_DESIRED_POOLS_NUMBER;
  getNetworkInfoRequest = new LocalizedRequest_1.default(
    this.api.ada.getNetworkInfo
  );
  getNetworkClockRequest = new LocalizedRequest_1.default(
    this.api.ada.getNetworkClock
  );
  getNetworkParametersRequest = new LocalizedRequest_1.default(
    this.api.ada.getNetworkParameters
  );
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
  blockSyncProgress = {
    [cardano_node_types_1.BlockSyncType.validatingChunk]: 0,
    [cardano_node_types_1.BlockSyncType.replayedBlock]: 0,
    [cardano_node_types_1.BlockSyncType.pushingLedger]: 0,
  };
  epochLength = null; // unit: 1 slot
  slotLength = null; // unit: 1 second
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
    cardano_ipc_1.cardanoTlsConfigChannel.onReceive(this._updateTlsConfig);
    // Passively receive state changes of the cardano-node
    cardano_ipc_1.cardanoStateChangeChannel.onReceive(
      this._handleCardanoNodeStateChange
    );
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
    getDiskSpaceChannel_1.getDiskSpaceStatusChannel.onReceive(
      this._onCheckDiskSpace
    );
    this._checkDiskSpace();
    this._getStateDirectoryPath();
    // Blockchain verification checking
    getBlockSyncChannel_1.getBlockSyncProgressChannel.onReceive(
      this._onBlockSyncProgressUpdate
    );
  }
  _restartNode = async () => {
    this._resetSystemTime();
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        'NetworkStatusStore: Requesting a restart of cardano-node'
      );
      await cardano_ipc_1.restartCardanoNodeChannel.send();
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        'NetworkStatusStore: Restart of cardano-node failed',
        {
          error,
        }
      );
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
      logging_1.logger.info('NetworkStatusStore: Connected');
      this._updateNetworkStatus();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
      this.actions.walletMigration.startMigration.trigger();
    }
  };
  _updateNodeStatus = async () => {
    if (this.environment.isTest && !this.isConnected) return;
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('NetworkStatusStore: Updating node status');
      await cardano_ipc_1.setCachedCardanoStatusChannel.send(
        this._extractNodeStatus(this)
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        'NetworkStatusStore: Error while updating node status',
        {
          error,
        }
      );
    }
  };
  // =============== PRIVATE ===============
  _getStartupTimeDelta() {
    return Date.now() - this._startTime;
  }
  _checkDiskSpace() {
    getDiskSpaceChannel_1.getDiskSpaceStatusChannel.send();
  }
  _getStateDirectoryPath = async () => {
    this._onReceiveStateDirectoryPath(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
      await getStateDirectoryPathChannel_1.getStateDirectoryPathChannel.request()
    );
  };
  _requestCardanoState = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('NetworkStatusStore: requesting node state');
    const state = await cardano_ipc_1.cardanoStateChangeChannel.request();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      `NetworkStatusStore: handling node state <${state}>`,
      {
        state,
      }
    );
    await this._handleCardanoNodeStateChange(state);
  };
  _requestCardanoStatus = async () => {
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('NetworkStatusStore: requesting node status');
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
      const status = await cardano_ipc_1.getCachedCardanoStatusChannel.request();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('NetworkStatusStore: received cached node status', {
        status,
      });
      if (status)
        (0, mobx_1.runInAction)('assigning node status', () =>
          Object.assign(this, status)
        );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        'NetworkStatusStore: error while requesting node state',
        {
          error,
        }
      );
    }
  };
  _requestTlsConfig = async () => {
    try {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        'NetworkStatusStore: requesting tls config from main process'
      );
      const tlsConfig = await cardano_ipc_1.cardanoTlsConfigChannel.request();
      await this._updateTlsConfig(tlsConfig);
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        'NetworkStatusStore: error while requesting tls config',
        {
          error,
        }
      );
    }
  };
  // @ts-ignore
  _updateTlsConfig = (config) => {
    if (config == null || (0, lodash_1.isEqual)(config, this.tlsConfig))
      return Promise.resolve();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'NetworkStatusStore: received tls config from main process'
    );
    this.api.ada.setRequestConfig(config);
    (0, mobx_1.runInAction)('updating tlsConfig', () => {
      this.tlsConfig = config;
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.networkStatus.tlsConfigIsReady.trigger();
    return Promise.resolve();
  };
  _handleCardanoNodeStateChange = async (state) => {
    if (state === this.cardanoNodeState) return Promise.resolve();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      `NetworkStatusStore: handling cardano-node state <${state}>`,
      {
        state,
      }
    );
    const wasConnected = this.isConnected;
    switch (state) {
      case cardano_node_types_1.CardanoNodeStates.STARTING:
        break;
      case cardano_node_types_1.CardanoNodeStates.RUNNING:
        await this._requestTlsConfig();
        await this._requestCardanoStatus();
        break;
      case cardano_node_types_1.CardanoNodeStates.STOPPING:
      case cardano_node_types_1.CardanoNodeStates.EXITING:
      case cardano_node_types_1.CardanoNodeStates.UPDATING:
        (0, mobx_1.runInAction)('updating tlsConfig', () => {
          this.tlsConfig = null;
        });
        this._setDisconnected(wasConnected);
        this.stores.app._closeActiveDialog();
        break;
      default:
        this._setDisconnected(wasConnected);
    }
    (0, mobx_1.runInAction)('setting cardanoNodeState', () => {
      this.cardanoNodeState = state;
      this.isNodeStopping = (0, lodash_1.includes)(NODE_STOPPING_STATES, state);
      this.isNodeStopped = (0, lodash_1.includes)(NODE_STOPPED_STATES, state);
    });
    return Promise.resolve();
  };
  _extractNodeStatus = (from) => {
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
      analytics_1.EventCategories.SETTINGS,
      `RTS flags ${this.isRTSFlagsModeEnabled ? 'disabled' : 'enabled'}`
    );
    await toggleRTSFlagsModeChannel_1.toggleRTSFlagsModeChannel.send();
  };
  _setNetworkStatusPollingInterval = () => {
    this._networkStatusPollingInterval = setInterval(
      this._updateNetworkStatus,
      timingConfig_1.NETWORK_STATUS_POLL_INTERVAL
    );
  };
  _setNetworkClockPollingInterval = () => {
    this._networkClockPollingInterval = setInterval(
      this._updateNetworkClock,
      timingConfig_1.NETWORK_CLOCK_POLL_INTERVAL
    );
  };
  _setNetworkParametersPollingInterval = () => {
    this._networkParametersPollingInterval = setInterval(
      this._getNetworkParameters,
      timingConfig_1.DECENTRALIZATION_LEVEL_POLLING_INTERVAL
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
    logging_1.logger.info(
      'NetworkStatusStore: Force checking network clock...'
    );
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
    logging_1.logger.info('NetworkStatusStore: Checking network clock...', {
      isForceCheck,
    });
    try {
      const networkClock = await this.getNetworkClockRequest.execute({
        isForceCheck,
      }).promise;
      // System time is correct if local time difference is below allowed threshold
      (0, mobx_1.runInAction)(
        'update localTimeDifference and isNodeTimeCorrect',
        () => {
          // Update localTimeDifference only in case NTP check status is not still pending
          if (networkClock.status !== 'pending') {
            this.localTimeDifference = networkClock.offset;
            this.isNodeTimeCorrect =
              this.localTimeDifference != null && // If we receive 'null' it means NTP check failed
              Math.abs(this.localTimeDifference) <=
                timingConfig_1.ALLOWED_TIME_DIFFERENCE;
            this._clearNetworkClockPollingInterval();
          }
        }
      );
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        'NetworkStatusStore: Network clock response received',
        {
          localTimeDifference: this.localTimeDifference,
          isNodeTimeCorrect: this.isNodeTimeCorrect,
          allowedDifference: timingConfig_1.ALLOWED_TIME_DIFFERENCE,
          isForceCheck,
        }
      );
    } catch (error) {} // eslint-disable-line
  };
  _updateNetworkStatus = async () => {
    // In case we haven't received TLS config we shouldn't trigger any API calls
    if (!this.tlsConfig) return;
    // Record connection status before running network status call
    const wasConnected = this.isConnected;
    try {
      const networkStatus = await this.getNetworkInfoRequest.execute().promise;
      // In case we no longer have TLS config we ignore all API call responses
      // as this means we are in the Cardano shutdown (stopping|exiting|updating) sequence
      if (!this.tlsConfig) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.debug(
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
            ? (0, moment_1.default)(nextEpoch.epochStart)
                .add(startDelta, 'seconds')
                .toISOString()
            : '',
        };
      }
      // We got response which means node is responding
      (0, mobx_1.runInAction)('update isNodeResponding', () => {
        this.isNodeResponding = true;
      });
      (0, mobx_1.runInAction)(
        'update localTip, networkTip, nextEpoch and futureEpoch',
        () => {
          this.localTip = localTip;
          this.networkTip = networkTip;
          this.nextEpoch = nextEpoch;
          this.futureEpoch = futureEpoch;
        }
      );
      if (this._networkStatus === NETWORK_STATUS.CONNECTING) {
        // We are connected for the first time, move on to syncing stage
        this._networkStatus = NETWORK_STATUS.SYNCING;
        const connectingTimeDelta = this._getStartupTimeDelta();
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          `Connected after ${connectingTimeDelta} milliseconds`,
          {
            connectingTimeDelta,
          }
        );
      }
      // Update sync progress
      const lastSyncProgress = this.syncProgress;
      (0, mobx_1.runInAction)('update syncProgress', () => {
        this.syncProgress = syncProgress;
      });
      (0, mobx_1.runInAction)('update isNodeInSync', () => {
        this.isNodeInSync = this.syncProgress === 100;
      });
      (0, mobx_1.runInAction)('update isSyncProgressStalling', () => {
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
        const lastSyncProgressChangeStall = (0, moment_1.default)(
          Date.now()
        ).diff((0, moment_1.default)(this.lastSyncProgressChangeTimestamp));
        this.isSyncProgressStalling =
          lastSyncProgressChangeStall >
          timingConfig_1.MAX_ALLOWED_STALL_DURATION;
      });
      (0, mobx_1.runInAction)('update isNodeSyncing', () => {
        this.isNodeSyncing = !this.isSyncProgressStalling;
      });
      if (this._networkStatus === NETWORK_STATUS.SYNCING && this.isNodeInSync) {
        // We are synced for the first time, move on to running stage
        this._networkStatus = NETWORK_STATUS.RUNNING;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.networkStatus.isSyncedAndReady.trigger();
        const syncingTimeDelta = this._getStartupTimeDelta();
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(`Synced after ${syncingTimeDelta} milliseconds`, {
          syncingTimeDelta,
        });
      }
      if (wasConnected !== this.isConnected) {
        if (!this.isConnected) {
          if (!this.hasBeenConnected) {
            (0, mobx_1.runInAction)('update hasBeenConnected', () => {
              this.hasBeenConnected = true;
            });
          }
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.debug(
            'NetworkStatusStore: Connection Lost. Reconnecting...'
          );
        } else if (this.hasBeenConnected) {
          // Make sure all wallets data is fully reloaded after the connection is re-established
          this.stores.wallets.resetWalletsData();
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.debug('NetworkStatusStore: Connection Restored');
        }
        if (this.isTlsCertInvalid) {
          (0, mobx_1.runInAction)('set isTlsCertInvalid = false', () => {
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
      if (error instanceof errors_1.TlsCertificateNotValidError) {
        (0, mobx_1.runInAction)('set isTlsCertInvalid = true', () => {
          this.isTlsCertInvalid = true;
        });
      }
    }
  };
  _setDisconnected = (wasConnected) => {
    this.isNodeResponding = false;
    this.isNodeSyncing = false;
    this.isNodeInSync = false;
    this._resetSystemTime();
    if (wasConnected) {
      if (!this.hasBeenConnected) {
        (0, mobx_1.runInAction)('update hasBeenConnected', () => {
          this.hasBeenConnected = true;
        });
      }
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.debug(
        'NetworkStatusStore: Connection Lost. Reconnecting...'
      );
    }
  };
  _getNetworkParameters = async () => {
    // Skip checking network parameters if we are not connected
    if (!this.isNodeResponding) return;
    try {
      const networkParameters = await this.getNetworkParametersRequest.execute()
        .promise;
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
        shelleyActivationTime = (0, lodash_1.get)(
          eras,
          'shelley.epoch_start_time',
          ''
        );
        if (shelleyActivationTime !== '') {
          const shelleyActivationTimeStamp = new Date(
            shelleyActivationTime
          ).getTime();
          isShelleyActivated = currentTimeStamp >= shelleyActivationTimeStamp;
          isShelleyPending = currentTimeStamp < shelleyActivationTimeStamp;
        }
        alonzoActivationTime = (0, lodash_1.get)(
          eras,
          'alonzo.epoch_start_time',
          ''
        );
        if (alonzoActivationTime !== '') {
          const alonzoActivationTimeStamp = new Date(
            alonzoActivationTime
          ).getTime();
          isAlonzoActivated = currentTimeStamp >= alonzoActivationTimeStamp;
          isAlonzoPending = currentTimeStamp < alonzoActivationTimeStamp;
        }
      }
      (0, mobx_1.runInAction)('Update Decentralization Progress', () => {
        this.decentralizationProgress = decentralizationLevel.quantity;
        this.isShelleyActivated = isShelleyActivated;
        this.isShelleyPending = isShelleyPending;
        this.shelleyActivationTime = shelleyActivationTime;
        this.isAlonzoActivated = isAlonzoActivated;
        this.isAlonzoPending = isAlonzoPending;
        this.alonzoActivationTime = alonzoActivationTime;
      });
      (0, mobx_1.runInAction)('Update Desired Pool Number', () => {
        this.desiredPoolNumber =
          desiredPoolNumber || stakingConfig_1.INITIAL_DESIRED_POOLS_NUMBER;
      });
      (0, mobx_1.runInAction)('Update Epoch Config', () => {
        this.slotLength = slotLength.quantity;
        this.epochLength = epochLength.quantity;
      });
    } catch (e) {
      (0, mobx_1.runInAction)('Clear Decentralization Progress', () => {
        this.decentralizationProgress = 0;
      });
    }
  };
  openStateDirectory(path, event) {
    if (event) event.preventDefault();
    open_local_directory_1.openLocalDirectoryChannel.send(path);
  }
  _onCheckDiskSpace = ({
    isNotEnoughDiskSpace,
    diskSpaceRequired,
    diskSpaceMissing,
    diskSpaceRecommended,
    diskSpaceAvailable,
  }) => {
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
  _onBlockSyncProgressUpdate = async (blockSyncProgress) => {
    this.blockSyncProgress = blockSyncProgress;
  };
  _onReceiveStateDirectoryPath = (stateDirectoryPath) => {
    this.stateDirectoryPath = stateDirectoryPath;
  };
  _toggleSplash = () => {
    (0, mobx_1.runInAction)('Toggle splash visibility', () => {
      this.isSplashShown = !this.isSplashShown;
    });
  };
  _resetSystemTime = () => {
    (0, mobx_1.runInAction)('Reset system time', () => {
      this.getNetworkClockRequest.reset();
      this.localTimeDifference = null;
      this.isNodeTimeCorrect = true;
      this.isSystemTimeIgnored = false;
    });
  };
  get isConnected() {
    return this.isNodeResponding && this.isNodeSyncing;
  }
  get isSystemTimeCorrect() {
    return this.isNodeTimeCorrect || this.isSystemTimeIgnored;
  }
  get isSynced() {
    return this.isConnected && this.isNodeInSync && this.isSystemTimeCorrect;
  }
  get syncPercentage() {
    return this.syncProgress || 0;
  }
  get absoluteSlotNumber() {
    const { networkTip } = this;
    return (0, lodash_1.get)(networkTip, 'absoluteSlotNumber', 0);
  }
  get isEpochsInfoAvailable() {
    const { networkTip, nextEpoch } = this;
    return (
      (0, lodash_1.get)(nextEpoch, 'epochNumber', null) !== null &&
      (0, lodash_1.get)(nextEpoch, 'epochStart', null) !== null &&
      (0, lodash_1.get)(networkTip, 'epoch', null) !== null &&
      (0, lodash_1.get)(networkTip, 'slot', null) !== null
    );
  }
  get isVerifyingBlockchain() {
    return (
      !this.isConnected &&
      Object.values(this.blockSyncProgress).some((value) => value < 100)
    );
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'tlsConfig',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  NetworkStatusStore.prototype,
  'cardanoNodeState',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'cardanoNodePID',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'cardanoWalletPID',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isRTSFlagsModeEnabled',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeResponding',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeSyncing',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeInSync',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeStopping',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeStopped',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNodeTimeCorrect',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isSystemTimeIgnored',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isSplashShown',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isSyncProgressStalling',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'hasBeenConnected',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'syncProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'localTip',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'networkTip',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'nextEpoch',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'futureEpoch',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'lastSyncProgressChangeTimestamp',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  NetworkStatusStore.prototype,
  'localTimeDifference',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'decentralizationProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  NetworkStatusStore.prototype,
  'desiredPoolNumber',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NetworkStatusStore.prototype,
  'getNetworkInfoRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NetworkStatusStore.prototype,
  'getNetworkClockRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NetworkStatusStore.prototype,
  'getNetworkParametersRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isNotEnoughDiskSpace',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'diskSpaceRequired',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'diskSpaceMissing',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'diskSpaceRecommended',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'diskSpaceAvailable',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isTlsCertInvalid',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'stateDirectoryPath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isShelleyActivated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isShelleyPending',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isAlonzoActivated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'shelleyActivationTime',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'isAlonzoPending',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'alonzoActivationTime',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'blockSyncProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  NetworkStatusStore.prototype,
  'epochLength',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  NetworkStatusStore.prototype,
  'slotLength',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_toggleRTSFlagsMode',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_setNetworkStatusPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_setNetworkClockPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_setNetworkParametersPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_clearNetworkStatusPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_clearNetworkClockPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_clearNetworkParametersPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  'ignoreSystemTimeChecks',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_forceCheckNetworkClock',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_updateNetworkClock',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_updateNetworkStatus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_setDisconnected',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_getNetworkParameters',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_onCheckDiskSpace',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_onBlockSyncProgressUpdate',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_onReceiveStateDirectoryPath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NetworkStatusStore.prototype,
  '_toggleSplash',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'isConnected',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'isSystemTimeCorrect',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'isSynced',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'syncPercentage',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'absoluteSlotNumber',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'isEpochsInfoAvailable',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  NetworkStatusStore.prototype,
  'isVerifyingBlockchain',
  null
);
exports.default = NetworkStatusStore;
//# sourceMappingURL=NetworkStatusStore.js.map
