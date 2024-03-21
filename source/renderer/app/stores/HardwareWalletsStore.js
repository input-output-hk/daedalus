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
exports.AddressVerificationCheckStatuses = void 0;
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const semver_1 = __importDefault(require('semver'));
const ledgerjs_hw_app_cardano_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const Wallet_1 = require('../domains/Wallet');
const WalletAddress_1 = __importDefault(require('../domains/WalletAddress'));
const helper_1 = require('../../../common/utils/helper');
const hardwareWalletsConfig_1 = require('../config/hardwareWalletsConfig');
const api_1 = require('../../../common/ipc/api');
const txnsConfig_1 = require('../config/txnsConfig');
const getHardwareWalletChannel_1 = require('../ipc/getHardwareWalletChannel');
const shelleyLedger_1 = require('../utils/shelleyLedger');
const shelleyTrezor_1 = require('../utils/shelleyTrezor');
const hardware_wallets_types_1 = require('../../../common/types/hardware-wallets.types');
const formatters_1 = require('../utils/formatters');
const WalletTransaction_1 = require('../domains/WalletTransaction');
const hardwareWalletUtils_1 = require('../utils/hardwareWalletUtils');
const logging_1 = require('../utils/logging');
const analytics_1 = require('../analytics');
exports.AddressVerificationCheckStatuses = {
  VALID: 'valid',
  INVALID: 'invalid',
  REVERIFY: 'reverify',
};
const CARDANO_ADA_APP_POLLING_INTERVAL = 1000;
const DEFAULT_HW_NAME = 'Hardware Wallet';
const { network, isDev } = global.environment;
const hardwareWalletsNetworkConfig = (0,
hardwareWalletsConfig_1.getHardwareWalletsNetworkConfig)(network);
class HardwareWalletsStore extends Store_1.default {
  selectCoinsRequest = new LocalizedRequest_1.default(this.api.ada.selectCoins);
  sendMoneyRequest = new LocalizedRequest_1.default(
    this.api.ada.createExternalTransaction
  );
  // @TODO - improve types
  getPublicKeyRequest = new LocalizedRequest_1.default(
    this.api.ada.getPublicKey
  );
  // @TODO - improve types
  constructAddressRequest = new LocalizedRequest_1.default(
    this.api.ada.constructAddress
  );
  hardwareWalletsLocalDataRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getHardwareWalletsLocalData
  );
  setHardwareWalletLocalDataRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setHardwareWalletLocalData
  );
  unsetHardwareWalletLocalDataRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetHardwareWalletLocalData
  );
  hardwareWalletDevicesRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getHardwareWalletDevices
  );
  setHardwareWalletDeviceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setHardwareWalletDevice
  );
  overrideHardwareWalletDevicesRequest = new LocalizedRequest_1.default(
    this.api.localStorage.overrideHardwareWalletDevices
  );
  unsetHardwareWalletDeviceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetHardwareWalletDevice
  );
  unsetHardwareWalletDevicesAllRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetHardwareWalletDevicesAll
  );
  unsetHardwareWalletLocalDataAllRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetHardwareWalletLocalDataAll
  );
  hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
  extendedPublicKey = null;
  // @ts-ignore ts-migrate(2741) FIXME: Property 'coinSelection' is missing in type '{}' b... Remove this comment to see the full error message
  txSignRequest = {};
  transportDevice = null;
  txBody = null;
  isTransactionPending = false;
  isTrezorBridgeInstalled = false;
  isTransactionInitiated = false;
  activeDevicePath = null;
  unfinishedWalletTxSigning = null;
  isListeningForDevice = false;
  isConnectInitiated = false;
  isAddressVerificationInitiated = false;
  isWalletPairingInitiated = false;
  unfinishedWalletAddressVerification = null;
  isAddressDerived = false;
  isAddressChecked = false;
  isAddressCorrect = null;
  // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  tempAddressToVerify = {};
  isExportKeyAborted = false;
  activeDelegationWalletId = null;
  activeVotingWalletId = null;
  votingData = null;
  cardanoAdaAppPoller;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  checkTransactionTimeInterval = null;
  connectedHardwareWalletsDevices = new Map();
  setup() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] HWStore - setup');
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.sendMoney.listen(this._sendMoney);
    hardwareWalletsActions.refreshHardwareWalletsLocalData.listen(
      this._refreshHardwareWalletsLocalData
    );
    getHardwareWalletChannel_1.getHardwareWalletConnectionChannel.onReceive(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(params: HardwareWalletConnectio... Remove this comment to see the full error message
      this._changeHardwareWalletConnectionStatus
    );
    this.initTrezor();
    this.initLedger();
    this.hardwareWalletsLocalDataRequest.execute();
    this.hardwareWalletDevicesRequest.execute();
  }
  initTrezor = async () => {
    if (
      hardwareWalletsConfig_1.isHardwareWalletSupportEnabled &&
      hardwareWalletsConfig_1.isTrezorEnabled
    ) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - start trezor');
      await getHardwareWalletChannel_1.handleInitTrezorConnectChannel.request();
      await this.getAvailableDevices({
        isTrezor: true,
      });
    }
  };
  initLedger = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      `[HW-DEBUG] HWStore - initLedger() | isHardwareWalletSupportEnabled=${hardwareWalletsConfig_1.isHardwareWalletSupportEnabled.toString()} isLedgerEnabled=${hardwareWalletsConfig_1.isLedgerEnabled.toString()}`
    );
    if (
      hardwareWalletsConfig_1.isHardwareWalletSupportEnabled &&
      hardwareWalletsConfig_1.isLedgerEnabled
    ) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - start ledger');
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.hardwareWalletDevicesRequest.execute();
      const storedDevices = this.hardwareWalletDevicesRequest.result;
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - storedDevices fetched');
      const devicesWithoutLedgers = {};
      (0, lodash_1.map)(storedDevices, async (device) => {
        if (device.deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
          devicesWithoutLedgers[device.id] = device;
        }
      });
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - Remove all LEDGERS from LC');
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.overrideHardwareWalletDevicesRequest.execute(
        devicesWithoutLedgers
      );
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - Refresh LC');
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - INIT Ledger listeners');
      await getHardwareWalletChannel_1.handleInitLedgerConnectChannel.request();
      await this.getAvailableDevices({
        isTrezor: false,
      });
    }
  };
  waitForLedgerDevicesToConnect = async () => {
    const device = await getHardwareWalletChannel_1.waitForLedgerDevicesToConnectChannel.request();
    this.connectedHardwareWalletsDevices.set(device.path, device);
    return device;
  };
  useCardanoAppInterval = (devicePath, txWalletId, verificationAddress) => {
    this.cardanoAdaAppPoller?.stop();
    const poller = () => {
      let canRun = true;
      let isRunning = false;
      const connectedDevice = this.connectedHardwareWalletsDevices.get(
        devicePath
      );
      const product =
        connectedDevice?.deviceType === 'ledger'
          ? connectedDevice?.product
          : null;
      const run = async () => {
        try {
          if (!canRun) {
            return;
          }
          isRunning = true;
          await this.getCardanoAdaApp({
            path: devicePath,
            walletId: txWalletId,
            address: verificationAddress,
            product,
          });
        } catch (_error) {
          if (!canRun) {
            return;
          }
          isRunning = false;
          setTimeout(run, CARDANO_ADA_APP_POLLING_INTERVAL);
        }
      };
      run();
      return {
        stop: () => {
          canRun = false;
          isRunning = false;
        },
        isRunning: () => isRunning,
      };
    };
    this.cardanoAdaAppPoller = poller();
  };
  getAvailableDevices = async (params) => {
    const { isTrezor } = params;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletsLocalDataRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletDevicesRequest.execute();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] HWStore - getAvailableDevices');
    // Set all logical HW into disconnected state
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Set Hardware Wallets local data'
    );
    (0, lodash_1.map)(
      this.hardwareWalletsConnectionData,
      async (connectedWallet) => {
        await this._setHardwareWalletLocalData({
          walletId: connectedWallet.id,
          data: {
            disconnected: true,
          },
        });
      }
    );
    // Initiate Device Check for each stored device
    (0, lodash_1.map)(this.hardwareWalletDevices, async (device) => {
      // Prevent device check if device is TREZOR and bridge not installed
      if (
        (!isTrezor &&
          device.deviceType !== hardware_wallets_types_1.DeviceTypes.LEDGER) ||
        (isTrezor &&
          (device.deviceType !== hardware_wallets_types_1.DeviceTypes.TREZOR ||
            (device.deviceType ===
              hardware_wallets_types_1.DeviceTypes.TREZOR &&
              !this.isTrezorBridgeInstalled)))
      ) {
        return;
      }
      try {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('[HW-DEBUG] HWStore - CHECK device');
        if (device.deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
          await getHardwareWalletChannel_1.getHardwareWalletTransportChannel.request(
            {
              // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
              devicePath: device.path,
              isTrezor: true,
            }
          );
        }
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        // eslint-disable-next-line
        logging_1.logger.info(' HWStore - CHECK device Error');
      }
    });
    await this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();
  };
  _sendMoney = async (params) => {
    const isDelegationTransaction = (0, lodash_1.get)(
      params,
      'isDelegationTransaction'
    );
    const isVotingRegistrationTransaction = (0, lodash_1.get)(
      params,
      'isVotingRegistrationTransaction'
    );
    const activeWalletId = (0, lodash_1.get)(this.stores.wallets, [
      'active',
      'id',
    ]);
    const selectedWalletId = (0, lodash_1.get)(params, 'selectedWalletId');
    const walletId = selectedWalletId || activeWalletId;
    if (!walletId) {
      throw new Error('Active wallet required before sending.');
    }
    this.setTransactionPendingState(true);
    try {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const transaction = await this.sendMoneyRequest.execute({
        signedTransactionBlob: this.txBody,
      });
      if (!isDelegationTransaction) {
        // Start interval to check transaction state every second
        this.checkTransactionTimeInterval = setInterval(
          this.checkTransaction,
          1000,
          {
            transactionId: transaction.id,
            walletId,
            isVotingRegistrationTransaction,
          }
        );
        this.analytics.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Transaction made',
          'Hardware wallet'
        );
      } else {
        this.setTransactionPendingState(false);
      }
      this.stores.wallets.refreshWalletsData();
      this.sendMoneyRequest.reset();
      return transaction;
    } catch (e) {
      this.setTransactionPendingState(false);
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: reset Transaction verifying',
        () => {
          this.txBody = null;
          this.activeDevicePath = null;
          this.unfinishedWalletTxSigning = null;
          this.votingData = null;
        }
      );
      throw e;
    }
  };
  // Check stake pool transaction state and reset pending state when transaction is "in_ledger"
  checkTransaction = (request) => {
    const {
      transactionId,
      walletId,
      isVotingRegistrationTransaction,
    } = request;
    const recentTransactionsResponse = this.stores.transactions._getTransactionsRecentRequest(
      walletId
    ).result;
    const recentTransactions = recentTransactionsResponse
      ? recentTransactionsResponse.transactions
      : [];
    let targetTransaction;
    if (isVotingRegistrationTransaction) {
      // Return transaction when state is not "PENDING"
      targetTransaction = (0, lodash_1.find)(
        recentTransactions,
        (transaction) => transaction.id === transactionId
      );
      if (targetTransaction) {
        // Reset Poller
        if (this.checkTransactionTimeInterval) {
          clearInterval(this.checkTransactionTimeInterval);
          this.checkTransactionTimeInterval = null;
        }
        // Reset pending transaction
        this.setTransactionPendingState(false);
        // Start voting poller and go to the next step
        this.stores.voting._startTransactionPolling();
        this.stores.voting._nextRegistrationStep();
      }
    } else {
      // Return transaction when state is not "PENDING"
      targetTransaction = (0, lodash_1.find)(
        recentTransactions,
        (transaction) =>
          transaction.id === transactionId &&
          transaction.state === WalletTransaction_1.TransactionStates.OK
      );
      if (targetTransaction) {
        this.resetStakePoolTransactionChecker(walletId);
      }
    }
  };
  resetStakePoolTransactionChecker = (walletId) => {
    if (this.checkTransactionTimeInterval) {
      clearInterval(this.checkTransactionTimeInterval);
      this.checkTransactionTimeInterval = null;
    }
    this.stores.wallets.refreshWalletsData();
    this.isTransactionPending = false;
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.dialogs.closeActiveDialog.trigger();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this._resetTransaction();
    this.stores.wallets.goToWalletRoute(walletId);
  };
  setTransactionPendingState = (isTransactionPending) => {
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set transaction state',
      () => {
        this.isTransactionPending = isTransactionPending;
      }
    );
  };
  // @TODO - move to Transactions store once all logic fit and hardware wallets listed in general wallets list
  selectCoins = async (params) => {
    const { walletId, address, amount, assets, metadata } = params;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coins selections.');
    const { amount: totalAmount, availableAmount, reward } = wallet;
    try {
      this.selectCoinsRequest.reset();
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const coinSelection = await this.selectCoinsRequest.execute({
        walletId,
        walletBalance: totalAmount,
        availableBalance: availableAmount.plus(reward),
        rewardsBalance: reward,
        payments: {
          address,
          amount,
          assets,
        },
        metadata,
      });
      return coinSelection;
    } catch (e) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw e;
    }
  };
  updateTxSignRequest = (coinSelection) => {
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set coin selections',
      () => {
        this.txSignRequest = {
          coinSelection,
        };
      }
    );
  };
  selectDelegationCoins = async (params) => {
    const { walletId, poolId, delegationAction } = params;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coins selections.');
    const { amount: totalAmount, availableAmount, reward } = wallet;
    try {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const coinSelection = await this.selectCoinsRequest.execute({
        walletId,
        walletBalance: totalAmount,
        availableBalance: availableAmount.plus(reward),
        rewardsBalance: reward,
        delegation: {
          poolId,
          delegationAction,
        },
      });
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set coin selections',
        () => {
          this.txSignRequest = {
            coinSelection,
          };
        }
      );
      return coinSelection;
    } catch (e) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw e;
    }
  };
  establishHardwareWalletConnection = async () => {
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set HW device CONNECTING',
      () => {
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
      }
    );
    const { hardwareWalletDevices, hardwareWalletsConnectionData } = this;
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - establishHardwareWalletConnection',
      {
        hardwareWalletDevices: (0, helper_1.toJS)(hardwareWalletDevices),
        hardwareWalletsConnectionData: (0, helper_1.toJS)(
          hardwareWalletsConnectionData
        ),
        activeDelegationWalletId: (0, helper_1.toJS)(
          this.activeDelegationWalletId
        ),
        isTransactionInitiated: (0, helper_1.toJS)(this.isTransactionInitiated),
      }
    );
    try {
      // Check if active wallet exist - this means that hw exist but we need to check if relevant device connected to it
      let recognizedPairedHardwareWallet;
      let relatedConnectionData;
      let activeWalletId;
      if (
        (this.activeDelegationWalletId || this.activeVotingWalletId) &&
        this.isTransactionInitiated
      ) {
        // Active wallet can be different that wallet we want to delegate
        if (this.activeDelegationWalletId) {
          activeWalletId = this.activeDelegationWalletId;
        }
        if (this.activeVotingWalletId) {
          activeWalletId = this.activeVotingWalletId;
        }
      } else {
        // For regular tx we are using active wallet
        activeWalletId = (0, lodash_1.get)(this.stores.wallets, [
          'active',
          'id',
        ]);
      }
      if (activeWalletId) {
        // Check if device connected to wallet
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('[HW-DEBUG] HWStore - active wallet exists');
        recognizedPairedHardwareWallet = (0, lodash_1.find)(
          hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
          (recognizedDevice) => recognizedDevice.paired === activeWalletId
        );
        relatedConnectionData = (0, lodash_1.find)(
          hardwareWalletsConnectionData,
          (connection) => connection.id === activeWalletId
        );
      }
      const lastUnpairedDevice = this.getLastUnpairedDevice();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: START',
        {
          lastUnpairedDevice: (0, helper_1.toJS)(lastUnpairedDevice),
        }
      );
      // Tx Special cases!
      // This means that transaction needs to be signed but we don't know device connected to Software wallet
      let transportDevice;
      if (this.isTransactionInitiated || this.isAddressVerificationInitiated) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - check device'
        );
        // Return device that belongs to active hardware wallet if is already plugged-in
        if (
          recognizedPairedHardwareWallet &&
          !recognizedPairedHardwareWallet.disconnected
        ) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - Recognized device found'
          );
          logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 1', {
            recognizedPairedHardwareWallet: (0, helper_1.toJS)(
              recognizedPairedHardwareWallet
            ),
          });
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Set transport device',
            () => {
              this.transportDevice = recognizedPairedHardwareWallet;
            }
          );
          // Special case when Pub key export rejected by the user and then device reconnected
          // Force export again and proceed (continue) with last action
          const isTrezor =
            recognizedPairedHardwareWallet.deviceType ===
            hardware_wallets_types_1.DeviceTypes.TREZOR;
          if (this.isExportKeyAborted) {
            if (isTrezor) {
              await this._identifyAndHandleAssociatedWallet({
                address: this.unfinishedWalletAddressVerification,
                expectedWalletId: activeWalletId,
                path: recognizedPairedHardwareWallet.path,
              });
            } else {
              this.useCardanoAppInterval(
                recognizedPairedHardwareWallet.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            }
          }
          // End of special case
          return recognizedPairedHardwareWallet;
        }
        // Device not recognized or not plugged-in. Wait for next device (check by device type)
        const relatedConnectionDataDeviceType = (0,
        lodash_1.get)(relatedConnectionData, ['device', 'deviceType']);
        const isTrezor =
          relatedConnectionDataDeviceType ===
          hardware_wallets_types_1.DeviceTypes.TREZOR;
        let lastDeviceTransport = null;
        if (relatedConnectionDataDeviceType) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - Connect - New Transaction / Address verification initiated - return last device'
          );
          // @ts-ignore
          lastDeviceTransport = await getHardwareWalletChannel_1.getHardwareWalletTransportChannel.request(
            {
              devicePath: null,
              // Use last plugged device
              isTrezor,
            }
          );
          logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 2', {
            lastDeviceTransport: (0, helper_1.toJS)(lastDeviceTransport),
          });
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Set transport device',
            () => {
              this.transportDevice = lastDeviceTransport;
            }
          );
          // Special case when Pub key export rejected by the user and then device reconnected
          // Force export again and proceed (continue) with last action
          if (this.isExportKeyAborted) {
            if (isTrezor) {
              await this._identifyAndHandleAssociatedWallet({
                address: this.unfinishedWalletAddressVerification,
                expectedWalletId: activeWalletId,
                path: lastDeviceTransport.path,
              });
            } else {
              this.useCardanoAppInterval(
                lastDeviceTransport.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            }
          } // End of special case
        }
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - return lastDeviceTransport',
          {
            lastDeviceTransport: (0, helper_1.toJS)(lastDeviceTransport),
          }
        );
        return lastDeviceTransport;
      }
      // End of Tx Special cases!
      // Cases for wallet create / restore
      // it is triggered after flag activation "isListeningForDevice"
      if (lastUnpairedDevice) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Start process with last UNPAIRED device'
        );
        // Start listeners for specific (last plugged) device
        let devicePath = null;
        let isTrezor = false;
        if (lastUnpairedDevice) {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
          devicePath = lastUnpairedDevice.path;
          isTrezor =
            lastUnpairedDevice.deviceType ===
            hardware_wallets_types_1.DeviceTypes.TREZOR;
        }
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Listening for device'
        );
        if (
          lastUnpairedDevice.deviceType ===
          hardware_wallets_types_1.DeviceTypes.TREZOR
        ) {
          transportDevice = await getHardwareWalletChannel_1.getHardwareWalletTransportChannel.request(
            {
              devicePath,
              isTrezor,
            }
          );
        } else {
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: wait for new ledger devices'
          );
          const ledgerDevice = await this.waitForLedgerTransportDevice();
          this.stopCardanoAdaAppFetchPoller();
          // @ts-ignore ts-migrate(2554) FIXME: Expected 5 arguments, but got 3.
          this.useCardanoAppInterval(ledgerDevice.path);
          return null;
        }
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info('[HW-DEBUG] HWStore - Transport retrieved');
      } else {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Set device listener'
        );
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set device listener',
          () => {
            this.isListeningForDevice = true;
          }
        );
        return null;
      }
      // End of Cases for wallet create / restore
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: start process with known transport'
      );
      if (transportDevice) {
        const { deviceType, firmwareVersion } = transportDevice;
        // Check if device is supported
        if (
          (deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR &&
            !hardware_wallets_types_1.DeviceModels.TREZOR_T) ||
          (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER &&
            !hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S &&
            !hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S_PLUS &&
            !hardware_wallets_types_1.DeviceModels.LEDGER_NANO_X)
        ) {
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: set HW device CONNECTING FAILED - device not supported',
            () => {
              this.hwDeviceStatus =
                Wallet_1.HwDeviceStatuses.UNSUPPORTED_DEVICE;
            }
          );
          throw new Error('Device not Supported!');
        }
        // @TODO - missing firmware version for LEDGER
        // Check Firmware version
        if (deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
          const minFirmwareVersion =
            deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR
              ? hardwareWalletsConfig_1.MINIMAL_TREZOR_FIRMWARE_VERSION
              : hardwareWalletsConfig_1.MINIMAL_LEDGER_FIRMWARE_VERSION;
          const isFirmwareVersionValid = semver_1.default.gte(
            firmwareVersion,
            minFirmwareVersion
          );
          if (!isFirmwareVersionValid) {
            (0, mobx_1.runInAction)(
              'HardwareWalletsStore:: set HW device CONNECTING FAILED - wrong firmware',
              () => {
                this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.WRONG_FIRMWARE;
              }
            );
            throw new Error(
              `Firmware must be ${minFirmwareVersion} or greater!`
            );
          }
        }
        // All Checks pass - mark device as connected (set transport device for this session)
        logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 3', {
          transportDevice: (0, helper_1.toJS)(transportDevice),
        });
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set HW device CONNECTED',
          () => {
            this.transportDevice = transportDevice;
          }
        );
        logging_1.logger.info('[HW-DEBUG] deviceType', deviceType);
        if (deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
          await this._identifyAndHandleAssociatedWallet({
            path: transportDevice.path,
          });
        } else {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - START cardano app poller'
          );
          // Start poller to recognize if Cardano App is launched on device
          const devicePath = transportDevice.path;
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - getCardanoAdaApp - from  establishHardwareWalletConnection',
            {
              devicePath,
              connectedHardwareWalletsDevices: Array.from(
                this.connectedHardwareWalletsDevices.keys()
              ),
            }
          );
          this.stopCardanoAdaAppFetchPoller();
          // @ts-ignore ts-migrate(2554) FIXME: Expected 5 arguments, but got 3.
          this.useCardanoAppInterval(devicePath);
        }
      } else {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING_FAILED;
          }
        );
        throw new Error('Device not found');
      }
      return transportDevice;
    } catch (e) {
      if (e.statusCode === 28177) {
        throw new Error('Device is locked');
      }
      if (e.id === 'TransportLocked') {
        throw new Error('Transport Failure');
      }
      if (e.code === 'Transport_Missing' && !this.isTrezorBridgeInstalled) {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.TREZOR_BRIDGE_FAILURE;
          }
        );
        throw new Error('Trezor Bridge not installed!');
      }
      throw e;
    }
  };
  // Ledger method only
  getCardanoAdaApp = async (params) => {
    const { path, walletId, address, product } = params;
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - START FUNCTION getCardanoAdaApp PARAMS: ',
      {
        walletId,
        path,
        address,
      }
    );
    this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaApp = await getHardwareWalletChannel_1.getCardanoAdaAppChannel.request(
        {
          path,
          product,
        }
      );
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - cardanoAdaApp RESPONSE: ',
        (0, helper_1.toJS)(cardanoAdaApp)
      );
      // Cardano app recognized, stop poller
      this.stopCardanoAdaAppFetchPoller();
      if (cardanoAdaApp) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - cardanoAdaApp - Set device'
        );
        // Check is Cardano App version supported
        const cardanoAppVersion = `${cardanoAdaApp.major}.${cardanoAdaApp.minor}.${cardanoAdaApp.patch}`;
        const isValidAppVersion = semver_1.default.gte(
          cardanoAppVersion,
          hardwareWalletsConfig_1.MINIMAL_CARDANO_APP_VERSION
        );
        if (!isValidAppVersion) {
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: set HW device CONNECTING FAILED - wrong firmware',
            () => {
              this.hwDeviceStatus =
                Wallet_1.HwDeviceStatuses.WRONG_CARDANO_APP_VERSION;
            }
          );
          throw new Error(
            `Cardano app must be ${hardwareWalletsConfig_1.MINIMAL_CARDANO_APP_VERSION} or greater!`
          );
        }
        await this._identifyAndHandleAssociatedWallet({
          address,
          expectedWalletId: walletId,
          path,
        });
      }
    } catch (error) {
      logging_1.logger.info('[HW-DEBUG] HWStore - Cardano app fetching error', {
        error: (0, helper_1.toJS)(error),
      });
      const isDeviceBusy = (0, lodash_1.includes)(
        error.message,
        'Ledger Device is busy'
      );
      if (isDeviceBusy) {
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
        this.stopCardanoAdaAppFetchPoller();
        logging_1.logger.info('[HW-DEBUG] Device is busy: ', {
          walletId,
          error,
          address,
          isTransactionInitiated: this.isTransactionInitiated,
          isAddressVerificationInitiated: this.isAddressVerificationInitiated,
        });
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING_FAILED;
            this.activeDevicePath = null;
            this.unfinishedWalletTxSigning = this.isTransactionInitiated
              ? walletId
              : null;
            this.unfinishedWalletAddressVerification = this
              .isAddressVerificationInitiated
              ? address
              : null;
          }
        );
      }
      if (
        error.code === api_1.DEVICE_NOT_CONNECTED &&
        !this.isTransactionInitiated &&
        !this.isAddressVerificationInitiated &&
        !this.isWalletPairingInitiated
      ) {
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        logging_1.logger.info(
          '[HW-DEBUG] HW Store::getCardanoAdaApp::DEVICE_NOT_CONNECTED'
        );
        this.stopCardanoAdaAppFetchPoller();
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Re-run initiated connection',
          () => {
            this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
            this.isListeningForDevice = true;
          }
        );
      } else if (error.code === 'DEVICE_PATH_CHANGED' && error.path) {
        // Special case on Windows where device path changes after opening Cardano app
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();
        const pairedDevice = (0, lodash_1.find)(
          this.hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
          (recognizedDevice) => recognizedDevice.path === path
        );
        logging_1.logger.info(
          '[HW-DEBUG] HW Store::getCardanoAdaApp::DEVICE_PATH_CHANGED',
          {
            path,
            newPath: error.path,
            pairedDevice,
            walletId,
          }
        );
        if (this.isWalletPairingInitiated) {
          logging_1.logger.info(
            '[HW-DEBUG] HW Store::getCardanoAdaApp::wallet pairing::Retry with new path',
            {
              newPath: error.path,
            }
          );
          this.useCardanoAppInterval(error.path, walletId, address);
          return;
        }
        if (
          !pairedDevice &&
          walletId &&
          (this.isTransactionInitiated || this.isAddressVerificationInitiated)
        ) {
          this.useCardanoAppInterval(error.path, walletId, address);
          throw error;
        }
        // Update device with new path - LC
        await this._setHardwareWalletDevice({
          deviceId: pairedDevice.id,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ path: any; isPending: false; id: string; d... Remove this comment to see the full error message
          data: { ...pairedDevice, path: error.path, isPending: false },
        });
        // Update connected wallet data with new path - LC
        if (walletId) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] Update connected wallet data with new path'
          );
          const hardwareWalletConnectionData = (0, lodash_1.get)(
            this.hardwareWalletsConnectionData,
            walletId
          );
          if (hardwareWalletConnectionData) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info(
              '[HW-DEBUG] Update connected wallet data with new path - Set to LC'
            );
            await this._setHardwareWalletLocalData({
              walletId,
              data: {
                ...hardwareWalletConnectionData,
                // @ts-ignore ts-migrate(2322) FIXME: Type '{ path: any; device: { path: any; deviceId: ... Remove this comment to see the full error message
                path: error.path,
                device: {
                  ...hardwareWalletConnectionData.device,
                  path: error.path,
                },
              },
            });
          }
        }
        if (
          this.isTransactionInitiated ||
          this.isAddressVerificationInitiated
        ) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] Update connected wallet data with new path - Set to LC'
          );
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Change active device path for Transaction send',
            () => {
              this.activeDevicePath = error.path;
            }
          );
        }
        this.useCardanoAppInterval(error.path, walletId, address);
      }
      throw error;
    }
  };
  isAddressVerificationEnabled = (walletId) => {
    const hardwareWalletConnectionData = (0, lodash_1.get)(
      this.hardwareWalletsConnectionData,
      walletId,
      {}
    );
    const deviceType = (0, lodash_1.get)(hardwareWalletConnectionData, [
      'device',
      'deviceType',
    ]);
    return deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER;
  };
  initiateAddressVerification = async (address, path) => {
    if (this.isAddressVerificationInitiated) return;
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Initiate Address Verification: ',
      {
        address: (0, helper_1.toJS)(address),
        path,
      }
    );
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Initiate Address Verification',
      () => {
        this.isAddressVerificationInitiated = true;
        this.unfinishedWalletAddressVerification = address;
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
      }
    );
    const walletId = (0, lodash_1.get)(this.stores.wallets, ['active', 'id']);
    const hardwareWalletConnectionData = (0, lodash_1.get)(
      this.hardwareWalletsConnectionData,
      walletId
    );
    logging_1.logger.info('[HW-DEBUG] HWStore - Verify address with wallet: ', {
      walletId,
    });
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const { disconnected, device } = hardwareWalletConnectionData;
    const { deviceType } = device;
    let devicePath =
      path ||
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
      hardwareWalletConnectionData.path ||
      hardwareWalletConnectionData.device.path;
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Verify address - check is device connected: ',
      {
        disconnected,
        deviceType,
        devicePath,
      }
    );
    let transportDevice;
    if (disconnected) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] CHECK FOR NEXT device');
      try {
        if (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER) {
          logging_1.logger.info(
            '[HW-DEBUG] HW STORE::initiateAddressVerification:: wait for ledger devices'
          );
          await this.waitForLedgerDevicesToConnect();
          transportDevice = await this.establishHardwareWalletConnection();
        } else {
          transportDevice = await this.establishHardwareWalletConnection();
        }
        if (transportDevice) {
          devicePath = transportDevice.path;
          logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 4', {
            transportDevice: (0, helper_1.toJS)(transportDevice),
          });
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Set transport device for tx init',
            () => {
              this.transportDevice = transportDevice;
            }
          );
        }
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Establishing connection failed'
        );
      }
    } else if (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER) {
      const connectedDevice = await this.waitForLedgerDevicesToConnect();
      devicePath = connectedDevice.path;
      if (!transportDevice) {
        transportDevice = await this.establishHardwareWalletConnection();
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Set transport device for ledger device',
          {
            transportDevice: (0, helper_1.toJS)(transportDevice),
          }
        );
      }
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Set transport device from tx init',
        () => {
          this.transportDevice = transportDevice;
        }
      );
    }
    if (deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
      logging_1.logger.info('[HW-DEBUG] Verify Address with Trezor: ', {
        address: (0, helper_1.toJS)(address),
      });
      if (!transportDevice) {
        transportDevice = await this.establishHardwareWalletConnection();
        logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 4', {
          transportDevice: (0, helper_1.toJS)(transportDevice),
        });
      }
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Set transport device from tx init',
        () => {
          this.transportDevice = transportDevice;
        }
      );
      const newConnectionData = (0, lodash_1.get)(
        this.hardwareWalletsConnectionData,
        walletId
      );
      const activeDevice =
        (0, lodash_1.find)(
          this.hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
          (hardwareWalletDevice) => hardwareWalletDevice.paired === walletId
        ) || {};
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type '{}'.
      devicePath = activeDevice.path || path || newConnectionData.path || null;
      const extendedPublicKey = await this._requestExtendedPublicKey(
        devicePath,
        walletId,
        address
      );
      const associatedWallet = await this._findAssociatedWalletByExtendedPublicKey(
        { extendedPublicKey }
      );
      if (associatedWallet) {
        await this._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting(
          {
            address,
            associatedWallet,
            expectedWalletId: walletId,
            extendedPublicKey,
            path: devicePath,
          }
        );
      } else {
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Software wallet not recognized - Setting error states'
        );
        this._discardConnectedDeviceAndReInitiateAddressVerification({
          address,
          walletId,
        });
      }
    } else {
      logging_1.logger.info('[HW-DEBUG] Verify Address with Ledger: ', {
        address: (0, helper_1.toJS)(address),
        devicePath,
      });
      this.stopCardanoAdaAppFetchPoller();
      this.useCardanoAppInterval(devicePath, walletId, address);
    }
  };
  verifyAddress = async (params) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] - VERIFY Address');
    const { address, path, isTrezor } = params;
    this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS;
    this.tempAddressToVerify = params;
    try {
      const derivedAddress = await getHardwareWalletChannel_1.deriveAddressChannel.request(
        {
          devicePath: path,
          isTrezor,
          addressType:
            ledgerjs_hw_app_cardano_1.AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
          spendingPathStr: address.spendingPath,
          stakingPathStr: `${hardwareWalletsConfig_1.SHELLEY_PURPOSE_INDEX}'/${hardwareWalletsConfig_1.ADA_COIN_TYPE}'/0'/2/0`,
          networkId: hardwareWalletsNetworkConfig.networkId,
          protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
        }
      );
      if (derivedAddress === address.id) {
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Address successfully verified',
          {
            address: derivedAddress,
          }
        );
        if (isTrezor) {
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Address Verified and is correct - Trezor',
            () => {
              this.isAddressDerived = true;
              this.isAddressChecked = true;
              this.isListeningForDevice = false;
              this.hwDeviceStatus =
                Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION;
            }
          );
        } else {
          (0, mobx_1.runInAction)(
            'HardwareWalletsStore:: Address Verified and is correct - Ledger',
            () => {
              this.isAddressDerived = true;
            }
          );
          this.showAddress(params);
        }
        this.analytics.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Verified wallet address with hardware wallet'
        );
      } else {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Address Verified but not correct',
          () => {
            this.isAddressDerived = false;
            this.isAddressChecked = false;
            this.isAddressCorrect = false;
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
          }
        );
      }
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - Verifying address error');
      /**
       * ============  Verifying aborted  =============
       * e.statusCode === 28169
       * ============  Verifying cancelled - device unplugged during action  =============
       * e.name === DisconnectedDevice // Ledger
       */
      const isCancelled =
        error.statusCode === 28169 || error.code === 'Failure_ActionCancelled';
      const isAborted =
        error.name === 'DisconnectedDevice' ||
        error.error === 'device disconnected during action';
      logging_1.logger.info('[HW-DEBUG] HWStore - Verifying error case: ', {
        isCancelled,
        isAborted,
      });
      if (isCancelled || isAborted) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - verifyAddress:: WAIT FOR ANOTHER DEVICE'
        );
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Re-run initiated connection',
          () => {
            this.isAddressDerived = false;
            this.isAddressChecked = false;
            this.isAddressCorrect = false;
            this.isListeningForDevice = true;
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED;
          }
        );
      } else {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Cannot Verify Address',
          () => {
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
            this.isAddressDerived = false;
            this.isAddressChecked = false;
            this.isAddressCorrect = false;
          }
        );
      }
      throw error;
    }
  };
  initiateWalletPairing = () => {
    logging_1.logger.info('[HW-DEBUG] HWStore::initiateWalletPairing');
    this.isWalletPairingInitiated = true;
    this.establishHardwareWalletConnection();
  };
  resetWalletPairing = async () => {
    this.stopCardanoAdaAppFetchPoller();
    this.isWalletPairingInitiated = false;
    return this.cleanUpPendingDevices();
  };
  showAddress = async (params) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] - SHOW Address');
    const { address, path, isTrezor } = params;
    try {
      await getHardwareWalletChannel_1.showAddressChannel.request({
        devicePath: path,
        isTrezor,
        addressType:
          ledgerjs_hw_app_cardano_1.AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        spendingPathStr: address.spendingPath,
        stakingPathStr: `${hardwareWalletsConfig_1.SHELLEY_PURPOSE_INDEX}'/${hardwareWalletsConfig_1.ADA_COIN_TYPE}'/0'/2/0`,
        networkId: hardwareWalletsNetworkConfig.networkId,
        protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
      });
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Address show process finished',
        () => {
          this.isAddressChecked = true;
          this.isListeningForDevice = true;
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION;
        }
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - Show address error');
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Showing address failed',
        () => {
          this.isAddressChecked = false;
          this.isAddressCorrect = false;
          this.isListeningForDevice = true;
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
        }
      );
      throw error;
    }
  };
  setAddressVerificationCheckStatus = (checkStatus) => {
    // Yes / No - Reverify / No - Invalid
    if (checkStatus === exports.AddressVerificationCheckStatuses.VALID) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Set address verification status CORRECT',
        () => {
          this.isAddressCorrect = true;
          this.isListeningForDevice = true;
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_SUCCEEDED;
        }
      );
    }
    if (checkStatus === exports.AddressVerificationCheckStatuses.INVALID) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Set address verification status CORRECT',
        () => {
          this.isAddressCorrect = false;
          this.isListeningForDevice = true;
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED;
        }
      );
    }
    if (checkStatus === exports.AddressVerificationCheckStatuses.REVERIFY) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Set address verification status CORRECT',
        () => {
          this.isAddressDerived = false;
          this.isAddressChecked = false;
          this.isAddressCorrect = null;
          this.isListeningForDevice = true;
        }
      );
      this.verifyAddress(this.tempAddressToVerify);
    }
  };
  waitForLedgerTransportDevice = async () => {
    const ledgerDevice = await this.waitForLedgerDevicesToConnect();
    logging_1.logger.info(
      '[HW-DEBUG] HWStore::getLedgerTransportDevice::Use ledger device as transport',
      {
        transportDevice: (0, helper_1.toJS)(ledgerDevice),
      }
    );
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set HW transportDevice',
      () => {
        this.transportDevice = ledgerDevice;
      }
    );
    return ledgerDevice;
  };
  _resetInitiatedConnection = ({ isAborted }) => {
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Re-run initiated connection',
      () => {
        this.hwDeviceStatus = isAborted
          ? Wallet_1.HwDeviceStatuses.CONNECTING
          : Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
        this.isListeningForDevice = true;
        this.isExportKeyAborted = true;
      }
    );
  };
  _requestExtendedPublicKey = async (forcedPath, walletId, address) => {
    logging_1.logger.info('[HW-DEBUG] HWStore - extendedPublicKey', {
      forcedPath,
      walletId,
      address: (0, helper_1.toJS)(address),
    });
    this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { transportDevice } = this;
    if (!transportDevice) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore::_requestExtendedPublicKey:: Device not recognized '
      );
      throw new Error(
        'Can not export extended public key: Device not recognized!'
      );
    }
    const { deviceType, path } = transportDevice;
    const isTrezor = deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR;
    const devicePath = forcedPath || path;
    try {
      return await getHardwareWalletChannel_1.getExtendedPublicKeyChannel.request(
        {
          path: "1852'/1815'/0'",
          // Shelley 1852 ADA 1815 indicator for account '0'
          isTrezor,
          devicePath,
        }
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - Export key error');
      /**
       * ============  Exporting aborted  =============
       * e.statusCode === 28169 // Ledger
       * e.code === 'Failure_ActionCancelled' // Trezor
       * ============  Exporting cancelled - device unplugged during action  =============
       * e.name === DisconnectedDevice // Ledger
       * e.error === 'device disconnected during action' // Trezor
       */
      const isCancelled =
        error.statusCode === 28169 || error.code === 'Failure_ActionCancelled';
      const isAborted =
        error.name === 'DisconnectedDevice' ||
        error.error === 'device disconnected during action';
      logging_1.logger.info('[HW-DEBUG] HWStore - Export error case: ', {
        isCancelled,
        isAborted,
      });
      if (isCancelled || isAborted) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Export:: WAIT FOR ANOTHER DEVICE'
        );
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        if (isCancelled && isTrezor) {
          // Skip Trezor device-change events when rejected
          setTimeout(() => {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info('[HW-DEBUG] NOW RESET');
            this._resetInitiatedConnection({ isAborted });
          }, 2000);
        } else {
          this.stopCardanoAdaAppFetchPoller();
          this._resetInitiatedConnection({ isAborted });
        }
      } else {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Cannot export extended public key',
          () => {
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
          }
        );
      }
      // Pass other errors to caller (establishHardwareWalletConnection() in this case) and handle additional actions if needed
      throw error;
    }
  };
  _findAssociatedWalletByExtendedPublicKey = async ({ extendedPublicKey }) => {
    const deviceId =
      extendedPublicKey?.deviceId || this.transportDevice.deviceId;
    logging_1.logger.info('[HW-DEBUG] HWStore - EXPORT - deviceID: ', {
      deviceId,
    });
    const hardwareWalletConnectionData = (0, lodash_1.find)(
      this.hardwareWalletsConnectionData,
      (hardwareWalletData) =>
        extendedPublicKey.chainCodeHex ===
          hardwareWalletData.extendedPublicKey.chainCodeHex &&
        extendedPublicKey.publicKeyHex ===
          hardwareWalletData.extendedPublicKey.publicKeyHex
    );
    return hardwareWalletConnectionData
      ? this.stores.wallets.getWalletById(hardwareWalletConnectionData.id)
      : null;
  };
  // Delete initiated (pending) device with this path since now is paired to wallet
  _deletePendingDeviceWithGivenPath = async ({ path }) => {
    const device = (0, lodash_1.find)(
      this.hardwareWalletDevices,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
      (device) => device.path === path
    );
    if (!device) return;
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - __deletePendingDeviceWithGivenPath - UNSET Device with path: ',
      {
        deviceId: device.id,
      }
    );
    await this._unsetHardwareWalletDevice({
      deviceId: device.id,
    });
  };
  _discardConnectedDeviceAndReInitiateTransaction = ({ walletId } = {}) => {
    // Keep isTransactionInitiated active & Set new device listener by initiating transaction
    // Show message to reconnect proper software wallet device pair
    logging_1.logger.info(
      '[HW-DEBUG] unfinishedWalletTxSigning SET: ',
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      walletId
    );
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set HW device UNRECOGNIZED_WALLET',
      () => {
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.UNRECOGNIZED_WALLET;
        this.activeDevicePath = null;
        this.unfinishedWalletTxSigning = walletId;
        this.isExportKeyAborted = false;
      }
    );
  };
  _proceedWithTransactionAfterConnectingDevice = ({
    isTrezor,
    walletId,
    deviceId,
    devicePath,
  }) => {
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Transaction Initiated - Close: ',
      {
        walletId,
      }
    );
    logging_1.logger.info('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Initiate transaction',
      () => {
        this.isTransactionInitiated = false;
        this.unfinishedWalletTxSigning = null;
        this.isExportKeyAborted = false;
      }
    );
    if (isTrezor) {
      this._signTransactionTrezor(walletId, deviceId);
    } else {
      this._signTransactionLedger(walletId, devicePath);
    }
  };
  _discardConnectedDeviceAndReInitiateAddressVerification = ({
    address,
    walletId,
  }) => {
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Device not belongs to this wallet'
    );
    // Show message to reconnect proper software wallet device pair
    logging_1.logger.info(
      '[HW-DEBUG] unfinishedWalletAddressVerification SET: ',
      {
        walletId,
      }
    );
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set HW device UNRECOGNIZED_WALLET',
      () => {
        this.isAddressVerificationInitiated = false;
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.UNRECOGNIZED_WALLET;
        this.activeDevicePath = null;
        this.unfinishedWalletAddressVerification = address;
        this.isExportKeyAborted = false;
      }
    );
  };
  _proceedWithAddressVerificationAfterConnectingDevice = ({
    address,
    devicePath,
    isTrezor,
    walletId,
  }) => {
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - Address Verification - Close: ',
      {
        walletId,
      }
    );
    logging_1.logger.info('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Initiate transaction',
      () => {
        this.isAddressVerificationInitiated = false;
        this.unfinishedWalletAddressVerification = null;
        this.isExportKeyAborted = false;
      }
    );
    this.verifyAddress({
      address,
      isTrezor,
      path: devicePath,
    });
  };
  _storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting = async ({
    address,
    associatedWallet,
    expectedWalletId,
    extendedPublicKey,
    path,
  }) => {
    const { deviceType, deviceName, deviceModel } = this.transportDevice;
    const isTrezor = deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR;
    const devicePath = path || this.transportDevice.path;
    const deviceId =
      extendedPublicKey.deviceId || this.transportDevice.deviceId;
    // Check if public key matches already restored hardware wallet public key
    // Update LC data and redirect to paired wallet
    logging_1.logger.info('[HW-DEBUG] HWStore - I have recognized wallet: ', {
      recognizedWallet: associatedWallet.id,
    });
    this._setHardwareWalletLocalData({
      walletId: associatedWallet.id,
      data: {
        disconnected: false,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ disconnected: false; data: { deviceType: D... Remove this comment to see the full error message
        data: {
          deviceType,
          deviceModel,
          deviceName,
          path: devicePath,
          paired: associatedWallet.id,
          // device paired with software wallet
          disconnected: false, // device physically disconnected
        },
      },
    });
    await this._deletePendingDeviceWithGivenPath({ path });
    if (deviceId) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - SET device from key export: ',
        {
          deviceId,
        }
      );
      this._setHardwareWalletDevice({
        deviceId,
        data: {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ deviceId: string; deviceType: DeviceType; ... Remove this comment to see the full error message
          deviceId,
          deviceType,
          deviceModel,
          deviceName,
          path: devicePath,
          paired: associatedWallet.id,
          // device paired with software wallet
          disconnected: false,
          // device physically disconnected
          isPending: false,
        },
      });
    }
    // Prevent redirect / check if device is valid / proceed with tx
    if (this.isTransactionInitiated) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Re-initiate tx from _storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting: ',
        {
          expectedWalletId,
          recognizedWalletId: associatedWallet.id,
          deviceId,
          devicePath,
        }
      );
      // Check if sender wallet match transaction initialization
      if (!expectedWalletId || associatedWallet.id !== expectedWalletId) {
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Device not belongs to this wallet'
        );
        this._discardConnectedDeviceAndReInitiateTransaction({
          walletId: expectedWalletId,
        });
      } else {
        this._proceedWithTransactionAfterConnectingDevice({
          isTrezor,
          deviceId,
          devicePath,
          walletId: expectedWalletId,
        });
      }
      return;
    }
    // Prevent redirect / check if device is valid / proceed with address verification
    if (this.isAddressVerificationInitiated && address) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Re-initiate Address verification from _storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting: ',
        {
          address: (0, helper_1.toJS)(address),
          devicePath,
          expectedWalletId,
          recognizedWalletId: associatedWallet.id,
          deviceId,
        }
      );
      if (!expectedWalletId || associatedWallet.id !== expectedWalletId) {
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Device not belongs to this wallet'
        );
        this._discardConnectedDeviceAndReInitiateAddressVerification({
          address,
          walletId: expectedWalletId,
        });
      } else {
        this._proceedWithAddressVerificationAfterConnectingDevice({
          address,
          devicePath,
          isTrezor,
          walletId: expectedWalletId,
        });
      }
      return;
    }
    // --> Else
    this.stores.wallets.goToWalletRoute(associatedWallet.id);
    this.actions.dialogs.closeActiveDialog.trigger();
  };
  _createNewWalletForRecognizedPendingDevice = async ({
    extendedPublicKey,
    path,
  }) => {
    const { deviceType, deviceName, deviceModel } = this.transportDevice;
    const devicePath = path || this.transportDevice.path;
    const deviceId =
      extendedPublicKey.deviceId || this.transportDevice.deviceId;
    // Software Wallet not recognized, create new one with default name
    logging_1.logger.info('[HW-DEBUG] HWStore - Initiate HW create / restore', {
      transportDevice: (0, helper_1.toJS)(this.transportDevice),
      device: {
        deviceId,
        deviceType,
        deviceModel,
        deviceName,
        path: devicePath,
        firmwareVersion: null,
      },
    });
    await this.actions.wallets.createHardwareWallet.trigger({
      walletName: deviceName || DEFAULT_HW_NAME,
      extendedPublicKey,
      device: {
        deviceId,
        deviceType,
        deviceModel,
        deviceName,
        path: devicePath,
        firmwareVersion: null,
      },
    });
    logging_1.logger.info('[HW-DEBUG] HWStore - HW created / restored');
    // Get all Pending devices with this path and delete
    const recognizedPendingDevice = (0, lodash_1.find)(
      this.hardwareWalletDevices,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
      (device) => device.path === devicePath
    );
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isPending' does not exist on type 'Hardw... Remove this comment to see the full error message
    if (recognizedPendingDevice && recognizedPendingDevice.isPending) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Export key - UNSET Device with path: ',
        {
          path,
          recognizedPendingDevice: recognizedPendingDevice.id,
        }
      );
      await this._unsetHardwareWalletDevice({
        deviceId: recognizedPendingDevice.id,
      });
    }
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.resetInitializedConnection();
    this._refreshHardwareWalletsLocalData();
    this._refreshHardwareWalletDevices();
    await this.resetWalletPairing();
  };
  _identifyAndHandleAssociatedWallet = async ({
    address,
    path,
    expectedWalletId,
  }) => {
    logging_1.logger.info('[HW-DEBUG] Identifying ...: ');
    try {
      const extendedPublicKey = await this._requestExtendedPublicKey(
        path,
        expectedWalletId,
        address
      );
      const associatedWallet = await this._findAssociatedWalletByExtendedPublicKey(
        { extendedPublicKey }
      );
      if (associatedWallet) {
        await this._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting(
          {
            address,
            associatedWallet,
            expectedWalletId,
            extendedPublicKey,
            path,
          }
        );
      } else {
        const deviceId =
          extendedPublicKey.deviceId || this.transportDevice.deviceId;
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - I don not have recognized wallet - create new one or reject TX: ',
          {
            deviceId,
          }
        );
        if (this.isTransactionInitiated) {
          // Software Wallet not recognized and TX initiated. Show error
          this._discardConnectedDeviceAndReInitiateTransaction({
            walletId: expectedWalletId,
          });
        } else {
          await this._createNewWalletForRecognizedPendingDevice({
            extendedPublicKey,
            path,
          });
        }
      }
    } catch (e) {
      await this.resetWalletPairing();
      throw e;
    }
  };
  // Trezor - Shelley only
  _signTransactionTrezor = async (walletId, deviceId) => {
    const { coinSelection } = this.txSignRequest;
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set Transaction verifying',
      () => {
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION;
      }
    );
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] _signTransactionTrezor:: Execute');
    // @TODO - remove once signing delegation transaction will call coins selection
    // This case is covered in coins selection action
    if (!coinSelection) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw new Error(`Missing Coins Selection for wallet: ${walletId}`);
    }
    const {
      inputs,
      outputs,
      fee: flatFee,
      certificates,
      withdrawals,
    } = coinSelection;
    logging_1.logger.info('[HW-DEBUG] HWStore - sign transaction Trezor: ', {
      walletId,
    });
    const hardwareWalletConnectionData = (0, lodash_1.get)(
      this.hardwareWalletsConnectionData,
      walletId
    );
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const publicKeyHex = (0, lodash_1.get)(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'publicKeyHex',
    ]);
    const chainCodeHex = (0, lodash_1.get)(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'chainCodeHex',
    ]);
    const xpubHex = `${publicKeyHex}${chainCodeHex}`;
    const unsignedTxInputs = [];
    const inputsData = (0, lodash_1.map)(inputs, (input) => {
      const shelleyTxInput = (0, shelleyLedger_1.ShelleyTxInputFromUtxo)(input);
      unsignedTxInputs.push(shelleyTxInput);
      return (0, shelleyTrezor_1.prepareTrezorInput)(input);
    });
    const unsignedTxOutputs = [];
    const outputsData = [];
    for (const output of outputs) {
      const {
        address_style: addressStyle,
      } = await this.stores.addresses._inspectAddress({
        addressId: output.address,
      });
      const shelleyTxOutput = (0, shelleyLedger_1.ShelleyTxOutput)(
        output,
        addressStyle
      );
      unsignedTxOutputs.push(shelleyTxOutput);
      const ledgerOutput = (0, shelleyTrezor_1.prepareTrezorOutput)(output);
      outputsData.push(ledgerOutput);
    }
    // Construct certificates
    const unsignedTxCerts = [];
    const _certificatesData = (0, lodash_1.map)(
      certificates,
      async (certificate) => {
        const accountAddress = await this._getRewardAccountAddress(
          walletId,
          certificate.rewardAccountPath
        );
        const shelleyTxCert = (0, shelleyLedger_1.ShelleyTxCert)({
          accountAddress,
          pool: certificate.pool,
          // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'string'.
          type:
            hardwareWalletUtils_1.CERTIFICATE_TYPE[certificate.certificateType],
        });
        unsignedTxCerts.push(shelleyTxCert);
        return (0, shelleyTrezor_1.prepareTrezorCertificate)(certificate);
      }
    );
    const certificatesData = await Promise.all(_certificatesData);
    // Construct Withdrawals
    const _withdrawalsData = (0, lodash_1.map)(
      withdrawals,
      async (withdrawal) =>
        (0, shelleyTrezor_1.prepareTrezorWithdrawal)(withdrawal)
    );
    const withdrawalsData = await Promise.all(_withdrawalsData);
    let unsignedTxAuxiliaryData = null;
    let auxiliaryData = null;
    if (this.votingData) {
      const { address, stakeKey, votingKey, nonce } = this.votingData;
      unsignedTxAuxiliaryData = {
        nonce,
        // unique increaseable number e.g. current epoch number or absolute slot number ( identifies unique tx / vote registration )
        rewardDestinationAddress: {
          address,
          stakingPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
        stakePubKey: stakeKey,
        type: shelleyLedger_1.CATALYST_VOTING_REGISTRATION_TYPE,
        votingPubKey: votingKey,
      };
      auxiliaryData = (0, shelleyTrezor_1.prepareTrezorAuxiliaryData)({
        address,
        votingKey,
        nonce: nonce.toString(),
      });
    }
    const recognizedDevice = (0, lodash_1.find)(
      this.hardwareWalletDevices,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
      (hardwareWalletDevice) => hardwareWalletDevice.paired === walletId
    );
    logging_1.logger.info(
      '[HW-DEBUG] sign Trezor:: recognizedDevicePath and wallet: ',
      {
        walletId,
        deviceId,
        isTransactionInitiated: this.isTransactionInitiated,
      }
    );
    if (this.isTransactionInitiated) {
      // Check if sender wallet match transaction initialization
      if (
        !recognizedDevice ||
        (recognizedDevice && deviceId && recognizedDevice.id !== deviceId)
      ) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - Device not belongs to this wallet'
        );
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        logging_1.logger.info(
          '[HW-DEBUG] unfinishedWalletTxSigning SET: ',
          walletId
        );
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING_FAILED;
            this.activeDevicePath = null;
            this.unfinishedWalletTxSigning = walletId;
          }
        );
        return;
      }
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Transaction Initiated - RESET: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        walletId
      );
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Initiate transaction',
        () => {
          this.isTransactionInitiated = false;
          this.unfinishedWalletTxSigning = null;
        }
      );
    }
    const fee = (0, formatters_1.formattedAmountToLovelace)(flatFee.toString());
    const ttl = this._getTtl();
    try {
      const signedTransaction = await getHardwareWalletChannel_1.signTransactionTrezorChannel.request(
        {
          inputs: inputsData,
          outputs: outputsData,
          fee: fee.toString(),
          ttl: ttl.toString(),
          networkId: hardwareWalletsNetworkConfig.networkId,
          protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
          certificates: certificatesData,
          withdrawals: withdrawalsData,
          signingMode:
            shelleyTrezor_1.TrezorTransactionSigningMode.ORDINARY_TRANSACTION,
          auxiliaryData,
        }
      );
      if (signedTransaction && !signedTransaction.success) {
        throw signedTransaction.payload;
      }
      // Compatible with old firmwares
      const serializedTx = (0, lodash_1.get)(signedTransaction, [
        'payload',
        'serializedTx',
      ]);
      if (serializedTx) {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: transaction successfully signed',
          () => {
            this.txBody = serializedTx;
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
          }
        );
        return;
      }
      const unsignedTxWithdrawals =
        withdrawals.length > 0
          ? (0, shelleyLedger_1.ShelleyTxWithdrawal)(withdrawals)
          : null;
      // Prepare unsigned transaction structure for serialzation
      let txAuxData = {
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: unsignedTxCerts,
        withdrawals: unsignedTxWithdrawals,
      };
      let txAuxiliaryData = null;
      const auxiliaryDataSupplement = (0, lodash_1.get)(signedTransaction, [
        'payload',
        'auxiliaryDataSupplement',
      ]);
      if (unsignedTxAuxiliaryData && auxiliaryDataSupplement) {
        txAuxData = {
          ...txAuxData,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ txAuxiliaryData: any; txAuxiliaryDataHash:... Remove this comment to see the full error message
          txAuxiliaryData: unsignedTxAuxiliaryData,
          txAuxiliaryDataHash: auxiliaryDataSupplement.auxiliaryDataHash,
        };
        txAuxiliaryData = (0, shelleyLedger_1.cborizeTxAuxiliaryVotingData)(
          unsignedTxAuxiliaryData,
          auxiliaryDataSupplement.catalystSignature
        );
      }
      const unsignedTx = (0, shelleyLedger_1.prepareTxAux)(txAuxData);
      const witnesses = (0, lodash_1.get)(
        signedTransaction,
        ['payload', 'witnesses'],
        []
      );
      const signedWitnesses = await this._signWitnesses(witnesses, xpubHex);
      const txWitnesses = new Map();
      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }
      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await (0, shelleyLedger_1.prepareBody)(
        unsignedTx,
        txWitnesses,
        txAuxiliaryData
      );
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verified',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
          this.txBody = txBody;
          this.activeDevicePath = null;
        }
      );
    } catch (error) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
          this.isTransactionInitiated = false;
        }
      );
      if (error.code === 'Device_CallInProgress') {
        throw new Error('Device is busy - reconnect device and try again');
      } else if (error.code === 'Device_InvalidState') {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Unrecognized wallet (wrong passphrase)',
          () => {
            this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.UNRECOGNIZED_WALLET;
          }
        );
      }
      throw error;
    }
  };
  _signWitnesses = async (witnesses, xpubHex) => {
    const signedWitnesses = [];
    for (const witness of witnesses) {
      const signedWitness = await this.ShelleyWitness(witness, xpubHex);
      signedWitnesses.push(signedWitness);
    }
    return signedWitnesses;
  };
  ShelleyWitness = async (witness, xpubHex) => {
    let publicKey;
    let witnessSignatureHex;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'pubKey' does not exist on type 'TrezorWi... Remove this comment to see the full error message
    if (witness.pubKey && witness.signature) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'pubKey' does not exist on type 'TrezorWi... Remove this comment to see the full error message
      publicKey = Buffer.from(witness.pubKey, 'hex');
      // @ts-ignore ts-migrate(2339) FIXME: Property 'signature' does not exist on type 'Trezo... Remove this comment to see the full error message
      witnessSignatureHex = witness.signature;
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'TrezorWitn... Remove this comment to see the full error message
    } else if (witness.path && witness.witnessSignatureHex) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'TrezorWitn... Remove this comment to see the full error message
      const xpub = await this._deriveXpub(witness.path, xpubHex);
      publicKey = xpub.slice(0, 32);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'witnessSignatureHex' does not exist on t... Remove this comment to see the full error message
      witnessSignatureHex = witness.witnessSignatureHex;
    }
    if (witnessSignatureHex && publicKey) {
      const signature = Buffer.from(witnessSignatureHex, 'hex');
      return (0, shelleyLedger_1.ShelleyTxWitnessShelley)(publicKey, signature);
    }
    return null;
  };
  _deriveXpub = (0, shelleyLedger_1.CachedDeriveXpubFactory)(
    async (xpubHex) => {
      return Buffer.from(xpubHex, 'hex');
    }
  );
  _getRewardAccountAddress = async (walletId, path) => {
    const pathParams = (0, hardwareWalletUtils_1.getParamsFromPath)(path);
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const publicKey = await this.getPublicKeyRequest.execute({
      walletId,
      role: pathParams.roleIdentity,
      index: pathParams.index,
    });
    const data = {
      stake: publicKey,
    };
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const constructedAddress = await this.constructAddressRequest.execute({
      data,
    });
    return constructedAddress.address;
  };
  _signTransactionLedger = async (walletId, devicePath) => {
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: set Transaction verifying',
      () => {
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION;
      }
    );
    const { coinSelection } = this.txSignRequest;
    const {
      inputs,
      outputs,
      certificates,
      fee: flatFee,
      withdrawals,
    } = coinSelection;
    logging_1.logger.info('[HW-DEBUG] HWStore - sign transaction Ledger: ', {
      walletId,
    });
    const hardwareWalletConnectionData = (0, lodash_1.get)(
      this.hardwareWalletsConnectionData,
      walletId
    );
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const publicKeyHex = (0, lodash_1.get)(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'publicKeyHex',
    ]);
    const chainCodeHex = (0, lodash_1.get)(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'chainCodeHex',
    ]);
    const xpubHex = `${publicKeyHex}${chainCodeHex}`;
    const unsignedTxInputs = [];
    const inputsData = (0, lodash_1.map)(inputs, (input) => {
      const shelleyTxInput = (0, shelleyLedger_1.ShelleyTxInputFromUtxo)(input);
      unsignedTxInputs.push(shelleyTxInput);
      return (0, shelleyLedger_1.prepareLedgerInput)(input);
    });
    const unsignedTxOutputs = [];
    const outputsData = [];
    for (const output of outputs) {
      const {
        address_style: addressStyle,
      } = await this.stores.addresses._inspectAddress({
        addressId: output.address,
      });
      const shelleyTxOutput = (0, shelleyLedger_1.ShelleyTxOutput)(
        output,
        addressStyle
      );
      unsignedTxOutputs.push(shelleyTxOutput);
      const ledgerOutput = (0, shelleyLedger_1.prepareLedgerOutput)(
        output,
        addressStyle
      );
      outputsData.push(ledgerOutput);
    }
    // Construct certificates
    const unsignedTxCerts = [];
    const _certificatesData = (0, lodash_1.map)(
      certificates,
      async (certificate) => {
        const accountAddress = await this._getRewardAccountAddress(
          walletId,
          certificate.rewardAccountPath
        );
        const shelleyTxCert = (0, shelleyLedger_1.ShelleyTxCert)({
          accountAddress,
          pool: certificate.pool,
          // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'string'.
          type:
            hardwareWalletUtils_1.CERTIFICATE_TYPE[certificate.certificateType],
        });
        unsignedTxCerts.push(shelleyTxCert);
        return (0, shelleyLedger_1.prepareLedgerCertificate)(certificate);
      }
    );
    const certificatesData = await Promise.all(_certificatesData);
    // Construct Withdrawals
    const _withdrawalsData = (0, lodash_1.map)(
      withdrawals,
      async (withdrawal) =>
        (0, shelleyLedger_1.prepareLedgerWithdrawal)(withdrawal)
    );
    const withdrawalsData = await Promise.all(_withdrawalsData);
    const fee = (0, formatters_1.formattedAmountToLovelace)(flatFee.toString());
    const ttl = this._getTtl();
    let unsignedTxAuxiliaryData = null;
    if (this.votingData) {
      const { address, stakeKey, votingKey, nonce } = this.votingData;
      unsignedTxAuxiliaryData = {
        nonce,
        // unique increasable number e.g. current epoch number or absolute slot number ( identifies unique tx / vote registration )
        rewardDestinationAddress: {
          address,
          stakingPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
        stakePubKey: stakeKey,
        type: shelleyLedger_1.CATALYST_VOTING_REGISTRATION_TYPE,
        votingPubKey: votingKey,
      };
    }
    const auxiliaryData = unsignedTxAuxiliaryData
      ? (0, shelleyLedger_1.prepareLedgerAuxiliaryData)(unsignedTxAuxiliaryData)
      : null;
    try {
      const signedTransaction = await getHardwareWalletChannel_1.signTransactionLedgerChannel.request(
        {
          inputs: inputsData,
          outputs: outputsData,
          fee: fee.toString(),
          ttl: ttl.toString(),
          validityIntervalStartStr: null,
          networkId: hardwareWalletsNetworkConfig.networkId,
          protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ type: number; params: { stakeCredential: {... Remove this comment to see the full error message
          certificates: certificatesData,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ stakeCredential: { type: StakeCredentialPa... Remove this comment to see the full error message
          withdrawals: withdrawalsData,
          signingMode:
            ledgerjs_hw_app_cardano_1.TransactionSigningMode
              .ORDINARY_TRANSACTION,
          additionalWitnessPaths: [],
          auxiliaryData,
          devicePath,
        }
      );
      const unsignedTxWithdrawals =
        withdrawals.length > 0
          ? (0, shelleyLedger_1.ShelleyTxWithdrawal)(withdrawals)
          : null;
      // Prepare unsigned transaction structure for serialzation
      let txAuxData = {
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: unsignedTxCerts,
        withdrawals: unsignedTxWithdrawals,
      };
      let txAuxiliaryData = null;
      if (
        unsignedTxAuxiliaryData &&
        signedTransaction &&
        signedTransaction.auxiliaryDataSupplement
      ) {
        txAuxData = {
          ...txAuxData,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ txAuxiliaryData: any; txAuxiliaryDataHash:... Remove this comment to see the full error message
          txAuxiliaryData: unsignedTxAuxiliaryData,
          txAuxiliaryDataHash:
            signedTransaction.auxiliaryDataSupplement.auxiliaryDataHashHex,
        };
        txAuxiliaryData = (0, shelleyLedger_1.cborizeTxAuxiliaryVotingData)(
          unsignedTxAuxiliaryData,
          signedTransaction.auxiliaryDataSupplement
            .cip36VoteRegistrationSignatureHex
        );
      }
      const unsignedTx = (0, shelleyLedger_1.prepareTxAux)(txAuxData);
      const witnesses = (0, lodash_1.get)(signedTransaction, 'witnesses', []);
      const signedWitnesses = await this._signWitnesses(witnesses, xpubHex);
      const txWitnesses = new Map();
      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }
      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await (0, shelleyLedger_1.prepareBody)(
        unsignedTx,
        txWitnesses,
        txAuxiliaryData
      );
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verified',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
          this.txBody = txBody;
          this.activeDevicePath = null;
        }
      );
    } catch (error) {
      logging_1.logger.info('[HW-DEBUG] HWStore:: sign Transaction Ledger', {
        error,
      });
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus =
            Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw error;
    }
  };
  initiateTransaction = async (params) => {
    const { walletId, votingData } = params;
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Initiate Transaction',
      () => {
        this.isTransactionInitiated = true;
        this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
        this.activeDelegationWalletId = walletId;
        this.votingData = votingData || null;
        this.activeVotingWalletId = walletId;
      }
    );
    const hardwareWalletConnectionData = (0, lodash_1.get)(
      this.hardwareWalletsConnectionData,
      walletId
    );
    logging_1.logger.info('[HW-DEBUG] HWStore - initiateTransaction: ', {
      walletId,
    });
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const { disconnected, device, id } = hardwareWalletConnectionData;
    const { deviceType } = device;
    let devicePath = hardwareWalletConnectionData.device.path;
    if (disconnected) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - initiateTransaction - DISCONNECTED'
      );
      // Wait for connection to be established and continue to signing process
      try {
        let transportDevice;
        if (
          hardwareWalletConnectionData.device.deviceType ===
          hardware_wallets_types_1.DeviceTypes.TREZOR
        ) {
          // Do I have unpaired Trezor devices
          const lastUnpairedDevice = this.getLastUnpairedDevice();
          if (lastUnpairedDevice) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info('[HW-DEBUG] I HAVE UNPAIRED');
            transportDevice = lastUnpairedDevice;
          } else {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info('[HW-DEBUG] CHECK FOR NEXT device');
            transportDevice = await getHardwareWalletChannel_1.getHardwareWalletTransportChannel.request(
              {
                devicePath: null,
                isTrezor: true,
              }
            );
          }
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info('[HW-DEBUG] INITIATE tx - I have transport');
        } else {
          logging_1.logger.info('[HW-DEBUG] HW STORE WAIT FOR LEDGER DEVICE');
          transportDevice = await this.waitForLedgerTransportDevice();
          logging_1.logger.info('[HW-DEBUG] HW STORE Transport received', {
            transportDevice,
          });
        }
        if (!transportDevice) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            '[HW-DEBUG] No new devices recognized for tx signing'
          );
          throw new Error('Signing device not recognized!');
        }
        devicePath = transportDevice.path;
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - initiateTransaction - DISCONNECTED - ERROR'
        );
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Initiate transaction',
          () => {
            this.isTransactionInitiated = false;
            this.hwDeviceStatus =
              Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
          }
        );
        throw e;
      }
    } else if (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore::initiateTransaction::Device not connected'
      );
      const ledgerDevice = await this.waitForLedgerTransportDevice();
      devicePath = ledgerDevice.path;
    }
    (0, mobx_1.runInAction)(
      'HardwareWalletsStore:: Set active device path for Transaction send',
      () => {
        this.activeDevicePath = devicePath;
      }
    );
    // Add more cases / edge cases if needed
    if (
      deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR &&
      walletId
    ) {
      logging_1.logger.info('[HW-DEBUG] Sign Trezor: ', {
        id,
      });
      const transportDevice = await this.establishHardwareWalletConnection();
      if (transportDevice) {
        logging_1.logger.info('[HW-DEBUG] HWStore - Set transport device 4', {
          transportDevice: (0, helper_1.toJS)(transportDevice),
        });
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Set transport device for tx init',
          () => {
            this.transportDevice = transportDevice;
          }
        );
        const extendedPublicKey = await this._requestExtendedPublicKey(
          transportDevice.path,
          walletId
        );
        const associatedWallet = await this._findAssociatedWalletByExtendedPublicKey(
          { extendedPublicKey }
        );
        if (associatedWallet) {
          await this._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting(
            {
              associatedWallet,
              expectedWalletId: walletId,
              extendedPublicKey,
              path: transportDevice.path,
            }
          );
        } else {
          const deviceId =
            extendedPublicKey.deviceId || this.transportDevice.deviceId;
          logging_1.logger.info(
            '[HW-DEBUG] HWStore - I don not have recognized wallet - reject TX: ',
            {
              deviceId,
            }
          );
          // Software Wallet not recognized and TX initiated. Show error
          this._discardConnectedDeviceAndReInitiateTransaction({ walletId });
        }
      }
    } else {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - getCardanoAdaApp - from  initiateTransaction',
        {
          devicePath,
        }
      );
      if (walletId) {
        this.stopCardanoAdaAppFetchPoller();
        this.useCardanoAppInterval(devicePath, walletId);
      }
    }
  };
  _resetTransaction = async (params) => {
    if (hardwareWalletsConfig_1.isHardwareWalletSupportEnabled) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] RESET TX');
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Reset initiated transaction',
        () => {
          this.isTransactionInitiated = false;
        }
      );
      this.stopCardanoAdaAppFetchPoller();
      const cancelDeviceAction = (0, lodash_1.get)(
        params,
        'cancelDeviceAction',
        false
      );
      if (cancelDeviceAction) {
        getHardwareWalletChannel_1.resetTrezorActionChannel.request();
      }
      this.sendMoneyRequest.reset();
      this.selectCoinsRequest.reset();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: reset Transaction verifying',
        () => {
          this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.READY;
          this.txBody = null;
          this.activeDevicePath = null;
          this.unfinishedWalletTxSigning = null;
          this.activeDelegationWalletId = null;
          this.activeVotingWalletId = null;
          this.votingData = null;
        }
      );
    }
  };
  _changeHardwareWalletConnectionStatus = async (params) => {
    const {
      disconnected,
      deviceType,
      deviceId,
      deviceModel,
      deviceName,
      path,
      error,
      eventType,
      product,
    } = params;
    logging_1.logger.info('[HW-DEBUG] HWStore - CHANGE status: ', {
      params,
    });
    if (disconnected) {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - CHANGE status::in-memory-path::removing path from memory ',
        {
          path,
        }
      );
      this.connectedHardwareWalletsDevices.delete(path);
    } else {
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - CHANGE status::in-memory-path::adding path to memory ',
        {
          path,
        }
      );
      this.connectedHardwareWalletsDevices.set(path, {
        product,
        path,
        disconnected,
        deviceId,
        deviceModel,
        deviceName,
        deviceType,
      });
    }
    // Handle Trezor Bridge instance checker
    if (error && deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR) {
      if (
        error.payload &&
        error.payload &&
        error.payload.code === 'ECONNREFUSED'
      ) {
        (0, mobx_1.runInAction)(
          'HardwareWalletsStore:: Mark Trezor Bridge as not installed',
          () => {
            this.isTrezorBridgeInstalled = false;
          }
        );
      }
      return;
    }
    // Unset Trezor Bridge instance checker
    if (
      deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR &&
      !this.isTrezorBridgeInstalled
    ) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Mark Trezor Bridge as installed',
        () => {
          this.isTrezorBridgeInstalled = true;
        }
      );
    }
    const { hardwareWalletsConnectionData, hardwareWalletDevices } = this;
    // Add new recognized device - not connected to software wallet
    // Or update recognized device while paired with existing software wallet
    const recognizedPairedHardwareWallet = (0, lodash_1.find)(
      hardwareWalletsConnectionData,
      (
        connection // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
      ) =>
        deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR &&
        deviceId &&
        connection.device.deviceId === deviceId
    );
    if (
      disconnected &&
      deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER
    ) {
      // Remove all stored Ledger instances from LC - both pending and paired (with software Wallets)
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('[HW-DEBUG] HWStore - device disconnected', {
        hardwareWalletDevices: (0, helper_1.toJS)(hardwareWalletDevices),
        path,
      });
      const recognizedLedgerDevice = (0, lodash_1.find)(
        hardwareWalletDevices,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
        (hardwareWalletDevice) => hardwareWalletDevice.path === path
      );
      if (recognizedLedgerDevice) {
        logging_1.logger.info('[HW-DEBUG] HWStore - Remove Device from LC', {
          recognizedLedgerDevice: (0, helper_1.toJS)(recognizedLedgerDevice),
        });
        await this._unsetHardwareWalletDevice({
          deviceId: recognizedLedgerDevice.id,
        });
      }
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - GET Paired and set to disconnected'
      );
      const recognizedLedgerWallet = (0, lodash_1.find)(
        hardwareWalletsConnectionData,
        (
          connection // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
        ) =>
          deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER &&
          path &&
          connection.device.path === path
      );
      if (recognizedLedgerWallet) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - I have stored Ledger wallet'
        );
        await this._setHardwareWalletLocalData({
          walletId: recognizedLedgerWallet.id,
          data: {
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ deviceType: DeviceType; deviceModel: strin... Remove this comment to see the full error message
            deviceType,
            deviceModel,
            deviceName,
            disconnected: true,
            path,
          },
        });
      }
    }
    // Check if plugged-in device match one with already established wallet connection
    if (
      recognizedPairedHardwareWallet &&
      recognizedPairedHardwareWallet.device.deviceType ===
        hardware_wallets_types_1.DeviceTypes.TREZOR
    ) {
      // Change software wallet status - paired with device
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - set Hardware Wallet local data: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        recognizedPairedHardwareWallet.id
      );
      await this._setHardwareWalletLocalData({
        walletId: recognizedPairedHardwareWallet.id,
        data: {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ deviceType: DeviceType; deviceModel: strin... Remove this comment to see the full error message
          deviceType,
          deviceModel,
          deviceName,
          disconnected: true,
          // Always reset connecting state to force re-connect
          path,
        },
      });
    }
    // Set Pending Ledger or Trezor device with ID
    let pendingId;
    logging_1.logger.info('[HW-DEBUG] HWStore deviceId', { deviceId });
    if (
      deviceId ||
      (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER &&
        (!disconnected || recognizedPairedHardwareWallet))
    ) {
      pendingId =
        deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER &&
        recognizedPairedHardwareWallet
          ? recognizedPairedHardwareWallet.device.deviceId
          : new Date().valueOf();
      logging_1.logger.info('[HW-DEBUG] HWStore - SET DEVICE DATA: ', {
        deviceId,
        pendingId,
        disconnected,
      });
      if (deviceId || pendingId) {
        await this._setHardwareWalletDevice({
          deviceId: deviceId || pendingId.toString(),
          // device ID or timestamp (for pending devices without ID) - ledger Only
          data: {
            deviceType,
            deviceModel,
            deviceName,
            path,
            // paired: (recognizedPairedHardwareWallet && deviceType === DeviceTypes.LEDGER)
            //   ? recognizedPairedHardwareWallet.id
            //   : null, // Always reset pairing indication on Trezor to force re-connect and set if exist for Ledger
            paired: null,
            // Always reset pairing indication to force re-connect
            disconnected,
            // device physically disconnected
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ deviceType: DeviceType; deviceModel: strin... Remove this comment to see the full error message
            isPending: !deviceId && !recognizedPairedHardwareWallet,
          },
        });
      }
    }
    await this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();
    // Start connection establishing process if devices listener flag is UP
    logging_1.logger.info('[HW-DEBUG] HWStore - establish connection guard: ', {
      isListeningForDevice: this.isListeningForDevice,
    });
    if (
      this.isListeningForDevice &&
      !disconnected &&
      (!eventType ||
        eventType === hardware_wallets_types_1.DeviceEvents.CONNECT)
    ) {
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: remove device listener',
        () => {
          this.isListeningForDevice = false;
        }
      );
      if (deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER) {
        // To Force Ledger with manual parameters because ID is not available and device not stored to LC
        logging_1.logger.info(
          '[HW-DEBUG] HWStore - CALL establish connection with pendingID: ',
          pendingId
        );
        this.establishHardwareWalletConnection();
      } else {
        this.establishHardwareWalletConnection();
      }
    }
    // Case that allows us to re-trigger tx send process multiple times if device doesn't match sender wallet
    if (
      this.unfinishedWalletTxSigning &&
      !disconnected &&
      eventType === hardware_wallets_types_1.DeviceEvents.CONNECT
    ) {
      logging_1.logger.info(
        '[HW-DEBUG] CHANGE STATUS to: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        Wallet_1.HwDeviceStatuses.CONNECTING
      );
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Change status to Connecting',
        () => {
          this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
        }
      );
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Reinitialize TX signing: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        this.unfinishedWalletTxSigning
      );
      this.initiateTransaction({
        walletId: this.unfinishedWalletTxSigning,
      });
    }
    // Case that allows us to re-trigger address verification process multiple times if fails
    if (
      this.unfinishedWalletAddressVerification &&
      !disconnected &&
      (!eventType ||
        eventType === hardware_wallets_types_1.DeviceEvents.CONNECT)
    ) {
      logging_1.logger.info(
        '[HW-DEBUG] CHANGE STATUS to: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        Wallet_1.HwDeviceStatuses.CONNECTING
      );
      (0, mobx_1.runInAction)(
        'HardwareWalletsStore:: Change status to Connecting',
        () => {
          this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
        }
      );
      logging_1.logger.info(
        '[HW-DEBUG] HWStore - Reinitialize Address Verification process: ',
        (0, helper_1.toJS)(this.unfinishedWalletAddressVerification)
      );
      // It is not possible to pass null value that FLOW marks as error (FlowFixMe used)
      this.initiateAddressVerification(
        // @ts-ignore
        this.unfinishedWalletAddressVerification,
        path
      );
    }
  };
  getLastUnpairedDevice = () =>
    (0, lodash_1.last)(
      (0, lodash_1.sortBy)(
        (0, lodash_1.filter)(
          Object.entries(this.hardwareWalletDevices).map(([key, value]) => ({
            ...value,
            id: key,
          })),
          (hardwareWalletDevice) =>
            // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
            !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
        ),
        ['id']
      )
    );
  cleanUpPendingDevices = async () => {
    const transformedData = Object.entries(this.hardwareWalletDevices).map(
      ([key, value]) => ({
        // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
        isPending: value.isPending,
        id: key,
      })
    );
    const pendingHardwareWalletsIds = transformedData
      .filter(({ isPending }) => isPending)
      .map(({ id }) => id);
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - cleanUpPendingDevices - cleanup ids: ',
      {
        pendingHardwareWalletsIds,
      }
    );
    const unsetHardwareWalletDeviceRequests = pendingHardwareWalletsIds.map(
      (id) =>
        this._unsetHardwareWalletDevice({
          deviceId: id,
        })
    );
    return Promise.all(unsetHardwareWalletDeviceRequests);
  };
  resetInitializedConnection = async (params) => {
    const cancelDeviceAction = (0, lodash_1.get)(
      params,
      'cancelDeviceAction',
      false
    );
    if (cancelDeviceAction) {
      getHardwareWalletChannel_1.resetTrezorActionChannel.request();
    }
    this.stopCardanoAdaAppFetchPoller();
    this.stores.wallets.createHardwareWalletRequest.reset();
    this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    this.transportDevice = {};
    this.isListeningForDevice = false;
    this.isExportKeyAborted = false;
  };
  resetInitializedAddressVerification = async (params) => {
    const cancelDeviceAction = (0, lodash_1.get)(
      params,
      'cancelDeviceAction',
      false
    );
    if (cancelDeviceAction) {
      getHardwareWalletChannel_1.resetTrezorActionChannel.request();
    }
    this.stopCardanoAdaAppFetchPoller();
    this.hwDeviceStatus = Wallet_1.HwDeviceStatuses.CONNECTING;
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    this.transportDevice = {};
    this.isListeningForDevice = false;
    this.isAddressVerificationInitiated = false;
    this.unfinishedWalletAddressVerification = null;
    this.isAddressDerived = false;
    this.isAddressChecked = false;
    this.isAddressCorrect = null;
    // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    this.tempAddressToVerify = {};
    this.isExportKeyAborted = false;
    this.activeDevicePath = null;
  };
  _refreshHardwareWalletsLocalData = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletsLocalDataRequest.execute();
  };
  _refreshHardwareWalletDevices = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletDevicesRequest.execute();
  };
  get hardwareWalletsConnectionData() {
    return this.hardwareWalletsLocalDataRequest.result;
  }
  get hardwareWalletDevices() {
    return this.hardwareWalletDevicesRequest.result;
  }
  checkIsTrezorByWalletId = (walletId) => {
    const hardwareWalletConnectionData = (0, lodash_1.find)(
      this.hardwareWalletsConnectionData,
      (connectionData) => connectionData.id === walletId
    );
    return (
      hardwareWalletConnectionData &&
      hardwareWalletConnectionData.device.deviceType ===
        hardware_wallets_types_1.DeviceTypes.TREZOR
    );
  };
  _resetTxSignRequestData = () => {
    this.selectCoinsRequest.reset();
    // @ts-ignore ts-migrate(2741) FIXME: Property 'coinSelection' is missing in type '{}' b... Remove this comment to see the full error message
    this.txSignRequest = {};
  };
  _deviceType = (deviceModel) => {
    let type;
    switch (deviceModel) {
      case hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S:
        type = hardware_wallets_types_1.DeviceTypes.LEDGER;
        break;
      case hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S_PLUS:
        type = hardware_wallets_types_1.DeviceTypes.LEDGER;
        break;
      case hardware_wallets_types_1.DeviceModels.LEDGER_NANO_X:
        type = hardware_wallets_types_1.DeviceTypes.LEDGER;
        break;
      case hardware_wallets_types_1.DeviceModels.TREZOR_ONE:
        type = hardware_wallets_types_1.DeviceTypes.TREZOR;
        break;
      case hardware_wallets_types_1.DeviceModels.TREZOR_T:
        type = hardware_wallets_types_1.DeviceTypes.TREZOR;
        break;
      default:
        type = null;
    }
    return type;
  };
  _getTtl = () => {
    const { absoluteSlotNumber } = this.stores.networkStatus;
    const ttl = absoluteSlotNumber + txnsConfig_1.TIME_TO_LIVE;
    return ttl;
  };
  _getAbsoluteSlotNumber = () => {
    const { absoluteSlotNumber } = this.stores.networkStatus;
    return absoluteSlotNumber;
  };
  _getHardwareWalletDeviceInfoByWalletId = (walletId) => {
    return (0, lodash_1.find)(
      this.hardwareWalletsConnectionData,
      (connectionData) => connectionData.id === walletId
    );
  };
  _setHardwareWalletLocalData = async ({ walletId, data }) => {
    logging_1.logger.info(
      '[HW-DEBUG] HWStore - CALL SET - _setHardwareWalletLocalData METHOD: ',
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
      walletId
    );
    if (walletId) {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.setHardwareWalletLocalDataRequest.execute(walletId, data);
      this._refreshHardwareWalletsLocalData();
      this.stores.wallets.refreshWalletsData();
    }
  };
  _unsetHardwareWalletLocalData = async ({ walletId }) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('[HW-DEBUG] HWStore - _unsetHardwareWalletLocalData');
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.unsetHardwareWalletLocalDataRequest.execute(walletId);
    const pairedDevice = (0, lodash_1.find)(
      this.hardwareWalletDevices,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
      (recognizedDevice) => recognizedDevice.paired === walletId
    );
    // Unset device <-> wallet paired parameter
    if (pairedDevice) {
      this._setHardwareWalletDevice({
        deviceId: pairedDevice.id,
        data: {
          paired: null,
        },
      });
      this._refreshHardwareWalletDevices();
    }
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };
  _setHardwareWalletDevice = async ({ deviceId, data }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setHardwareWalletDeviceRequest.execute(deviceId, data);
    this._refreshHardwareWalletDevices();
  };
  _unsetHardwareWalletDevice = async ({ deviceId }) => {
    if (deviceId) {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.unsetHardwareWalletDeviceRequest.execute(deviceId);
    } else {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
    }
    this._refreshHardwareWalletDevices();
  };
  // For testing / development ONLY
  _resetHardwareWallets = async () => {
    if (isDev) {
      await Promise.all(
        this.stores.wallets.all.map(async (wallet) => {
          if (wallet.isHardwareWallet) {
            return this.stores.wallets._deleteWallet({
              walletId: wallet.id,
              isLegacy: wallet.isLegacy,
            });
          }
          return null;
        })
      );
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.unsetHardwareWalletDevicesAllRequest.execute();
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();
    }
  };
  stopCardanoAdaAppFetchPoller = () => {
    logging_1.logger.info('[HW-DEBUG] HWStore - STOP Ada App poller');
    if (this.cardanoAdaAppPoller) {
      this.cardanoAdaAppPoller.stop();
      this.cardanoAdaAppPoller = null;
    }
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'selectCoinsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'sendMoneyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'hardwareWalletsLocalDataRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'setHardwareWalletLocalDataRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'unsetHardwareWalletLocalDataRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'hardwareWalletDevicesRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'setHardwareWalletDeviceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'overrideHardwareWalletDevicesRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'unsetHardwareWalletDeviceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'unsetHardwareWalletDevicesAllRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  HardwareWalletsStore.prototype,
  'unsetHardwareWalletLocalDataAllRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'hwDeviceStatus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'extendedPublicKey',
  void 0
);
__decorate(
  [
    mobx_1.observable,
    // @ts-ignore ts-migrate(2741) FIXME: Property 'coinSelection' is missing in type '{}' b... Remove this comment to see the full error message
    __metadata('design:type', Object),
  ],
  HardwareWalletsStore.prototype,
  'txSignRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'transportDevice',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'txBody',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isTransactionPending',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isTrezorBridgeInstalled',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isTransactionInitiated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'activeDevicePath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'unfinishedWalletTxSigning',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isListeningForDevice',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isConnectInitiated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isAddressVerificationInitiated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isWalletPairingInitiated',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', WalletAddress_1.default)],
  HardwareWalletsStore.prototype,
  'unfinishedWalletAddressVerification',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isAddressDerived',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isAddressChecked',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  HardwareWalletsStore.prototype,
  'isAddressCorrect',
  void 0
);
__decorate(
  [
    mobx_1.observable,
    // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    __metadata('design:type', Object),
  ],
  HardwareWalletsStore.prototype,
  'tempAddressToVerify',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'isExportKeyAborted',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'activeDelegationWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  HardwareWalletsStore.prototype,
  'activeVotingWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'votingData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'checkTransaction',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'resetStakePoolTransactionChecker',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'setTransactionPendingState',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'establishHardwareWalletConnection',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'getCardanoAdaApp',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'verifyAddress',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'initiateWalletPairing',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'resetWalletPairing',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'showAddress',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'setAddressVerificationCheckStatus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_resetInitiatedConnection',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_requestExtendedPublicKey',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_findAssociatedWalletByExtendedPublicKey',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_deletePendingDeviceWithGivenPath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_discardConnectedDeviceAndReInitiateTransaction',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_proceedWithTransactionAfterConnectingDevice',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_discardConnectedDeviceAndReInitiateAddressVerification',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_proceedWithAddressVerificationAfterConnectingDevice',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_createNewWalletForRecognizedPendingDevice',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_identifyAndHandleAssociatedWallet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_signTransactionTrezor',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_signTransactionLedger',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_changeHardwareWalletConnectionStatus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'resetInitializedConnection',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  'resetInitializedAddressVerification',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_refreshHardwareWalletsLocalData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  HardwareWalletsStore.prototype,
  '_refreshHardwareWalletDevices',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  HardwareWalletsStore.prototype,
  'hardwareWalletsConnectionData',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  HardwareWalletsStore.prototype,
  'hardwareWalletDevices',
  null
);
exports.default = HardwareWalletsStore;
//# sourceMappingURL=HardwareWalletsStore.js.map
