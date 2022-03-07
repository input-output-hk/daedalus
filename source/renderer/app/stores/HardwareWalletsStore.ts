import { observable, action, runInAction, computed } from 'mobx';
import { get, map, find, findLast, includes } from 'lodash';
import semver from 'semver';
import {
  TransactionSigningMode,
  AddressType,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { HwDeviceStatuses } from '../domains/Wallet';
import WalletAddress from '../domains/WalletAddress';
import { toJS } from '../../../common/utils/helper';
import {
  SHELLEY_PURPOSE_INDEX,
  ADA_COIN_TYPE,
  MINIMAL_TREZOR_FIRMWARE_VERSION,
  MINIMAL_LEDGER_FIRMWARE_VERSION,
  MINIMAL_CARDANO_APP_VERSION,
  isHardwareWalletSupportEnabled,
  isTrezorEnabled,
  isLedgerEnabled,
  getHardwareWalletsNetworkConfig,
} from '../config/hardwareWalletsConfig';
import { TIME_TO_LIVE } from '../config/txnsConfig';
import {
  getHardwareWalletTransportChannel,
  getExtendedPublicKeyChannel,
  getCardanoAdaAppChannel,
  getHardwareWalletConnectionChannel,
  signTransactionLedgerChannel,
  signTransactionTrezorChannel,
  handleInitTrezorConnectChannel,
  handleInitLedgerConnectChannel,
  resetTrezorActionChannel,
  deriveAddressChannel,
  showAddressChannel,
} from '../ipc/getHardwareWalletChannel';
import {
  prepareLedgerInput,
  prepareLedgerOutput,
  prepareTxAux,
  prepareBody,
  prepareLedgerCertificate,
  prepareLedgerWithdrawal,
  CachedDeriveXpubFactory,
  ShelleyTxWitnessShelley,
  ShelleyTxInputFromUtxo,
  ShelleyTxOutput,
  ShelleyTxCert,
  ShelleyTxWithdrawal,
  cborizeTxAuxiliaryVotingData,
  prepareLedgerAuxiliaryData,
  CATALYST_VOTING_REGISTRATION_TYPE,
} from '../utils/shelleyLedger';
import {
  prepareTrezorInput,
  prepareTrezorOutput,
  prepareTrezorCertificate,
  prepareTrezorWithdrawal,
  prepareTrezorAuxiliaryData,
  TrezorTransactionSigningMode,
} from '../utils/shelleyTrezor';
import {
  DeviceModels,
  DeviceTypes,
  DeviceEvents,
} from '../../../common/types/hardware-wallets.types';
import { formattedAmountToLovelace } from '../utils/formatters';
import { TransactionStates } from '../domains/WalletTransaction';
import {
  CERTIFICATE_TYPE,
  getParamsFromPath,
} from '../utils/hardwareWalletUtils';
import type { HwDeviceStatus } from '../domains/Wallet';
import type {
  CoinSelectionsPaymentRequestType,
  CoinSelectionsDelegationRequestType,
  CreateExternalTransactionResponse,
  CoinSelectionsResponse,
  VotingDataType,
} from '../api/transactions/types';
import type {
  HardwareWalletLocalData,
  HardwareWalletsLocalData,
  HardwareWalletDevicesType,
  SetHardwareWalletLocalDataRequestType,
  SetHardwareWalletDeviceRequestType,
} from '../api/utils/localStorage';
import type {
  TransportDevice,
  LedgerModel,
  TrezorModel,
  HardwareWalletExtendedPublicKeyResponse,
  HardwareWalletConnectionRequest,
  Witness,
  TrezorWitness,
} from '../../../common/types/hardware-wallets.types';
import { logger } from '../utils/logging';

export type TxSignRequestTypes = {
  coinSelection: CoinSelectionsResponse;
};
export type ByronEncodeSignedTransactionRequest = {
  txDataHex: string;
  witnesses: Array<ByronSignedTransactionWitnesses>;
};
export type ByronSignedTransactionWitnesses = {
  signature: string;
  xpub: HardwareWalletExtendedPublicKeyResponse;
};
export type AddressVerificationCheckStatus = 'valid' | 'invalid' | 'reverify';
export type TempAddressToVerify = {
  address: WalletAddress;
  path: string | null | undefined;
  isTrezor: boolean;
};
export const AddressVerificationCheckStatuses: {
  VALID: string;
  INVALID: string;
  REVERIFY: string;
} = {
  VALID: 'valid',
  INVALID: 'invalid',
  REVERIFY: 'reverify',
};
const CARDANO_ADA_APP_POLLING_INTERVAL = 1000;
const DEFAULT_HW_NAME = 'Hardware Wallet';

const useCardanoAppInterval = (
  getCardanoAdaApp: any,
  interval: number,
  path: string | null | undefined,
  address: string | null | undefined,
  addressVerification: WalletAddress | null | undefined
) =>
  setInterval(
    (devicePath, txWalletId, verificationAddress): any => {
      try {
        return getCardanoAdaApp({
          path: devicePath,
          walletId: txWalletId,
          address: verificationAddress,
        });
      } catch (_error) {
        return null;
      }
    },
    interval,
    path,
    address,
    addressVerification
  );

const { network, isDev } = global.environment;
const hardwareWalletsNetworkConfig = getHardwareWalletsNetworkConfig(network);
export default class HardwareWalletsStore extends Store {
  @observable
  selectCoinsRequest: Request<CoinSelectionsResponse> = new Request(
    this.api.ada.selectCoins
  );
  @observable
  sendMoneyRequest: Request<CreateExternalTransactionResponse> = new Request(
    this.api.ada.createExternalTransaction
  );
  // @TODO - improve types
  getPublicKeyRequest: Request<any> = new Request(this.api.ada.getPublicKey);
  // @TODO - improve types
  constructAddressRequest: Request<any> = new Request(
    this.api.ada.constructAddress
  );
  @observable
  hardwareWalletsLocalDataRequest: Request<
    HardwareWalletsLocalData
  > = new Request(this.api.localStorage.getHardwareWalletsLocalData);
  @observable
  setHardwareWalletLocalDataRequest: Request<
    HardwareWalletLocalData
  > = new Request(this.api.localStorage.setHardwareWalletLocalData);
  @observable
  unsetHardwareWalletLocalDataRequest: Request<void> = new Request(
    this.api.localStorage.unsetHardwareWalletLocalData
  );
  @observable
  hardwareWalletDevicesRequest: Request<HardwareWalletsLocalData> = new Request(
    this.api.localStorage.getHardwareWalletDevices
  );
  @observable
  setHardwareWalletDeviceRequest: Request<
    HardwareWalletLocalData
  > = new Request(this.api.localStorage.setHardwareWalletDevice);
  @observable
  overrideHardwareWalletDevicesRequest: Request<
    HardwareWalletDevicesType
  > = new Request(this.api.localStorage.overrideHardwareWalletDevices);
  @observable
  unsetHardwareWalletDeviceRequest: Request<
    HardwareWalletLocalData
  > = new Request(this.api.localStorage.unsetHardwareWalletDevice);
  @observable
  unsetHardwareWalletDevicesAllRequest: Request<void> = new Request(
    this.api.localStorage.unsetHardwareWalletDevicesAll
  );
  @observable
  unsetHardwareWalletLocalDataAllRequest: Request<
    HardwareWalletLocalData
  > = new Request(this.api.localStorage.unsetHardwareWalletLocalDataAll);
  @observable
  hwDeviceStatus: HwDeviceStatus = HwDeviceStatuses.CONNECTING;
  @observable
  extendedPublicKey:
    | HardwareWalletExtendedPublicKeyResponse
    | null
    | undefined = null;
  @observable
  // @ts-ignore ts-migrate(2741) FIXME: Property 'coinSelection' is missing in type '{}' b... Remove this comment to see the full error message
  txSignRequest: TxSignRequestTypes = {};
  @observable
  transportDevice: TransportDevice | null | undefined = null;
  @observable
  txBody: string | null | undefined = null;
  @observable
  isTransactionPending = false;
  @observable
  isTrezorBridgeInstalled = false;
  @observable
  isTransactionInitiated = false;
  @observable
  activeDevicePath: string | null | undefined = null;
  @observable
  unfinishedWalletTxSigning: string | null | undefined = null;
  @observable
  isListeningForDevice = false;
  @observable
  isConnectInitiated = false;
  @observable
  isAddressVerificationInitiated = false;
  @observable
  unfinishedWalletAddressVerification: WalletAddress | null | undefined = null;
  @observable
  isAddressDerived = false;
  @observable
  isAddressChecked = false;
  @observable
  isAddressCorrect: boolean | null | undefined = null;
  @observable
  // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  tempAddressToVerify: TempAddressToVerify = {};
  @observable
  isExportKeyAborted = false;
  @observable
  activeDelegationWalletId: string | null | undefined = null;
  @observable
  activeVotingWalletId: string | null | undefined = null;
  @observable
  votingData: VotingDataType | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  cardanoAdaAppPollingInterval: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  checkTransactionTimeInterval: IntervalID | null | undefined = null;

  setup() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] HWStore - setup');
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.sendMoney.listen(this._sendMoney);
    hardwareWalletsActions.refreshHardwareWalletsLocalData.listen(
      this._refreshHardwareWalletsLocalData
    );
    getHardwareWalletConnectionChannel.onReceive(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(params: HardwareWalletConnectio... Remove this comment to see the full error message
      this._changeHardwareWalletConnectionStatus
    );
    this.initTrezor();
    this.initLedger();
    this.hardwareWalletsLocalDataRequest.execute();
    this.hardwareWalletDevicesRequest.execute();
  }

  initTrezor = async () => {
    if (isHardwareWalletSupportEnabled && isTrezorEnabled) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - start trezor');
      await handleInitTrezorConnectChannel.request();
      await this.getAvailableDevices({
        isTrezor: true,
      });
    }
  };
  initLedger = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug(
      `[HW-DEBUG] HWStore - initLedger() | isHardwareWalletSupportEnabled=${isHardwareWalletSupportEnabled.toString()} isLedgerEnabled=${isLedgerEnabled.toString()}`
    );

    if (isHardwareWalletSupportEnabled && isLedgerEnabled) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - start ledger');
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.hardwareWalletDevicesRequest.execute();
      const storedDevices = this.hardwareWalletDevicesRequest.result;
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - storedDevices fetched');
      const devicesWithoutLedgers = {};
      map(storedDevices, async (device) => {
        if (device.deviceType === DeviceTypes.TREZOR) {
          devicesWithoutLedgers[device.id] = device;
        }
      });
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - Remove all LEDGERS from LC');
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.overrideHardwareWalletDevicesRequest.execute(
        devicesWithoutLedgers
      );
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - Refresh LC');
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - INIT Ledger listeners');
      await handleInitLedgerConnectChannel.request();
      await this.getAvailableDevices({
        isTrezor: false,
      });
    }
  };
  getAvailableDevices = async (params: { isTrezor: boolean }) => {
    const { isTrezor } = params;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletsLocalDataRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletDevicesRequest.execute();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] HWStore - getAvailableDevices');
    // Set all logical HW into disconnected state
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] HWStore - Set Hardware Wallets local data');
    map(this.hardwareWalletsConnectionData, async (connectedWallet) => {
      await this._setHardwareWalletLocalData({
        walletId: connectedWallet.id,
        data: {
          disconnected: true,
        },
      });
    });
    // Initiate Device Check for each stored device
    map(this.hardwareWalletDevices, async (device) => {
      // Prevent device check if device is TREZOR and bridge not installed
      if (
        (!isTrezor && device.deviceType !== DeviceTypes.LEDGER) ||
        (isTrezor &&
          (device.deviceType !== DeviceTypes.TREZOR ||
            (device.deviceType === DeviceTypes.TREZOR &&
              !this.isTrezorBridgeInstalled)))
      ) {
        return;
      }

      try {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - CHECK device');

        if (device.deviceType === DeviceTypes.TREZOR) {
          await getHardwareWalletTransportChannel.request({
            // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
            devicePath: device.path,
            isTrezor: true,
          });
        }
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        // eslint-disable-next-line
        logger.debug(' HWStore - CHECK device Error');
      }
    });
    await this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();
  };
  _sendMoney = async (params?: {
    isDelegationTransaction?: boolean;
    isVotingRegistrationTransaction?: boolean;
    selectedWalletId?: string;
  }) => {
    const isDelegationTransaction = get(params, 'isDelegationTransaction');
    const isVotingRegistrationTransaction = get(
      params,
      'isVotingRegistrationTransaction'
    );
    const activeWalletId = get(this.stores.wallets, ['active', 'id']);
    const selectedWalletId = get(params, 'selectedWalletId');
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
      } else {
        this.setTransactionPendingState(false);
      }

      this.stores.wallets.refreshWalletsData();
      this.sendMoneyRequest.reset();
      return transaction;
    } catch (e) {
      this.setTransactionPendingState(false);
      runInAction('HardwareWalletsStore:: reset Transaction verifying', () => {
        this.txBody = null;
        this.activeDevicePath = null;
        this.unfinishedWalletTxSigning = null;
        this.votingData = null;
      });
      throw e;
    }
  };
  // Check stake pool transaction state and reset pending state when transction is "in_ledger"
  @action
  checkTransaction = (request: {
    transactionId: string;
    walletId: string;
    isVotingRegistrationTransaction: boolean;
  }) => {
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
      targetTransaction = find(
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
      targetTransaction = find(
        recentTransactions,
        (transaction) =>
          transaction.id === transactionId &&
          transaction.state === TransactionStates.OK
      );

      if (targetTransaction) {
        this.resetStakePoolTransactionChecker(walletId);
      }
    }
  };
  @action
  resetStakePoolTransactionChecker = (walletId: string) => {
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
  @action
  setTransactionPendingState = (isTransactionPending: boolean) => {
    runInAction('HardwareWalletsStore:: set transaction state', () => {
      this.isTransactionPending = isTransactionPending;
    });
  };
  // @TODO - move to Transactions store once all logic fit and hardware wallets listed in general wallets list
  selectCoins = async (params: CoinSelectionsPaymentRequestType) => {
    const { walletId, address, amount, assets, metadata } = params;
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
        payments: {
          address,
          amount,
          assets,
        },
        metadata,
      });
      runInAction('HardwareWalletsStore:: set coin selections', () => {
        this.txSignRequest = {
          coinSelection,
        };
      });
      return coinSelection;
    } catch (e) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw e;
    }
  };
  selectDelegationCoins = async (
    params: CoinSelectionsDelegationRequestType
  ) => {
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
      runInAction('HardwareWalletsStore:: set coin selections', () => {
        this.txSignRequest = {
          coinSelection,
        };
      });
      return coinSelection;
    } catch (e) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw e;
    }
  };
  @action
  establishHardwareWalletConnection = async () => {
    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { hardwareWalletDevices, hardwareWalletsConnectionData } = this;
    logger.debug('[HW-DEBUG] HWStore - establishHardwareWalletConnection', {
      hardwareWalletDevices: toJS(hardwareWalletDevices),
      hardwareWalletsConnectionData: toJS(hardwareWalletsConnectionData),
      activeDelegationWalletId: toJS(this.activeDelegationWalletId),
      isTransactionInitiated: toJS(this.isTransactionInitiated),
    });

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
        activeWalletId = get(this.stores.wallets, ['active', 'id']);
      }

      if (activeWalletId) {
        // Check if device connected to wallet
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - active wallet exists');
        recognizedPairedHardwareWallet = find(
          hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
          (recognizedDevice) => recognizedDevice.paired === activeWalletId
        );
        relatedConnectionData = find(
          hardwareWalletsConnectionData,
          (connection) => connection.id === activeWalletId
        );
      }

      const lastUnpairedDevice = findLast(
        this.hardwareWalletDevices,
        (hardwareWalletDevice) =>
          // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
          !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
      );
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug(
        '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: START'
      );
      // Tx Special cases!
      // This means that transaction needs to be signed but we don't know device connected to Software wallet
      let transportDevice;

      if (this.isTransactionInitiated || this.isAddressVerificationInitiated) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - check device'
        );

        // Return device that belongs to active hardwate wallet if is already plugged-in
        if (
          recognizedPairedHardwareWallet &&
          !recognizedPairedHardwareWallet.disconnected
        ) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug(
            '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - Recognized device found'
          );
          logger.debug('[HW-DEBUG] HWStore - Set transport device 1', {
            recognizedPairedHardwareWallet: toJS(
              recognizedPairedHardwareWallet
            ),
          });
          runInAction('HardwareWalletsStore:: Set transport device', () => {
            this.transportDevice = recognizedPairedHardwareWallet;
          });
          // Special case when Pub key export rejected by the user and then device reconnected
          // Force export again and proceed (continue) with last action
          const isTrezor =
            recognizedPairedHardwareWallet.deviceType === DeviceTypes.TREZOR;

          if (this.isExportKeyAborted) {
            if (isTrezor) {
              await this._getExtendedPublicKey(
                recognizedPairedHardwareWallet.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            } else {
              this.cardanoAdaAppPollingInterval = useCardanoAppInterval(
                this.getCardanoAdaApp,
                CARDANO_ADA_APP_POLLING_INTERVAL,
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
        const relatedConnectionDataDeviceType = get(relatedConnectionData, [
          'device',
          'deviceType',
        ]);
        const isTrezor = relatedConnectionDataDeviceType === DeviceTypes.TREZOR;
        let lastDeviceTransport = null;

        if (relatedConnectionDataDeviceType) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug(
            '[HW-DEBUG] HWStore - Connect - New Transaction / Address verification initiated - return last device'
          );
          // @ts-ignore
          lastDeviceTransport = await getHardwareWalletTransportChannel.request(
            {
              devicePath: null,
              // Use last plugged device
              isTrezor,
            }
          );
          logger.debug('[HW-DEBUG] HWStore - Set transport device 2', {
            lastDeviceTransport: toJS(lastDeviceTransport),
          });
          runInAction('HardwareWalletsStore:: Set transport device', () => {
            this.transportDevice = lastDeviceTransport;
          });

          // Special case when Pub key export rejected by the user and then device reconnected
          // Force export again and proceed (continue) with last action
          if (this.isExportKeyAborted) {
            if (isTrezor) {
              await this._getExtendedPublicKey(
                lastDeviceTransport.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            } else {
              this.cardanoAdaAppPollingInterval = useCardanoAppInterval(
                this.getCardanoAdaApp,
                CARDANO_ADA_APP_POLLING_INTERVAL,
                lastDeviceTransport.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            }
          } // End of special case
        }

        return lastDeviceTransport;
      }

      // End of Tx Special cases!
      // Cases for wallet create / restore
      // it is triggered after flag activation "isListeningForDevice"
      if (lastUnpairedDevice) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Start process with last UNPAIRED device'
        );
        // Start listeners for specific (last pluged) device
        let devicePath = null;
        let isTrezor = false;

        if (lastUnpairedDevice) {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
          devicePath = lastUnpairedDevice.path;
          isTrezor = lastUnpairedDevice.deviceType === DeviceTypes.TREZOR;
        }

        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Listening for device'
        );

        if (lastUnpairedDevice.deviceType === DeviceTypes.TREZOR) {
          transportDevice = await getHardwareWalletTransportChannel.request({
            devicePath,
            isTrezor,
          });
        } else {
          transportDevice = lastUnpairedDevice;
        }

        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - Transport retreived');
      } else {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Set device listener'
        );
        runInAction('HardwareWalletsStore:: set device listener', () => {
          this.isListeningForDevice = true;
        });
        return null;
      }

      // End of Cases for wallet create / restore
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug(
        '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: start process with known transport'
      );

      if (transportDevice) {
        const { deviceType, firmwareVersion } = transportDevice;

        // Check if device is supported
        if (
          (deviceType === DeviceTypes.TREZOR && !DeviceModels.TREZOR_T) ||
          (deviceType === DeviceTypes.LEDGER &&
            !DeviceModels.LEDGER_NANO_S &&
            !DeviceModels.LEDGER_NANO_X)
        ) {
          runInAction(
            'HardwareWalletsStore:: set HW device CONNECTING FAILED - device not supported',
            () => {
              this.hwDeviceStatus = HwDeviceStatuses.UNSUPPORTED_DEVICE;
            }
          );
          throw new Error('Device not Supported!');
        }

        // @TODO - missing firmware version for LEDGER
        // Check Firmware version
        if (deviceType === DeviceTypes.TREZOR) {
          const minFirmwareVersion =
            deviceType === DeviceTypes.TREZOR
              ? MINIMAL_TREZOR_FIRMWARE_VERSION
              : MINIMAL_LEDGER_FIRMWARE_VERSION;
          const isFirmwareVersionValid = semver.gte(
            firmwareVersion,
            minFirmwareVersion
          );

          if (!isFirmwareVersionValid) {
            runInAction(
              'HardwareWalletsStore:: set HW device CONNECTING FAILED - wrong firmware',
              () => {
                this.hwDeviceStatus = HwDeviceStatuses.WRONG_FIRMWARE;
              }
            );
            throw new Error(
              `Firmware must be ${minFirmwareVersion} or greater!`
            );
          }
        }

        // All Checks pass - mark device as connected (set transport device for this session)
        logger.debug('[HW-DEBUG] HWStore - Set transport device 3', {
          transportDevice: toJS(transportDevice),
        });
        runInAction('HardwareWalletsStore:: set HW device CONNECTED', () => {
          this.transportDevice = transportDevice;
        });

        if (deviceType === DeviceTypes.TREZOR) {
          // Jump to exporting public key
          await this._getExtendedPublicKey(transportDevice.path);
        } else {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug('[HW-DEBUG] HWStore - START cardano app poller');
          // Start poller to recognize if Cardano App is launched on device
          const devicePath = transportDevice.path;
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug(
            '[HW-DEBUG] HWStore - getCardanoAdaApp - from  establishHardwareWalletConnection'
          );
          this.stopCardanoAdaAppFetchPoller();
          // @ts-ignore ts-migrate(2554) FIXME: Expected 5 arguments, but got 3.
          this.cardanoAdaAppPollingInterval = useCardanoAppInterval(
            this.getCardanoAdaApp,
            CARDANO_ADA_APP_POLLING_INTERVAL,
            devicePath
          );
        }
      } else {
        runInAction(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
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
        runInAction(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.TREZOR_BRIDGE_FAILURE;
          }
        );
        throw new Error('Trezor Bridge not installed!');
      }

      throw e;
    }
  };
  // Ledger method only
  @action
  getCardanoAdaApp = async (params: {
    path: string | null | undefined;
    walletId?: string;
    address?: WalletAddress | null | undefined;
  }) => {
    const { path, walletId, address } = params;
    logger.debug(
      '[HW-DEBUG] HWStore - START FUNCTION getCardanoAdaApp PARAMS: ',
      {
        walletId,
        path,
        address,
      }
    );
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;

    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request({
        path,
      });
      logger.debug(
        '[HW-DEBUG] HWStore - cardanoAdaApp RESPONSE: ',
        toJS(cardanoAdaApp)
      );
      // Cardano app recognized, stop poller
      this.stopCardanoAdaAppFetchPoller();

      if (cardanoAdaApp) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - cardanoAdaApp - Set device');
        // Check is Cardano App version supported
        const cardanoAppVersion = `${cardanoAdaApp.major}.${cardanoAdaApp.minor}.${cardanoAdaApp.patch}`;
        const isValidAppVersion = semver.gte(
          cardanoAppVersion,
          MINIMAL_CARDANO_APP_VERSION
        );

        if (!isValidAppVersion) {
          runInAction(
            'HardwareWalletsStore:: set HW device CONNECTING FAILED - wrong firmware',
            () => {
              this.hwDeviceStatus = HwDeviceStatuses.WRONG_CARDANO_APP_VERSION;
            }
          );
          throw new Error(
            `Cardano app must be ${MINIMAL_CARDANO_APP_VERSION} or greater!`
          );
        }

        await this._getExtendedPublicKey(path, walletId, address);
      }
    } catch (error) {
      logger.debug('[HW-DEBUG] HWStore - Cardano app fetching error', {
        error: toJS(error),
      });
      const isDeviceBusy = includes(error.message, 'Ledger Device is busy');

      if (isDeviceBusy) {
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
        this.stopCardanoAdaAppFetchPoller();
        logger.debug('[HW-DEBUG] Device is busy: ', {
          walletId,
          error,
          address,
          isTransactionInitiated: this.isTransactionInitiated,
          isAddressVerificationInitiated: this.isAddressVerificationInitiated,
        });
        runInAction(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
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

      if (error.code === 'DEVICE_NOT_CONNECTED') {
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();
        runInAction(
          'HardwareWalletsStore:: Re-run initiated connection',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
            this.isListeningForDevice = true;
          }
        );
      } else if (error.code === 'DEVICE_PATH_CHANGED' && error.path) {
        // Special case on Windows where device path changes after opening Cardano app
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();
        const pairedDevice = find(
          this.hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
          (recognizedDevice) => recognizedDevice.path === path
        );
        // Update device with new path - LC
        await this._setHardwareWalletDevice({
          deviceId: pairedDevice.id,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ path: any; isPending: false; id: string; d... Remove this comment to see the full error message
          data: { ...pairedDevice, path: error.path, isPending: false },
        });

        // Update connected wallet data with new path - LC
        if (walletId) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug('[HW-DEBUG] Update connected wallet data with new path');
          const hardwareWalletConnectionData = get(
            this.hardwareWalletsConnectionData,
            walletId
          );

          if (hardwareWalletConnectionData) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug(
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
          logger.debug(
            '[HW-DEBUG] Update connected wallet data with new path - Set to LC'
          );
          runInAction(
            'HardwareWalletsStore:: Change active device path for Transaction send',
            () => {
              this.activeDevicePath = error.path;
            }
          );
        }

        this.cardanoAdaAppPollingInterval = setInterval(
          (devicePath, txWalletId, verificationAddress) =>
            this.getCardanoAdaApp({
              path: devicePath,
              walletId: txWalletId,
              address: verificationAddress,
            }),
          CARDANO_ADA_APP_POLLING_INTERVAL,
          error.path,
          walletId,
          address
        );
      }

      throw error;
    }
  };
  isAddressVerificationEnabled = (walletId: string) => {
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId,
      {}
    );
    const deviceType = get(hardwareWalletConnectionData, [
      'device',
      'deviceType',
    ]);
    return deviceType === DeviceTypes.LEDGER;
  };
  initiateAddressVerification = async (
    address: WalletAddress,
    path: string | null | undefined
  ) => {
    if (this.isAddressVerificationInitiated) return;
    logger.debug('[HW-DEBUG] HWStore - Initiate Address Verification: ', {
      address: toJS(address),
      path,
    });
    runInAction('HardwareWalletsStore:: Initiate Address Verification', () => {
      this.isAddressVerificationInitiated = true;
      this.unfinishedWalletAddressVerification = address;
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const walletId = get(this.stores.wallets, ['active', 'id']);
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );
    logger.debug('[HW-DEBUG] HWStore - Verify address with wallet: ', {
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
    logger.debug(
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
      logger.debug('[HW-DEBUG] CHECK FOR NEXT device');

      try {
        transportDevice = await this.establishHardwareWalletConnection();

        if (transportDevice) {
          devicePath = transportDevice.path;
          logger.debug('[HW-DEBUG] HWStore - Set transport device 4', {
            transportDevice: toJS(transportDevice),
          });
          runInAction(
            'HardwareWalletsStore:: Set transport device fomr tx init',
            () => {
              this.transportDevice = transportDevice;
            }
          );
        }
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - Establishing connection failed');
      }
    }

    if (deviceType === DeviceTypes.TREZOR) {
      logger.debug('[HW-DEBUG] Verify Address with Trezor: ', {
        address: toJS(address),
      });

      if (!transportDevice) {
        transportDevice = await this.establishHardwareWalletConnection();
        logger.debug('[HW-DEBUG] HWStore - Set transport device 4', {
          transportDevice: toJS(transportDevice),
        });
      }

      runInAction(
        'HardwareWalletsStore:: Set transport device from tx init',
        () => {
          this.transportDevice = transportDevice;
        }
      );
      const newConnectionData = get(
        this.hardwareWalletsConnectionData,
        walletId
      );
      const activeDevice =
        find(
          this.hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
          (hardwareWalletDevice) => hardwareWalletDevice.paired === walletId
        ) || {};
      // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type '{}'.
      devicePath = activeDevice.path || path || newConnectionData.path || null;
      await this._getExtendedPublicKey(devicePath, walletId, address);
    } else {
      logger.debug('[HW-DEBUG] Verify Address with Ledger: ', {
        address: toJS(address),
        devicePath,
      });
      this.stopCardanoAdaAppFetchPoller();
      this.cardanoAdaAppPollingInterval = setInterval(
        (verificationDevicePath, addressToVerify) =>
          this.getCardanoAdaApp({
            path: verificationDevicePath,
            walletId,
            address: addressToVerify,
          }),
        CARDANO_ADA_APP_POLLING_INTERVAL,
        devicePath,
        address
      );
    }
  };
  @action
  verifyAddress = async (params: {
    address: WalletAddress;
    path: string | null | undefined;
    isTrezor: boolean;
  }) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] - VERIFY Address');
    const { address, path, isTrezor } = params;
    this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS;
    this.tempAddressToVerify = params;

    try {
      const derivedAddress = await deriveAddressChannel.request({
        devicePath: path,
        isTrezor,
        addressType: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        spendingPathStr: address.spendingPath,
        stakingPathStr: `${SHELLEY_PURPOSE_INDEX}'/${ADA_COIN_TYPE}'/0'/2/0`,
        networkId: hardwareWalletsNetworkConfig.networkId,
        protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
      });

      if (derivedAddress === address.id) {
        logger.debug('[HW-DEBUG] HWStore - Address successfully verified', {
          address: derivedAddress,
        });

        if (isTrezor) {
          runInAction(
            'HardwareWalletsStore:: Address Verified and is correct - Trezor',
            () => {
              this.isAddressDerived = true;
              this.isAddressChecked = true;
              this.isListeningForDevice = false;
              this.hwDeviceStatus =
                HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION;
            }
          );
        } else {
          runInAction(
            'HardwareWalletsStore:: Address Verified and is correct - Ledger',
            () => {
              this.isAddressDerived = true;
            }
          );
          this.showAddress(params);
        }
      } else {
        runInAction(
          'HardwareWalletsStore:: Address Verified but not correct',
          () => {
            this.isAddressDerived = false;
            this.isAddressChecked = false;
            this.isAddressCorrect = false;
            this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
          }
        );
      }
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - Verifying address error');

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
      logger.debug('[HW-DEBUG] HWStore - Verifying error case: ', {
        isCancelled,
        isAborted,
      });

      if (isCancelled || isAborted) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - verifyAddress:: WAIT FOR ANOTHER DEVICE'
        );
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();
        runInAction(
          'HardwareWalletsStore:: Re-run initiated connection',
          () => {
            this.isAddressDerived = false;
            this.isAddressChecked = false;
            this.isAddressCorrect = false;
            this.isListeningForDevice = true;
            this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED;
          }
        );
      } else {
        runInAction('HardwareWalletsStore:: Cannot Verify Address', () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
          this.isAddressDerived = false;
          this.isAddressChecked = false;
          this.isAddressCorrect = false;
        });
      }

      throw error;
    }
  };
  @action
  showAddress = async (params: {
    address: WalletAddress;
    path: string | null | undefined;
    isTrezor: boolean;
  }) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] - SHOW Address');
    const { address, path, isTrezor } = params;

    try {
      await showAddressChannel.request({
        devicePath: path,
        isTrezor,
        addressType: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
        spendingPathStr: address.spendingPath,
        stakingPathStr: `${SHELLEY_PURPOSE_INDEX}'/${ADA_COIN_TYPE}'/0'/2/0`,
        networkId: hardwareWalletsNetworkConfig.networkId,
        protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
      });
      runInAction(
        'HardwareWalletsStore:: Address show process finished',
        () => {
          this.isAddressChecked = true;
          this.isListeningForDevice = true;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION;
        }
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - Show address error');
      runInAction('HardwareWalletsStore:: Showing address failed', () => {
        this.isAddressChecked = false;
        this.isAddressCorrect = false;
        this.isListeningForDevice = true;
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_FAILED;
      });
      throw error;
    }
  };
  @action
  setAddressVerificationCheckStatus = (
    checkStatus: AddressVerificationCheckStatus
  ) => {
    // Yes / No - Reverify / No - Invalid
    if (checkStatus === AddressVerificationCheckStatuses.VALID) {
      runInAction(
        'HardwareWalletsStore:: Set address verification status CORRECT',
        () => {
          this.isAddressCorrect = true;
          this.isListeningForDevice = true;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_SUCCEEDED;
        }
      );
    }

    if (checkStatus === AddressVerificationCheckStatuses.INVALID) {
      runInAction(
        'HardwareWalletsStore:: Set address verification status CORRECT',
        () => {
          this.isAddressCorrect = false;
          this.isListeningForDevice = true;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED;
        }
      );
    }

    if (checkStatus === AddressVerificationCheckStatuses.REVERIFY) {
      runInAction(
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
  @action
  _getExtendedPublicKey = async (
    forcedPath: string | null | undefined,
    walletId?: string,
    address?: WalletAddress | null | undefined
  ) => {
    logger.debug('[HW-DEBUG] HWStore - extendedPublicKey', {
      forcedPath,
      walletId,
      address: toJS(address),
    });
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { transportDevice } = this;

    if (!transportDevice) {
      throw new Error(
        'Can not export extended public key: Device not recognized!'
      );
    }

    const { deviceType, path, deviceName, deviceModel } = transportDevice;
    const isTrezor = deviceType === DeviceTypes.TREZOR;
    const devicePath = forcedPath || path;

    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path: "1852'/1815'/0'",
        // Shelley 1852 ADA 1815 indicator for account '0'
        isTrezor,
        devicePath,
      });
      const deviceId = extendedPublicKey.deviceId || transportDevice.deviceId;
      logger.debug('[HW-DEBUG] HWStore - EXPORT - deviceID: ', {
        deviceId,
      });
      const recognizedStoredWallet = find(
        this.hardwareWalletsConnectionData,
        (hardwareWalletData) =>
          extendedPublicKey.chainCodeHex ===
            hardwareWalletData.extendedPublicKey.chainCodeHex &&
          extendedPublicKey.publicKeyHex ===
            hardwareWalletData.extendedPublicKey.publicKeyHex
      );
      const recognizedWallet = recognizedStoredWallet
        ? this.stores.wallets.getWalletById(recognizedStoredWallet.id)
        : null;

      // Check if public key matches already restored hardware wallet public key
      // Update LC data and redirect to paired wallet
      if (recognizedWallet) {
        logger.debug('[HW-DEBUG] HWStore - I have recognized wallet: ', {
          recognizedWallet: recognizedWallet.id,
        });

        this._setHardwareWalletLocalData({
          walletId: recognizedWallet.id,
          data: {
            disconnected: false,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ disconnected: false; data: { deviceType: D... Remove this comment to see the full error message
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path: devicePath,
              paired: recognizedWallet.id,
              // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          },
        });

        // Delete initiated (pending) device with this path since now is paired to wallet
        const recognizedDevice = find(
          this.hardwareWalletDevices,
          // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
          (device) => device.path === forcedPath
        );

        if (recognizedDevice) {
          logger.debug(
            '[HW-DEBUG] HWStore - _getExtendedPublicKey - UNSET Device with path: ',
            {
              recognizedDevice: recognizedDevice.id,
            }
          );
          await this._unsetHardwareWalletDevice({
            deviceId: recognizedDevice.id,
          });
        }

        logger.debug('[HW-DEBUG] HWStore - SET device from key export: ', {
          deviceId,
        });

        if (deviceId) {
          this._setHardwareWalletDevice({
            deviceId,
            data: {
              // @ts-ignore ts-migrate(2322) FIXME: Type '{ deviceId: string; deviceType: DeviceType; ... Remove this comment to see the full error message
              deviceId,
              deviceType,
              deviceModel,
              deviceName,
              path: devicePath,
              paired: recognizedWallet.id,
              // device paired with software wallet
              disconnected: false,
              // device physically disconnected
              isPending: false,
            },
          });
        }

        // Prevent redirect / check if device is valid / proceed with tx
        if (this.isTransactionInitiated) {
          logger.debug(
            '[HW-DEBUG] HWStore - Re-initiate tx from _getExtendedPublicKey: ',
            {
              walletId,
              recognizedWalletId: recognizedWallet.id,
              deviceId,
              devicePath,
            }
          );

          // Check if sender wallet match transaction initialization
          if (!walletId || recognizedWallet.id !== walletId) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug(
              '[HW-DEBUG] HWStore - Device not belongs to this wallet'
            );
            // Keep isTransactionInitiated active & Set new device listener by initiating transaction
            // Show message to reconnect proper software wallet device pair
            logger.debug(
              '[HW-DEBUG] unfinishedWalletTxSigning SET: ',
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
              walletId
            );
            runInAction(
              'HardwareWalletsStore:: set HW device CONNECTING FAILED',
              () => {
                this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
                this.activeDevicePath = null;
                this.unfinishedWalletTxSigning = walletId;
                this.isExportKeyAborted = false;
              }
            );
          } else {
            logger.debug(
              '[HW-DEBUG] HWStore - Transaction Initiated - Close: ',
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
              walletId
            );
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
            runInAction('HardwareWalletsStore:: Initiate transaction', () => {
              this.isTransactionInitiated = false;
              this.unfinishedWalletTxSigning = null;
              this.isExportKeyAborted = false;
            });

            if (isTrezor) {
              this._signTransactionTrezor(walletId, deviceId);
            } else {
              this._signTransactionLedger(walletId, devicePath);
            }
          }

          return;
        }

        // Prevent redirect / check if device is valid / proceed with address verification
        if (this.isAddressVerificationInitiated && address) {
          logger.debug(
            '[HW-DEBUG] HWStore - Re-initiate Address verification from _getExtendedPublicKey: ',
            {
              address: toJS(address),
              devicePath,
              walletId,
              recognizedWalletId: recognizedWallet.id,
              deviceId,
            }
          );

          if (!walletId || recognizedWallet.id !== walletId) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug(
              '[HW-DEBUG] HWStore - Device not belongs to this wallet'
            );
            // Show message to reconnect proper software wallet device pair
            logger.debug(
              '[HW-DEBUG] unfinishedWalletAddressVerification SET: ',
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
              walletId
            );
            runInAction(
              'HardwareWalletsStore:: set HW device CONNECTING FAILED',
              () => {
                this.isAddressVerificationInitiated = false;
                this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
                this.activeDevicePath = null;
                this.unfinishedWalletAddressVerification = address;
                this.isExportKeyAborted = false;
              }
            );
          } else {
            logger.debug(
              '[HW-DEBUG] HWStore - Address Verification - Close: ',
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
              walletId
            );
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
            runInAction('HardwareWalletsStore:: Initiate transaction', () => {
              this.isAddressVerificationInitiated = false;
              this.unfinishedWalletAddressVerification = null;
              this.isExportKeyAborted = false;
            });
            this.verifyAddress({
              address,
              path: devicePath,
              isTrezor,
            });
          }

          return;
        }

        // --> Else
        this.stores.wallets.goToWalletRoute(recognizedStoredWallet.id);
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.dialogs.closeActiveDialog.trigger();
        return;
      }

      logger.debug(
        '[HW-DEBUG] HWStore - I don not have recognized wallet - create new one or reject TX: ',
        {
          deviceId,
        }
      );

      // Software Wallet not recognized and TX initiated. Show error
      if (this.isTransactionInitiated) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - Device not belongs to this wallet');
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
        runInAction(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
            this.activeDevicePath = null;
            this.unfinishedWalletTxSigning = walletId;
            this.isExportKeyAborted = false;
          }
        );
        return;
      }

      // Software Wallet not recognized, create new one with default name
      logger.debug('[HW-DEBUG] HWStore - Initiate HW create / restore', {
        transportDevice: toJS(transportDevice),
        device: {
          deviceId,
          deviceType,
          deviceModel,
          deviceName,
          path: forcedPath || path,
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
          path: forcedPath || path,
          firmwareVersion: null,
        },
      });
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - HW created / restored');
      // Get all Pending devices with this path and delete
      const recognizedPendingDevice = find(
        this.hardwareWalletDevices,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
        (device) => device.path === devicePath
      );

      // @ts-ignore ts-migrate(2339) FIXME: Property 'isPending' does not exist on type 'Hardw... Remove this comment to see the full error message
      if (recognizedPendingDevice && recognizedPendingDevice.isPending) {
        logger.debug(
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
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - Export key error');

      /**
       * ============  Exporting aborted  =============
       * e.statusCode === 28169 // Ledger
       * e.code === 'Failure_ActionCancelled' // Trezor
       * ============  Exporting cancellet - device unplugged during action  =============
       * e.name === DisconnectedDevice // Ledger
       * e.error === 'device disconnected during action' // Trezor
       */
      const isCancelled =
        error.statusCode === 28169 || error.code === 'Failure_ActionCancelled';
      const isAborted =
        error.name === 'DisconnectedDevice' ||
        error.error === 'device disconnected during action';
      logger.debug('[HW-DEBUG] HWStore - Export error case: ', {
        isCancelled,
        isAborted,
      });

      if (isCancelled || isAborted) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - Export:: WAIT FOR ANOTHER DEVICE');

        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        if (isCancelled && isTrezor) {
          // Skip Trezor device-change events when rejected
          setTimeout(() => {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug('[HW-DEBUG] NOW RESET');
            runInAction(
              'HardwareWalletsStore:: Re-run initiated connection',
              () => {
                this.hwDeviceStatus = isAborted
                  ? HwDeviceStatuses.CONNECTING
                  : HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
                this.isListeningForDevice = true;
                this.isExportKeyAborted = true;
              }
            );
          }, 2000);
        } else {
          this.stopCardanoAdaAppFetchPoller();
          runInAction(
            'HardwareWalletsStore:: Re-run initiated connection',
            () => {
              this.hwDeviceStatus = isAborted
                ? HwDeviceStatuses.CONNECTING
                : HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
              this.isListeningForDevice = true;
              this.isExportKeyAborted = true;
            }
          );
        }
      } else {
        runInAction(
          'HardwareWalletsStore:: Cannot export extended public key',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
          }
        );
      }

      // Pass other errors to caller (establishHardwareWalletConnection() in this case) and handle additional actions if needed
      throw error;
    }
  };
  // Trezor - Shelley only
  @action
  _signTransactionTrezor = async (
    walletId: string,
    deviceId?: string | null | undefined
  ) => {
    const { coinSelection } = this.txSignRequest;
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] _signTransactionTrezor:: Execute');

    // @TODO - remove once signing delegation transaction will call coins selection
    // This case is covered in coins selection action
    if (!coinSelection) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
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
    logger.debug('[HW-DEBUG] HWStore - sign transaction Trezor: ', {
      walletId,
    });
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const publicKeyHex = get(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'publicKeyHex',
    ]);
    const chainCodeHex = get(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'chainCodeHex',
    ]);
    const xpubHex = `${publicKeyHex}${chainCodeHex}`;
    const unsignedTxInputs = [];
    const inputsData = map(inputs, (input) => {
      const shelleyTxInput = ShelleyTxInputFromUtxo(input);
      unsignedTxInputs.push(shelleyTxInput);
      return prepareTrezorInput(input);
    });
    const unsignedTxOutputs = [];
    const outputsData = [];

    for (const output of outputs) {
      const {
        address_style: addressStyle,
      } = await this.stores.addresses._inspectAddress({
        addressId: output.address,
      });
      const shelleyTxOutput = ShelleyTxOutput(output, addressStyle);
      unsignedTxOutputs.push(shelleyTxOutput);
      const ledgerOutput = prepareTrezorOutput(output);
      outputsData.push(ledgerOutput);
    }

    // Construct certificates
    const unsignedTxCerts = [];

    const _certificatesData = map(certificates, async (certificate) => {
      const accountAddress = await this._getRewardAccountAddress(
        walletId,
        certificate.rewardAccountPath
      );
      const shelleyTxCert = ShelleyTxCert({
        accountAddress,
        pool: certificate.pool,
        // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'string'.
        type: CERTIFICATE_TYPE[certificate.certificateType],
      });
      unsignedTxCerts.push(shelleyTxCert);
      return prepareTrezorCertificate(certificate);
    });

    const certificatesData = await Promise.all(_certificatesData);

    // Construct Withdrawals
    const _withdrawalsData = map(withdrawals, async (withdrawal) =>
      prepareTrezorWithdrawal(withdrawal)
    );

    const withdrawalsData = await Promise.all(_withdrawalsData);
    let unsignedTxAuxiliaryData = null;
    let auxiliaryData = null;

    if (this.votingData) {
      const { stakeAddress, stakeKey, votingKey, nonce } = this.votingData;
      unsignedTxAuxiliaryData = {
        nonce,
        // unique increaseable number e.g. current epoch number or absolute slot number ( identifies unique tx / vote registration )
        rewardDestinationAddress: {
          address: stakeAddress,
          stakingPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
        stakePubKey: stakeKey,
        type: CATALYST_VOTING_REGISTRATION_TYPE,
        votingPubKey: votingKey,
      };
      auxiliaryData = prepareTrezorAuxiliaryData({
        votingKey,
        nonce: nonce.toString(),
      });
    }

    const recognizedDevice = find(
      this.hardwareWalletDevices,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
      (hardwareWalletDevice) => hardwareWalletDevice.paired === walletId
    );
    const recognizedDevicePath = get(recognizedDevice, 'path', null);
    logger.debug('[HW-DEBUG] sign Trezor:: recognizedDevicePath and walelt: ', {
      walletId,
      deviceId,
      isTransactionInitiated: this.isTransactionInitiated,
    });

    if (this.isTransactionInitiated) {
      // Check if sender wallet match transaction initialization
      if (
        !recognizedDevice ||
        (recognizedDevice && deviceId && recognizedDevice.id !== deviceId)
      ) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - Device not belongs to this wallet');
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        logger.debug('[HW-DEBUG] unfinishedWalletTxSigning SET: ', walletId);
        runInAction(
          'HardwareWalletsStore:: set HW device CONNECTING FAILED',
          () => {
            this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
            this.activeDevicePath = null;
            this.unfinishedWalletTxSigning = walletId;
          }
        );
        return;
      }

      logger.debug(
        '[HW-DEBUG] HWStore - Transaction Initiated - RESET: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        walletId
      );
      runInAction('HardwareWalletsStore:: Initiate transaction', () => {
        this.isTransactionInitiated = false;
        this.unfinishedWalletTxSigning = null;
      });
    }

    const fee = formattedAmountToLovelace(flatFee.toString());

    const ttl = this._getTtl();

    const absoluteSlotNumber = this._getAbsoluteSlotNumber();

    try {
      const signedTransaction = await signTransactionTrezorChannel.request({
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ path: string; prev_hash: string; prev_inde... Remove this comment to see the full error message
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: ttl.toString(),
        validityIntervalStartStr: absoluteSlotNumber.toString(),
        networkId: hardwareWalletsNetworkConfig.networkId,
        protocolMagic: hardwareWalletsNetworkConfig.protocolMagic,
        // @ts-ignore ts-migrate(2322) FIXME: Type '({ type: number; path: string; pool: string;... Remove this comment to see the full error message
        certificates: certificatesData,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ path: string; amount: string; }[]' is not ... Remove this comment to see the full error message
        withdrawals: withdrawalsData,
        devicePath: recognizedDevicePath,
        signingMode: TrezorTransactionSigningMode.ORDINARY_TRANSACTION,
        auxiliaryData,
      });

      if (signedTransaction && !signedTransaction.success) {
        throw signedTransaction.payload;
      }

      // Compatible with old firmwares
      const serializedTx = get(signedTransaction, ['payload', 'serializedTx']);

      if (serializedTx) {
        runInAction(
          'HardwareWalletsStore:: transaction successfully signed',
          () => {
            this.txBody = serializedTx;
            this.hwDeviceStatus =
              HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
          }
        );
        return;
      }

      const unsignedTxWithdrawals =
        withdrawals.length > 0 ? ShelleyTxWithdrawal(withdrawals) : null;
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
      const auxiliaryDataSupplement = get(signedTransaction, [
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
        txAuxiliaryData = cborizeTxAuxiliaryVotingData(
          unsignedTxAuxiliaryData,
          auxiliaryDataSupplement.catalystSignature
        );
      }

      const unsignedTx = prepareTxAux(txAuxData);
      const witnesses = get(signedTransaction, ['payload', 'witnesses'], []);
      const signedWitnesses = await this._signWitnesses(witnesses, xpubHex);
      const txWitnesses = new Map();

      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }

      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await prepareBody(
        unsignedTx,
        txWitnesses,
        txAuxiliaryData
      );
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.txBody = txBody;
        this.activeDevicePath = null;
      });
    } catch (error) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
          this.isTransactionInitiated = false;
        }
      );

      if (error.code === 'Device_CallInProgress') {
        throw new Error('Device is busy - reconnect device and try again');
      }

      throw error;
    }
  };
  _signWitnesses = async (
    witnesses: Array<TrezorWitness | Witness>,
    xpubHex: string
  ) => {
    const signedWitnesses = [];

    for (const witness of witnesses) {
      const signedWitness = await this.ShelleyWitness(witness, xpubHex);
      signedWitnesses.push(signedWitness);
    }

    return signedWitnesses;
  };
  ShelleyWitness = async (
    witness: TrezorWitness | Witness,
    xpubHex: string
  ) => {
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
      return ShelleyTxWitnessShelley(publicKey, signature);
    }

    return null;
  };
  _deriveXpub = CachedDeriveXpubFactory(async (xpubHex) => {
    return Buffer.from(xpubHex, 'hex');
  });
  _getRewardAccountAddress = async (walletId: string, path: Array<string>) => {
    const pathParams = getParamsFromPath(path);
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
  @action
  _signTransactionLedger = async (
    walletId: string,
    devicePath: string | null | undefined
  ) => {
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });
    const { coinSelection } = this.txSignRequest;
    const {
      inputs,
      outputs,
      certificates,
      fee: flatFee,
      withdrawals,
    } = coinSelection;
    logger.debug('[HW-DEBUG] HWStore - sign transaction Ledger: ', {
      walletId,
    });
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );
    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');
    const publicKeyHex = get(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'publicKeyHex',
    ]);
    const chainCodeHex = get(hardwareWalletConnectionData, [
      'extendedPublicKey',
      'chainCodeHex',
    ]);
    const xpubHex = `${publicKeyHex}${chainCodeHex}`;
    const unsignedTxInputs = [];
    const inputsData = map(inputs, (input) => {
      const shelleyTxInput = ShelleyTxInputFromUtxo(input);
      unsignedTxInputs.push(shelleyTxInput);
      return prepareLedgerInput(input);
    });
    const unsignedTxOutputs = [];
    const outputsData = [];

    for (const output of outputs) {
      const {
        address_style: addressStyle,
      } = await this.stores.addresses._inspectAddress({
        addressId: output.address,
      });
      const shelleyTxOutput = ShelleyTxOutput(output, addressStyle);
      unsignedTxOutputs.push(shelleyTxOutput);
      const ledgerOutput = prepareLedgerOutput(output, addressStyle);
      outputsData.push(ledgerOutput);
    }

    // Construct certificates
    const unsignedTxCerts = [];

    const _certificatesData = map(certificates, async (certificate) => {
      const accountAddress = await this._getRewardAccountAddress(
        walletId,
        certificate.rewardAccountPath
      );
      const shelleyTxCert = ShelleyTxCert({
        accountAddress,
        pool: certificate.pool,
        // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'string'.
        type: CERTIFICATE_TYPE[certificate.certificateType],
      });
      unsignedTxCerts.push(shelleyTxCert);
      return prepareLedgerCertificate(certificate);
    });

    const certificatesData = await Promise.all(_certificatesData);

    // Construct Withdrawals
    const _withdrawalsData = map(withdrawals, async (withdrawal) =>
      prepareLedgerWithdrawal(withdrawal)
    );

    const withdrawalsData = await Promise.all(_withdrawalsData);
    const fee = formattedAmountToLovelace(flatFee.toString());

    const ttl = this._getTtl();

    let unsignedTxAuxiliaryData = null;

    if (this.votingData) {
      const { stakeAddress, stakeKey, votingKey, nonce } = this.votingData;
      unsignedTxAuxiliaryData = {
        nonce,
        // unique increaseable number e.g. current epoch number or absolute slot number ( identifies unique tx / vote registration )
        rewardDestinationAddress: {
          address: stakeAddress,
          stakingPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
        stakePubKey: stakeKey,
        type: CATALYST_VOTING_REGISTRATION_TYPE,
        votingPubKey: votingKey,
      };
    }

    const auxiliaryData = unsignedTxAuxiliaryData
      ? prepareLedgerAuxiliaryData(unsignedTxAuxiliaryData)
      : null;

    try {
      const signedTransaction = await signTransactionLedgerChannel.request({
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
        signingMode: TransactionSigningMode.ORDINARY_TRANSACTION,
        additionalWitnessPaths: [],
        auxiliaryData,
        devicePath,
      });
      const unsignedTxWithdrawals =
        withdrawals.length > 0 ? ShelleyTxWithdrawal(withdrawals) : null;
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
        txAuxiliaryData = cborizeTxAuxiliaryVotingData(
          unsignedTxAuxiliaryData,
          signedTransaction.auxiliaryDataSupplement
            .catalystRegistrationSignatureHex
        );
      }

      const unsignedTx = prepareTxAux(txAuxData);
      const witnesses = get(signedTransaction, 'witnesses', []);
      const signedWitnesses = await this._signWitnesses(witnesses, xpubHex);
      const txWitnesses = new Map();

      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }

      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await prepareBody(
        unsignedTx,
        txWitnesses,
        txAuxiliaryData
      );
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.txBody = txBody;
        this.activeDevicePath = null;
      });
    } catch (error) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      throw error;
    }
  };
  initiateTransaction = async (params: {
    walletId: string | null | undefined;
    votingData?: VotingDataType;
  }) => {
    const { walletId, votingData } = params;
    runInAction('HardwareWalletsStore:: Initiate Transaction', () => {
      this.isTransactionInitiated = true;
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      this.activeDelegationWalletId = walletId;
      this.votingData = votingData || null;
      this.activeVotingWalletId = walletId;
    });
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );
    logger.debug('[HW-DEBUG] HWStore - initiateTransaction: ', {
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
      logger.debug('[HW-DEBUG] HWStore - initiateTransaction - DISCONNECTED');

      // Wait for connection to be established and continue to signing process
      try {
        let transportDevice;

        if (
          hardwareWalletConnectionData.device.deviceType === DeviceTypes.TREZOR
        ) {
          // Do I have unpaired Trezor devices
          const lastUnpairedDevice = findLast(
            this.hardwareWalletDevices,
            (hardwareWalletDevice) =>
              // @ts-ignore ts-migrate(2339) FIXME: Property 'paired' does not exist on type 'Hardware... Remove this comment to see the full error message
              !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
          );

          if (lastUnpairedDevice) {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug('[HW-DEBUG] I HAVE UNPAIRED');
            transportDevice = lastUnpairedDevice;
          } else {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.debug('[HW-DEBUG] CHECK FOR NEXT device');
            transportDevice = await getHardwareWalletTransportChannel.request({
              devicePath: null,
              isTrezor: true,
            });
          }

          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug('[HW-DEBUG] INITIATE tx - I have transport');
        } else {
          transportDevice = await this.establishHardwareWalletConnection();
        }

        if (!transportDevice) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logger.debug('[HW-DEBUG] No new devices recognized for tx signing');
          throw new Error('Signing device not recognized!');
        }

        devicePath = transportDevice.path;
      } catch (e) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug(
          '[HW-DEBUG] HWStore - initiateTransaction - DISCONNECTED - ERROR'
        );
        runInAction('HardwareWalletsStore:: Initiate transaction', () => {
          this.isTransactionInitiated = false;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        });
        throw e;
      }
    }

    runInAction(
      'HardwareWalletsStore:: Set active device path for Transaction send',
      () => {
        this.activeDevicePath = devicePath;
      }
    );

    // Add more cases / edge cases if needed
    if (deviceType === DeviceTypes.TREZOR && walletId) {
      logger.debug('[HW-DEBUG] Sign Trezor: ', {
        id,
      });
      const transportDevice = await this.establishHardwareWalletConnection();

      if (transportDevice) {
        logger.debug('[HW-DEBUG] HWStore - Set transport device 4', {
          transportDevice: toJS(transportDevice),
        });
        runInAction(
          'HardwareWalletsStore:: Set transport device fomr tx init',
          () => {
            this.transportDevice = transportDevice;
          }
        );
        await this._getExtendedPublicKey(transportDevice.path, walletId);
      }
    } else {
      logger.debug(
        '[HW-DEBUG] HWStore - getCardanoAdaApp - from  initiateTransaction',
        {
          devicePath,
        }
      );

      if (walletId) {
        this.stopCardanoAdaAppFetchPoller();
        this.cardanoAdaAppPollingInterval = setInterval(
          (path, wid) =>
            this.getCardanoAdaApp({
              path,
              walletId: wid,
            }),
          CARDANO_ADA_APP_POLLING_INTERVAL,
          devicePath,
          walletId
        );
      }
    }
  };
  _resetTransaction = async (
    params:
      | {
          cancelDeviceAction: boolean;
        }
      | null
      | undefined
  ) => {
    if (isHardwareWalletSupportEnabled) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] RESET TX');
      runInAction('HardwareWalletsStore:: Reset initiated transaction', () => {
        this.isTransactionInitiated = false;
      });
      this.stopCardanoAdaAppFetchPoller();
      const cancelDeviceAction = get(params, 'cancelDeviceAction', false);

      if (cancelDeviceAction) {
        resetTrezorActionChannel.request();
      }

      this.sendMoneyRequest.reset();
      this.selectCoinsRequest.reset();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
      runInAction('HardwareWalletsStore:: reset Transaction verifying', () => {
        this.hwDeviceStatus = HwDeviceStatuses.READY;
        this.txBody = null;
        this.activeDevicePath = null;
        this.unfinishedWalletTxSigning = null;
        this.activeDelegationWalletId = null;
        this.activeVotingWalletId = null;
        this.votingData = null;
      });
    }
  };
  @action
  _changeHardwareWalletConnectionStatus = async (
    params: HardwareWalletConnectionRequest
  ) => {
    const {
      disconnected,
      deviceType,
      deviceId,
      deviceModel,
      deviceName,
      path,
      error,
      eventType,
    } = params;
    logger.debug('[HW-DEBUG] HWStore - CHANGE status: ', {
      params,
    });

    // Handle Trezor Bridge instance checker
    if (error && deviceType === DeviceTypes.TREZOR) {
      if (
        error.payload &&
        error.payload &&
        error.payload.code === 'ECONNREFUSED'
      ) {
        runInAction(
          'HardwareWalletsStore:: Mark Trezor Bridge as not installed',
          () => {
            this.isTrezorBridgeInstalled = false;
          }
        );
      }

      return;
    }

    // Unset Trezor Bridge instance checker
    if (deviceType === DeviceTypes.TREZOR && !this.isTrezorBridgeInstalled) {
      runInAction(
        'HardwareWalletsStore:: Mark Trezor Bridge as installed',
        () => {
          this.isTrezorBridgeInstalled = true;
        }
      );
    }

    const { hardwareWalletsConnectionData, hardwareWalletDevices } = this;
    // Add new recognized device - not connected to software wallet
    // Or update recognized device while paired with existing software wallet
    const recognizedPairedHardwareWallet = find(
      hardwareWalletsConnectionData,
      (
        connection // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
      ) =>
        deviceType === DeviceTypes.TREZOR &&
        deviceId &&
        connection.device.deviceId === deviceId
    );

    if (disconnected && deviceType === DeviceTypes.LEDGER) {
      // Remove all stored Ledger instances from LC - both pending and paired (with software Wallets)
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - device disconnected');
      const recognizedLedgerDevice = find(
        hardwareWalletDevices,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'path' does not exist on type 'HardwareWa... Remove this comment to see the full error message
        (hardwareWalletDevice) => hardwareWalletDevice.path === path
      );

      if (recognizedLedgerDevice) {
        logger.debug('[HW-DEBUG] HWStore - Remove Device from LC', {
          recognizedLedgerDevice: toJS(recognizedLedgerDevice),
        });
        await this._unsetHardwareWalletDevice({
          deviceId: recognizedLedgerDevice.id,
        });
      }

      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.debug('[HW-DEBUG] HWStore - GET Paired and set to disconnected');
      const recognizedLedgerWallet = find(
        hardwareWalletsConnectionData,
        (
          connection // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
        ) =>
          deviceType === DeviceTypes.LEDGER &&
          path &&
          connection.device.path === path
      );

      if (recognizedLedgerWallet) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.debug('[HW-DEBUG] HWStore - I have stored Ledger wallet');
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
      recognizedPairedHardwareWallet.device.deviceType === DeviceTypes.TREZOR
    ) {
      // Change software wallet status - paired with device
      logger.debug(
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

    if (
      deviceId ||
      (deviceType === DeviceTypes.LEDGER &&
        (!disconnected || recognizedPairedHardwareWallet))
    ) {
      pendingId =
        deviceType === DeviceTypes.LEDGER && recognizedPairedHardwareWallet
          ? recognizedPairedHardwareWallet.device.deviceId
          : new Date().valueOf();
      logger.debug('[HW-DEBUG] HWStore - SET DEVICE DATA: ', {
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
    if (
      this.isListeningForDevice &&
      !disconnected &&
      (!eventType || eventType === DeviceEvents.CONNECT)
    ) {
      runInAction('HardwareWalletsStore:: remove device listener', () => {
        this.isListeningForDevice = false;
      });

      if (deviceType === DeviceTypes.LEDGER) {
        // To Force Ledger with manual parameters because ID is not available and device not stored to LC
        logger.debug(
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
      eventType === DeviceEvents.CONNECT
    ) {
      logger.debug(
        '[HW-DEBUG] CHANGE STATUS to: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        HwDeviceStatuses.CONNECTING
      );
      runInAction('HardwareWalletsStore:: Change status to Connecting', () => {
        this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      });
      logger.debug(
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
      (!eventType || eventType === DeviceEvents.CONNECT)
    ) {
      logger.debug(
        '[HW-DEBUG] CHANGE STATUS to: ',
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
        HwDeviceStatuses.CONNECTING
      );
      runInAction('HardwareWalletsStore:: Change status to Connecting', () => {
        this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      });
      logger.debug(
        '[HW-DEBUG] HWStore - Reinitialize Address Verification process: ',
        toJS(this.unfinishedWalletAddressVerification)
      );
      // It is not possible to pass null value that FLOW marks as error (FlowFixMe used)
      this.initiateAddressVerification(
        // @ts-ignore
        this.unfinishedWalletAddressVerification,
        path
      );
    }
  };
  @action
  resetInitializedConnection = async (
    params:
      | {
          cancelDeviceAction: boolean;
        }
      | null
      | undefined
  ) => {
    const cancelDeviceAction = get(params, 'cancelDeviceAction', false);

    if (cancelDeviceAction) {
      resetTrezorActionChannel.request();
    }

    this.stopCardanoAdaAppFetchPoller();
    this.stores.wallets.createHardwareWalletRequest.reset();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    this.transportDevice = {};
    this.isListeningForDevice = false;
    this.isExportKeyAborted = false;
  };
  @action
  resetInitializedAddressVerification = async (
    params:
      | {
          cancelDeviceAction: boolean;
        }
      | null
      | undefined
  ) => {
    const cancelDeviceAction = get(params, 'cancelDeviceAction', false);

    if (cancelDeviceAction) {
      resetTrezorActionChannel.request();
    }

    this.stopCardanoAdaAppFetchPoller();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
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
  @action
  _refreshHardwareWalletsLocalData = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletsLocalDataRequest.execute();
  };
  @action
  _refreshHardwareWalletDevices = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.hardwareWalletDevicesRequest.execute();
  };

  @computed
  get hardwareWalletsConnectionData(): HardwareWalletsLocalData {
    return this.hardwareWalletsLocalDataRequest.result;
  }

  @computed
  get hardwareWalletDevices(): HardwareWalletsLocalData {
    return this.hardwareWalletDevicesRequest.result;
  }

  checkIsTrezorByWalletId = (walletId: string): boolean => {
    const hardwareWalletConnectionData = find(
      this.hardwareWalletsConnectionData,
      (connectionData) => connectionData.id === walletId
    );
    return (
      hardwareWalletConnectionData &&
      hardwareWalletConnectionData.device.deviceType === DeviceTypes.TREZOR
    );
  };
  _resetTxSignRequestData = () => {
    this.selectCoinsRequest.reset();
    // @ts-ignore ts-migrate(2741) FIXME: Property 'coinSelection' is missing in type '{}' b... Remove this comment to see the full error message
    this.txSignRequest = {};
  };
  _deviceType = (deviceModel: LedgerModel | TrezorModel) => {
    let type;

    switch (deviceModel) {
      case DeviceModels.LEDGER_NANO_S:
        type = DeviceTypes.LEDGER;
        break;

      case DeviceModels.LEDGER_NANO_X:
        type = DeviceTypes.LEDGER;
        break;

      case DeviceModels.TREZOR_ONE:
        type = DeviceTypes.TREZOR;
        break;

      case DeviceModels.TREZOR_T:
        type = DeviceTypes.TREZOR;
        break;

      default:
        type = null;
    }

    return type;
  };
  _getTtl = (): number => {
    const { absoluteSlotNumber } = this.stores.networkStatus;
    const ttl = absoluteSlotNumber + TIME_TO_LIVE;
    return ttl;
  };
  _getAbsoluteSlotNumber = (): number => {
    const { absoluteSlotNumber } = this.stores.networkStatus;
    return absoluteSlotNumber;
  };
  _getHardwareWalletDeviceInfoByWalletId = (
    walletId: string
  ): HardwareWalletLocalData => {
    return find(
      this.hardwareWalletsConnectionData,
      (connectionData) => connectionData.id === walletId
    );
  };
  _setHardwareWalletLocalData = async ({
    walletId,
    data,
  }: SetHardwareWalletLocalDataRequestType) => {
    logger.debug(
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
  _unsetHardwareWalletLocalData = async ({
    walletId,
  }: {
    walletId: string;
  }) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] HWStore - _unsetHardwareWalletLocalData');
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.unsetHardwareWalletLocalDataRequest.execute(walletId);
    const pairedDevice = find(
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
  _setHardwareWalletDevice = async ({
    deviceId,
    data,
  }: SetHardwareWalletDeviceRequestType) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setHardwareWalletDeviceRequest.execute(deviceId, data);

    this._refreshHardwareWalletDevices();
  };
  _unsetHardwareWalletDevice = async ({
    deviceId,
  }: {
    deviceId?: string | null | undefined;
  }) => {
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
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('[HW-DEBUG] HWStore - STOP Ada App poller');

    if (this.cardanoAdaAppPollingInterval) {
      clearInterval(this.cardanoAdaAppPollingInterval);
    }
  };
}
