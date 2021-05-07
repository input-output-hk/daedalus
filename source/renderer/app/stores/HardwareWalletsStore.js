// @flow
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
import {
  HW_SHELLEY_CONFIG,
  SHELLEY_PURPOSE_INDEX,
  ADA_COIN_TYPE,
  MINIMAL_TREZOR_FIRMWARE_VERSION,
  MINIMAL_LEDGER_FIRMWARE_VERSION,
  MINIMAL_CARDANO_APP_VERSION,
  isHardwareWalletSupportEnabled,
  isTrezorEnabled,
  isLedgerEnabled,
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
  groupTokensByPolicyId,
} from '../utils/shelleyLedger';
import {
  prepareTrezorInput,
  prepareTrezorOutput,
  prepareTrezorCertificate,
  prepareTrezorWithdrawal,
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
} from '../../../common/types/hardware-wallets.types';

import { logger } from '../utils/logging';

export type TxSignRequestTypes = {
  coinSelection: CoinSelectionsResponse,
};

export type ByronEncodeSignedTransactionRequest = {|
  txDataHex: string,
  witnesses: Array<ByronSignedTransactionWitnesses>,
|};

export type ByronSignedTransactionWitnesses = {
  signature: string,
  xpub: HardwareWalletExtendedPublicKeyResponse,
};

export type AddressVerificationCheckStatus = 'valid' | 'invalid' | 'reverify';

export type TempAddressToVerify = {
  address: WalletAddress,
  path: string,
  isTrezor: boolean,
};

export const AddressVerificationCheckStatuses: {
  VALID: string,
  INVALID: string,
  REVERIFY: string,
} = {
  VALID: 'valid',
  INVALID: 'invalid',
  REVERIFY: 'reverify',
};

const CARDANO_ADA_APP_POLLING_INTERVAL = 1000;
const DEFAULT_HW_NAME = 'Hardware Wallet';

export default class HardwareWalletsStore extends Store {
  @observable selectCoinsRequest: Request<CoinSelectionsResponse> = new Request(
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
  hardwareWalletsLocalDataRequest: Request<HardwareWalletsLocalData> = new Request(
    this.api.localStorage.getHardwareWalletsLocalData
  );
  @observable
  setHardwareWalletLocalDataRequest: Request<HardwareWalletLocalData> = new Request(
    this.api.localStorage.setHardwareWalletLocalData
  );
  @observable unsetHardwareWalletLocalDataRequest: Request<void> = new Request(
    this.api.localStorage.unsetHardwareWalletLocalData
  );
  @observable
  hardwareWalletDevicesRequest: Request<HardwareWalletsLocalData> = new Request(
    this.api.localStorage.getHardwareWalletDevices
  );
  @observable
  setHardwareWalletDeviceRequest: Request<HardwareWalletLocalData> = new Request(
    this.api.localStorage.setHardwareWalletDevice
  );
  @observable
  overrideHardwareWalletDevicesRequest: Request<HardwareWalletDevicesType> = new Request(
    this.api.localStorage.overrideHardwareWalletDevices
  );
  @observable
  unsetHardwareWalletDeviceRequest: Request<HardwareWalletLocalData> = new Request(
    this.api.localStorage.unsetHardwareWalletDevice
  );
  @observable
  unsetHardwareWalletDevicesAllRequest: Request<void> = new Request(
    this.api.localStorage.unsetHardwareWalletDevicesAll
  );
  @observable
  unsetHardwareWalletLocalDataAllRequest: Request<HardwareWalletLocalData> = new Request(
    this.api.localStorage.unsetHardwareWalletLocalDataAll
  );
  @observable hwDeviceStatus: HwDeviceStatus = HwDeviceStatuses.CONNECTING;
  @observable
  extendedPublicKey: ?HardwareWalletExtendedPublicKeyResponse = null;
  @observable txSignRequest: TxSignRequestTypes = {};
  @observable transportDevice: ?TransportDevice = null;
  @observable txBody: ?string = null;
  @observable isTransactionPending: boolean = false;
  @observable isTrezorBridgeInstalled: boolean = false;
  @observable isTransactionInitiated: boolean = false;
  @observable activeDevicePath: ?string = null;
  @observable unfinishedWalletTxSigning: ?string = null;
  @observable isListeningForDevice: boolean = false;
  @observable isConnectInitiated: boolean = false;
  @observable isAddressVerificationInitiated: boolean = false;
  @observable unfinishedWalletAddressVerification: ?WalletAddress = null;
  @observable isAddressDerived: boolean = false;
  @observable isAddressChecked: boolean = false;
  @observable isAddressCorrect: ?boolean = null;
  @observable tempAddressToVerify: TempAddressToVerify = {};
  @observable isExportKeyAborted: boolean = false;
  @observable activeDelegationWalletId: ?string = null;

  cardanoAdaAppPollingInterval: ?IntervalID = null;
  checkTransactionTimeInterval: ?IntervalID = null;

  setup() {
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.sendMoney.listen(this._sendMoney);
    hardwareWalletsActions.refreshHardwareWalletsLocalData.listen(
      this._refreshHardwareWalletsLocalData
    );
    getHardwareWalletConnectionChannel.onReceive(
      this._changeHardwareWalletConnectionStatus
    );
    this.initTrezor();
    this.initLedger();
    this.hardwareWalletsLocalDataRequest.execute();
    this.hardwareWalletDevicesRequest.execute();
  }

  test1 = () => {
    const aa = JSON.parse('{"inputs":[{"address":"addr_test1qrhm5sqga9whsg9ea2l30ejdq9wcyqg2wwr38d9gtfkxf4kss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6scjppzz","amount":{"quantity":65936866,"unit":"lovelace"},"id":"2cea6bf5ed16685480275bb21a4d25144704af1b258db4ff414aa1788c76f6bb","index":1,"derivationPath":["1852H","1815H","0H","1","155"],"assets":[{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"","quantity":1},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"6861707079636f696e","quantity":2},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"7375706572636f696e","quantity":3},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"74727565636f696e","quantity":11},{"policyId":"94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c","assetName":"","quantity":4},{"policyId":"94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c","assetName":"676f6f64636f696e","quantity":3}]},{"address":"addr_test1qzy5dkyavfj0tnqsrxf8fn7ffk2uw2c7vtfwhs3rsywvxvkss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6ss3n5k3","amount":{"quantity":2000000,"unit":"lovelace"},"id":"1de741f03ae8587bb023230a196e5df2e01045463092f4650cbc31aa9dbb72cb","index":0,"derivationPath":["1852H","1815H","0H","0","26"],"assets":[{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"","quantity":1}]},{"address":"addr_test1qprs0vx8v08c4l0ruqmu2drajxrnz7dwx8xtmnlany86x7kss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6sf8utl9","amount":{"quantity":2000000,"unit":"lovelace"},"id":"1c6bfb1652a01ba4f397ba5db718390f670d3a53124bb3363b6558919ec01591","index":0,"derivationPath":["1852H","1815H","0H","0","25"],"assets":[]}],"outputs":[{"address":"addr_test1qq9xeux6exqrv7wh8uw04hc7lr8mutgcg8dvv06p5m35vr7ss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6slp4hwt","amount":{"quantity":2000000,"unit":"lovelace"},"derivationPath":null,"assets":[{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"","quantity":1}]},{"address":"addr_test1qr7v9p5ewkymv5z7f8n6904239y5tvnqpj76364fyrymauwss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6slwph6d","amount":{"quantity":67740917,"unit":"lovelace"},"derivationPath":["1852H","1815H","0H","1","178"],"assets":[{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"","quantity":1},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"6861707079636f696e","quantity":2},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"7375706572636f696e","quantity":3},{"policyId":"789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1","assetName":"74727565636f696e","quantity":11},{"policyId":"94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c","assetName":"","quantity":4},{"policyId":"94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c","assetName":"676f6f64636f696e","quantity":3}]}],"certificates":[],"withdrawals":[],"fee":"0.195949","deposits":"0","depositsReclaimed":"0"}');

    console.debug('>>> PARSED: ', aa)
    groupTokensByPolicyId(aa.outputs[1].assets)
  }

  initTrezor = async () => {
    if (isHardwareWalletSupportEnabled && isTrezorEnabled) {
      logger.debug('[HW-DEBUG] HWStore - start trezor');
      await handleInitTrezorConnectChannel.request();
      await this.getAvailableDevices({ isTrezor: true });
    }
  };

  initLedger = async () => {
    if (isHardwareWalletSupportEnabled && isLedgerEnabled) {
      logger.debug('[HW-DEBUG] HWStore - HW STORE ACTIVE');
      await this.hardwareWalletDevicesRequest.execute();
      const storedDevices = this.hardwareWalletDevicesRequest.result;
      logger.debug('[HW-DEBUG] HWStore - storedDevices fetched');

      const devicesWithoutLedgers = {};
      map(storedDevices, async (device) => {
        if (device.deviceType === DeviceTypes.TREZOR) {
          devicesWithoutLedgers[device.id] = device;
        }
      });
      logger.debug('[HW-DEBUG] HWStore - Remove all LEDGERS from LC');
      await this.overrideHardwareWalletDevicesRequest.execute(
        devicesWithoutLedgers
      );

      logger.debug('[HW-DEBUG] HWStore - Refresh LC');
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();

      logger.debug('[HW-DEBUG] HWStore - INIT Ledger listeners');
      await handleInitLedgerConnectChannel.request();
      await this.getAvailableDevices({ isTrezor: false });
    }
  };

  getAvailableDevices = async (params: { isTrezor: boolean }) => {
    const { isTrezor } = params;
    await this.hardwareWalletsLocalDataRequest.execute();
    await this.hardwareWalletDevicesRequest.execute();
    logger.debug('[HW-DEBUG] HWStore - getAvailableDevices');

    // Set all logical HW into disconnected state
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
        logger.debug('[HW-DEBUG] HWStore - CHECK device');
        if (device.deviceType === DeviceTypes.TREZOR) {
          await getHardwareWalletTransportChannel.request({
            devicePath: device.path,
            isTrezor: true,
          });
        }
      } catch (e) {
        // eslint-disable-next-line
        logger.debug(' HWStore - CHECK device Error');
      }
    });

    await this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();
  };

  _sendMoney = async (params?: { isDelegationTransaction: boolean }) => {
    const isDelegationTransaction = get(params, 'isDelegationTransaction');
    const wallet = this.stores.wallets.active;

    if (!wallet) {
      throw new Error('Active wallet required before sending.');
    }

    this.setTransactionPendingState(true);

    try {
      const transaction = await this.sendMoneyRequest.execute({
        signedTransactionBlob: this.txBody,
      });
      if (!isDelegationTransaction) {
        // Start interval to check transaction state every second
        this.checkTransactionTimeInterval = setInterval(
          this.checkTransaction,
          1000,
          { transactionId: transaction.id, walletId: wallet.id }
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
      });
      throw e;
    }
  };

  // Check stake pool transaction state and reset pending state when transction is "in_ledger"
  @action checkTransaction = (request: {
    transactionId: string,
    walletId: string,
  }) => {
    const { transactionId, walletId } = request;
    const recentTransactionsResponse = this.stores.transactions._getTransactionsRecentRequest(
      walletId
    ).result;
    const recentTransactions = recentTransactionsResponse
      ? recentTransactionsResponse.transactions
      : [];

    // Return transaction when state is not "PENDING"
    const targetTransaction = find(
      recentTransactions,
      (transaction) =>
        transaction.id === transactionId &&
        transaction.state === TransactionStates.OK
    );

    if (targetTransaction) {
      this.resetStakePoolTransactionChecker(walletId);
    }
  };

  @action resetStakePoolTransactionChecker = (walletId: string) => {
    if (this.checkTransactionTimeInterval) {
      clearInterval(this.checkTransactionTimeInterval);
      this.checkTransactionTimeInterval = null;
    }
    this.stores.wallets.refreshWalletsData();
    this.isTransactionPending = false;
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetTransaction();
    this.stores.wallets.goToWalletRoute(walletId);
  };

  @action setTransactionPendingState = (isTransactionPending: boolean) => {
    runInAction('HardwareWalletsStore:: set transaction state', () => {
      this.isTransactionPending = isTransactionPending;
    });
  };

  // @TODO - move to Transactions store once all logic fit and hardware wallets listed in general wallets list
  selectCoins = async (params: CoinSelectionsPaymentRequestType) => {
    const { walletId, address, amount, assets } = params;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coins selections.');
    const { amount: totalAmount, availableAmount, reward } = wallet;
    try {
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

  @action establishHardwareWalletConnection = async () => {
    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { hardwareWalletDevices, hardwareWalletsConnectionData } = this;

    logger.debug('[HW-DEBUG] HWStore - establishHardwareWalletConnection');
    try {
      // Check if active wallet exist - this means that hw exist but we need to check if relevant device connected to it
      let recognizedPairedHardwareWallet;
      let relatedConnectionData;

      let activeWalletId;
      if (this.activeDelegationWalletId && this.isTransactionInitiated) {
        // Active wallet can be different that wallet we want to delegate
        activeWalletId = this.activeDelegationWalletId;
      } else {
        // For regular tx we are using active wallet
        activeWalletId = get(this.stores.wallets, ['active', 'id']);
      }

      if (activeWalletId) {
        // Check if device connected to wallet
        logger.debug('[HW-DEBUG] HWStore - active wallet exists');
        recognizedPairedHardwareWallet = find(
          hardwareWalletDevices,
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
          !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
      );

      logger.debug(
        '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: START'
      );

      // Tx Special cases!
      // This means that transaction needs to be signed but we don't know device connected to Software wallet
      let transportDevice;
      if (this.isTransactionInitiated || this.isAddressVerificationInitiated) {
        logger.debug(
          '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - check device'
        );

        // Return device that belongs to active hardwate wallet if is already plugged-in
        if (
          recognizedPairedHardwareWallet &&
          !recognizedPairedHardwareWallet.disconnected
        ) {
          logger.debug(
            '[HW-DEBUG] HWStore - Establish connection:: New Transaction / Address verification initiated - Recognized device found'
          );
          logger.debug('[HW-DEBUG] HWStore - Set transport device 1', {
            recognizedPairedHardwareWallet,
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
              this.cardanoAdaAppPollingInterval = setInterval(
                (devicePath, txWalletId, verificationAddress) =>
                  this.getCardanoAdaApp({
                    path: devicePath,
                    walletId: txWalletId,
                    address: verificationAddress,
                  }),
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
          logger.debug(
            '[HW-DEBUG] HWStore - Connect - New Transaction / Address verification initiated - return last device'
          );
          lastDeviceTransport = await getHardwareWalletTransportChannel.request(
            {
              devicePath: null, // Use last plugged device
              isTrezor,
            }
          );
          logger.debug('[HW-DEBUG] HWStore - Set transport device 2', {
            lastDeviceTransport,
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
              this.cardanoAdaAppPollingInterval = setInterval(
                (devicePath, txWalletId, verificationAddress) =>
                  this.getCardanoAdaApp({
                    path: devicePath,
                    walletId: txWalletId,
                    address: verificationAddress,
                  }),
                CARDANO_ADA_APP_POLLING_INTERVAL,
                lastDeviceTransport.path,
                activeWalletId,
                this.unfinishedWalletAddressVerification
              );
            }
          }
          // End of special case
        }
        return lastDeviceTransport;
      }
      // End of Tx Special cases!

      // Cases for wallet create / restore
      // it is triggered after flag activation "isListeningForDevice"
      if (lastUnpairedDevice) {
        logger.debug(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Start process with last UNPAIRED device'
        );
        // Start listeners for specific (last pluged) device
        let devicePath = null;
        let isTrezor = false;

        if (lastUnpairedDevice) {
          devicePath = lastUnpairedDevice.path;
          isTrezor = lastUnpairedDevice.deviceType === DeviceTypes.TREZOR;
        }

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

        logger.debug('[HW-DEBUG] HWStore - Transport retreived');
      } else {
        logger.debug(
          '[HW-DEBUG] HWStore - establishHardwareWalletConnection:: Set device listener'
        );
        runInAction('HardwareWalletsStore:: set device listener', () => {
          this.isListeningForDevice = true;
        });
        return null;
      }
      // End of Cases for wallet create / restore

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
          transportDevice,
        });
        runInAction('HardwareWalletsStore:: set HW device CONNECTED', () => {
          this.transportDevice = transportDevice;
        });

        if (deviceType === DeviceTypes.TREZOR) {
          // Jump to exporting public key
          await this._getExtendedPublicKey(transportDevice.path);
        } else {
          logger.debug('[HW-DEBUG] HWStore - START cardano app poller');
          // Start poller to recognize if Cardano App is launched on device
          const devicePath = transportDevice.path;
          logger.debug(
            '[HW-DEBUG] HWStore - getCardanoAdaApp - from  establishHardwareWalletConnection'
          );
          this.stopCardanoAdaAppFetchPoller();
          this.cardanoAdaAppPollingInterval = setInterval(
            (path) => this.getCardanoAdaApp({ path }),
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
  @action getCardanoAdaApp = async (params: {
    path: ?string,
    walletId?: string,
    address?: ?WalletAddress,
  }) => {
    const { path, walletId, address } = params;
    logger.debug(
      '[HW-DEBUG] HWStore - START FUNCTION getCardanoAdaApp PARAMS: ',
      { walletId, path, address }
    );

    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request({ path });
      logger.debug(
        '[HW-DEBUG] HWStore - cardanoAdaApp RESPONSE: ',
        cardanoAdaApp
      );
      // Cardano app recognized, stop poller
      this.stopCardanoAdaAppFetchPoller();

      if (cardanoAdaApp) {
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
        error,
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
          (recognizedDevice) => recognizedDevice.path === path
        );

        // Update device with new path - LC
        await this._setHardwareWalletDevice({
          deviceId: pairedDevice.id,
          data: {
            ...pairedDevice,
            path: error.path,
            isPending: false,
          },
        });

        // Update connected wallet data with new path - LC
        if (walletId) {
          logger.debug('[HW-DEBUG] Update connected wallet data with new path');
          const hardwareWalletConnectionData = get(
            this.hardwareWalletsConnectionData,
            walletId
          );

          if (hardwareWalletConnectionData) {
            logger.debug(
              '[HW-DEBUG] Update connected wallet data with new path - Set to LC'
            );
            await this._setHardwareWalletLocalData({
              walletId,
              data: {
                ...hardwareWalletConnectionData,
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
    path?: string
  ) => {
    if (this.isAddressVerificationInitiated) return;
    logger.debug('[HW-DEBUG] HWStore - Initiate Address Verification: ', {
      address,
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
    let devicePath = path || hardwareWalletConnectionData.device.path;

    logger.debug(
      '[HW-DEBUG] HWStore - Verify address - check is device connected: ',
      {
        disconnected,
        deviceType,
        devicePath,
      }
    );

    let transportDevice;
    if (disconnected && !path) {
      // Wait for connection to be established and continue with address verification
      try {
        transportDevice = await this.establishHardwareWalletConnection();
        if (transportDevice) {
          devicePath = transportDevice.path;
        }
      } catch (e) {
        logger.debug('[HW-DEBUG] HWStore - Establishing connection failed');
      }
    }

    // Add more cases / edge cases if needed
    if (deviceType === DeviceTypes.TREZOR) {
      logger.debug('[HW-DEBUG] Verify Address with Trezor: ', { address });
      // @TODO - Verifying address feature allowed only on Ledger devices for now
      // this.verifyAddress(address, devicePath);
      // runInAction('HardwareWalletsStore:: Initiate address verification', () => {
      //   this.isAddressVerificationInitiated = false;
      // });
    } else {
      logger.debug('[HW-DEBUG] Verify Address with Ledger: ', {
        address,
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

  @action verifyAddress = async (params: {
    address: WalletAddress,
    path: string,
    isTrezor: boolean,
  }) => {
    logger.debug('[HW-DEBUG] - VERIFY Address');
    const { address, path, isTrezor } = params;
    const { isMainnet } = this.environment;

    this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_ADDRESS;
    this.tempAddressToVerify = params;
    try {
      const derivedAddress = await deriveAddressChannel.request({
        devicePath: path,
        isTrezor,
        addressType: AddressType.BASE,
        spendingPathStr: address.spendingPath,
        stakingPathStr: `${SHELLEY_PURPOSE_INDEX}'/${ADA_COIN_TYPE}'/0'/2/0`,
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.protocolMagic,
      });
      // Derive address from path and check
      if (derivedAddress === address.id) {
        logger.debug('[HW-DEBUG] HWStore - Address successfully verified', {
          address: derivedAddress,
        });
        runInAction(
          'HardwareWalletsStore:: Address Verified and is correct',
          () => {
            this.isAddressDerived = true;
          }
        );
        this.showAddress(params);
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

  @action showAddress = async (params: {
    address: WalletAddress,
    path: string,
    isTrezor: boolean,
  }) => {
    logger.debug('[HW-DEBUG] - SHOW Address');
    const { address, path, isTrezor } = params;
    const { isMainnet } = this.environment;

    try {
      await showAddressChannel.request({
        devicePath: path,
        isTrezor,
        addressType: AddressType.BASE,
        spendingPathStr: address.spendingPath,
        stakingPathStr: `${SHELLEY_PURPOSE_INDEX}'/${ADA_COIN_TYPE}'/0'/2/0`,
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.protocolMagic,
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

  @action setAddressVerificationCheckStatus = (
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

  @action _getExtendedPublicKey = async (
    forcedPath: ?string,
    walletId?: string,
    address?: ?WalletAddress
  ) => {
    logger.debug('[HW-DEBUG] HWStore - extendedPublicKey', {
      forcedPath,
      walletId,
      address,
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
        path: "1852'/1815'/0'", // Shelley 1852 ADA 1815 indicator for account '0'
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
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path: devicePath,
              paired: recognizedWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          },
        });

        // Delete initiated (pending) device with this path since now is paired to wallet
        const recognizedDevice = find(
          this.hardwareWalletDevices,
          (device) => device.path === forcedPath
        );
        if (recognizedDevice) {
          logger.debug(
            '[HW-DEBUG] HWStore - _getExtendedPublicKey - UNSET Device with path: ',
            { recognizedDevice: recognizedDevice.id }
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
              deviceId,
              deviceType,
              deviceModel,
              deviceName,
              path: devicePath,
              paired: recognizedWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
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
            logger.debug(
              '[HW-DEBUG] HWStore - Device not belongs to this wallet'
            );
            // Keep isTransactionInitiated active & Set new device listener by initiating transaction
            // Show message to reconnect proper software wallet device pair
            logger.debug(
              '[HW-DEBUG] unfinishedWalletTxSigning SET: ',
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
              walletId
            );
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

        if (this.isAddressVerificationInitiated && address && devicePath) {
          logger.debug(
            '[HW-DEBUG] HWStore - Re-initiate Address verification from _getExtendedPublicKey: ',
            {
              address,
              devicePath,
            }
          );
          runInAction(
            'HardwareWalletsStore:: Re-Initiate Address verification',
            () => {
              this.isAddressVerificationInitiated = false;
              this.unfinishedWalletAddressVerification = address;
              this.isExportKeyAborted = false;
            }
          );
          this.verifyAddress({ address, path: devicePath, isTrezor: false });
          return;
        }

        // --> Else
        this.stores.wallets.goToWalletRoute(recognizedStoredWallet.id);
        this.actions.dialogs.closeActiveDialog.trigger();
        return;
      }

      logger.debug(
        '[HW-DEBUG] HWStore - I don not have recognized wallet - create new one or reject TX: ',
        { deviceId }
      );

      // Software Wallet not recognized and TX initiated. Show error
      if (this.isTransactionInitiated) {
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
        transportDevice,
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
      logger.debug('[HW-DEBUG] HWStore - HW created / restored');

      // Get all Pending devices with this path and delete
      const recognizedPendingDevice = find(
        this.hardwareWalletDevices,
        (device) => device.path === devicePath
      );
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

      this.resetInitializedConnection();
      this._refreshHardwareWalletsLocalData();
      this._refreshHardwareWalletDevices();
    } catch (error) {
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
        logger.debug('[HW-DEBUG] HWStore - Export:: WAIT FOR ANOTHER DEVICE');
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        if (isCancelled && isTrezor) {
          // Skip Trezor device-change events when rejected
          setTimeout(() => {
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
  @action _signTransactionTrezor = async (
    walletId: string,
    deviceId?: ?string
  ) => {
    const { coinSelection } = this.txSignRequest;
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });

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

    const { inputs, outputs, fee, certificates, withdrawals } = coinSelection;

    const inputsData = map(inputs, (input) => {
      return prepareTrezorInput(input);
    });

    const outputsData = map(outputs, (output) => {
      return prepareTrezorOutput(output);
    });

    const withdrawalsData = map(withdrawals, (withdrawal) => {
      return prepareTrezorWithdrawal(withdrawal);
    });

    const certificatesData = map(certificates, (certificate) =>
      prepareTrezorCertificate(certificate)
    );

    const recognizedDevice = find(
      this.hardwareWalletDevices,
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
        logger.debug('[HW-DEBUG] HWStore - Device not belongs to this wallet');
        // Keep isTransactionInitiated active & Set new device listener by initiating transaction
        // Show message to reconnect proper software wallet device pair
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
        walletId
      );
      runInAction('HardwareWalletsStore:: Initiate transaction', () => {
        this.isTransactionInitiated = false;
        this.unfinishedWalletTxSigning = null;
      });
    }

    const { isMainnet } = this.environment;
    const ttl = this._getTtl();
    const absoluteSlotNumber = this._getAbsoluteSlotNumber();

    try {
      const signedTransaction = await signTransactionTrezorChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: formattedAmountToLovelace(fee.toString()).toString(),
        ttl: ttl.toString(),
        validityIntervalStartStr: absoluteSlotNumber.toString(),
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.trezorProtocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.trezorProtocolMagic,
        certificates: certificatesData,
        withdrawals: withdrawalsData,
        devicePath: recognizedDevicePath,
      });

      if (!signedTransaction.success) {
        throw signedTransaction.payload;
      }

      runInAction(
        'HardwareWalletsStore:: transaction successfully signed',
        () => {
          this.txBody = signedTransaction.payload.serializedTx;
          this.hwDeviceStatus =
            HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        }
      );
    } catch (error) {
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
          this.isTransactionInitiated = false;
        }
      );
      // @TODO - Maybe we should handle this case as separated message in tx dialog
      if (error.code === 'Device_CallInProgress') {
        throw new Error('Device is busy - reconnect device and try again');
      }
      throw error;
    }
  };

  _signWitnesses = async (witnesses: Array<Witness>, xpubHex: string) => {
    const signedWitnesses = [];
    for (const witness of witnesses) {
      const signedWitness = await this.ShelleyWitness(witness, xpubHex);
      signedWitnesses.push(signedWitness);
    }
    return signedWitnesses;
  };

  ShelleyWitness = async (witness: Witness, xpubHex: string) => {
    const xpub = await this._deriveXpub(witness.path, xpubHex);
    const publicKey = xpub.slice(0, 32);
    const signature = Buffer.from(witness.witnessSignatureHex, 'hex');
    return ShelleyTxWitnessShelley(publicKey, signature);
  };

  _deriveXpub = CachedDeriveXpubFactory(async (xpubHex) => {
    return Buffer.from(xpubHex, 'hex');
  });

  _getRewardAccountAddress = async (walletId: string, path: Array<string>) => {
    const pathParams = getParamsFromPath(path);
    const publicKey = await this.getPublicKeyRequest.execute({
      walletId,
      role: pathParams.roleIdentity,
      index: pathParams.index,
    });
    const data = {
      stake: publicKey,
    };
    const constructedAddress = await this.constructAddressRequest.execute({
      data,
    });
    return constructedAddress.address;
  };

  // Ledger - Shelley only
  @action _signTransactionLedger = async (
    walletId: string,
    devicePath: ?string
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

    console.debug('>>> Coin selection: ', JSON.stringify(coinSelection));

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
    const absoluteSlotNumber = this._getAbsoluteSlotNumber();
    const auxiliaryData = null;
    const { isMainnet } = this.environment;

    try {
      const signedTransaction = await signTransactionLedgerChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: ttl.toString(),
        validityIntervalStartStr: absoluteSlotNumber.toString(),
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.protocolMagic,
        certificates: certificatesData,
        withdrawals: withdrawalsData,
        signingMode: TransactionSigningMode.ORDINARY_TRANSACTION,
        auxiliaryData,
        devicePath,
      });

      console.debug('>>> signedTransaction: ', JSON.stringify(signedTransaction));

      const unsignedTxWithdrawals =
        withdrawals.length > 0 ? ShelleyTxWithdrawal(withdrawals) : null;

      console.debug('>>>> Data for TX Aux: ', {
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: unsignedTxCerts,
        withdrawals: unsignedTxWithdrawals,
        signedTransaction,
      })

      // Prepare unsigned transaction structure for serialzation
      const unsignedTx = prepareTxAux({
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: unsignedTxCerts,
        withdrawals: unsignedTxWithdrawals,
      });

      console.debug('>>> unsigned TX: ', unsignedTx);

      const signedWitnesses = await this._signWitnesses(
        signedTransaction.witnesses,
        xpubHex
      );
      const txWitnesses = new Map();
      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }

      console.debug('>>> txWitnesses: ', txWitnesses);

      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await prepareBody(unsignedTx, txWitnesses);

      console.debug('>>> TX Body: ', txBody)

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

  initiateTransaction = async (params: { walletId: ?string }) => {
    const { walletId } = params;
    runInAction('HardwareWalletsStore:: Initiate Transaction', () => {
      this.isTransactionInitiated = true;
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      this.activeDelegationWalletId = walletId;
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
              !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
          );

          if (lastUnpairedDevice) {
            logger.debug('[HW-DEBUG] I HAVE UNPAIRED');
            transportDevice = lastUnpairedDevice;
          } else {
            logger.debug('[HW-DEBUG] CHECK FOR NEXT device');
            transportDevice = await getHardwareWalletTransportChannel.request({
              devicePath: null,
              isTrezor: true,
            });
          }
          logger.debug('[HW-DEBUG] INITIATE tx - I have transport');
        } else {
          transportDevice = await this.establishHardwareWalletConnection();
        }

        if (!transportDevice) {
          logger.debug('[HW-DEBUG] No new devices recognized for tx signing');
          throw new Error('Signing device not recognized!');
        }

        devicePath = transportDevice.path;
      } catch (e) {
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
      logger.debug('[HW-DEBUG] Sign Trezor: ', { id });
      const transportDevice = await this.establishHardwareWalletConnection();
      if (transportDevice) {
        logger.debug('[HW-DEBUG] HWStore - Set transport device 4', {
          transportDevice,
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
        { devicePath }
      );
      if (walletId) {
        this.stopCardanoAdaAppFetchPoller();
        this.cardanoAdaAppPollingInterval = setInterval(
          (path, wid) => this.getCardanoAdaApp({ path, walletId: wid }),
          CARDANO_ADA_APP_POLLING_INTERVAL,
          devicePath,
          walletId
        );
      }
    }
  };

  _resetTransaction = async (
    params: ?{
      cancelDeviceAction: boolean,
    }
  ) => {
    if (isHardwareWalletSupportEnabled) {
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
      logger.debug('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
      runInAction('HardwareWalletsStore:: reset Transaction verifying', () => {
        this.hwDeviceStatus = HwDeviceStatuses.READY;
        this.txBody = null;
        this.activeDevicePath = null;
        this.unfinishedWalletTxSigning = null;
        this.activeDelegationWalletId = null;
      });
    }
  };

  @action _changeHardwareWalletConnectionStatus = async (
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
    console.debug('>> DEVICE CHANGED: ', params);
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
      (connection) =>
        // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
        deviceType === DeviceTypes.TREZOR &&
        deviceId &&
        connection.device.deviceId === deviceId
    );

    if (disconnected && deviceType === DeviceTypes.LEDGER) {
      // Remove all stored Ledger instances from LC - both pending and paired (with software Wallets)
      logger.debug('[HW-DEBUG] HWStore - device disconnected');

      const recognizedLedgerDevice = find(
        hardwareWalletDevices,
        (hardwareWalletDevice) => hardwareWalletDevice.path === path
      );

      if (recognizedLedgerDevice) {
        logger.debug('[HW-DEBUG] HWStore - Remove Device from LC', {
          recognizedLedgerDevice,
        });
        await this._unsetHardwareWalletDevice({
          deviceId: recognizedLedgerDevice.id,
        });
      }

      logger.debug('[HW-DEBUG] HWStore - GET Paired and set to disconnected');
      const recognizedLedgerWallet = find(
        hardwareWalletsConnectionData,
        (connection) =>
          // We can not be sure that Ledger is right Wallet device because we don't have device ID at this point
          deviceType === DeviceTypes.LEDGER &&
          path &&
          connection.device.path === path
      );

      if (recognizedLedgerWallet) {
        logger.debug('[HW-DEBUG] HWStore - I have stored Ledger wallet');
        await this._setHardwareWalletLocalData({
          walletId: recognizedLedgerWallet.id,
          data: {
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
        recognizedPairedHardwareWallet.id
      );
      await this._setHardwareWalletLocalData({
        walletId: recognizedPairedHardwareWallet.id,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          disconnected: true, // Always reset connecting state to force re-connect
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
          deviceId: deviceId || pendingId.toString(), // device ID or timestamp (for pending devices without ID) - ledger Only
          data: {
            deviceType,
            deviceModel,
            deviceName,
            path,
            // paired: (recognizedPairedHardwareWallet && deviceType === DeviceTypes.LEDGER)
            //   ? recognizedPairedHardwareWallet.id
            //   : null, // Always reset pairing indication on Trezor to force re-connect and set if exist for Ledger
            paired: null, // Always reset pairing indication to force re-connect
            disconnected, // device physically disconnected
            isPending: !deviceId && !recognizedPairedHardwareWallet,
          },
        });
      }
    }

    await this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();

    // Start connection establishing process if devices listener flag is UP
    if (this.isListeningForDevice && !disconnected) {
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
        HwDeviceStatuses.CONNECTING
      );
      runInAction('HardwareWalletsStore:: Change status to Connecting', () => {
        this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      });
      logger.debug(
        '[HW-DEBUG] HWStore - Reinitialize TX signing: ',
        this.unfinishedWalletTxSigning
      );
      this.initiateTransaction({ walletId: this.unfinishedWalletTxSigning });
    }

    // Case that allows us to re-trigger address verification process multiple times if fails
    // TODO: Check if "eventType === DeviceEvents.CONNECT" is necessary here
    if (this.unfinishedWalletAddressVerification && !disconnected && path) {
      logger.debug(
        '[HW-DEBUG] CHANGE STATUS to: ',
        HwDeviceStatuses.CONNECTING
      );
      runInAction('HardwareWalletsStore:: Change status to Connecting', () => {
        this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      });
      logger.debug(
        '[HW-DEBUG] HWStore - Reinitialize Address Verification process: ',
        this.unfinishedWalletAddressVerification
      );
      // It is not possible to pass null value that FLOW marks as error (FlowFixMe used)
      this.initiateAddressVerification(
        // $FlowFixMe
        this.unfinishedWalletAddressVerification,
        path
      );
    }
  };

  @action resetInitializedConnection = async (
    params: ?{
      cancelDeviceAction: boolean,
    }
  ) => {
    const cancelDeviceAction = get(params, 'cancelDeviceAction', false);
    if (cancelDeviceAction) {
      resetTrezorActionChannel.request();
    }
    this.stopCardanoAdaAppFetchPoller();
    this.stores.wallets.createHardwareWalletRequest.reset();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    this.transportDevice = {};
    this.isListeningForDevice = false;
    this.isExportKeyAborted = false;
  };

  @action resetInitializedAddressVerification = async () => {
    this.stopCardanoAdaAppFetchPoller();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.transportDevice = {};
    this.isListeningForDevice = false;
    this.isAddressVerificationInitiated = false;
    this.unfinishedWalletAddressVerification = null;
    this.isAddressDerived = false;
    this.isAddressChecked = false;
    this.isAddressCorrect = null;
    this.tempAddressToVerify = {};
    this.isExportKeyAborted = false;
    this.activeDevicePath = null;
  };

  @action _refreshHardwareWalletsLocalData = async () => {
    await this.hardwareWalletsLocalDataRequest.execute();
  };

  @action _refreshHardwareWalletDevices = async () => {
    await this.hardwareWalletDevicesRequest.execute();
  };

  @computed get hardwareWalletsConnectionData(): HardwareWalletsLocalData {
    return this.hardwareWalletsLocalDataRequest.result;
  }

  @computed get hardwareWalletDevices(): HardwareWalletsLocalData {
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
      walletId
    );
    if (walletId) {
      await this.setHardwareWalletLocalDataRequest.execute(walletId, data);
      this._refreshHardwareWalletsLocalData();
      this.stores.wallets.refreshWalletsData();
    }
  };

  _unsetHardwareWalletLocalData = async ({
    walletId,
  }: {
    walletId: string,
  }) => {
    logger.debug('[HW-DEBUG] HWStore - _unsetHardwareWalletLocalData');
    await this.unsetHardwareWalletLocalDataRequest.execute(walletId);

    const pairedDevice = find(
      this.hardwareWalletDevices,
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
    await this.setHardwareWalletDeviceRequest.execute(deviceId, data);
    this._refreshHardwareWalletDevices();
  };

  _unsetHardwareWalletDevice = async ({ deviceId }: { deviceId?: ?string }) => {
    if (deviceId) {
      await this.unsetHardwareWalletDeviceRequest.execute(deviceId);
    } else {
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
    }
    this._refreshHardwareWalletDevices();
  };

  // For testing / development ONLY
  _resetHardwareWallets = async () => {
    if (global.environment.isDev) {
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
      await this.unsetHardwareWalletDevicesAllRequest.execute();
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();
    }
  };

  stopCardanoAdaAppFetchPoller = () => {
    logger.debug('[HW-DEBUG] HWStore - STOP Ada App poller');
    if (this.cardanoAdaAppPollingInterval) {
      clearInterval(this.cardanoAdaAppPollingInterval);
    }
  };
}
