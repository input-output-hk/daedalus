// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { get, map, find, findLast, filter } from 'lodash';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { HwDeviceStatuses } from '../domains/Wallet';
import {
  HW_SHELLEY_CONFIG,
  MINIMAL_TREZOR_FIRMWARE_VERSION,
  MINIMAL_LEDGER_FIRMWARE_VERSION,
  MINIMAL_CARDANO_APP_VERSION,
  isHardwareWalletSupportEnabled,
  isTrezorEnabled,
  isLedgerEnabled,
} from '../config/hardwareWalletsConfig';
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
} from '../ipc/getHardwareWalletChannel';
import {
  prepareLedgerInput,
  prepareLedgerOutput,
  prepareTxAux,
  prepareBody,
  prepareLedgerCertificate,
  CachedDeriveXpubFactory,
  ShelleyTxWitnessShelley,
  ShelleyTxInputFromUtxo,
  ShelleyTxOutput,
  ShelleyTxCert,
} from '../utils/shelleyLedger';
import {
  prepareTrezorInput,
  prepareTrezorOutput,
  prepareCertificate,
} from '../utils/shelleyTrezor';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../common/types/hardware-wallets.types';
import { formattedAmountToLovelace } from '../utils/formatters';
import { TransactionStates } from '../domains/WalletTransaction';
import {
  CERTIFICATE_TYPE,
  getParamsFromPath,
  hardenedPathToString,
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
  // @TODO - improve types
  inspectAddressRequest: Request<any> = new Request(
    this.api.ada.inspectAddress
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

  cardanoAdaAppPollingInterval: ?IntervalID = null;
  checkTransactionTimeInterval: ?IntervalID = null;

  setup() {
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.sendMoney.listen(this._sendMoney);
    hardwareWalletsActions.refreshHardwareWalletsLocalData.listen(
      this._refreshHardwareWalletsLocalData
    );
    hardwareWalletsActions.setHardwareWalletLocalData.listen(
      this._setHardwareWalletLocalData
    );
    hardwareWalletsActions.unsetHardwareWalletLocalData.listen(
      this._unsetHardwareWalletLocalData
    );
    hardwareWalletsActions.setHardwareWalletDevice.listen(
      this._setHardwareWalletDevice
    );
    hardwareWalletsActions.unsetHardwareWalletDevice.listen(
      this._unsetHardwareWalletDevice
    );
    getHardwareWalletConnectionChannel.onReceive(
      this._changeHardwareWalletConnectionStatus
    );
    this.initTrezor();
    this.initLedger();
    this.hardwareWalletsLocalDataRequest.execute();
    this.hardwareWalletDevicesRequest.execute();
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
        // this._resetTransaction();
      }
      this.stores.wallets.refreshWalletsData();
      this.sendMoneyRequest.reset();
      return transaction;
    } catch (e) {
      this.setTransactionPendingState(false);
      // this._resetTransaction();
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
    const { walletId, address, amount } = params;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coins selections.');

    try {
      const coinSelection = await this.selectCoinsRequest.execute({
        walletId,
        payments: {
          address,
          amount,
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
    try {
      const coinSelection = await this.selectCoinsRequest.execute({
        walletId,
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
    console.debug('>>> ESTABLISH CONNECTION');
    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { hardwareWalletDevices, hardwareWalletsConnectionData } = this;
    const activeWallet = this.stores.wallets.active;

    logger.debug('[HW-DEBUG] HWStore - establishHardwareWalletConnection');

    try {
      // Check if active wallet exist - this means that hw exist but we need to check if relevant device connected to it
      let recognizedPairedHardwareWallet;
      let relatedConnectionData;
      if (activeWallet) {
        // Check if device connected to wallet
        logger.debug('[HW-DEBUG] HWStore - active wallet exists');
        recognizedPairedHardwareWallet = find(
          hardwareWalletDevices,
          (recognizedDevice) => recognizedDevice.paired === activeWallet.id
        );

        relatedConnectionData = find(
          hardwareWalletsConnectionData,
          (connection) => connection.id === activeWallet.id
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
      if (this.isTransactionInitiated) {
        logger.debug(
          '[HW-DEBUG] HWStore - Establish connection:: Transaction initiated - check device'
        );

        // Return device that belongs to active hardwate wallet if is already plugged-in
        if (
          recognizedPairedHardwareWallet &&
          !recognizedPairedHardwareWallet.disconnected
        ) {
          logger.debug(
            '[HW-DEBUG] HWStore - Establish connection:: Transaction initiated - Recognized device found'
          );
          return recognizedPairedHardwareWallet;
        }

        // Device not recognized or not plugged-in. Wait for next device (check by device type)
        const relatedConnectionDataDeviceType = get(relatedConnectionData, [
          'device',
          'deviceType',
        ]);

        let lastDeviceTransport = null;
        if (relatedConnectionDataDeviceType) {
          logger.debug(
            '[HW-DEBUG] HWStore - Connect - TRANSACTION initiated - return last device'
          );
          lastDeviceTransport = await getHardwareWalletTransportChannel.request(
            {
              devicePath: null, // Use last plugged device
              isTrezor: relatedConnectionDataDeviceType === DeviceTypes.TREZOR,
            }
          );
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
  }) => {
    const { path, walletId } = params;
    logger.debug(
      '[HW-DEBUG] HWStore - START FUNCTION getCardanoAdaApp PARAMS: ',
      { walletId, path }
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

        // Check if I have Software wallet with this deviceId
        const recognizedWallet = find(
          this.hardwareWalletsConnectionData,
          (hardwareWalletData) =>
            hardwareWalletData.device.deviceId === cardanoAdaApp.deviceId
        );

        let isDisconnected = true;
        if (recognizedWallet) {
          isDisconnected = false;
          logger.debug(
            '[HW-DEBUG] HWStore - cardanoAdaApp - SET Software wallet as connected',
            { recognizedWalletID: recognizedWallet.id }
          );
          this._setHardwareWalletLocalData({
            walletId: recognizedWallet.id,
            data: {
              disconnected: false,
              path,
              device: {
                ...recognizedWallet.device,
                path,
              },
            },
          });

          // Delete initiate (pending) device with this path
          const recognizedDevice = find(
            this.hardwareWalletDevices,
            (device) => device.path === path
          );
          if (recognizedDevice) {
            logger.debug(
              '[HW-DEBUG] HWStore - cardanoAdaApp - UNSET Device with path: ',
              { recognizedDevice: recognizedDevice.id }
            );
            await this._unsetHardwareWalletDevice({
              deviceId: recognizedDevice.id,
            });
          }

          // Add PAIRED device to LC
          if (recognizedDevice) {
            await this._setHardwareWalletDevice({
              deviceId: cardanoAdaApp.deviceId,
              data: {
                ...recognizedDevice,
                id: cardanoAdaApp.deviceId,
                path,
                paired: recognizedWallet.id, // device paired with software wallet
                disconnected: isDisconnected, // device physically disconnected
                isPending: false,
              },
            });
          }
        }

        if (this.isTransactionInitiated) {
          // Check if sender wallet match transaction initialization
          if (
            !walletId ||
            !recognizedWallet ||
            (recognizedWallet && recognizedWallet.id !== walletId)
          ) {
            logger.debug(
              '[HW-DEBUG] HWStore - Device not belongs to this wallet'
            );
            // Stop poller
            this.stopCardanoAdaAppFetchPoller();
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
            });
            this._signTransactionLedger(walletId, path);
          }
        } else if (recognizedWallet) {
          // While Cardano ADA app recognized & existing wallet mathes device ID, set wallet as active
          this.stores.wallets.goToWalletRoute(recognizedWallet.id);
          this.actions.dialogs.closeActiveDialog.trigger();
          runInAction(
            'HardwareWalletsStore:: set HW device to initial state',
            () => {
              this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
            }
          );
        } else {
          // While Cardano ADA app recognized on Ledger, proceed to exporting public key
          await this._getExtendedPublicKey(path);
        }
      }
    } catch (error) {
      console.debug('>>> [HW-DEBUG] HWStore - Cardano app fetching error', {
        error,
      });
      if (error.code === 'DEVICE_NOT_CONNECTED') {
        // Special case. E.g. device unplugged before cardano app is opened
        // Stop poller and re-initiate connecting state / don't kill devices listener
        this.stopCardanoAdaAppFetchPoller();

        // Wait for 1.5 sec and switch to CONNECTING if status still in LAUNCHING_CARDANO_APP
        setTimeout(() => {
          console.debug('>>> [HW-DEBUG] SET AFTER DELAY')
          if (this.hwDeviceStatus === HwDeviceStatuses.LAUNCHING_CARDANO_APP) {
            console.debug('>>> [HW-DEBUG] YES - SET AFTER DELAY')
            runInAction(
              'HardwareWalletsStore:: Set connecting status',
              () => {
                this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
                this.isListeningForDevice = true;
              }
            );
          }
        }, 3000);

      } else if (error.code === 'DEVICE_PATH_CHANGED' && error.path) {
        // Special case on Windows where device path changes after opening Cardano app
        // Stop poller and re-initiate connecting state / don't kill devices listener
        logger.debug('[HW-DEBUG] Update LC data with new path');
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

        if (this.isTransactionInitiated) {
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
          (devicePath, txWalletId) =>
            this.getCardanoAdaApp({ path: devicePath, walletId: txWalletId }),
          CARDANO_ADA_APP_POLLING_INTERVAL,
          error.path,
          walletId
        );
      }
      throw error;
    }
  };

  @action _getExtendedPublicKey = async (forcedPath: ?string) => {
    logger.debug('[HW-DEBUG] - extendedPublicKey');
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
          hardwareWalletData.extendedPublicKey.chainCodeHex ===
            extendedPublicKey.chainCodeHex &&
          hardwareWalletData.extendedPublicKey.publicKeyHex ===
            extendedPublicKey.publicKeyHex
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

        // @TODO - guard against Ledger - logic needs to be changed and deviceId (serial) applied
        logger.debug('[HW-DEBUG] HWStore - SET device from key export: ', {
          deviceId,
        });
        if (deviceId) {
          this._setHardwareWalletDevice({
            deviceId,
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path: devicePath,
              paired: recognizedWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          });
        }

        this.stores.wallets.goToWalletRoute(recognizedStoredWallet.id);
        this.actions.dialogs.closeActiveDialog.trigger();
        return;
      }

      logger.debug(
        '[HW-DEBUG] HWStore - I don not have recognized wallet - create new one: ',
        { deviceId }
      );
      // Software Wallet not recognized, create new one with default name
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
    deviceId?: string
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

    const { inputs, outputs, fee, certificates } = coinSelection;

    const inputsData = map(inputs, (input) => {
      return prepareTrezorInput(input);
    });

    const outputsData = map(outputs, (output) => {
      return prepareTrezorOutput(output);
    });

    const certificatesData = map(certificates, (certificate) =>
      prepareCertificate(certificate)
    );

    const recognizedDevice = find(
      this.hardwareWalletDevices,
      (hardwareWalletDevice) => hardwareWalletDevice.paired === walletId
    );
    const recognizedDevicePath = get(recognizedDevice, 'path', null);

    // Check if I have Software wallet with this deviceId
    // const recognizedWallet = find(
    //   this.hardwareWalletsConnectionData,
    //   (hardwareWalletData) =>
    //     hardwareWalletData.device.deviceId === recognizedDevice.id
    // );
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

    try {
      const signedTransaction = await signTransactionTrezorChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: formattedAmountToLovelace(fee.toString()).toString(),
        ttl: '150000000',
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.trezorProtocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.trezorProtocolMagic,
        certificates: certificatesData,
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

  _deriveXpub = CachedDeriveXpubFactory(async (absDerivationPath) => {
    logger.debug('[HW-DEBUG] HWStore - DERIVE xpub');
    const response = await getExtendedPublicKeyChannel.request({
      path: hardenedPathToString(absDerivationPath),
      isTrezor: false,
      devicePath: this.activeDevicePath,
    });
    const xpubHex = `${response.publicKeyHex}${response.chainCodeHex}`;
    return Buffer.from(xpubHex, 'hex');
  });

  _signWitnesses = async (witnesses: Array<Witness>) => {
    const signedWitnesses = [];
    for (const witness of witnesses) {
      const signedWitness = await this.ShelleyWitness(witness);
      signedWitnesses.push(signedWitness);
    }
    return signedWitnesses;
  };

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

  ShelleyWitness = async (witness: Witness) => {
    const xpub = await this._deriveXpub(witness.path);
    const publicKey = xpub.slice(0, 32);
    const signature = Buffer.from(witness.witnessSignatureHex, 'hex');
    return ShelleyTxWitnessShelley(publicKey, signature);
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
    const { inputs, outputs, certificates, fee: flatFee } = coinSelection;
    logger.debug('[HW-DEBUG] HWStore - sign transaction Ledger: ', {
      walletId,
    });

    const unsignedTxInputs = [];
    const inputsData = map(inputs, (input) => {
      const shelleyTxInput = ShelleyTxInputFromUtxo(input);
      unsignedTxInputs.push(shelleyTxInput);
      return prepareLedgerInput(input);
    });

    const unsignedTxOutputs = [];
    const outputsData = map(outputs, (output) => {
      const shelleyTxOutput = ShelleyTxOutput(output);
      unsignedTxOutputs.push(shelleyTxOutput);
      return prepareLedgerOutput(output);
    });

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

    const fee = formattedAmountToLovelace(flatFee.toString());
    const ttl = 150000000;
    const withdrawals = [];
    const metadataHashHex = null;
    const { isMainnet } = this.environment;

    try {
      const signedTransaction = await signTransactionLedgerChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: ttl.toString(),
        networkId: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.networkId,
        protocolMagic: isMainnet
          ? HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic
          : HW_SHELLEY_CONFIG.NETWORK.TESTNET.protocolMagic,
        certificates: certificatesData,
        withdrawals,
        metadataHashHex,
        devicePath,
      });
      // Prepare unsigned transaction structure for serialzation
      const unsignedTx = prepareTxAux({
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: unsignedTxCerts,
        withdrawals,
      });

      const signedWitnesses = await this._signWitnesses(
        signedTransaction.witnesses
      );
      const txWitnesses = new Map();
      if (signedWitnesses.length > 0) {
        txWitnesses.set(0, signedWitnesses);
      }

      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await prepareBody(unsignedTx, txWitnesses);

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
    runInAction('HardwareWalletsStore:: Initiate Transaction', () => {
      this.isTransactionInitiated = true;
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { walletId } = params;
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
    let deviceId;
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
          // throw new Error('Signing device not recognized!');
        } else {
          transportDevice = await this.establishHardwareWalletConnection();
        }

        if (!transportDevice) {
          logger.debug('[HW-DEBUG] No new devices recognized for tx signing');
          throw new Error('Signing device not recognized!');
        }

        devicePath = transportDevice.path;
        deviceId = transportDevice.id || transportDevice.deviceId;
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
      this._signTransactionTrezor(walletId, deviceId);
      runInAction('HardwareWalletsStore:: Initiate transaction', () => {
        this.isTransactionInitiated = false;
      });
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
    });
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
    } = params;

    logger.debug('[HW-DEBUG] HWStore - CHANGE status: ', {
      disconnected,
      deviceId,
      deviceType,
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
      const recognizedLedgerDevices = filter(
        hardwareWalletDevices,
        (hardwareWalletDevice) =>
          hardwareWalletDevice.deviceType === DeviceTypes.LEDGER &&
          hardwareWalletDevice.path === path
      );

      // Delete or pending or paired Ledger device
      map(recognizedLedgerDevices, (recognizedLedgerDevice) => {
        logger.debug('[HW-DEBUG] HWStore - UNSET: ', recognizedLedgerDevice.id);
        return this._unsetHardwareWalletDevice({
          deviceId: recognizedLedgerDevice.id,
        });
      });

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
          disconnected,
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
            paired: recognizedPairedHardwareWallet
              ? recognizedPairedHardwareWallet.id
              : null, // device paired with software wallet
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
    if (this.unfinishedWalletTxSigning && !disconnected) {
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
      logger.debug('[HW-DEBUG] unfinishedWalletTxSigning UNSET');
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
