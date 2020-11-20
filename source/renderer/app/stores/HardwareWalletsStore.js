// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { get, map, find, findLast } from 'lodash';
import BigNumber from 'bignumber.js';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { HwDeviceStatuses } from '../domains/Wallet';
import {
  HW_SHELLEY_CONFIG,
  MINIMAL_TREZOR_FIRMWARE_VERSION,
  MINIMAL_LEDGER_FIRMWARE_VERSION,
  MINIMAL_CARDANO_APP_VERSION,
  WRONG_CARDANO_APP_VERSION,
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
import { CERTIFICATE_TYPE, getParamsFromPath, hardenedPathToString } from '../utils/hardwareWalletUtils';

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
  SetHardwareWalletLocalDataRequestType,
  SetHardwareWalletDeviceRequestType,
} from '../api/utils/localStorage';
import type {
  TransportDevice,
  LedgerModel,
  TrezorModel,
  HardwareWalletExtendedPublicKeyResponse,
  HardwareWalletConnectionRequest,
} from '../../../common/types/hardware-wallets.types';

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
  getPublicKeyRequest: Request<any> = new Request(
    this.api.ada.getPublicKey
  );
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
      console.debug('>>> start trezor');
      await handleInitTrezorConnectChannel.request();
      await this.getAvailableDevices({ isTrezor: true });
    }
  };

  initLedger = async () => {
    console.debug('>>> HW STORE ACTIVE <<<');
    if (isHardwareWalletSupportEnabled && isLedgerEnabled) {
      console.debug('>>> start ledger');
      await handleInitLedgerConnectChannel.request();
      await this.getAvailableDevices({ isTrezor: false });
    }
  };

  getAvailableDevices = async (params: { isTrezor: boolean }) => {
    const { isTrezor } = params;
    await this.hardwareWalletsLocalDataRequest.execute();
    await this.hardwareWalletDevicesRequest.execute();
    console.debug('>>>> getAvailableDevices <<<<<', {
      hardwareWalletsConnectionData: this.hardwareWalletsConnectionData,
      hardwareWalletDevices: this.hardwareWalletDevices,
    });

    // Set all logical HW into disconnected state
    map(this.hardwareWalletsConnectionData, async (connectedWallet) => {
      console.debug('>>> SET 0 _setHardwareWalletLocalData: ', connectedWallet);
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
        (!isTrezor && (device.deviceType !== DeviceTypes.LEDGER)) ||
        (isTrezor && (device.deviceType !== DeviceTypes.TREZOR || (device.deviceType === DeviceTypes.TREZOR && !this.isTrezorBridgeInstalled)))
      ) {
        return;
      }

      try {
        console.debug('>>> CHECK device - TRIGGER: ', device);
        if (device.deviceType === DeviceTypes.TREZOR) {
          await getHardwareWalletTransportChannel.request({
            devicePath: device.path,
            isTrezor: true,
          });
        } else {
          if (device.isPending) {
            console.debug('>>> UNSET pending')
            this._unsetHardwareWalletDevice({ deviceId: device.id });
          } else {
            console.debug('>>> CHECK if Cardano App opened');
            console.debug('>>> getCardanoAdaApp - from  getAvailableDevices');
            this.getCardanoAdaApp({ path: device.path, isCheck: true });
          }
        }
      } catch (e) {
        // eslint-disable-next-line
        console.debug('getAvailableDevices::Error', e);
      }
    });

    this._refreshHardwareWalletsLocalData();
    this._refreshHardwareWalletDevices();
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
      console.debug('>>> SEND MONEY - DONE: ', { transaction, params });

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
      console.debug('>>> SEND MONEY ERROR: ', e);
      this.setTransactionPendingState(false);
      this._resetTransaction();
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

  @action establishHardwareWalletConnection = async (forcedDeviceData?: { type?: string, path?: string, pendingId: number }) => {
    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { hardwareWalletDevices } = this;
    const activeWallet = this.stores.wallets.active;

    console.debug('>>> establishHardwareWalletConnection: ', {
      hardwareWalletDevices: this.hardwareWalletDevices,
      activeWallet,
      isListeningForDevice: this.isListeningForDevice,
      hardwareWalletDevices22: this.hardwareWalletDevicesRequest.result,
      isExec: this.hardwareWalletDevicesRequest.isExecuting,
    });


    // Get next connected device
    // if (this.isListeningForDevice) {
    //   runInAction(
    //     'HardwareWalletsStore:: set device listener',
    //     () => {
    //       this.isListeningForDevice = false;
    //     }
    //   );
    // }

    try {
      // Check if active wallet exist - this means that hw exist but we need to check if relevant device connected to it
      let recognizedPairedHardwareWallet;
      if (activeWallet) {
        // Check if device connected to wallet
        recognizedPairedHardwareWallet = find(
          hardwareWalletDevices,
          (recognizedDevice) => recognizedDevice.paired === activeWallet.id
        );
      }

      const lastUnpairedConnectedDevice = findLast(
        this.hardwareWalletDevices,
        (hardwareWalletDevice) =>
          !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected
      );

      console.debug('>>> establishHardwareWalletConnection:: START ', {recognizedPairedHardwareWallet, lastUnpairedConnectedDevice});

      // Special case! When device not connected and TxSend is initialized we need to hook this process before and return void just to indicate that txSend process can continue
      let transportDevice;
      if (
        recognizedPairedHardwareWallet &&
        recognizedPairedHardwareWallet.disconnected
      ) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 1 ', {recognizedPairedHardwareWallet});
        // Device exist and but not physically connected - initiate listener to first recognized device and let txSend to reject wrong device
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: recognizedPairedHardwareWallet.path, // Path can change because device is disconnected now.
          isTrezor:
            recognizedPairedHardwareWallet.deviceType === DeviceTypes.TREZOR,
        });
        console.debug('>>> transportDevice: ', transportDevice);
        return transportDevice; // Special Case when we are waiting on response return to continue with tx signing
      }

      if (this.isTransactionInitiated && !recognizedPairedHardwareWallet) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 2.1 - from txSend (1)', {recognizedPairedHardwareWallet});
        return null; // Special Case when we are waiting on response return to continue with tx signing
      }

      if (this.isTransactionInitiated && recognizedPairedHardwareWallet && !recognizedPairedHardwareWallet.disconnected) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 2.2 - from txSend (2) ', {recognizedPairedHardwareWallet});
        return recognizedPairedHardwareWallet; // Special Case when we are waiting on response return to continue with tx signing
      }

      // Cases for wallet create / restore - it is triggered after flag activation "isListeningForDevice"
      if (lastUnpairedConnectedDevice || (forcedDeviceData)) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 3 ', { lastUnpairedConnectedDevice, forcedDeviceData });
        // Start listeners for specific (last connected) device
        let devicePath = null;
        let isTrezor;
        if (forcedDeviceData) {
          devicePath = forcedDeviceData.path;
          isTrezor = forcedDeviceData.type === DeviceTypes.TREZOR;
        }

        if (lastUnpairedConnectedDevice) {
          devicePath = lastUnpairedConnectedDevice.path;
          isTrezor = lastUnpairedConnectedDevice.deviceType === DeviceTypes.TREZOR;
        }

        if (!isTrezor) {
          transportDevice = null;
        };


        console.debug('>>>> CASE WHEN LISTENING DEVICE <<<<: ', lastUnpairedConnectedDevice);

        if (lastUnpairedConnectedDevice.deviceType === DeviceTypes.TREZOR) {
          transportDevice = await getHardwareWalletTransportChannel.request({
            devicePath,
            isTrezor,
          });
        } else {
          transportDevice = lastUnpairedConnectedDevice;
        }


        console.debug('>>> Transport: ', transportDevice);

        // Only Ledger has Pending devices
        // if (transportDevice && transportDevice.deviceType === DeviceTypes.LEDGER && lastUnpairedConnectedDevice && lastUnpairedConnectedDevice.isPending) {
        //   // Override transport because of missing ID in other ADA lib methods
        //   transportDevice = lastUnpairedConnectedDevice;
        // }

      } else {
        console.debug('>>> establishHardwareWalletConnection:: CASE 4');
        // Start listeners for both device types
        // Start Trezor listener
        /* transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: null,
          isTrezor: true,
        }); */

        runInAction(
          'HardwareWalletsStore:: set device listener',
          () => {
            this.isListeningForDevice = true;
          }
        );
        return;

        // @TODO - improve logic for Ledger (no devices -> connect device when connecting state initialized)
        if (!transportDevice) {
          // Start Ledger listener
          // transportDevice = await getHardwareWalletTransportChannel.request({
          //   devicePath: null,
          //   isTrezor: false,
          // });
        }
      }

      console.debug('>>> establishHardwareWalletConnection:: Exec: ', { transportDevice });

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
            throw new Error(`Firmware must be ${minFirmwareVersion} or greater!`);
          }
        }

        // All Checks pass - mark device as connected (set transport device for this session)
        runInAction('HardwareWalletsStore:: set HW device CONNECTED', () => {
          this.transportDevice = transportDevice;
        });

        if (deviceType === DeviceTypes.TREZOR) {
          // Jump to exporting public key
          await this._getExtendedPublicKey();
        } else {
          console.debug('>>> START cardano app poller: ', {forcedDeviceData});
          // Start poller to recognize if Cardano App is launched on device
          // const devicePath = forcedDeviceData ? forcedDeviceData.path : transportDevice.path;
          const devicePath = transportDevice.path;
          const pendingId = forcedDeviceData ? forcedDeviceData.pendingId : null;
          console.debug('>>> getCardanoAdaApp - from  establishHardwareWalletConnection: ', { cardanoAdaAppPollingInterval: this.cardanoAdaAppPollingInterval });
          // if (!this.cardanoAdaAppPollingInterval) {
            this.cardanoAdaAppPollingInterval = setInterval(
              (path, pendingId) => this.getCardanoAdaApp({ path, pendingId }),
              CARDANO_ADA_APP_POLLING_INTERVAL,
              devicePath,
              forcedDeviceData ? forcedDeviceData.pendingId : null,
            );
          // }
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
  @action getCardanoAdaApp = async (params?: { path?: string, isCheck?: boolean, pendingId?: boolean, walletId?: string }) => {
    const { path, isCheck, pendingId, walletId } = params;
    console.debug('>>> START FUNCTION getCardanoAdaApp PARAMS: ', params);
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request({ path });

      console.debug('>>> cardanoAdaApp RESPONSE: ', cardanoAdaApp);
      // @TODO - keep poller until app recognized or process exited
      this.stopCardanoAdaAppFetchPoller();
      if (cardanoAdaApp) {
        console.debug('>>> cardanoAdaApp - Set device: ', {
          cardanoAdaApp,
          path,
          hardwareWalletsConnectionData: this.hardwareWalletsConnectionData,
          hardwareWalletDevices: this.hardwareWalletDevices,
        });

        const cardanoAppVersion = `${cardanoAdaApp.major}.${cardanoAdaApp.minor}.${cardanoAdaApp.patch}`

        console.debug('>>cardanoAppVersion: ', {cardanoAppVersion, MINIMAL_CARDANO_APP_VERSION})
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
          throw new Error(`Cardano app must be ${MINIMAL_CARDANO_APP_VERSION} or greater!`);
        }

        // CHECK if I have wallet with THIS deviceId
        const recognizedWallet = find(
          this.hardwareWalletsConnectionData,
          (hardwareWalletData) =>
            hardwareWalletData.device.deviceId === cardanoAdaApp.deviceId
        );

        console.debug('>>> cardanoAdaApp - I have recognized wallet: ', recognizedWallet);

        let devicePath = path;

        let isDisconnected = true
        if (recognizedWallet) {
          devicePath = path || recognizedWallet.device.path;
          isDisconnected = false;
          console.debug('>>> SET 1: ', recognizedWallet);
          this._setHardwareWalletLocalData({
            walletId: recognizedWallet.id,
            data: {
              disconnected: false,
              path: devicePath,
              device: {
                ...recognizedWallet.device,
                path: devicePath,
              }
            },
          });
        }

        let pendingDevice;
        if (pendingId) {
          // Get all data from pending entry
          pendingDevice = find(
            this.hardwareWalletDevices,
            (hardwareWalletDevice) =>
              hardwareWalletDevice.id === pendingId
          );
          console.debug('>>>> PENDING DEVICE: ', pendingDevice);
          // Remove pending device entry from LC
          this._unsetHardwareWalletDevice({ deviceId: pendingId });
        }

        const deviceDate = pendingDevice || {};
        this._setHardwareWalletDevice({
          deviceId: cardanoAdaApp.deviceId,
          data: {
            ...deviceDate,
            id: cardanoAdaApp.deviceId,
            path: devicePath || deviceDate.path,
            paired: recognizedWallet ? recognizedWallet.id : null, // device paired with software wallet
            disconnected: isDisconnected, // device physically disconnected
            isPending: false,
          },
        });

        // While Cardano ADA app recognized on Ledger, proceed to exporting public key
        if (!isCheck) {
          await this._getExtendedPublicKey(devicePath);
        }
        if (this.isTransactionInitiated) {
          // Check if sender wallet match transaction initialization
          if (!recognizedWallet || (recognizedWallet && recognizedWallet.id !== walletId)) {
            // Stop poller
            this.stopCardanoAdaAppFetchPoller();
            // Keep isTransactionInitiated active
            // Set new device listener by initiating transaction
            console.debug('>>> INITIATE TRANSACTION - from getCardanoAdaApp');
            // this.initiateTransaction({ walletId })
            // Show message to reconnect proper software wallet device pair
            runInAction(
            'HardwareWalletsStore:: set HW device CONNECTING FAILED',
              () => {
                this.hwDeviceStatus = HwDeviceStatuses.CONNECTING_FAILED;
                this.activeDevicePath = null;
                this.unfinishedWalletTxSigning = walletId;
              }
            );
          } else {
            console.debug('>>>>> Transaction Initiated - Close: ', walletId);
            runInAction('HardwareWalletsStore:: Initiate transaction', () => {
              this.isTransactionInitiated = false;
              this.unfinishedWalletTxSigning = null;
            });
            this._signTransactionLedger(walletId, devicePath);
          }

        }
      }
    } catch (error) {
      console.debug('>>> cardanoAdaApp - error: ', error);
      throw error;
    }
  };

  @action _getExtendedPublicKey = async (forcedPath?: string) => {
    console.debug('>>>> extendedPublicKey: ', forcedPath);
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { transportDevice } = this;
    if (!transportDevice) {
      throw new Error(
        'Can not export extended public key: Device not recognized!'
      );
    }
    const {
      deviceType,
      path,
      deviceName,
      deviceModel,
    } = transportDevice;
    const isTrezor = deviceType === DeviceTypes.TREZOR;

    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path: "1852'/1815'/0'", // Shelley 1852 ADA 1815 indicator for account '0'
        isTrezor,
        devicePath: forcedPath || path,
      });

      const deviceId = extendedPublicKey.deviceId || transportDevice.deviceId;

      console.debug('>>> EXPORT - deviceID: ', {deviceId, extendedPublicKey});

      const recognizedDevice = find(
        this.hardwareWalletsConnectionData,
        (hardwareWalletData) =>
          hardwareWalletData.extendedPublicKey.chainCodeHex ===
            extendedPublicKey.chainCodeHex &&
          hardwareWalletData.extendedPublicKey.publicKeyHex ===
            extendedPublicKey.publicKeyHex
      );
      const recognizedWallet = recognizedDevice
        ? this.stores.wallets.getWalletById(recognizedDevice.id)
        : null;

      // Check if public key matches already restored hardware wallet public key
      if (recognizedWallet) {
        console.debug('>>> I have recognized wallet: ', {recognizedWallet});
        console.debug('>>> SET 2 _setHardwareWalletLocalData: ', recognizedWallet);
        this._setHardwareWalletLocalData({
          walletId: recognizedWallet.id,
          data: {
            disconnected: false,
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path: forcedPath || path,
              paired: recognizedWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          },
        });

        // @TODO - guard against Ledger - logic needs to be changed and deviceId (serial) applied
        console.debug('>> SET device from key export: ', {transportDevice, extendedPublicKey, deviceId});
        if (deviceId) {
          this._setHardwareWalletDevice({
            deviceId,
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path: forcedPath || path,
              paired: recognizedWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          });
        }
        this.stores.wallets.goToWalletRoute(recognizedDevice.id);
        this.actions.dialogs.closeActiveDialog.trigger();
        return;
      }
      console.debug('>>> I don not have recognized wallet - create new one: ', {transportDevice, deviceId});
      // Active wallet not exist, create new one with default name
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: deviceName || DEFAULT_HW_NAME,
        extendedPublicKey,
        device: {
          deviceId,
          deviceType,
          deviceModel,
          deviceName,
          path: forcedPath || path,
        },
      });

      this.resetInitializedConnection();
      this._refreshHardwareWalletsLocalData();
      this._refreshHardwareWalletDevices();
    } catch (e) {
      /**
       * ============  Exporting aborted  =============
       * e.statusCode === 28169 // Ledger
       * e.code === 'Failure_ActionCancelled' // Trezor
       */
      runInAction(
        'HardwareWalletsStore:: Cannot export extended public key',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
        }
      );
      // Pass other errors to caller (establishHardwareWalletConnection() in this case) and handle additional actions if needed
      throw e;
    }
  };

  // Trezor - Shelley only
  @action _signTransactionTrezor = async (walletId: string) => {
    const { coinSelection } = this.txSignRequest;
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });

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

    try {
      const signedTransaction = await signTransactionTrezorChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: formattedAmountToLovelace(fee.toString()).toString(),
        ttl: '15000000',
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
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
    console.debug('>>> DERIVE xpub: ', {absDerivationPath, activeDevicePath: this.activeDevicePath})
    const response = await getExtendedPublicKeyChannel.request({
      path: hardenedPathToString(absDerivationPath),
      isTrezor: false,
      devicePath: this.activeDevicePath,
    });
    const xpubHex = `${response.publicKeyHex}${response.chainCodeHex}`;
    return Buffer.from(xpubHex, 'hex');
  });

  _signWitnesses = async (witnesses) => {
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
    const constructedAddress = await this.constructAddressRequest.execute({ data });
    return constructedAddress.address;
  };

  ShelleyWitness = async (witness) => {
    const xpub = await this._deriveXpub(witness.path);
    const publicKey = xpub.slice(0, 32);
    const signature = Buffer.from(witness.witnessSignatureHex, 'hex');
    return ShelleyTxWitnessShelley(publicKey, signature);
  };

  // Ledger - Shelley only
  @action _signTransactionLedger = async (walletId: string, devicePath?: string) => {
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });
    const { coinSelection } = this.txSignRequest;
    const { inputs, outputs, certificates, fee: flatFee } = coinSelection;
    console.debug('>>> _signTransactionLedger: ', { walletId, inputs, outputs, certificates, flatFee });

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
      const accountAddress = await this._getRewardAccountAddress(walletId, certificate.rewardAccountPath)
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

    try {
      const signedTransaction = await signTransactionLedgerChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: ttl.toString(),
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
        certificates: certificatesData,
        withdrawals,
        metadataHashHex,
        devicePath
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

      const signedWitnesses = await this._signWitnesses(signedTransaction.witnesses);
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

  initiateTransaction = async (params: { walletId: string }) => {
    runInAction('HardwareWalletsStore:: Initiate Transaction', () => {
      this.isTransactionInitiated = true;
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { walletId } = params;
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );

    console.debug('>>>> initiateTransaction: ', {walletId, hardwareWalletConnectionData});

    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');

    const { disconnected, device, id } = hardwareWalletConnectionData;
    const { deviceType } = device;



    let devicePath = hardwareWalletConnectionData.device.path;
    if (disconnected) {
      console.debug('>>>> initiateTransaction - DISCONNECTED');
      // Wait for connection to be established and continue to signing process
      try {
        const transportDevice = await this.establishHardwareWalletConnection();
        console.debug('>>>> initiateTransaction - transportDevice: ', transportDevice);
        if (!transportDevice) {
          throw new Error('Signing device not recognized!');
        }
        devicePath = transportDevice.path;
      } catch (e) {
        console.debug('>>>> initiateTransaction - DISCONNECTED - ERROR: ', e);
        runInAction('HardwareWalletsStore:: Initiate transaction', () => {
          this.isTransactionInitiated = false;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        });
        throw e;
      }
    }

    runInAction('HardwareWalletsStore:: Set active device path for Transaction send', () => {
      this.activeDevicePath = devicePath;
    });

    // Add more cases / edge cases if needed
    if (deviceType === DeviceTypes.TREZOR) {
      this._signTransactionTrezor(id);
      runInAction('HardwareWalletsStore:: Initiate transaction', () => {
        this.isTransactionInitiated = false;
      });
    } else {
      console.debug('>>> getCardanoAdaApp - from  initiateTransaction');
      this.cardanoAdaAppPollingInterval = setInterval(
        (path, walletId) => this.getCardanoAdaApp({ path, isCheck: true, walletId }),
        CARDANO_ADA_APP_POLLING_INTERVAL,
        devicePath,
        walletId,
      );
      // this._signTransactionLedger(id);
    }
  };

  _resetTransaction = async (
    params: ?{
      cancelDeviceAction: boolean,
    }
  ) => {
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
    runInAction('HardwareWalletsStore:: reset Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.READY;
      this.txBody = null;
      this.activeDevicePath = null;
      this.unfinishedWalletTxSigning = null;
    });
  };

  changeLedgerStatus = async (params) => {
    console.debug('>>> CHANGE Ledger status');
    // Check is Cardano App started
    // try {
    //   const ca = await this.getCardanoAdaApp();
    //   console.debug('>>> CHANGE Ledger status - SUCCESS: ', ca);
    // } catch (e) {
    //   console.debug('>>> CHANGE Ledger status - error: ', e);
    // }
  }

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

    console.debug('>>> CHANGE status: ', params);

    // if (deviceType === DeviceTypes.LEDGER) {
    //   this.changeLedgerStatus(params);
    //   return;
    // }


    if (error) {
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

    if (deviceType === DeviceTypes.TREZOR && !this.isTrezorBridgeInstalled) {
      runInAction(
        'HardwareWalletsStore:: Mark Trezor Bridge as installed',
        () => {
          this.isTrezorBridgeInstalled = true;
        }
      );
    }

    // If Hardware Wallet exist and connected with device, set current connection status to LC
    const activeHardwareWalletId = get(
      this.stores,
      ['wallets', 'active', 'id'],
      null
    );

    const { hardwareWalletsConnectionData, hardwareWalletDevices } = this;
    // Add new recognized device - not connected to software wallet
    // Or update recognized device while paired with existing software wallet
    const recognizedPairedHardwareWallet = find(
      hardwareWalletsConnectionData,
      (connection) => {
        if (deviceType === DeviceTypes.LEDGER) {
          // Ledger doesn't have ID in this phase
          return path && connection.device.path === path
        } else {
          return deviceId && connection.device.deviceId === deviceId;
        }
      }
    );

    console.debug('>>> recognizedPairedHardwareWallet: ', recognizedPairedHardwareWallet);

    if (disconnected) {
      // @TODO - This logic should be changed once we allow multiple hardware wallets
      if (deviceType === DeviceTypes.LEDGER) {
      //   this.stopCardanoAdaAppFetchPoller();
//
      //   // Get pending device by path
        const recognizedPendingDevice = find(
          hardwareWalletDevices,
          (hardwareWalletDevice) => hardwareWalletDevice.isPending && hardwareWalletDevice.path === path
        );
        console.debug('>>> recognized PENDING device: ', recognizedPendingDevice);
        // Delete disconnected pending device from LC
        if (recognizedPendingDevice) {
          this._unsetHardwareWalletDevice({ deviceId: recognizedPendingDevice.id });
        }
      }
    }

    // Check if plugged-in device match one already established connections
    // If deviceId recognized - get software wallet and set to connected.
    //   - update possible device changes
    // Else deviceId not recognized - for now just break process

    if (recognizedPairedHardwareWallet) {
      // Change software wallet status paired with device
      console.debug('>>> SET 3 _setHardwareWalletLocalData: ', recognizedPairedHardwareWallet);
      await this._setHardwareWalletLocalData({
        walletId: recognizedPairedHardwareWallet.id,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          disconnected,
          path,
          state: disconnected
            ? HwDeviceStatuses.CONNECTING
            : HwDeviceStatuses.READY,
        },
      });
    }

    // Uncomment this to prevent PENDING devices
    // if (deviceId && deviceType !== DeviceTypes.LEDGER) {
    //   await this._setHardwareWalletDevice({
    //     deviceId: deviceId || new Date().valueOf(), // device ID or timestamp (for pending devices without ID) - ledger Only
    //     data: {
    //       deviceType,
    //       deviceModel,
    //       deviceName,
    //       path,
    //       paired: recognizedPairedHardwareWallet
    //         ? recognizedPairedHardwareWallet.id
    //         : null, // device paired with software wallet
    //       disconnected, // device physically disconnected
    //       isPending: !deviceId,
    //     },
    //   });
    // }

    let pendingId
    if (deviceId || (deviceType === DeviceTypes.LEDGER && (!disconnected || recognizedPairedHardwareWallet))) {
      pendingId = deviceType === DeviceTypes.LEDGER  && recognizedPairedHardwareWallet ? recognizedPairedHardwareWallet.device.deviceId : new Date().valueOf();
      console.debug('>>> SET DEVICE DATA: ', {deviceId, pendingId,  disconnected, recognizedPairedHardwareWallet})
      if (deviceId || pendingId) {
        await this._setHardwareWalletDevice({
          deviceId: deviceId || pendingId, // device ID or timestamp (for pending devices without ID) - ledger Only
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


    this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();

    if (this.isListeningForDevice && !disconnected) {
      runInAction(
        'HardwareWalletsStore:: remove device listener',
        () => {
          this.isListeningForDevice = false;
        }
      );

      if (deviceType === DeviceTypes.LEDGER) {
        // To Force Ledger with manual parameters because ID is not available and device not stored to LC
        // this.establishHardwareWalletConnection({ path, type: DeviceTypes.LEDGER});
        console.debug('>>> CALL establish connection with pendingID: ', pendingId);
        this.establishHardwareWalletConnection({ pendingId });
      } else {
        this.establishHardwareWalletConnection();
      }
    }

    if (this.unfinishedWalletTxSigning && !disconnected) {
      console.debug('>>> Reinitialize TX signing: ', this.unfinishedWalletTxSigning);
      this.initiateTransaction({ walletId: this.unfinishedWalletTxSigning });
      runInAction(
        'HardwareWalletsStore:: remove unfinished signing listener',
        () => {
          this.unfinishedWalletTxSigning = null;
        }
      );
    }


    // Check each Ledger listener on app start or on device change event
    if (!disconnected && deviceType === DeviceTypes.LEDGER) {
      console.debug('>>> Device changed from MAIN - check is Cardano APP opened: ', path);
      // this.getCardanoAdaApp({ path, isCheck: true });
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
    console.debug('>>> CALL SET - _setHardwareWalletLocalData METHOD: ', walletId)
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
    console.debug('>>> _unsetHardwareWalletLocalData');
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

  _resetHardwareWallets = async () => {
    if (global.environment.isDev) {
      await Promise.all(
        this.stores.wallets.all.map(async wallet => {
          if (wallet.isHardwareWallet) {
            return await this.stores.wallets._deleteWallet({ walletId: wallet.id, isLegacy: wallet.isLegacy  })
          }
        })
      );
      await this.unsetHardwareWalletDevicesAllRequest.execute();
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
      await this._refreshHardwareWalletsLocalData();
      await this._refreshHardwareWalletDevices();
    }
  }

  stopCardanoAdaAppFetchPoller = () => {
    console.debug('>>> STOP POLLER: ', this.cardanoAdaAppPollingInterval);
    if (this.cardanoAdaAppPollingInterval) {
      clearInterval(this.cardanoAdaAppPollingInterval);
    }
  };
}
