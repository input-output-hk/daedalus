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
  recieverAddress: ?string,
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
  unsetHardwareWalletLocalDataAllRequest: Request<HardwareWalletLocalData> = new Request(
    this.api.localStorage.unsetHardwareWalletLocalDataAll
  );
  @observable hwDeviceStatus: HwDeviceStatus = HwDeviceStatuses.CONNECTING;
  @observable
  extendedPublicKey: ?HardwareWalletExtendedPublicKeyResponse = null;
  @observable txSignRequest: TxSignRequestTypes = {};
  @observable transportDevice: ?TransportDevice = null;
  @observable txBody: ?string = null;
  @observable isConnectionInitialized: boolean = false;
  @observable isTransactionPending: boolean = false;
  @observable isTrezorBridgeInstalled: boolean = false;
  @observable isTransactionInitiated: boolean = false;

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

    // Set all logical HW into disconnected state
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
        (!isTrezor && (device.deviceType !== DeviceTypes.LEDGER)) ||
        (isTrezor && (device.deviceType !== DeviceTypes.TREZOR || (device.deviceType === DeviceTypes.TREZOR && !this.isTrezorBridgeInstalled)))
      ) {
        return;
      }

      try {
        console.debug('>>> TRIGGER: ', device);
        await getHardwareWalletTransportChannel.request({
          devicePath: device.path,
          isTrezor: device.deviceType === DeviceTypes.TREZOR,
        });
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

    console.debug('>>> SEND MONEY: ', params);

    if (!wallet) {
      throw new Error('Active wallet required before sending.');
    }

    this.setTransactionPendingState(true);

    try {
      const transaction = await this.sendMoneyRequest.execute({
        signedTransactionBlob: this.txBody,
      });
      console.debug('>>> SEND MONEY - DONE: ', transaction);

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
      console.debug('>>> COIN SELECTION: ', coinSelection);
      runInAction('HardwareWalletsStore:: set coin selections', () => {
        this.txSignRequest = {
          recieverAddress: address,
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
          recieverAddress: null,
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

  @action establishHardwareWalletConnection = async (isTrezor?: boolean) => {
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
      runInAction('HardwareWalletsStore:: set connecting helper flag', () => {
        this.isConnectionInitialized = true;
      });

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

      // Special case! When device not connected and TxSend is initialized we need to hook this process before and return void just to indicate that txSend process can continue
      let transportDevice;
      if (
        recognizedPairedHardwareWallet &&
        recognizedPairedHardwareWallet.disconnected
      ) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 1 ', {recognizedPairedHardwareWallet});
        // Device exist and but not physically connected - initiate listener to first recognized device and let txSend to reject wrong device
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: null, // Path can change because device is disconnected now.
          isTrezor:
            recognizedPairedHardwareWallet.deviceType === DeviceTypes.TREZOR,
        });
        return transportDevice; // Special Case when we are waiting on response return to continue with tx signing
      }

      if (this.isTransactionInitiated && !recognizedPairedHardwareWallet) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 2 - from txSend ', {recognizedPairedHardwareWallet});
        return null; // Special Case when we are waiting on response return to continue with tx signing
      }

      // Cases for wallet create / restore
      if (lastUnpairedConnectedDevice) {
        console.debug('>>> establishHardwareWalletConnection:: CASE 3 ', {lastUnpairedConnectedDevice});
        // Start listeners for specific (last connected) device
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: lastUnpairedConnectedDevice.path,
          isTrezor:
            lastUnpairedConnectedDevice.deviceType === DeviceTypes.TREZOR,
        });
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
          console.debug('>>> START cardano app poller');
          // Start poller to recognize if Cardano App is launched on device
          this.cardanoAdaAppPollingInterval = setInterval(
            this.getCardanoAdaApp,
            CARDANO_ADA_APP_POLLING_INTERVAL
          );
        }
      } else {
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
  @action getCardanoAdaApp = async () => {
    console.debug('>>> getCardanoAdaApp');
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request();
      console.debug('>>> cardanoAdaApp: ', cardanoAdaApp);
      // @TODO - keep poller until app recognized or process exited
      this.stopCardanoAdaAppFetchPoller();
      if (cardanoAdaApp) {
        console.debug('>>> cardanoAdaApp - Set device: ', cardanoAdaApp);
        this._setHardwareWalletDevice({
          deviceId: cardanoAdaApp.deviceId,
          data: {},
        });
        // While Cardano ADA app recognized on Ledger, proceed to exporting public key
        await this._getExtendedPublicKey();
      }
    } catch (error) {
      console.debug('>>> cardanoAdaApp - error: ', error);
      throw error;
    }
  };

  @action _getExtendedPublicKey = async () => {
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
        devicePath: path,
      });

      const deviceId = extendedPublicKey.deviceId || transportDevice.deviceId;

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
        this._setHardwareWalletLocalData({
          walletId: recognizedWallet.id,
          data: {
            disconnected: false,
            device: {
              ...transportDevice,
              deviceId,
            },
          },
        });

        // @TODO - guard against Ledger - logic needs to be changed and deviceId (serial) applied
        console.debug('>> SET device from key export: ', {transportDevice, extendedPublicKey, deviceId});
        if (deviceId || extendedPublicKey.deviceId) {
          this._setHardwareWalletDevice({
            deviceId: extendedPublicKey.deviceId || deviceId,
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path,
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
          ...transportDevice,
          deviceId,
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
    console.debug('>>> _deriveXpub: ', absDerivationPath);
    const response = await getExtendedPublicKeyChannel.request({
      // path: absDerivationPath, // @TODO - check
      path: "1852'/1815'/0'",
      isTrezor: false,
      devicePath: null,
    });
    console.debug('>>> response: ', response);
    const xpubHex = `${response.publicKeyHex}${response.chainCodeHex}`;
    return Buffer.from(xpubHex, 'hex');
  });

  processArray = async (array) => {
    const _shelleyWitnesses = [];
    for (const item of array) {
      console.debug('>>> TOMO - get ShelleyWitness: ', {item, path: item.path});
      const aa = await this.ShelleyWitness(item);
      console.debug('>>> SGN: ', aa);
      _shelleyWitnesses.push(aa);
    }
    console.log('Done!: ', _shelleyWitnesses);
    return _shelleyWitnesses;
  }


  ShelleyWitness = async (witness) => {
    console.debug('>>> TOMO - ShelleyWitness: ', witness);
    const xpub = await this._deriveXpub(witness.path); // @TODO - error is here
    console.debug('>>> TOMO - DERIVATION xpub: ', xpub);
    console.debug('>>> xPub: ', xpub);
    const publicKey = xpub.slice(0, 32); // TODO: export from addresses
    console.debug('>>> publicKey: ', publicKey);
    const signature = Buffer.from(witness.witnessSignatureHex, 'hex');
    console.debug('>>> signature: ', signature);
    return ShelleyTxWitnessShelley(publicKey, signature);
  }

  // Ledger - Shelley only
  @action _signTransactionLedger = async () => {
    console.debug('>>> _signTransactionLedger');
    runInAction('HardwareWalletsStore:: set Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
    });
    // @TODO - once data with CHANGE applied remove recieverAddress from store
    const { coinSelection, recieverAddress } = this.txSignRequest;

    const { inputs, outputs, certificates } = coinSelection;

    let totalInputs = 0;
    const unsignedTxInputs = [];
    const inputsData = map(inputs, (input) => {
      totalInputs += input.amount.quantity;
      const shelleyTxInput = ShelleyTxInputFromUtxo(input);
      unsignedTxInputs.push(shelleyTxInput);
      return prepareLedgerInput(input);
    });

    let totalOutputs = 0;
    const unsignedTxOutputs = [];
    const outputsData = map(outputs, (output) => {
      totalOutputs += output.amount.quantity;
      const isChange = output.address !== recieverAddress;
      const addressIndex = this.stores.addresses.getAddressIndex(
        output.address
      );
      const shelleyTxOutput = ShelleyTxOutput(output);
      unsignedTxOutputs.push(shelleyTxOutput);
      return prepareLedgerOutput(output);
    });

    const certificatesData = map(certificates, (certificate) =>
      prepareLedgerCertificate(certificate)
    );

    const fee = totalInputs - totalOutputs;
    // const fee = 185660 // @TODO - revert
    const ttl = 150000000;
    // const ttl = 13602704 // @TODO - revert
    // const certificates = [];
    const withdrawals = [];
    const metadataHashHex = null;

    console.debug('>>> DATA to SIGN: ', {
      inputs: inputsData,
      outputs: outputsData,
      fee: fee.toString(),
      ttl: ttl.toString(),
      networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
      protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
      certificates: certificatesData,
      withdrawals,
      metadataHashHex,
    });

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
      });

      // Prepare unsigned transaction structure for serialzation
      // @TODO - remove
      const unsignedTx = prepareTxAux({
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates: certificatesData, // @TODO - prepare unsigned certs
        withdrawals,
      });

      console.debug('>>> signedTransaction: ', signedTransaction);


      // @TODO - remove
    // const wt = [
    //   {
    //     path: [2147485500, 2147485463, 2147483648, 0, 39],
    //     witnessSignatureHex: "3db1a6c4b515a94a20af204792430e63848e3b3fa01ba6e454b3dc642fa19a05ddb245e382327d2b3759464742f7bc939210cdc1e26885c4d9506c00f4619804",
    //   },
    //   {
    //     path: [2147485500, 2147485463, 2147483648, 0, 0],
    //     witnessSignatureHex: "e5864abcdb7d39c8dd281bc8dd02b687f5f53f57975daade930476e7989acf253f9c7479cf643078e1b211abbad2f70cd124a2c4168ffce7a276c37164503107",
    //   }
    // ]
    const testWitnessesSigned = await this.processArray(signedTransaction.witnesses);
    console.debug('>>> _shelleyWitnesses TEST: ', testWitnessesSigned);

    console.debug('>>> DONE');

    const txWitnesses_test = new Map();
    if (testWitnessesSigned.length > 0) {
      txWitnesses_test.set(0, testWitnessesSigned);
    }
    console.debug('>>> START ENCODING: ', { unsignedTx, testWitnessesSigned, txWitnesses_test });


      const txBody = await prepareBody(unsignedTx, txWitnesses_test);
      console.debug('>>>> FINAL: ', txBody);
      // return;
      console.debug('>>> SIGNED TX: ', signedTransaction);

      // Sign and prepare witnesses
      const _shelleyWitnesses = [];

      // const _shelleyWitnesses = map(
      //   signedTransaction.witnesses,
      //   async (witness) => {
      //     let xpub;
      //     try {
      //       xpub = await this._deriveXpub(witness.path); // @TODO - error is here
      //       console.debug('>>> DERIVATION xpub: ', xpub);
      //     } catch (e) {
      //       console.debug('>>> DERIVATION ERROR: ', e);
      //       throw e;
      //     }
      //     console.debug('>>> xPub: ', xpub);
      //     const publicKey = xpub.slice(0, 32); // TODO: export from addresses
      //     console.debug('>>> publicKey: ', publicKey);
      //     const signature = Buffer.from(witness.witnessSignatureHex, 'hex');
      //     console.debug('>>> signature: ', signature);
      //     return ShelleyTxWitnessShelley(publicKey, signature);
      //   }
      // );

      // signedTransaction.witnesses.forEach((witness) => {
      //   console.debug('>>> TOMO - get ShelleyWitness: ', witness);
      //   _shelleyWitnesses.push(this.ShelleyWitness(witness))
      // })
//
//
//
      // const shelleyWitnesses = await Promise.all(_shelleyWitnesses);
      // console.debug('>>> _shelleyWitnesses: ', shelleyWitnesses);
      // const txWitnesses = new Map();
      // if (shelleyWitnesses.length > 0) {
      //   txWitnesses.set(0, shelleyWitnesses);
      // }
//
      // console.debug('>>> txWitnesses: ', txWitnesses);
//
      // // Prepare unsigned transaction structure for serialzation
      // const txAux = prepareTxAux({
      //   txInputs: unsignedTxInputs,
      //   txOutputs: unsignedTxOutputs,
      //   fee,
      //   ttl,
      //   certificates,
      //   withdrawals,
      // });
//
      // console.debug('>>> txAux: ', txAux);

      // Prepare serialized transaction with unsigned data and signed witnesses
      // const txBody = await prepareBody(txAux, txWitnesses);
      console.debug('>>> txBody: ', txBody);
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.txBody = txBody;
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
    });
    const { walletId } = params;
    const hardwareWalletConnectionData = get(
      this.hardwareWalletsConnectionData,
      walletId
    );

    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');

    const { disconnected, device, id } = hardwareWalletConnectionData;
    const { deviceType } = device;

    if (disconnected) {
      // Wait for connection to be established and continue to signing process
      try {
        const transportDevice = await this.establishHardwareWalletConnection();
        if (!transportDevice) {
          throw new Error('Signing device not recognized!');
        }
      } catch (e) {
        runInAction('HardwareWalletsStore:: Initiate transaction', () => {
          this.isTransactionInitiated = false;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        });
        throw e;
      }
    }

    // Add more cases / edge cases if needed
    if (deviceType === DeviceTypes.TREZOR) {
      this._signTransactionTrezor(id);
    } else {
      this._signTransactionLedger();
    }
    runInAction('HardwareWalletsStore:: Initiate transaction', () => {
      this.isTransactionInitiated = false;
    });
  };

  _resetTransaction = async (
    params: ?{
      cancelDeviceAction: boolean,
    }
  ) => {
    runInAction('HardwareWalletsStore:: Reset initiated transaction', () => {
      this.isTransactionInitiated = false;
    });
    const cancelDeviceAction = get(params, 'cancelDeviceAction', false);
    if (cancelDeviceAction) {
      resetTrezorActionChannel.request();
    }
    this.sendMoneyRequest.reset();
    this.selectCoinsRequest.reset();
    runInAction('HardwareWalletsStore:: reset Transaction verifying', () => {
      this.hwDeviceStatus = HwDeviceStatuses.READY;
      this.txBody = null;
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

    const { hardwareWalletsConnectionData } = this;
    // Add new recognized device - not connected to software wallet
    // Or update recognized device while paired with existing software wallet
    const recognizedPairedHardwareWallet = find(
      hardwareWalletsConnectionData,
      (connection) => connection.device.deviceId === deviceId
    );

    if (disconnected) {
      // @TODO - This logic should be changed once we allow multiple hardware wallets
      if (deviceType === DeviceTypes.LEDGER) {
        this.stopCardanoAdaAppFetchPoller();
      }
      // If device is disconnected, connection is initialized and is waiting for device to be plugged-in
      // If device is connected, connection is initialized and device confirmation steps are in progress
      if (
        activeHardwareWalletId &&
        hardwareWalletsConnectionData[activeHardwareWalletId].device
          .deviceId === deviceId
      ) {
        // Try to re-establish connection only if disconnected device is connected with currently active software wallet
        // this.establishHardwareWalletConnection();
      }
    }
    // Check if plugged-in device match one already established connections
    // If deviceId recognized - get software wallet and set to connected.
    //   - update possible device changes
    // Else deviceId not recognized - for now just break process

    if (recognizedPairedHardwareWallet) {
      // Change software wallet status paired with device
      await this._setHardwareWalletLocalData({
        walletId: recognizedPairedHardwareWallet.id,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          disconnected,
          state: disconnected
            ? HwDeviceStatuses.CONNECTING
            : HwDeviceStatuses.READY,
        },
      });
    }

    // if (deviceId) {
      // @TODO - Ledger can not return id initially - uncomment
      await this._setHardwareWalletDevice({
        deviceId: deviceId || new Date().valueOf(), // device ID or timestamp (for pending devices without ID)
        data: {
          deviceType,
          deviceModel,
          deviceName,
          path,
          paired: recognizedPairedHardwareWallet
            ? recognizedPairedHardwareWallet.id
            : null, // device paired with software wallet
          disconnected, // device physically disconnected
          isPending: !deviceId,
        },
      });
   // }
    this._refreshHardwareWalletsLocalData();
    await this._refreshHardwareWalletDevices();

    if (this.isListeningForDevice) {
      runInAction(
        'HardwareWalletsStore:: remove device listener',
        () => {
          this.isListeningForDevice = false;
        }
      );
      this.establishHardwareWalletConnection();
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
    if (this.cardanoAdaAppPollingInterval) {
      clearInterval(this.cardanoAdaAppPollingInterval);
    }
    this.stores.wallets.createHardwareWalletRequest.reset();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    this.transportDevice = {};
    this.isConnectionInitialized = false;
    this.isListeningForDevice = false;
  };

  @action _refreshHardwareWalletsLocalData = () => {
    this.hardwareWalletsLocalDataRequest.execute();
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
    await this.setHardwareWalletLocalDataRequest.execute(walletId, data);
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };

  _unsetHardwareWalletLocalData = async ({
    walletId,
  }: {
    walletId: string,
  }) => {
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

  stopCardanoAdaAppFetchPoller = () => {
    if (this.cardanoAdaAppPollingInterval) {
      clearInterval(this.cardanoAdaAppPollingInterval);
    }
  };
}
