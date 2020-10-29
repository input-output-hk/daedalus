// @flow
/* eslint-disable no-console */
import { observable, action, runInAction, computed } from 'mobx';
import { get, map, find, findLast } from 'lodash';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { HwDeviceStatuses } from '../domains/Wallet';
import { HW_SHELLEY_CONFIG, MINIMAL_TREZOR_FIRMWARE_VERSION, MINIMAL_LEDGER_FIRMWARE_VERSION } from '../config/hardwareWalletsConfig';
import {
  getHardwareWalletTransportChannel,
  getExtendedPublicKeyChannel,
  getCardanoAdaAppChannel,
  getHardwareWalletConnectionChannel,
  signTransactionLedgerChannel,
  signTransactionTrezorChannel,
  handleInitTrezorConnectChannel,
} from '../ipc/getHardwareWalletChannel';
import {
  prepareLedgerInput,
  prepareLedgerOutput,
  prepareTxAux,
  prepareBody,
  CachedDeriveXpubFactory,
  ShelleyTxWitnessShelley,
  ShelleyTxInputFromUtxo,
  ShelleyTxOutput,
} from '../utils/shelleyLedger';
import { prepareTrezorInput, prepareTrezorOutput, prepareCertificate } from '../utils/shelleyTrezor';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../common/types/hardware-wallets.types';
import { formattedAmountToLovelace } from '../utils/formatters';
import { TransactionStates } from '../domains/WalletTransaction';

import type { HwDeviceStatus } from '../domains/Wallet';
import type { CoinSelectionsResponse } from '../api/transactions/types';
import type {
  HardwareWalletLocalData,
  HardwareWalletsLocalData,
  SetHardwareWalletLocalDataRequestType,
} from '../api/utils/localStorage';
import type {
  TransportDevice,
  LedgerModel,
  TrezorModel,
  HardwareWalletExtendedPublicKeyResponse,
} from '../../../common/types/hardware-wallets.types';

export type TxSignRequestTypes = {
  recieverAddress: string,
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

const CARDANO_ADA_APP_POLLING_INTERVAL = 3000;
const DEFAULT_HW_NAME = 'Hardware Wallet';

export default class HardwareWalletsStore extends Store {
  @observable selectCoinsRequest: Request<CoinSelectionsResponse> = new Request(
    this.api.ada.selectCoins
  );
  @observable sendMoneyRequest: Request<WalletTransaction> = new Request(
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
  @observable extendedPublicKey: ?HardwareWalletExtendedPublicKeyResponse = null;
  @observable txSignRequest: TxSignRequestTypes = {};
  @observable transportDevice: ?TransportDevice = null;
  @observable txBody: ?string = null;
  @observable isConnectionInitialized: boolean = false;
  @observable isTransactionPending: boolean = false;
  @observable isTrezorBridgeInstalled: boolean = false;

  cardanoAdaAppPollingInterval: ?IntervalID = null;
  checkTransactionTimeInterval: ?IntervalID = null;

  setup() {
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.selectCoins.listen(this._selectCoins);
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
    this.hardwareWalletsLocalDataRequest.execute();
    this.hardwareWalletDevicesRequest.execute();
  }

  initTrezor = async () => {
    console.debug('>>>> INIT TREZOR');
    await handleInitTrezorConnectChannel.request();
    console.debug('>>>> INIT TREZOR - DONE');
    await this.getAvailableDevices();
    console.debug('>>>> INIT TREZOR - getAvailableDevices');
  }

  getAvailableDevices = async () => {
    await this.hardwareWalletsLocalDataRequest.execute();
    await this.hardwareWalletDevicesRequest.execute();
    console.debug('>>> SETUP:: getAvailableDevices ', {
      hardwareWalletsConnectionData: this.hardwareWalletsConnectionData,
      hardwareWalletDevices: this.hardwareWalletDevices,
    })
    // Set all logical HW into disconnected state
    map(this.hardwareWalletsConnectionData, async (connectedWallet) => {
      console.debug('>> SET connectedWallet to disconnected state: ', connectedWallet);
      await this._setHardwareWalletLocalData({
        walletId: connectedWallet.id,
        data: {
          disconnected: true,
        },
      });
    });

    // Initiate Device Check for each stored device
    map(this.hardwareWalletDevices, async device => {
      console.debug('>> Check Device: ', device);
      // Prevent device check if device is TREZOR and bridge not installed
      if (device.deviceType === DeviceTypes.TREZOR && !this.isTrezorBridgeInstalled) {
        return;
      }

      try {
        await getHardwareWalletTransportChannel.request({
          devicePath: device.path,
          isTrezor: device.deviceType === DeviceTypes.TREZOR,
        });
      } catch (e) {
        console.debug('>> Check Device - ERROR: ', e);
      }
    });

    this._refreshHardwareWalletsLocalData();
    this._refreshHardwareWalletDevices();
    console.debug('>>>> getAvailableDevices - DONE');
  };

  _sendMoney = async (params?: { isDelegationTransaction: boolean }) => {
    const { isDelegationTransaction } = params;
    const wallet = this.stores.wallets.active;

    this.setTransactionPendingState(true);

    if (!isDelegationTransaction && !wallet) throw new Error('Active wallet required before sending.');
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

  @action resetStakePoolTransactionChecker = (walletId) => {
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

  @action setTransactionPendingState = (isTransactionPending) => {
    runInAction('HardwareWalletsStore:: set transaction state', () => {
      this.isTransactionPending = isTransactionPending
    });
  }

  // @TODO - move to Transactions store once all logic fit and hardware wallets listed in general wallets list
  selectCoins = async (params: {
    walletId: string,
    address: string,
    amount: string,
    poolId?: string, // Only for delegation
  }) => {
    const { walletId, address, amount, poolId, delegationAction } = params;
    console.debug('>>> selectCoins: ', params);
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coins selections.');

    try {
      const coinSelection = await this.selectCoinsRequest.execute({
        walletId,
        address,
        amount,
        poolId,
        walletBalance: wallet.amount,
        availableBalance: wallet.availableAmount,
        isLegacy: wallet.isLegacy,
        delegationAction,
      });
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

  @action establishHardwareWalletConnection = async () => {
    console.debug('>>> establishHardwareWalletConnection');

    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    const { hardwareWalletDevices } = this;
    const activeWallet = this.stores.wallets.active;
    try {
      runInAction('HardwareWalletsStore:: set connecting helper flag', () => {
        this.isConnectionInitialized = true;
      });

      // Check if active wallet exist - this means that hw exist but we need to check if relevant device connected to it
      let recognizedPairedHardwareWallet;
      if (activeWallet) {
        console.debug('>>> Active HW recognized: ', activeWallet.id);
        // Check if device connected to wallet
        recognizedPairedHardwareWallet = find(hardwareWalletDevices, recognizedDevice => (
          recognizedDevice.paired === activeWallet.id
        ));
        console.debug('>>> Active HW paired device: ', recognizedPairedHardwareWallet);
      }

      const lastUnpairedConnectedDevice = findLast(this.hardwareWalletDevices, hardwareWalletDevice => !hardwareWalletDevice.paired && !hardwareWalletDevice.disconnected);

      // Special case! When device not connected and TxSend is initialized we need to hook this process before and return void just to indicate that txSend process can continue
      let transportDevice;
      if (recognizedPairedHardwareWallet && recognizedPairedHardwareWallet.disconnected) {
        console.debug('>>> DEVICE PAIRED but not PHYSICALLY CONNECTED')
        // Device exist and but not physically connected - initiate listener to first recognized device and let txSend to reject wrong device
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: null, // Path can change because device is disconnected now
          isTrezor: recognizedPairedHardwareWallet.deviceType === DeviceTypes.TREZOR,
        });
        console.debug('>>> DEVICE PAIRED but not PHYSICALLY CONNECTED - transportDevice: ', transportDevice);
        return;
      }

      // Cases for wallet create / restore
      if (lastUnpairedConnectedDevice) {
        console.debug('>>> UNPAIRED: ', lastUnpairedConnectedDevice);
        // Start listeners for specific (last connected) device
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: lastUnpairedConnectedDevice.path,
          isTrezor: lastUnpairedConnectedDevice.deviceType === DeviceTypes.TREZOR,
        });
      } else {
        console.debug('>>> establishHardwareWalletConnection - connect with NEW device');
        // Start listeners for both device types
        // Start Trezor listener
        transportDevice = await getHardwareWalletTransportChannel.request({
          devicePath: null,
          isTrezor: true,
        });
        // @TODO - improve logic for Ledger (no devices -> connect device when connecting state initialized)
        if (!transportDevice) {
          // Start Ledger listener
          // transportDevice = await getHardwareWalletTransportChannel.request({
          //   devicePath: null,
          //   isTrezor: false,
          // });
          console.debug('>>> transportDevice 2: ', transportDevice);
        }
      }


      console.debug('>>> transportDevice FETCHED - proceed with wallet creation ', transportDevice);

      if (transportDevice) {
        const { deviceType, firmwareVersion } = transportDevice;
        // Check Device model
        if ((deviceType === DeviceTypes.TREZOR && !DeviceModels.TREZOR_T) || (deviceType === DeviceTypes.LEDGER && (!DeviceModels.LEDGER_NANO_S && !DeviceModels.LEDGER_NANO_X))) {
          runInAction('HardwareWalletsStore:: set HW device CONNECTING FAILED - device not supported', () => {
            this.hwDeviceStatus = HwDeviceStatuses.UNSUPPORTED_DEVICE;
          });
          throw new Error('Device not Supported!', {
            errorMessage: 'Device not supported',
            code: 'Device_Not_Supported',
          });
        }

        // Check Firmware version
        const minFirmwareVersion = deviceType === DeviceTypes.TREZOR ? MINIMAL_TREZOR_FIRMWARE_VERSION : MINIMAL_LEDGER_FIRMWARE_VERSION;
        const isFirmwareVersionValid = semver.gte(firmwareVersion, minFirmwareVersion);
        if (!isFirmwareVersionValid) {
          runInAction('HardwareWalletsStore:: set HW device CONNECTING FAILED - wrong firmware', () => {
            this.hwDeviceStatus = HwDeviceStatuses.WRONG_FIRMWARE;
          });
          throw new Error(`Firmware must be ${minFirmwareVersion} or greater!`, {
            errorMessage: 'Wrong Firmware',
            code: 'Wrong_Firmware',
          });
        }

        // All Checks pass - mark device as connected (set transport device for this session)
        runInAction('HardwareWalletsStore:: set HW device CONNECTED', () => {
          this.transportDevice = transportDevice;
        });

        if (deviceType === DeviceTypes.TREZOR) {
          // Jump to exporting public key
          await this._getExtendedPublicKey();
        } else {
          // Start poller to recognize if Cardano App is launched on device
          this.cardanoAdaAppPollingInterval = setInterval(
            this.getCardanoAdaApp,
            CARDANO_ADA_APP_POLLING_INTERVAL
          );
        }
      } else {
        throw new Error('Device not found')
      }
    } catch (e) {
      console.debug('>>> DEVICE CONNECT FAILED: ', e);
      if (e.statusCode === 28177) {
        throw new Error('Device is locked');
      }
      if (e.id === 'TransportLocked') {
        throw new Error('Transport Failure');
      }
      if (e.code === 'Transport_Missing' && !this.isTrezorBridgeInstalled) {
        runInAction('HardwareWalletsStore:: set HW device CONNECTING FAILED', () => {
          this.hwDeviceStatus = HwDeviceStatuses.TREZOR_BRIDGE_FAILURE;
        });
        throw new Error('Trezor Bridge not installed!');
      }
      throw e;
    }
  };

  // Ledger method only
  @action getCardanoAdaApp = async () => {
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    console.debug('>>> getCardanoAdaApp <<<');
    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request();
      // @TODO - keep poller until app recognized or process exited
      this.stopCardanoAdaAppFetchPoller();
      if (cardanoAdaApp) {
        console.debug('>>> getCardanoAdaApp - Initiate Ledger device with ID:', cardanoAdaApp.deviceId);
        this._setHardwareWalletDevice({
          deviceId: cardanoAdaApp.deviceId,
          data: {},
        });
        console.debug('>>> getCardanoAdaApp - stop poller <<<');
        // While Cardano ADA app recognized on Ledger, proceed to exporting public key
        await this._getExtendedPublicKey();
      }
    } catch (error) {
      throw error;
    }
  };

  @action _getExtendedPublicKey = async () => {
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { transportDevice } = this;
    console.debug('>>>> _getExtendedPublicKey <<<<: ', transportDevice);
    if (!transportDevice)
      throw new Error('Can not export extended public key: Device not recognized!');
    console.debug('>>> _getExtendedPublicKey::Continue: ', transportDevice);
    const { deviceType, path, deviceName, deviceId, deviceModel } = transportDevice;
    const isTrezor = deviceType === DeviceTypes.TREZOR;

    const { active: activeHardwareWallet } = this.stores.wallets;
    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path: "1852'/1815'/0'", // Shelley 1852 ADA 1815 indicator for account '0'
        isTrezor,
        devicePath: path,
      });
      console.debug('>>> _getExtendedPublicKey::Exported: ', extendedPublicKey);

      // Check if public key matches active hardware wallet public key
      if (activeHardwareWallet && this.hardwareWalletsConnectionData) {
        const activeHardwareWalletConnection = this
          .hardwareWalletsConnectionData[activeHardwareWallet.id];
        const activeHardwareWalletConnectionKeys = get(
          activeHardwareWalletConnection,
          'extendedPublicKey',
          {}
        );
        if (
          activeHardwareWalletConnectionKeys.publicKeyHex ===
          extendedPublicKey.publicKeyHex
        ) {
          console.debug('>>> WALLET ALREADY PAIRED - SET TO connected!');
          // Active wallet exists and match device - set to CONNECTED state
          this._setHardwareWalletLocalData({
            walletId: activeHardwareWallet.id,
            data: {
              disconnected: false,
              device: transportDevice,
            },
          });

          this._setHardwareWalletDevice({
            deviceId,
            data: {
              deviceType,
              deviceModel,
              deviceName,
              path,
              paired: activeHardwareWallet.id, // device paired with software wallet
              disconnected: false, // device physically disconnected
            },
          });
          return;
        }
        // Active wallet exists but wrong device supplied
        console.debug('>>> WALLET ALREADY PAIRED - KEYS NOT MATCH!');
        throw new Error(
          `Wrong Hardware Wallet device supplied to "${activeHardwareWallet.name}" wallet`
        );
      }
      // Active wallet not exist, create new one with default name
      console.debug('>>> WALLET NOT PAIRED - CREATE NEW ONE!');
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: deviceName || DEFAULT_HW_NAME,
        extendedPublicKey,
        device: transportDevice,
      });

      runInAction('HardwareWalletsStore:: set wallet READY', () => {
        this.extendedPublicKey = extendedPublicKey;
        this.hwDeviceStatus = HwDeviceStatuses.READY;
        this.isConnectionInitialized = false;
      });

      this._refreshHardwareWalletsLocalData();
      this._refreshHardwareWalletDevices();
    } catch (e) {
      /**
       * ============  Exporting aborted  =============
       * e.statusCode === 28169 // Ledger
       * e.code === 'Failure_ActionCancelled' // Trezor
       */
      runInAction('HardwareWalletsStore:: Cannot export extended public key', () => {
        this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
      });
      // Pass other errors to caller (establishHardwareWalletConnection() in this case) and handle additional actions if needed
      throw e;
    }
  };

  // Trezor - Shelley only
  @action _signTransactionTrezor = async (walletId: string) => {
    const { coinSelection } = this.txSignRequest;
    runInAction(
      'HardwareWalletsStore:: set Transaction verifying',
      () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION;
      }
    );

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

    const inputsData = map(inputs, input => {
      return prepareTrezorInput(input);
    });

    const outputsData = map(outputs, output => {
      return prepareTrezorOutput(output);
    })

    const certificatesData = map(certificates, certificate => prepareCertificate(certificate));

    console.debug('>>> SIGN TRANSACTION: ', {
      inputs: inputsData,
      outputs: outputsData,
      fee: formattedAmountToLovelace(fee.toString()).toString(),
      ttl: '15000000',
      networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
      protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
      certificates: certificatesData,
    });
    try {
      const signedTransaction = await signTransactionTrezorChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: formattedAmountToLovelace(fee.toString()).toString(),
        ttl: '15000000',
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
        certificates: certificatesData,
      });

      console.debug('>>> signedTransaction: ', signedTransaction);

      if (!signedTransaction.success) {
        throw signedTransaction.payload;
      }

      runInAction(
        'HardwareWalletsStore:: transaction successfully signed',
        () => {
          this.txBody = signedTransaction.payload.serializedTx;
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        }
      );
    } catch (error) {
      console.debug('>>>> HERE: ', error);
      runInAction(
        'HardwareWalletsStore:: set Transaction verifying failed',
        () => {
          this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
        }
      );
      // @TODO - Maybe we should handle this case as separated message in tx dialog
      if (error.code === 'Device_CallInProgress') {
        throw new Error('Device is busy - reconnect device and try again')
      }
      throw error;
    }
  };

  // Ledger - Shelley only
  @action _signTransactionLedger = async () => {
    const { coinSelection, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    let totalInputs = 0;
    const unsignedTxInputs = [];
    const inputsData = map(inputs, input => {
      const addressIndex = this.stores.addresses.getAddressIndex(input.address);
      totalInputs += input.amount.quantity;
      const shelleyTxInput = ShelleyTxInputFromUtxo(input);
      unsignedTxInputs.push(shelleyTxInput);
      return prepareLedgerInput(input, addressIndex);
    });

    let totalOutputs = 0;
    const unsignedTxOutputs = [];
    const outputsData = map(outputs, output => {
      totalOutputs += output.amount.quantity;
      const isChange = output.address !== recieverAddress;
      const addressIndex = this.stores.addresses.getAddressIndex(output.address);
      const shelleyTxOutput = ShelleyTxOutput(output, addressIndex, isChange);
      unsignedTxOutputs.push(shelleyTxOutput);
      return prepareLedgerOutput(output, addressIndex, isChange);
    });

    const fee = totalInputs - totalOutputs;
    const ttl = 15000000;
    const certificates = [];
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
        certificates,
        withdrawals,
        metadataHashHex,
      });

      // Sign and prepare witnesses
      const _shelleyWitnesses = map(signedTransaction.witnesses, async (witness) => {
        const xpub = await this._deriveXpub(witness.path);
        const publicKey = xpub.slice(0, 32); // TODO: export from addresses
        const signature = Buffer.from(witness.witnessSignatureHex, 'hex')
        return ShelleyTxWitnessShelley(publicKey, signature);
      })
      const shelleyWitnesses = await Promise.all(_shelleyWitnesses);
      const txWitnesses = new Map();
      if (shelleyWitnesses.length > 0) {
        txWitnesses.set(0, shelleyWitnesses);
      }

      // Prepare unsigned transaction structure for serialzation
      const txAux = prepareTxAux({
        txInputs: unsignedTxInputs,
        txOutputs: unsignedTxOutputs,
        fee,
        ttl,
        certificates,
        withdrawals,
      });

      // Prepare serialized transaction with unsigned data and signed witnesses
      const txBody = await prepareBody(txAux, txWitnesses);
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.txBody = txBody
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
    walletId: string,
  }) => {
    console.debug('>>> INIT TX Send!: ', params);
    const { walletId } = params;
    const hardwareWalletConnectionData = get(this.hardwareWalletsConnectionData, walletId);

    // Guard against potential null value
    if (!hardwareWalletConnectionData)
      throw new Error('Wallet not paired or Device not connected');

    const { disconnected, device, id } = hardwareWalletConnectionData;
    const { deviceType } = device

    if (disconnected) {
      // Wait for connection to be established and continue to signing process
      console.debug('>>> INIT:: establish connection before INIT');
      await this.establishHardwareWalletConnection();
      console.debug('>>> INIT:: CONTINUE: ', deviceType);
    }

    // Add more cases / edge cases if needed
    if (deviceType === DeviceTypes.TREZOR) {
      console.debug('>>> INIT:: Sign Trezor');
      this._signTransactionTrezor(id);
    } else {
      console.debug('>>> INIT:: Sign Ledger');
      this._signTransactionLedger(id);
    }
  };

  _resetTransaction = async (params: ?{
    cancelDeviceAction: boolean;
  }) => {
    const { cancelDeviceAction } = params;
    console.debug('>>> CANCEL: ', cancelDeviceAction);
    if (cancelDeviceAction) {
      signTransactionTrezorChannel.request({
        reset: true,
      });
    }
    this.sendMoneyRequest.reset();
    runInAction(
      'HardwareWalletsStore:: reset Transaction verifying',
      () => {
        this.hwDeviceStatus = HwDeviceStatuses.READY;
        this.txBody = null;
      }
    );
  };

  @action _changeHardwareWalletConnectionStatus = async (params: {
    disconnected: boolean,
  }) => {
    const { disconnected, deviceType, deviceId, deviceModel, deviceName, path, error } = params;

    if (error) {
      console.debug('>>> TRANSPORT ERROR <<<: ', error);
      if (error.payload && error.payload && error.payload.code === 'ECONNREFUSED') {
        console.debug('>>> TRANSPORT - Mark Trezor Bridge as NOT Installed <<<');
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
      console.debug('>>> TRANSPORT - Mark Trezor Bridge as Installed <<<');
      runInAction(
        'HardwareWalletsStore:: Mark Trezor Bridge as installed',
        () => {
          this.isTrezorBridgeInstalled = true;
        }
      );
    }

    console.debug('>>> _changeHardwareWalletConnectionStatus: ', params);
    // If Hardware Wallet exist and connected with device, set current connection status to LC
    const activeHardwareWalletId = get(
      this.stores,
      ['wallets', 'active', 'id'],
      null
    );

    const { hardwareWalletsConnectionData } = this;
    // Add new recognized device - not connected to software wallet
    // Or update recognized device while paired with existing software wallet
    const recognizedPairedHardwareWallet = find(hardwareWalletsConnectionData, connection => (
      connection.device.deviceId === deviceId
    ))
    console.debug('>>> _changeHardwareWalletConnectionStatus:: DATA: ', {
      activeHardwareWalletId,
      hardwareWalletsConnectionData,
      recognizedPairedHardwareWallet,
      params
    });

    if (disconnected) {
      console.debug('>>>> DISCONNECT');
      // this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      // @TODO - This logic should be changed once we allow multiple hardware wallets
      if (deviceType !== DeviceTypes.TREZOR) {
        console.debug('>>>> DISCONNECT - stop Cardano app poller');
        this.stopCardanoAdaAppFetchPoller();
      }
      // If device is disconnected, connection is initialized and is waiting for device to be plugged-in
      // If device is connected, connection is initialized and device confirmation steps are in progress
      if (activeHardwareWalletId && hardwareWalletsConnectionData[activeHardwareWalletId].device.deviceId === deviceId) {
        // Try to re-establish connection only if disconnected device is connected with currently active software wallet
        console.debug('>>>> DISCONNECT - re-establish connection');
        // this.establishHardwareWalletConnection();
      }
    }
    console.debug('>>>> CONNECT: ', recognizedPairedHardwareWallet);
    // Check if plugged-in device match one already established connections
    // If deviceId recognized - get software wallet and set to connected.
    //   - update possible device changes
    // Else deviceId not recognized - for now just break process

    if (recognizedPairedHardwareWallet) {
      // Change software wallet status paired with device
      console.debug('>>> Connect device with existing software wallet');
      await this._setHardwareWalletLocalData({
        walletId: recognizedPairedHardwareWallet.id,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          disconnected,
          state: disconnected ? HwDeviceStatuses.CONNECTING : HwDeviceStatuses.READY,
        },
      });
    }
    console.debug('>>> Add new device or update - LC- not connected to software wallet: ', { recognizedPairedHardwareWallet });

    if (deviceId) {
      // @TODO - Ledger can not return id initially
      await this._setHardwareWalletDevice({
        deviceId,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          path,
          paired: recognizedPairedHardwareWallet ? recognizedPairedHardwareWallet.id : null, // device paired with software wallet
          disconnected, // device physically disconnected
        },
      });
    }

    this._refreshHardwareWalletsLocalData();
    this._refreshHardwareWalletDevices();
    console.debug('>>> LC WALLETS: ', this.hardwareWalletsLocalDataRequest.result);
  };

  @action resetInitializedConnection = () => {
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    this.transportDevice = {};
  };

  @action _refreshHardwareWalletsLocalData = () => {
    this.hardwareWalletsLocalDataRequest.execute();
  };

  @action _refreshHardwareWalletDevices = () => {
    this.hardwareWalletDevicesRequest.execute();
  };

  @computed get hardwareWalletsConnectionData(): HardwareWalletsLocalData {
    return this.hardwareWalletsLocalDataRequest.result;
  };

  @computed get hardwareWalletDevices(): HardwareWalletsLocalData {
    return this.hardwareWalletDevicesRequest.result;
  };

  _deriveXpub = CachedDeriveXpubFactory(async (absDerivationPath) => {
    const response = await getExtendedPublicKeyChannel.request({
      path: absDerivationPath,
      isTrezor: false,
    });
    const xpubHex = `${response.publicKeyHex}${response.chainCodeHex}`;
    return Buffer.from(xpubHex, 'hex');
  });

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

    const pairedDevice = find(this.hardwareWalletDevices, recognizedDevice => (
      recognizedDevice.paired === walletId
    ));

    // Unset device <-> wallet paired parameter
    if (pairedDevice) {
      this._setHardwareWalletDevice({
        deviceId: pairedDevice.id,
        data: {
          paired: null,
        }
      });
      this._refreshHardwareWalletDevices();
    }
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };

  _setHardwareWalletDevice = async ({
    deviceId,
    data,
  }: SetHardwareWalletLocalDataRequestType) => {
    await this.setHardwareWalletDeviceRequest.execute(deviceId, data);
    this._refreshHardwareWalletDevices();
  };

  _unsetHardwareWalletDevice = async ({
    deviceId,
  }: {
    deviceId?: ?string,
  }) => {
    if (deviceId) {
      await this.unsetHardwareWalletDeviceRequest.execute(deviceId);
    } else {
      await this.unsetHardwareWalletLocalDataAllRequest.execute();
    }
    this._refreshHardwareWalletDevices();
  };

  stopCardanoAdaAppFetchPoller = () => {
    if (this.cardanoAdaAppPollingInterval)
      clearInterval(this.cardanoAdaAppPollingInterval);
  };
}
