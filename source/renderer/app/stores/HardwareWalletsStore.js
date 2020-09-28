// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { get, map } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { HwDeviceStatuses } from '../domains/Wallet';
import { getRawWalletId, WalletIdPrefixes } from '../api/utils';
import { HW_SHELLEY_CONFIG } from '../config/hardwareWalletsConfig';
import {
  getHardwareWalletTransportChannel,
  getExtendedPublicKeyChannel,
  getCardanoAdaAppChannel,
  getHardwareWalletConnectionChannel,
  signTransactionChannel,
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
import { prepareTrezorInput, prepareTrezorOutput } from '../utils/shelleyTrezor';

import type { HwDeviceStatus } from '../domains/Wallet';
import type { CoinSelectionsResponse } from '../api/transactions/types';
import type {
  HardwareWalletLocalData,
  HardwareWalletsLocalData,
  SetHardwareWalletLocalDataRequestType,
} from '../api/utils/localStorage';
import type {
  DeviceModels,
  DeviceTypes,
  TransportDevice,
  HardwareWalletExtendedPublicKeyResponse,
} from '../../../common/types/hardware-wallets.types';

export type TxSignRequestTypes = {
  recieverAddress: string,
  coinSelection: CoinSelectionsResponse,
};

const CARDANO_ADA_APP_POLLING_INTERVAL = 1000;
const DEFAULT_HW_NAME = 'Hardware Wallet';

export default class HardwareWalletsStore extends Store {
  @observable selectCoinsRequest: Request<CoinSelectionsResponse> = new Request(
    this.api.ada.selectCoins
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
  @observable hwDeviceStatus: HwDeviceStatus = HwDeviceStatuses.CONNECTING;
  @observable extendedPublicKey: ?HardwareWalletExtendedPublicKeyResponse = null;
  @observable txSignRequest: TxSignRequestTypes = {};
  @observable transportDevice: TransportDevice = {};

  cardanoAdaAppPollingInterval: ?IntervalID = null;

  setup() {
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.selectCoins.listen(this._selectCoins);
    hardwareWalletsActions.refreshHardwareWalletsLocalData.listen(
      this._refreshHardwareWalletsLocalData
    );
    hardwareWalletsActions.setHardwareWalletLocalData.listen(
      this._setHardwareWalletLocalData
    );
    hardwareWalletsActions.unsetHardwareWalletLocalData.listen(
      this._unsetHardwareWalletLocalData
    );
    getHardwareWalletConnectionChannel.onReceive(
      this._changeHardwareWalletConnectionStatus
    );
    this.hardwareWalletsLocalDataRequest.execute();
  }

  @action _selectCoins = async (params: {
    walletId: string,
    address: string,
    amount: string,
  }) => {
    const { walletId, address, amount } = params;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet)
      throw new Error('Active wallet required before coin selections.');
    const rawWalletId = getRawWalletId(
      walletId,
      WalletIdPrefixes.HARDWARE_WALLET
    );
    const coinSelection = await this.selectCoinsRequest.execute({
      walletId: rawWalletId,
      address,
      amount,
    });
    runInAction('HardwareWalletsStore:: set coin selections', () => {
      this.txSignRequest = {
        recieverAddress: address,
        coinSelection,
      };
    });
  };

  @action establishHardwareWalletConnection = async () => {
    runInAction('HardwareWalletsStore:: set HW device CONNECTING', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    try {
      const transportDevice = await getHardwareWalletTransportChannel.request({ isTrezor: false });
      const { deviceType } = transportDevice;
      runInAction('HardwareWalletsStore:: set HW device CONNECTED', () => {
        this.transportDevice = {
          ...transportDevice,
          deviceType,
        };
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
    } catch (e) {
      if (e.statusCode === 28177) {
        throw new Error('Device is locked');
      }
      if (e.id === 'TransportLocked') {
        throw new Error('Transport Failure');
      }
      throw e;
    }
  };

  @action _changeHardwareWalletConnectionStatus = async (params: {
    disconnected: boolean,
  }) => {
    const { disconnected } = params;
    if (disconnected) {
      // @TODO - This logic should be changed once we allow multiple hardware wallets
      this.stopCardanoAdaAppFetchPoller();
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
      // If device is disconnected, connection is initialized and is waiting for device to be plugged-in
      // If device is connected, connection is initialized and device confirmation steps are in progress
      this.establishHardwareWalletConnection();
    }
    // If Hardware Wallet exist and connected with device, set current connection status to LC
    const activeHardwareWalletId = get(
      this.stores,
      ['wallets', 'activeHardwareWallet', 'id'],
      null
    );
    const firstHardwareWalletId = get(
      this.stores,
      ['wallets', 'allHardwareWallets', 0, 'id'],
      null
    );
    const hardwareWalletId = activeHardwareWalletId || firstHardwareWalletId;
    if (!hardwareWalletId) return;

    await this._setHardwareWalletLocalData({
      walletId: hardwareWalletId,
      data: {
        disconnected: params.disconnected,
      },
    });
  };

  // Ledger method only
  @action getCardanoAdaApp = async () => {
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaApp = await getCardanoAdaAppChannel.request();
      // @TODO - keep poller until app recognized or process exited
      this.stopCardanoAdaAppFetchPoller();
      if (cardanoAdaApp.result) {
        // While Cardano ADA app recognized on Ledger, proceed to exporting public key
        await this._getExtendedPublicKey();
      }
    } catch (error) {
      throw error;
    }
  };

  @action _getExtendedPublicKey = async () => {
    const isTrezor = false; // @TODO - add active device recognizing logic
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { activeHardwareWallet } = this.stores.wallets;
    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path: "1852'/1815'/0'", // Shelley 1852 indicator for account '0'
        isTrezor,
      });
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
          // Active wallet exists and match device - set to CONNECTED state
          this._setHardwareWalletLocalData({
            walletId: activeHardwareWallet.id,
            data: {
              disconnected: false,
            },
          });
          return;
        }
        // Active wallet exists but wrong device supplied
        throw new Error(
          `Wrong Hardware Wallet device supplied to "${activeHardwareWallet.name}" wallet`
        );
      }
      // Active wallet not exist, create new one with default name
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: DEFAULT_HW_NAME, // @TODO - pick name from device or pass default
        extendedPublicKey,
        device: this.transportDevice,
      });

      runInAction('HardwareWalletsStore:: set wallet READY', () => {
        this.extendedPublicKey = extendedPublicKey;
        this.hwDeviceStatus = HwDeviceStatuses.READY;
      });
    } catch (e) {
      if (e.statusCode === 28169) {
        runInAction('HardwareWalletsStore:: exporting key aborted', () => {
          this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
        });
      }
      // @TODO - Decide what error we need to show if exporting error occure and exporting not aborted by user
      // e.g. wallet creation failed, device busy...
      runInAction('HardwareWalletsStore:: exporting key aborted', () => {
        this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
      });
      throw e;
    }
  };

  // Trezor - Shelley only
  @action _signTransactionTrezor = async () => {
    const { coinSelection, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    let totalInputs = 0;
    const inputsData = map(inputs, input => {
      const addressIndex = this.stores.addresses.getAddressIndex(input.address);
      totalInputs += input.amount.quantity;
      return prepareTrezorInput(input, addressIndex);
    });

    let totalOutputs = 0;
    const outputsData = map(outputs, output => {
      const addressIndex = this.stores.addresses.getAddressIndex(output.address);
      const isChange = output.address !== recieverAddress;
      totalOutputs += output.amount.quantity;
      return prepareTrezorOutput(output, addressIndex, isChange);
    })

    const fee = totalInputs - totalOutputs;

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: '15000000',
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
        isTrezor: true,
      });

      runInAction(
        'HardwareWalletsStore:: set signed transaction data',
        () => {
          this.txBody = signedTransaction.serializedTx;
        }
      );
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
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        fee: fee.toString(),
        ttl: ttl.toString(),
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
        certificates,
        withdrawals,
        metadataHashHex,
        isTrezor: false,
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

  @action resetInitializedConnection = () => {
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.extendedPublicKey = null;
    this.transportDevice = {};
  };

  @action _refreshHardwareWalletsLocalData = () => {
    this.hardwareWalletsLocalDataRequest.execute();
  };

  @computed get hardwareWalletsConnectionData(): HardwareWalletsLocalData {
    return this.hardwareWalletsLocalDataRequest.result;
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

  _deviceType = (deviceModel: DeviceModel) => {
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
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };

  stopCardanoAdaAppFetchPoller = () => {
    if (this.cardanoAdaAppPollingInterval)
      clearInterval(this.cardanoAdaAppPollingInterval);
  };
}
