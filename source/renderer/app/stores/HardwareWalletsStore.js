// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
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
  derivationScheme,
  CachedDeriveXpubFactory,
  ShelleyTxWitnessShelley,
  ShelleyTxInputFromUtxo,
  ShelleyTxOutput,
  ShelleyFee,
  ShelleyTtl,
  ShelleyTxAux,
} from '../utils/shelleyLedger';
import { prepareTrezorInput, prepareTrezorOutput } from '../utils/shelleyTrezor';

import type { HwDeviceStatus } from '../domains/Wallet';
import type { CoinSelectionsResponse } from '../api/transactions/types';
import type {
  HardwareWalletLocalData,
  HardwareWalletsLocalData,
  SetHardwareWalletLocalDataRequestType,
} from '../api/utils/localStorage';
import type { BIP32Path } from '../../../common/ipc/api';

export type TxSignRequestTypes = {
  txDataHex: string,
  recieverAddress: string,
  coinSelection: CoinSelectionsResponse,
};
export type ExtendedPublicKey = {
  publicKeyHex: string,
  chainCodeHex: string,
};

export type Witness = {|
  path: BIP32Path,
  witnessSignatureHex: string,
|};

export type SignedTransactionResponse = {|
  txDataHex: string,
  witnesses: Array<Witness>,
|};

export type SignedTransactionWitnesses = {
  signature: string,
  xpub: ExtendedPublicKey,
};

export type TransportDevice = {
  deviceID: ?string,
  deviceType: DeviceType,
  deviceModel: DeviceModel,
  deviceName: string,
};

export type LedgerModel = 'nanoS' | 'nanoX';
export type TrezorModel = '1' | 'T';
export type DeviceType = 'ledger' | 'trezor';

export const DeviceModels: {
  LEDGER_NANO_S: LedgerModel,
  LEDGER_NANO_X: LedgerModel,
  TREZOR: TrezorModel,
} = {
  LEDGER_NANO_S: 'nanoS',
  LEDGER_NANO_X: 'nanoX',
  TREZOR_ONE: '1',
  TREZOR_T: 'T',
};

export const DeviceTypes: {
  LEDGER: DeviceType,
  TREZOR: DeviceType,
} = {
  LEDGER: 'ledger',
  TREZOR: 'trezor',
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
  @observable extendedPublicKey: ?string = null;
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
    try {
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
          inputs: coinSelection.inputs,
          outputs: coinSelection.outputs,
          coinSelection,
        };
      });
    } catch (error) {
      throw error;
    }
  };

  establishHardwareWalletConnection = async () => {
    runInAction('HardwareWalletsStore:: set HW device connecting', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    try {
      const transportDevice = await getHardwareWalletTransportChannel.request({ isTrezor: false });
      const deviceType = transportDevice.deviceType;
      runInAction('HardwareWalletsStore:: set HW device connected', () => {
        this.transportDevice = {
          ...transportDevice,
          deviceType,
        };
      });
      if (deviceType === DeviceTypes.TREZOR) {
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
    // @TODO - This logic should be changed once we allow multiple hardware wallets
    this.stopCardanoAdaAppFetchPoller();
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    // If device is disconnected, connection is initialized and is waiting for device to be plugged-in
    // If device is connected, connection is initialized and device confirmation steps are in progress
    this.establishHardwareWalletConnection();
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

  @action getCardanoAdaApp = async () => {
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      await getCardanoAdaAppChannel.request();
      this.stopCardanoAdaAppFetchPoller();
      await this._getExtendedPublicKey();
    } catch (error) {
      throw error;
    }
  };

  @action _getExtendedPublicKey = async () => {
    const isTrezor = false; // @TODO - add active device recognizing logic
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { activeHardwareWallet } = this.stores.wallets;
    const path = cardano.str_to_path("1852'/1815'/0'");

    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path,
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
          // If keys match, change device state to connected
          this._setHardwareWalletLocalData({
            walletId: activeHardwareWallet.id,
            data: {
              disconnected: false,
            },
          });
          return;
        }
        throw new Error(
          `Wrong Hardware Wallet device supplied to "${activeHardwareWallet.name}" wallet`
        );
      }
      // Wallet not set, create new one with default name
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: DEFAULT_HW_NAME,
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
      runInAction('HardwareWalletsStore:: exporting key aborted', () => {
        this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
      });
      throw e;
    }
  };

  // Shelley only
  @action _signTransactionTrezor = async (isTest = true) => {
    const { coinSelection, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    let totalInputs = 0;
    map(inputs, input => {
      const addressIndex = this.stores.addresses.getAddressIndex(input.address);
      totalInputs = totalInputs + input.amount.quantity;
      return prepareTrezorInput(input, addressIndex);
    });

    let totalOutputs = 0;
    const outputsData = map(outputs, output => {
      const addressIndex = this.stores.addresses.getAddressIndex(output.address);
      const isChange = output.address !== recieverAddress;
      totalOutputs = totalOutputs + output.amount.quantity;
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

  // Shelley only
  @action _signTransactionLedger = async () => {
    const { coinSelection, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    let unsignedTxInputs = [];
    let unsignedTxOutputs = [];

    let totalInputs = 0;
    const inputsData = map(inputs, input => {
      const addressIndex = this.stores.addresses.getAddressIndex(input.address);
      totalInputs = totalInputs + input.amount.quantity;
      const unsignedTxInput = {
        address: input.address,
        coins: input.amount.quantity,
        outputNo: input.index,
        txHash: input.id,
      };
      unsignedTxInputs.push(unsignedTxInput);
      return prepareLedgerInput(input, addressIndex);
    });

    let totalOutputs = 0;
    const outputsData = map(outputs, output => {
      totalOutputs = totalOutputs + output.amount.quantity;
      const isChange = output.address !== recieverAddress;
      const addressIndex = this.stores.addresses.getAddressIndex(output.address);
      const unsignedTxOutput = {
        address: output.address,
        coins: output.amount.quantity,
        isChange,
        spendingPath: isChange ? [2147485500, 2147485463, 2147483648, 0, addressIndex] : null,
        stakingPath: isChange ? [2147485500, 2147485463, 2147483648, 2, 0] : null,
      };
      unsignedTxOutputs.push(unsignedTxOutput);
      return prepareLedgerOutput(output, addressIndex, isChange);
    });

    const fee = totalInputs - totalOutputs;
    const ttl = 15000000;

    // @TODO - isolate method for txAux creation
    const unsignedTx = {
      inputs: unsignedTxInputs,
      outputs: unsignedTxOutputs,
      fee,
      ttl,
      certs: [],
      withdrawals: undefined,
    }

    try {
      const signedTransaction = await signTransactionChannel.request({
        networkId: HW_SHELLEY_CONFIG.NETWORK.MAINNET.networkId,
        protocolMagic: HW_SHELLEY_CONFIG.NETWORK.MAINNET.protocolMagic,
        inputs: inputsData,
        outputs: outputsData,
        feeStr: fee.toString(),
        ttlStr: ttl.toString(),
        certificates: [],
        withdrawals: [],
        metadataHashHex: null,
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
      // Prepare unsigned transaction strusture for serialzation
      const txAux = prepareTxAux(unsignedTx);
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
    return this.hardwareWalletsLocalDataRequest.result
      ? this.hardwareWalletsLocalDataRequest.result
      : {};
  };

  _deriveXpub = CachedDeriveXpubFactory(derivationScheme, async (absDerivationPath) => {
    const response = await getExtendedPublicKeyChannel.request({
      path: absDerivationPath,
      isTrezor: false,
    });
    const xpubHex = response.publicKeyHex + response.chainCodeHex
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
        type = DeviceTypes.TREZOR_T;
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
