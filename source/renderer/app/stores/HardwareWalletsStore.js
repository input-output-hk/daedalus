// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { get, map } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  getHardwareWalletTransportChannel,
  getExtendedPublicKeyChannel,
  getCardanoAdaAppChannel,
  getHardwareWalletConnectionChannel,
  deriveAddressChannel,
  showAddressChannel,
  signTransactionChannel,
} from '../ipc/getHardwareWalletChannel';
import { HwDeviceStatuses } from '../domains/Wallet';
import {
  thDataHexGenerator,
  encodeSignedTransaction,
} from '../utils/transaction';
import { getRawWalletId, WalletIdPrefixes } from '../api/utils';
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

export type EncodeSignedTransactionRequest = {|
  txDataHex: string,
  witnesses: Array<SignedTransactionWitnesses>,
|};

export type TransportDevice = {
  id: DeviceModel,
  productName: string,
  deviceType: DeviceType,
};

export type DeviceModel = 'nanoS' | 'nanoX' | 'trezor';
export type DeviceType = 'ledger' | 'trezor';

export const DeviceModels: {
  LEDGER_NANO_S: DeviceModel,
  LEDGER_NANO_X: DeviceModel,
  TREZOR: DeviceModel,
} = {
  LEDGER_NANO_S: 'nanoS',
  LEDGER_NANO_X: 'nanoX',
  TREZOR: 'trezor',
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
  @observable txDataHex: ?string = null;
  @observable txSignRequest: TxSignRequestTypes = {};
  @observable extendedPublicKey: ?string = null;
  @observable derivedAddress: ?string = null;
  @observable signedTransaction: ?string = null;
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
      const txDataHex = thDataHexGenerator(coinSelection);
      runInAction('HardwareWalletsStore:: set coin selections', () => {
        this.txSignRequest = {
          txDataHex,
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
      const transportDevice = await getHardwareWalletTransportChannel.request();
      const deviceType = this._deviceType(transportDevice.id);
      runInAction('HardwareWalletsStore:: set HW device connected', () => {
        this.transportDevice = {
          ...transportDevice,
          deviceType,
        };
      });
      // Start poller to recognize if Cardano App is launched on device
      this.cardanoAdaAppPollingInterval = setInterval(
        this.getCardanoAdaApp,
        CARDANO_ADA_APP_POLLING_INTERVAL
      );
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
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { activeHardwareWallet } = this.stores.wallets;
    const path = [
      utils.HARDENED + 44,
      utils.HARDENED + 1815,
      utils.HARDENED + 0,
    ];
    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path,
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
      throw e;
    }
  };

  @action _deriveAddress = async (path: string) => {
    try {
      const derivedAddress = await deriveAddressChannel.request({
        derivationPath: path,
      });
      runInAction('HardwareWalletsStore:: set derived address', () => {
        this.derivedAddress = derivedAddress;
      });
    } catch (error) {
      throw error;
    }
  };

  @action _showAddress = async (path: string) => {
    try {
      await showAddressChannel.request({
        derivationPath: path,
      });
    } catch (error) {
      throw error;
    }
  };

  @action _signTransaction = async () => {
    const { coinSelection, txDataHex, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    const inputsData = map(inputs, input => {
      return {
        txDataHex,
        outputIndex: input.index,
        path: utils.str_to_path("44'/1815'/0'/0/0"),
      };
    });

    const outputsData = map(outputs, output => {
      if (output.address !== recieverAddress) {
        // ChangeAddress
        return {
          amountStr: output.amount.quantity.toString(),
          path: utils.str_to_path("44'/1815'/0'/1/0"), // 4th param can be (0 or 1), 1 will say that address is change address
        };
      }
      return {
        amountStr: output.amount.quantity.toString(),
        address58: output.address,
      };
    });

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
      });
      // get xPub from signed transaction witnesses by exporting extended public key
      const witnesses = await Promise.all(
        signedTransaction.witnesses.map(async witness => {
          const xPub = await this._getXpub(witness.path);
          return {
            xpub: xPub,
            signature: witness.witnessSignatureHex,
          };
        })
      );
      const signedTransactionData = {
        txDataHex,
        witnesses,
      };
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.signedTransaction = encodeSignedTransaction(signedTransactionData);
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
  }

  _resetTxSignRequestData = () => {
    this.selectCoinsRequest.reset();
    this.txSignRequest = {};
  };

  _getXpub = async (path: BIP32Path) => {
    try {
      return await getExtendedPublicKeyChannel.request({ path });
    } catch (e) {
      if (e.statusCode === 28169) {
        runInAction('HardwareWalletsStore:: exporting key aborted', () => {
          this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
        });
      }
      throw e;
    }
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
      case DeviceModels.TREZOR:
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
