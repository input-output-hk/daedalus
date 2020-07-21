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
      console.debug('>>> COIN SELECTION :', coinSelection);
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
      // const deviceType = this._deviceType(transportDevice.deviceModel);
      const deviceType = transportDevice.deviceType;
      runInAction('HardwareWalletsStore:: set HW device connected', () => {
        this.transportDevice = {
          ...transportDevice,
          deviceType,
        };
      });
      console.debug('>>> deviceType: ', deviceType);
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
    const path = [
      utils.HARDENED + 44,
      utils.HARDENED + 1815,
      utils.HARDENED + 0,
    ];
    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path,
        isTrezor,
      });
      console.debug('>>> extendedPublicKey: ', extendedPublicKey);
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
      console.debug('>>> CONTINUE');
      // Wallet not set, create new one with default name
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: DEFAULT_HW_NAME,
        extendedPublicKey,
        device: this.transportDevice,
      });

      console.debug('>>> HardwareWalletsStore:: set wallet READY');
      runInAction('HardwareWalletsStore:: set wallet READY', () => {
        this.extendedPublicKey = extendedPublicKey;
        this.hwDeviceStatus = HwDeviceStatuses.READY;
      });
    } catch (e) {
      console.debug('>>> extendedPublicKey - ERROR: ', e);
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
        path: "m/44'/1815'/0'/0/1",
        prev_hash: txDataHex,
        prev_index: 1,
        type: 1
      };
    });

    const outputsData = map(outputs, output => {
      if (output.address !== recieverAddress) {
        // ChangeAddress
        return {
          path: "m/44'/1815'/0'/0/1",
          amount: output.amount.quantity.toString(),
          // path: utils.str_to_path("44'/1815'/0'/1/0"), // 4th param can be (0 or 1), 1 will say that address is change address
        };
      }
      return {
        address: output.address,
        amount: output.amount.quantity.toString(),
      };
    });

    // Example
    // const data = {
    //   inputs: [
    //     {
    //         path: "m/44'/1815'/0'/0/1",
    //         prev_hash: "1af8fa0b754ff99253d983894e63a2b09cbb56c833ba18c3384210163f63dcfc",
    //         prev_index: 0,
    //         type: 0
    //     }
    //   ],
    //   outputs: [
    //       {
    //           address: "Ae2tdPwUPEZCanmBz5g2GEwFqKTKpNJcGYPKfDxoNeKZ8bRHr8366kseiK2",
    //           amount: "3003112"
    //       },
    //       {
    //           path: "m/44'/1815'/0'/0/1",
    //           amount: "7120787"
    //       }
    //   ],
    //   transactions: [
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //       "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
    //   ],
    //   protocol_magic: 764824073
    // }

    console.debug('>>> Sign START: ', { inputsData, outputsData });
    console.debug('>>> TEST: ', inputs[0].id);
    try {
      const signedTransaction = await signTransactionChannel.request({
        //inputs: inputsData,
        // outputs: outputsData,
      inputs: [
         {
             path: "m/44'/1815'/0'/0/1",
             prev_hash: "1af8fa0b754ff99253d983894e63a2b09cbb56c833ba18c3384210163f63dcfc",
             prev_index: 0,
             type: 0
         }
       ],
       outputs: [
           {
               address: "DdzFFzCqrhshMav9kXdWuSYDe71zbN625sGXYAeYbUzjzctQB1NDRXrWa8EwbtsGQA4FKQ48H39zsADgdCRJ5g9QZ691Uzr1WXYpteZw",
               amount: "100000"
           },
           {
               path: "m/44'/1815'/0'/0/1",
               amount: "729106"
           }
       ],
        transactions: [
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
            "839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0",
        ],
        protocolMagic: 764824073,
        isTrezor: true,
      });
      console.debug('>>> Sign SUCCESS: ', signedTransaction);
      // get xPub from signed transaction witnesses by exporting extended public key
      // const witnesses = await Promise.all(
      //   signedTransaction.witnesses.map(async witness => {
      //     const xPub = await this._getXpub(witness.path);
      //     return {
      //       xpub: xPub,
      //       signature: witness.witnessSignatureHex,
      //     };
      //   })
      // );
      // const signedTransactionData = {
      //   txDataHex,
      //   witnesses,
      // };
      // runInAction('HardwareWalletsStore:: set Transaction verified', () => {
      //   this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
      //   this.signedTransaction = encodeSignedTransaction(signedTransactionData);
      // });
    } catch (error) {
      // runInAction(
      //   'HardwareWalletsStore:: set Transaction verifying failed',
      //   () => {
      //     this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
      //   }
      // );
      console.debug('>>> SIGN error: ', error);
      throw error;
    }
  };

  @action _signTransaction22 = async () => {
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
