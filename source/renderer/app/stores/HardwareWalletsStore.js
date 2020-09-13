// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { get, map } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import BigNumber from 'bignumber.js';
// import { wasm } from '@emurgo/cardano-serialization-lib-nodejs';
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
  @observable txSendParams: Object = {};

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
    // console.debug('>>> WASM: ', wasm);
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
      console.debug('>>> COIN SELECTION :', {coinSelection, params});
      // const txDataHex = thDataHexGenerator(coinSelection);
      runInAction('HardwareWalletsStore:: set coin selections', () => {
        this.txSignRequest = {
          // txDataHex,
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
    console.debug('>>> establishHardwareWalletConnection');
    runInAction('HardwareWalletsStore:: set HW device connecting', () => {
      this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    });
    try {
      // const transportDevice = await getHardwareWalletTransportChannel.request()
      const transportDevice = await getHardwareWalletTransportChannel.request({ isTrezor: true });
      console.debug('>>> establishHardwareWalletConnection - transportDevice: ', transportDevice);
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
    const isTrezor = true; // @TODO - add active device recognizing logic
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;
    const { activeHardwareWallet } = this.stores.wallets;
    // const path = [
    //   utils.HARDENED + 44,
    //   utils.HARDENED + 1815,
    //   utils.HARDENED + 0,
    // ];
    console.debug('>> UTILS: ', {utils, cardano});
    const path = cardano.str_to_path("1852'/1815'/0'");

    console.debug('>>>> _getExtendedPublicKey: ', path)

    try {
      const extendedPublicKey = await getExtendedPublicKeyChannel.request({
        path,
        isTrezor,
      });
      console.debug('>>> extendedPublicKey: ', {
        extendedPublicKey,
        activeHardwareWallet,
        hardwareWalletsConnectionData: this.hardwareWalletsConnectionData
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
        console.debug('>>> activeHardwareWalletConnectionKeys: ', activeHardwareWalletConnectionKeys);
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


  prepareInput = (input, addressToAbsPathMapper) => {
    const data = {
      // path: addressToAbsPathMapper(input.address),
      path: "m/1852'/1815'/0'/0/0",
      prev_hash: input.id,
      prev_index: input.index,
    }

    return data
  }

  prepareOutput = (output, addressToAbsPathMapper) => {
    if (output.isChange) {
      return {
        amount: output.amount.quantity,
        addressParameters: {
          addressType: 0, // TODO: 0 for base address
          path: output.spendingPath,
          stakingPath: output.stakingPath,
        },
      }
    } else {
      return {
        address: output.address,
        amount: output.amount.quantity.toString(),
      }
    }
  }




  @action _signTransactionTrezor = async (isTest = true) => {
    const { coinSelection, txDataHex, recieverAddress } = this.txSignRequest;
    // const { inputs, outputs } = coinSelection;


    // Example data from `https://github.com/trezor/connect/blob/cardano-shelley/tests/__fixtures__/cardanoSignTransaction.js`
    /* {
      description: 'signMainnetBaseHashAddress',
      params: {
          inputs: [SAMPLE_INPUTS['shelley_input']],
          outputs: [
              SAMPLE_OUTPUTS['simple_shelley_output'],
              SAMPLE_OUTPUTS['staking_key_hash_output'],
          ],
          fee: FEE,
          ttl: TTL,
          protocolMagic: PROTOCOL_MAGICS['mainnet'],
          networkId: NETWORK_IDS['mainnet'],
      },
      result: {
          hash: 'd1610bb89bece22ed3158738bc1fbb31c6af0685053e2993361e3380f49afad9',
          serializedTx: '83a400818258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b700018282583901eb0baa5e570cffbe2934db29df0b6a3d7c0430ee65d4c3a7ab2fefb91bc428e4720702ebd5dab4fb175324c192dc9bb76cc5da956e3c8dff018258390180f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa32c728d3861e164cab28cb8f006448139c8f1740ffb8e7aa9e5232dc1a006ca79302182a030aa100818258205d010cf16fdeff40955633d6c565f3844a288a24967cf6b76acbeb271b4f13c15840622f22d03bc9651ddc5eb2f5dc709ac4240a64d2b78c70355dd62106543c407d56e8134c4df7884ba67c8a1b5c706fc021df5c4d0ff37385c30572e73c727d00f6',
      },
    }, */

    let inputsData;
    let outputsData;
    if (isTest) {
      inputsData = [{
        path: "m/1852'/1815'/0'/0/0",
        prev_hash: '3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7',
        prev_index: 0,
      }]
      outputsData = [{
        address: 'addr1q84sh2j72ux0l03fxndjnhctdg7hcppsaejafsa84vh7lwgmcs5wgus8qt4atk45lvt4xfxpjtwfhdmvchdf2m3u3hlsd5tq5r',
        amount: '1',
      },
      //{
      //  addressParameters: {
      //    addressType: 0,
      //    path: "m/1852'/1815'/0'/0/0",
      //    stakingKeyHash: '32c728d3861e164cab28cb8f006448139c8f1740ffb8e7aa9e5232dc',
      //  },
      //  amount: '7120787',
      //}
      ];
    } else {
      const _inputs = [];
      for (const input of coinSelection.inputs) {
        const data = this.prepareInput(input)
        _inputs.push(data)
      }

      const _outputs = []
      for (const output of coinSelection.outputs) {
        // @TODO - change address SKIPPED, we need to enable
        if (output.address === recieverAddress) {
          const data = this.prepareOutput(output)
          _outputs.push(data)
        }
      }

      console.debug('>>>> CONSTRUCTOED: ', {
        _inputs,
        _outputs,
      })

      inputsData = _inputs;
      outputsData = _outputs;
      // return;
    }

    console.debug('>>> Sign START: ', { inputsData, outputsData });

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        // fee: '42',
        fee: '172189',
        // ttl: '7200',
        ttl: '7200',
        protocolMagic: 764824073, // ProtocolMagics.MAINNET,
        networkId: 0x01, // NetworkIds.MAINNET,
        isTrezor: true,
      });
      console.debug('>>> DONE - signedTransaction: ', signedTransaction);
      runInAction(
        'HardwareWalletsStore:: set signed transaction data',
        () => {
          this.txSendParams = {
            signedTransaction: signedTransaction.signedTransaction.payload.serializedTx,
            blob1: signedTransaction.buffer1,
            blob2: signedTransaction.buffer2,
          };
        }
      );
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







  @action _signTransaction11 = async () => {
    const { coinSelection, txDataHex, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;

    const inputsData = map(inputs, input => {
      return {
        path: "m/44'/1815'/0'/0/1",
        prev_hash: txDataHex,
        prev_index: 1,
        type: 1,
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
            prev_hash:
              '1af8fa0b754ff99253d983894e63a2b09cbb56c833ba18c3384210163f63dcfc',
            prev_index: 0,
            type: 0,
          },
        ],
        outputs: [
          {
            address:
              'DdzFFzCqrhshMav9kXdWuSYDe71zbN625sGXYAeYbUzjzctQB1NDRXrWa8EwbtsGQA4FKQ48H39zsADgdCRJ5g9QZ691Uzr1WXYpteZw',
            amount: '100000',
          },
          {
            path: "m/44'/1815'/0'/0/1",
            amount: '729106',
          },
        ],
        transactions: [
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
          '839f8200d818582482582008abb575fac4c39d5bf80683f7f0c37e48f4e3d96e37d1f6611919a7241b456600ff9f8282d818582183581cda4da43db3fca93695e71dab839e72271204d28b9d964d306b8800a8a0001a7a6916a51a00305becffa0',
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

  // For Shelley
  @action _signTransaction = async () => {
    const { coinSelection, txDataHex, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;


    console.debug('>>> SIGN (prepare) - ', {
      coinSelection, txDataHex, recieverAddress, inputs, outputs, utils,
    })

    // const inputs = {
    //   utxoByron: {
    //     txHashHex: "1af8fa0b754ff99253d983894e63a2b09cbb56c833ba18c3384210163f63dcfc",
    //     outputIndex: 0,
    //     path: str_to_path("44'/1815'/0'/0/0")
    //   },
    //   utxoShelley: {
    //     txHashHex: "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7",
    //     outputIndex: 0,
    //     path: str_to_path("1852'/1815'/0'/0/0"),
    //   }
    // };

    // const outputs = {
    //   externalByronMainnet: {
    //     amountStr: "3003112",
    //     addressHex: utils.buf_to_hex(utils.base58_decode(
    //       "Ae2tdPwUPEZCanmBz5g2GEwFqKTKpNJcGYPKfDxoNeKZ8bRHr8366kseiK2"
    //     ))
    //   },
    //   externalByronDaedalusMainnet: {
    //     amountStr: "3003112",
    //     addressHex: utils.buf_to_hex(utils.base58_decode(
    //       "DdzFFzCqrht7HGoJ87gznLktJGywK1LbAJT2sbd4txmgS7FcYLMQFhawb18ojS9Hx55mrbsHPr7PTraKh14TSQbGBPJHbDZ9QVh6Z6Di"
    //     ))
    //   },
    //   externalByronTestnet: {
    //     amountStr: "3003112",
    //     addressHex: utils.buf_to_hex(utils.base58_decode(
    //       "2657WMsDfac6Cmfg4Varph2qyLKGi2K9E8jrtvjHVzfSjmbTMGy5sY3HpxCKsmtDA"
    //     ))
    //   },
    //   externalShelley: {
    //     amountStr: "1",
    //     addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(
    //       "addr1q97tqh7wzy8mnx0sr2a57c4ug40zzl222877jz06nt49g4zr43fuq3k0dfpqjh3uvqcsl2qzwuwsvuhclck3scgn3vya5cw5yhe5vyg5x20akz"
    //     ))
    //   },
    //   externalShelleyScripthash: {
    //     amountStr: "1",
    //     addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(
    //       "addr_test1zp0z7zqwhya6mpk5q929ur897g3pp9kkgalpreny8y304rfw6j2jxnwq6enuzvt0lp89wgcsufj7mvcnxpzgkd4hz70z3h2pnc8lhq8r"
    //     ))
    //   },
    //   internalBaseWithStakingKeyHash: {
    //     addressTypeNibble: AddressTypeNibbles.BASE,
    //     spendingPath: str_to_path("1852'/1815'/0'/0/0"),
    //     stakingKeyHashHex: "122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277",
    //     amountStr: "7120787"
    //   },
    //   internalBaseWithStakingPath: {
    //     addressTypeNibble: AddressTypeNibbles.BASE,
    //     spendingPath: str_to_path("1852'/1815'/0'/0/0"),
    //     stakingPath: str_to_path("1852'/1815'/0'/2/0"),
    //     amountStr: "7120787"
    //   },
    //   internalEnterprise: {
    //     addressTypeNibble: AddressTypeNibbles.ENTERPRISE,
    //     spendingPath: str_to_path("1852'/1815'/0'/0/0"),
    //     amountStr: "7120787"
    //   },
    //   internalPointer: {
    //     addressTypeNibble: AddressTypeNibbles.POINTER,
    //     spendingPath: str_to_path("1852'/1815'/0'/0/0"),
    //     stakingBlockchainPointer: {
    //       blockIndex: 1,
    //       txIndex: 2,
    //       certificateIndex: 3
    //     },
    //     amountStr: "7120787"
    //   }
    // };

    // const certificates = {
    //   stakeRegistration: {
    //     type: 0,
    //     path: str_to_path("1852'/1815'/0'/2/0")
    //   },
    //   stakeDeregistration: {
    //     type: 1,
    //     path: str_to_path("1852'/1815'/0'/2/0")
    //   },
    //   stakeDelegation: {
    //     type: 2,
    //     path: str_to_path("1852'/1815'/0'/2/0"),
    //     poolKeyHashHex: "f61c42cbf7c8c53af3f520508212ad3e72f674f957fe23ff0acb4973"
    //   }
    // }
    // const withdrawals = {
    //   withdrawal0: {
    //     path: str_to_path("1852'/1815'/0'/2/0"),
    //     amountStr: "111"
    //   }
    // }


    // it("Should correctly sign tx without change address with Shelley output", async () => {
    //   const response = await ada.signTransaction(
    //     NetworkIds.MAINNET,
    //     ProtocolMagics.MAINNET,
    //     [inputs.utxoShelley],
    //     [
    //       outputs.externalShelley,
    //     ],
    //     sampleFeeStr,
    //     sampleTtlStr,
    //     [],
    //     [],
    //     null
    //   );
    //   expect(response).to.deep.equal(results.noChangeShelley);
    // });

    // const sampleMetadata = "deadbeef".repeat(8);
    // const sampleFeeStr = "42";
    // const sampleTtlStr = "10"

    // export const ProtocolMagics = {
    //   MAINNET: 764824073,
    //   TESTNET: 42
    // }
    // export const NetworkIds = {
    //   TESTNET: 0x00,
    //   MAINNET: 0x01
    // }

    const inputsData = map(inputs, input => {
      return {
        txHashHex: input.id,
        outputIndex: input.index,
        path: cardano.str_to_path("1852'/1815'/0'/0/0"),
      };
    });

    // Without Change address
    const outputsData = [];
    map(outputs, output => {
      // if (output.address !== recieverAddress) {
      //   // ChangeAddress
      //   return {
      //     amountStr: output.amount.quantity.toString(),
      //     path: cardano.str_to_path("44'/1815'/0'/1/0"), // 4th param can be (0 or 1), 1 will say that address is change address
      //   };
      // }
      if (output.address === recieverAddress) {
        outputsData.push({
          amountStr: output.amount.quantity.toString(),
          // address58: output.address,
          addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(output.address))
        });
      }
    });


    console.debug('>>> SIGN (READY) - ', {
      inputsData,
      outputsData,
    })


    try {
      const signedTransaction = await signTransactionChannel.request({
        networkId: 0x01, // NetworkIds.MAINNET,
        protocolMagic: 764824073, // ProtocolMagics.MAINNET,
        //   utxoShelley: {
        //     txHashHex: "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7",
        //     outputIndex: 0,
        //     path: str_to_path("1852'/1815'/0'/0/0"),
        //   }
        inputs: inputsData,
        // inputs exaMPLEMPLE
        //   externalShelley: {
        //     amountStr: "1",
        //     addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(
        //       "addr1q97tqh7wzy8mnx0sr2a57c4ug40zzl222877jz06nt49g4zr43fuq3k0dfpqjh3uvqcsl2qzwuwsvuhclck3scgn3vya5cw5yhe5vyg5x20akz"
        //     ))
        //   },
        outputs: outputsData,
        feeStr: '42', // in lovelaces
        bigFee: new BigNumber(72),
        ttlStr: '7200', // e.g. slot 0, epoch 10
        certificates: [], // [certificates.stakeDelegation],
        withdrawals: [], // [withdrawals.withdrawal0],
        metadataHashHex: null, // sampleMetadata
        coinSelection
      });


     console.debug('>>>> SIGNED: ', signedTransaction)
     return;


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

      console.debug('>>>> witnesses: ', witnesses);

      const txDataHex = thDataHexGenerator(coinSelection);

      const signedTransactionData = {
        // txDataHex: signedTransaction.txHashHex,
        txDataHex,
        witnesses,
      };

      console.debug('>>>> signedTransactionData: ', signedTransactionData);
      console.debug('>>> ENCODED: ', encodeSignedTransaction(signedTransactionData));

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
    console.debug('>>> hardwareWalletsConnectionData');
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
    console.debug('>>> _setHardwareWalletLocalData');
    await this.setHardwareWalletLocalDataRequest.execute(walletId, data);
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };

  _unsetHardwareWalletLocalData = async ({
    walletId,
  }: {
    walletId: string,
  }) => {
    console.debug('>>> _unsetHardwareWalletLocalData');
    await this.unsetHardwareWalletLocalDataRequest.execute(walletId);
    this._refreshHardwareWalletsLocalData();
    this.stores.wallets.refreshWalletsData();
  };

  stopCardanoAdaAppFetchPoller = () => {
    if (this.cardanoAdaAppPollingInterval)
      clearInterval(this.cardanoAdaAppPollingInterval);
  };
}
