// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { get, map } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import BigNumber from 'bignumber.js';
import { encode } from 'borc'
import blakejs from 'blakejs';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
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

const derivationScheme = {
  type: 'v2',
  ed25519Mode: 2,
  keyfileVersion: '2.0.0',
};

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
      const transportDevice = await getHardwareWalletTransportChannel.request({ isTrezor: false });
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
    const isTrezor = false; // @TODO - add active device recognizing logic
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
    const addressIndex = this.stores.addresses.getAddressIndex(input.address);
    console.debug('>>> addressIndex: ', addressIndex);
    console.debug('>>> cardano: ', cardano);
    const addressIndexPath = [cardano.str_to_path("1852'/1815'/0'"), 0, addressIndex];
    console.debug('>>> addressIndexPath: ', addressIndexPath);

    const data = {
      // path: addressToAbsPathMapper(input.address), // From AdaLite example
      // path: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
      path: `m/1852'/1815'/0'/0/${addressIndex}`,
      prev_hash: input.id,
      prev_index: input.index,
    }

    return data
  };

  prepareOutputs = (output, isChange) => {
    if (isChange) {
      // const someInputAddress = this.txSignRequest.coinSelection.inputs[0].address;
      const addressIndex = this.stores.addresses.getAddressIndex(output.address);
      // const addressIndex = this.stores.addresses.getAddressIndex(someInputAddress);
      return {
        amount: output.amount.quantity.toString(),
        addressParameters: {
          addressType: 0, // TODO: 0 for base address
          // path: output.spendingPath, // output.path "m/1852'/1815'/0'/1/{index??}"
          // stakingPath: output.stakingPath, // "m/1852'/1815'/0'/2/0"
          // path: cardano.str_to_path(`1852'/1815'/0'/1/${addressIndex}`),
          // path: cardano.str_to_path("1852'/1815'/0'/0/0"), // OK when NO change address

          path: `m/1852'/1815'/0'/0/${addressIndex}`,
          stakingPath: "m/1852'/1815'/0'/2/0",
          // path: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
          // stakingPath: cardano.str_to_path("1852'/1815'/0'/2/0"),
        },
      }
    } else {
      return {
        address: output.address,
        amount: output.amount.quantity.toString(),
      }
    }
  };

  _prepareLedgerInput = (input) => {
    const addressIndex = this.stores.addresses.getAddressIndex(input.address);
    return {
      txHashHex: input.id,
      outputIndex: input.index,
      // path: `m/1852'/1815'/0'/0/${addressIndex}`,
      path: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
    }
  };

  _prepareLedgerOutput = (output, isChange) => {
    const addressIndex = this.stores.addresses.getAddressIndex(output.address);
    if (isChange) {
      return {
        addressTypeNibble: 0, // TODO: get from address
        // spendingPath: `m/1852'/1815'/0'/0/${addressIndex}`,
        spendingPath: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
        amountStr: output.amount.quantity.toString(),
        // stakingPath: "m/1852'/1815'/0'/2/0",
        stakingPath: cardano.str_to_path("1852'/1815'/0'/2/0"),
      };
    }

    return {
      amountStr: output.amount.quantity.toString(),
      addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(output.address))
    };
  };


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
    let totalInputs = 0;
    let totalOutputs = 0;
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
        const data = this.prepareInput(input);
        _inputs.push(data);
        totalInputs = totalInputs + input.amount.quantity;
      }

      const _outputs = [];
      // let fakedChangeIndex = 0; // index from addresses list
      for (const output of coinSelection.outputs) {
        totalOutputs = totalOutputs + output.amount.quantity;
        // @TODO - change address SKIPPED, we need to enable
        // if (output.address === recieverAddress) {
          const isChange = output.address !== recieverAddress;
          console.debug('>>> Output: ', {
            output,
            isChange,
          })
          const data = this.prepareOutputs(output, isChange);
          // if (isChange) {
          //   fakedChangeIndex = fakedChangeIndex + 1;
          // }
          console.debug('>>> Output data contst: ', data);
          _outputs.push(data)
        // }
      }

      console.debug('>>>> CONSTRUCTOED: ', {
        _inputs,
        _outputs,
      })

      inputsData = _inputs;
      outputsData = _outputs;
      // return;
    }

    console.debug('>>> Sign START: ', { inputsData, outputsData, totalInputs, totalOutputs });

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
        // fee: '42',
        fee: (totalInputs - totalOutputs).toString(),
        // ttl: '7200',
        ttl: '15000000',
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


    // For Shelley
  @action _signTransactionLedger = async () => {
    const { coinSelection, txDataHex, recieverAddress } = this.txSignRequest;
    const { inputs, outputs } = coinSelection;
    let totalInputs = 0;
    let totalOutputs = 0;


    console.debug('>>> SIGN (prepare) - ', {
      txDataHex, recieverAddress, inputs, outputs, utils,
    })


    let inputsData = map(inputs, input => {
      totalInputs = totalInputs + input.amount.quantity;
      return this._prepareLedgerInput(input);
    });

    let outputsData = map(outputs, output => {
      totalOutputs = totalOutputs + output.amount.quantity;
      const isChange = output.address !== recieverAddress;
      return this._prepareLedgerOutput(output, isChange);
    });


    console.debug('>>>> MY DATA: ', {
      inputsData,
      outputsData,
    })

    // FAKED FROM ADA LITE
    inputsData = [
      {
        path: [2147485500, 2147485463, 2147483648, 0, 0],
        txHashHex: "7cfcc86ecf47b15df9beb1dccd3e9bfa4e4e99a53241d8d7bd27536e8311ab1c",
        outputIndex: 1
      },
      {
        path: [2147485500, 2147485463, 2147483648, 0, 1],
        txHashHex: "605d54a6bbd10db3d6b0363cb40358f21fdee21c5cbeff1bef0246aa64f1f65e",
        outputIndex: 0
      }
    ];

    outputsData = [
      {
        addressHex: "0165afcacea378acc3ce3417bbc926af84ac390e2f9cb89555e1b74c389b63aba776c239684e574af94f2516878a97630f75a569511b8f13ba",
        amountStr: "2000000",
      },
      {
        addressTypeNibble: 0,
        amountStr: "1128680",
        spendingPath: [2147485500, 2147485463, 2147483648, 0, 0],
        stakingPath: [2147485500, 2147485463, 2147483648, 2, 0]
      }
    ];

    const unsignedTx = {
      inputs: [
        {
          address: "addr1qypq77cn4ugc2tzy5wsna2r6w6fffcc8ygatkz5hm79j2gxss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6sztdpc2",
          coins: 1814340,
          outputNo: 1,
          txHash: "7cfcc86ecf47b15df9beb1dccd3e9bfa4e4e99a53241d8d7bd27536e8311ab1c",
        },
        {
          address: "addr1qxxsrzqvt94pe2es4upp5q0ufqpsgne4lvutcxhc6trnnpwss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6sel85za",
          coins: 1500000,
          outputNo: 0,
          txHash: "605d54a6bbd10db3d6b0363cb40358f21fdee21c5cbeff1bef0246aa64f1f65e",
        }
      ],
      outputs: [
        {
          address: "addr1q9j6ljkw5du2es7wxstmhjfx47z2cwgw97wt3924uxm5cwymvw46wakz895yu462l98j295832tkxrm45454zxu0zwaqgu7xj2",
          coins: 2000000,
          isChange: false,
          spendingPath: null,
          stakingPath: null,
        }, {
          address: "addr1qypq77cn4ugc2tzy5wsna2r6w6fffcc8ygatkz5hm79j2gxss5ftcx6ppfxak3ydjjm06q6kqprfu38fqdc7fr5xqy6sztdpc2",
          coins: 1128680,
          isChange: true,
          spendingPath: [2147485500, 2147485463, 2147483648, 0, 0],
          stakingPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
      ],
      fee: 185660,
      ttl: 8875164,
      certs: [],
      withdrawals: undefined,
    }



    let fee = totalInputs - totalOutputs;

    // FAKED FROM ADA LITE
    fee = 185660;

    console.debug('>>> SIGN (READY) - ', {
      networkId: 1, // NetworkIds.MAINNET,
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
      feeStr: fee.toString(),
      bigFee: new BigNumber(fee),
      // ttlStr: '15000000', // e.g. slot 0, epoch 10
      ttlStr: "8875164",
      certificates: [], // [certificates.stakeDelegation],
      withdrawals: [], // [withdrawals.withdrawal0],
      metadataHashHex: null, // sampleMetadata
      coinSelection,
    })
    try {
      const signedTransaction = await signTransactionChannel.request({
        networkId: 1, // NetworkIds.MAINNET,
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
        feeStr: fee.toString(),
        bigFee: new BigNumber(fee),
        // ttlStr: '15000000', // e.g. slot 0, epoch 10
        ttlStr: "8875164",
        certificates: [], // [certificates.stakeDelegation],
        withdrawals: [], // [withdrawals.withdrawal0],
        metadataHashHex: null, // sampleMetadata
        coinSelection,
      });


     console.debug('>>>> SIGNED: ', signedTransaction)


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
//
      // console.debug('>>>> witnesses: ', witnesses);
//
      // const txDataHex = thDataHexGenerator(coinSelection);

      // const signedTransactionData = {
      //   txDataHex: signedTransaction.txHashHex,
      //   txDataHex,
      //   witnesses,
      // };

      console.debug('>>> START ENCODING: ', {
        witnesses: signedTransaction.signedTransaction.witnesses,
        unsignedTx: unsignedTx,
      })

      const adaLiteWitnesses = [
        {
          path:[2147485500, 2147485463, 2147483648, 0, 0],
          witnessSignatureHex:"128507227833015854da7afc85e04d8cb305187cd7bb790578ddf3ab7a2b582da67a3ca3340fd6a2d25cc17de9c393a0bf4c84aa3226f2462ba12f7f7251b70c",
        },
        {
          path: [2147485500, 2147485463, 2147483648, 0, 1],
          witnessSignatureHex: "581d0d3fe79908d0e9958c460ac4654f02c78a1827dee926c8dc883afdc5f29c5a4e0be4955bbd75fff5051e96488408299f57b3fa709b4f0bfced360c667801",
        },
      ]

      const txWitnesses = await this.prepareWitnesses(signedTransaction.signedTransaction.witnesses);
      // const txWitnesses = await this.prepareWitnesses(adaLiteWitnesses);
      console.debug('>>> txWitnesses: ', txWitnesses);

      const txAux = this.prepareTxAux(unsignedTx);
      console.debug('>>> TX AUX: ', txAux);

      const txBody = await this.prepareBody(txAux, txWitnesses);
      console.debug('>>> txBody: ', txBody);


      // console.debug('>>>> signedTransactionData: ', signedTransactionData);
      // console.debug('>>> ENCODED: ', encodeSignedTransaction(signedTransactionData));

      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        // this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        // this.signedTransaction = signedTransactionData;
        this.signedTransaction = txBody;
        this.txSendParams = {
          signTransaction: txBody,
        }
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
        isTrezor: false,
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


  CachedDeriveXpubFactory = (derivationScheme, deriveXpubHardenedFn) => {

    console.debug('>>> CachedDeriveXpubFactory - INSTANCE: ', {
      derivationScheme, deriveXpubHardenedFn
    })

    const derivedXpubs = {}
    // HARDENED_THRESHOLD = 0x80000000;
    const indexIsHardened = (index) => {
      console.debug('>>> indexIsHardened: ', index);
      return index >= 0x80000000;
    };

    async function deriveXpub(absDerivationPath) {
      console.debug('>>> CachedDeriveXpubFactory - deriveXpub: ', absDerivationPath)

      const memoKey = JSON.stringify(absDerivationPath)

      const derivedXpubsMemo = await derivedXpubs[memoKey];
      console.debug('>>>> CachedDeriveXpubFactory derivedXpubsMemo: ', derivedXpubsMemo);
      console.debug('>>> CachedDeriveXpubFactory - memoKey: ', {memoKey, derivedXpubs })

      if (!derivedXpubs[memoKey]) {
        const deriveHardened =
          absDerivationPath.length === 0 || indexIsHardened(absDerivationPath.slice(-1)[0])

        /*
        * TODO - reset cache if the promise fails, for now it does not matter
        * since a failure (e.g. rejection by user) there leads to
        * the creation of a fresh wallet and crypto provider instance
        */
        derivedXpubs[memoKey] = deriveHardened
          ? deriveXpubHardenedFn(absDerivationPath)
          : deriveXpubNonhardenedFn(absDerivationPath)
        console.debug('>>> CachedDeriveXpubFactory - IF CASE : ', {
          deriveHardened,
          derivedXpubs: derivedXpubs[memoKey],
        });
      }

      /*
      * the derivedXpubs map stores promises instead of direct results
      * to deal with concurrent requests to derive the same xpub
      */
      const derivedXpubsMemoKey = await derivedXpubs[memoKey]
      console.debug('>>> CachedDeriveXpubFactory - derivedXpubs[memoKey]: ', derivedXpubsMemoKey)
      return derivedXpubsMemoKey;
    }

    async function deriveXpubNonhardenedFn(derivationPath) {
      console.debug('>>> CALL deriveXpubNonhardenedFn METHOD: ', derivationPath);
      const lastIndex = derivationPath.slice(-1)[0]
      const parentXpub = await deriveXpub(derivationPath.slice(0, -1))
      console.debug('>>> CALL deriveXpubNonhardenedFn METHOD - PARAMS: ', {
        lastIndex,
        parentXpub,
        derivationScheme,
      });

      const res = deriveChildXpub(parentXpub, lastIndex, derivationScheme.ed25519Mode);
      console.debug('>>> CALL deriveXpubNonhardenedFn RESULT: ', res);
      return res;
      // return
    }

    console.debug('>>> CachedDeriveXpubFactory - MAIN RETURN: ', deriveXpub);
    return deriveXpub
  }

  deriveXpub = this.CachedDeriveXpubFactory(derivationScheme, async (absDerivationPath) => {
    console.debug('>>> deriveXpub - START: ', {absDerivationPath, derivationScheme});
    // const response = await deviceConnection.getExtendedPublicKey(absDerivationPath);

    const response = await getExtendedPublicKeyChannel.request({
      path: absDerivationPath,
      isTrezor: false,
    });

    console.debug('>>> deriveXpub - RESPONSE: ', response);

    const xpubHex = response.publicKeyHex + response.chainCodeHex

    console.debug('>>> xpubHex: ', {xpubHex, res: Buffer.from(xpubHex, 'hex')});

    const xpubHexBuffer = Buffer.from(xpubHex, 'hex');

    console.debug('>>> xpubHexBuffer: ', xpubHexBuffer);
    this.xpubHexBuffer = xpubHexBuffer;
    return xpubHexBuffer;
  })

  xpub2pub = (xpub: Buffer) => {
      console.debug('>>> GET xpub2pub: ', xpub);
      // console.debug('>>> GET xpub2pub: ', this.xpubHexBuffer);
      // const xpub = this.xpubHexBuffer;
      const res = xpub.slice(0, 32);
      console.debug('>>> RES xpub2pub: ', res);
      return res;
      // return xpub2.slice(0, 32)
    } // TODO: export from addresses


  ShelleyTxWitnessShelley = (publicKey, signature) => {
    console.debug('>>> ShelleyTxWitnessShelley: ', publicKey, signature);
    function encodeCBOR(encoder) {
      return encoder.pushAny([publicKey, signature])
    }

    return {
      publicKey,
      signature,
      encodeCBOR,
    }
  }



  ShelleyWitness = async (witness) => {
    console.debug('>>> ShelleyWitness: ', witness);
    const xpub = await this.deriveXpub(witness.path);
    console.debug('>>> ShelleyWitness xPub: ', xpub, this.xpubHexBuffer);
    const publicKey = this.xpub2pub(xpub); // send "this.xpubHexBuffer"
    console.debug('>>> ShelleyWitness publicKey: ', publicKey);
    const signature = Buffer.from(witness.witnessSignatureHex, 'hex')
    console.debug('>>> ShelleyWitness signature: ', signature);
    const shelleyWitness = this.ShelleyTxWitnessShelley(publicKey, signature);
    console.debug('>>> ShelleyWitness - DONE: ', shelleyWitness);
    return shelleyWitness;
  }

  prepareWitnesses = async (ledgerWitnesses) => {
    console.debug('>>> prepareWitnesses: ', ledgerWitnesses);
    const _shelleyWitnesses = []
    ledgerWitnesses.forEach((witness) => {
      /* isShelleyPath(witness.path)
        ? _shelleyWitnesses.push(ShelleyWitness(witness))
        : _byronWitnesses.push(ByronWitness(witness)) */
        _shelleyWitnesses.push(this.ShelleyWitness(witness))
    })
    console.debug('>>> ShelleyWitnesses: ', _shelleyWitnesses);
    const shelleyWitnesses = await Promise.all(_shelleyWitnesses);

    const witnesses = new Map();
    if (shelleyWitnesses.length > 0) {
      witnesses.set(0, shelleyWitnesses)
    }

    console.debug('>>> Returned Witnesses: ', witnesses);

    this.transformedWitnesses = witnesses;

    return witnesses
  }

  ShelleyTxInputFromUtxo = (utxo) => {
    const coins = utxo.coins
    const txid = utxo.txHash
    const outputNo = utxo.outputNo
    const address = utxo.address
    const txHash = Buffer.from(txid, 'hex')
    function encodeCBOR(encoder) {
      return encoder.pushAny([txHash, outputNo])
    }

    return {
      coins,
      address,
      txid,
      outputNo,
      encodeCBOR,
    }
  }

  ShelleyTxOutput = (address, coins, isChange, spendingPath = null, stakingPath = null) => {
    function encodeCBOR(encoder) {
      // const addressBuff = isShelleyFormat(address)
      //   ? bech32.decode(address).data
      //   : base58.decode(address)
      // addressBuff = bech32.decode(address).data;
      const addressBuff = utils.bech32_decodeAddress(address);
      // addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(output.address))

      return encoder.pushAny([addressBuff, coins])
    }

    return {
      address,
      coins,
      isChange,
      spendingPath,
      stakingPath,
      encodeCBOR,
    }
  }

  ShelleyFee = (fee) => {
    function encodeCBOR(encoder) {
      return encoder.pushAny(fee)
    }

    return {
      fee,
      encodeCBOR,
    }
  }

  ShelleyTtl = (ttl) => {
    function encodeCBOR(encoder) {
      return encoder.pushAny(ttl)
    }

    return {
      ttl,
      encodeCBOR,
    }
  }

  prepareTxAux = (unsignedTx) => {
    console.debug('>>> PREPARE prepareTxAux: ', unsignedTx);
    const txInputs = unsignedTx.inputs.map(this.ShelleyTxInputFromUtxo)
    const txOutputs = unsignedTx.outputs.map(({address, coins, isChange, spendingPath, stakingPath}) => {
      if (isChange) {
        // const spendingPath = [2147485500, 2147485463, 2147483648, 0, 0];
        // const stakingPath = [2147485500, 2147485463, 2147483648, 2, 0];
        return this.ShelleyTxOutput(address, coins, true, spendingPath, stakingPath);
      } else {
        return this.ShelleyTxOutput(address, coins, false)
      }
    })
    // const txCerts = unsignedTx.certs.map(({type, accountAddress, poolHash}) =>
    //   ShelleyTxCert(type, accountAddress, poolHash)
    // )
    const txCerts = [];

    const txFee = this.ShelleyFee(unsignedTx.fee)
    // const txTtl = ShelleyTtl(await calculateTtl())
    const txTtl = this.ShelleyTtl(unsignedTx.ttl)
    // const txWithdrawals = unsignedTx.withdrawals.map(({accountAddress, rewards}) => {
    //   return ShelleyWitdrawal(accountAddress, rewards)
    // })
    const txWithdrawals = [undefined];
    /* if (unsignedTx.isChange) {
      const { address, coins, accountAddress } = unsignedTx.change
      const absDerivationPath = myAddresses.getAddressToAbsPathMapper()(address)
      const stakingPath = myAddresses.getAddressToAbsPathMapper()(accountAddress)
      txOutputs.push(ShelleyTxOutput(address, coins, true, absDerivationPath, stakingPath))
    } */
    // TODO: Without withdrawals
    console.debug('>>>> CREATE FROM prepareTxAux: ', {
      txInputs, txOutputs, txFee, txTtl, txCerts, txWithdrawals: txWithdrawals[0],
    })
    return this.ShelleyTxAux(txInputs, txOutputs, txFee, txTtl, txCerts, txWithdrawals[0])
  }

  ShelleyTxAux = (inputs, outputs, fee, ttl, certs, withdrawals) => {
    const blake2b = data => blakejs.blake2b(data, null, 32);
    function getId() {
      return blake2b(
        encode(this.ShelleyTxAux(inputs, outputs, fee, ttl, certs, withdrawals)),
        32
      ).toString('hex')
    }

    function encodeCBOR(encoder) {
      const txMap = new Map()
      txMap.set(0, inputs)
      txMap.set(1, outputs)
      txMap.set(2, fee)
      txMap.set(3, ttl)
      if (certs && certs.length) txMap.set(4, certs)
      if (withdrawals) txMap.set(5, withdrawals)
      return encoder.pushAny(txMap)
    }

    return {
      getId,
      inputs,
      outputs,
      fee,
      ttl,
      certs,
      withdrawals,
      encodeCBOR,
    }
  }

  ShelleySignedTransactionStructured = (txAux, witnesses, meta) => {
    function getId() {
      return txAux.getId()
    }

    function encodeCBOR(encoder) {
      return encoder.pushAny([txAux, witnesses, meta])
    }

    return {
      getId,
      witnesses,
      txAux,
      encodeCBOR,
    }
  }

  prepareBody = (unsignedTx, txWitnesses) => {
    console.debug('>>> prepareBody <<<');
    // const txWitnesses = this.transformedWitnesses;
    console.debug('>>> txWitnesses: ', txWitnesses);
    // const unsignedTx = this.txSignRequest.coinSelection;
    console.debug('>>> unsignedTx: ', unsignedTx);

    const signedTransactionStructure = this.ShelleySignedTransactionStructured(unsignedTx, txWitnesses, null);
    console.debug('>>> signedTransactionStructure: ', signedTransactionStructure);
    const encodedSignedTransactionStructure = encode(signedTransactionStructure).toString('hex');
    console.debug('>>> encodedSignedTransactionStructure: ', encodedSignedTransactionStructure);
    return encodedSignedTransactionStructure;
  }

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
