// @flow
import { observable, action, runInAction, computed } from 'mobx';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano'; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import { get, map } from 'lodash';
import cbor from 'cbor';
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
import { thDataHexGenerator, encodeSignedTransaction } from '../utils/transaction';
import type { HwDeviceStatus } from '../domains/Wallet';

const POLLING_DEVICES_INTERVAL = 1000;

export default class HardwareWalletsStore extends Store {
  @observable selectCoinsRequest: Request<CoinSelectionsResponse> = new Request(
    this.api.ada.selectCoins
  );

  @observable fetchingDevice: boolean = false;
  @observable transport: ?Object = null;
  @observable extendedPublicKey: string = null;

  // Ledger
  @observable isDeviceConnected: boolean = false;
  @observable connectedDevices: Object = {};

  @observable isExportingExtendedPublicKey: boolean = false;
  @observable isExtendedPublicKeyExported: boolean = false;
  @observable isExportingPublicKeyAborted: boolean = false;
  @observable isCardanoAppLaunched: boolean = false;
  @observable derivedAddress: Object = {};
  @observable txSignRequest: Object = {};
  @observable hwDeviceStatus: HwDeviceStatus = HwDeviceStatuses.CONNECTING;

  @observable txDataHex: string = null; // @TODO - remove after testing
  // @TODO - remove after testing
  @observable signedTransaction: string = JSON.stringify({type:"Buffer",data:[130,131,159,130,0,216,24,88,36,130,88,32,145,140,17,225,192,65,160,203,4,186,234,101,27,159,177,189,239,126,229,41,95,3,35,7,226,229,125,16,157,225,24,184,0,130,0,216,24,88,36,130,88,32,143,52,228,247,25,239,254,130,194,140,143,244,94,66,98,51,101,31,192,54,134,19,12,183,225,212,188,109,226,14,104,156,1,255,159,130,130,216,24,88,33,131,88,28,182,244,177,147,224,131,83,10,202,131,255,3,222,74,96,244,231,166,115,43,104,180,250,105,114,244,44,17,160,0,26,144,122,181,199,26,0,15,66,64,130,130,216,24,88,66,131,88,28,181,186,205,64,90,45,206,220,225,152,153,248,100,122,140,79,69,216,76,6,251,83,44,99,249,71,154,64,161,1,88,30,88,28,107,132,135,233,210,40,80,183,83,157,178,85,226,125,212,141,192,165,12,121,148,214,120,105,107,230,79,33,0,26,197,0,13,135,26,3,220,57,111,255,160,129,130,0,216,24,88,133,130,88,64,227,37,75,167,162,238,155,107,178,64,145,59,134,153,49,177,71,106,186,199,9,16,253,64,217,104,14,3,57,255,118,39,80,62,28,237,84,184,249,252,252,35,209,38,206,69,134,250,44,156,59,230,197,197,53,247,23,110,137,157,140,28,46,143,88,64,207,249,82,183,163,34,18,75,222,87,255,122,229,229,84,203,230,163,214,222,8,226,33,19,42,201,66,225,29,33,249,137,20,220,44,73,237,26,189,201,108,131,6,123,14,207,109,68,213,130,179,44,211,202,161,160,52,252,172,247,132,176,103,15]});

  pollingDeviceInterval: ?IntervalID = null;

  setup() {
    const { hardwareWallets: hardwareWalletsActions } = this.actions;
    hardwareWalletsActions.getHardwareWalletDevice.listen(
      this._getHardwareWalletDevice
    );
    getHardwareWalletConnectionChannel.onReceive(
      this._checkHardwareWalletConnection
    );
  }

  selectCoins = async ({
    walletId,
    address,
    amount,
  }: {
    walletId: string,
    address: string,
    amount: string,
  }) => {
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error('Active wallet required before coin selections.');
    }

    const coinSelection = await this.selectCoinsRequest.execute({
      walletId,
      address,
      amount,
    });

    runInAction('HardwareWalletsStore:: coin selections', () => {
      this.txSignRequest = {
        recieverAddress: address,
        inputs: coinSelection.inputs,
        outputs: coinSelection.outputs,
      };
    });

    console.debug('>> TRY TO CREATE thDataHex: ', thDataHexGenerator);
    const txDataHex = thDataHexGenerator(coinSelection);
    console.debug('>> thDataHex: ', txDataHex);


    runInAction('HardwareWalletsStore:: set txDataHex', () => {
      this.txDataHex = txDataHex
    });

    console.debug('>>> coinSelection RES: ', coinSelection);
  };

  @action _checkHardwareWalletConnection = async (params: {
    disconnected: boolean,
  }) => {
    const activeHardwareWalletId = get(
      this.stores,
      ['wallets', 'activeHardwareWallet', 'id'],
      null
    );
    console.debug('>>>> C H E C K <<<< ', {
      disconnected: params.disconnected,
      activeHardwareWalletId,
      stores: this.stores,
      FN: this.stores.wallets._setHardwareWalletConnectionStatus,
    });
    if (params.disconnected) {
      console.debug('>>>> WALLET DISCONNECTED <<<< ');
      this.resetInitializedConnection();
      this.stopDeviceFetchPoller(false);
      // Try to re-establish connection
      this._establishConnection2();
    }
    if (!activeHardwareWalletId) return;
    await this.stores.wallets._setHardwareWalletConnectionStatus({
      disconnected: params.disconnected,
      walletId: activeHardwareWalletId,
    });
    if (!params.disconnected) {
      this.hwDeviceStatus = HwDeviceStatuses.READY;
    }
    this.stores.wallets.refreshWalletsData();
    return activeHardwareWalletId;
  };

  @action startDeviceFetchPoller = () => {
    console.debug('!!!!!!!! STORE:: startDeviceFetchPoller !!!!!!!!');
    this.fetchingDevice = true;
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    // this.pollingDeviceInterval = setInterval(
    //   this._establishConnection2,
    //   POLLING_DEVICES_INTERVAL
    // );
    this._establishConnection2();
  };

  @action stopDeviceFetchPoller = isConnected => {
    console.debug('!!!!!!!! STORE:: stopDeviceFetchPoller !!!!!!!!');
    if (this.pollingDeviceInterval) clearInterval(this.pollingDeviceInterval);
    this.fetchingDevice = false;
    this.isDeviceConnected = isConnected;
  };

  @action _establishConnection2 = async () => {
    console.debug('>>>> ESTABLISH CONNECTION');
    try {
      await this._getHardwareWalletDevice();
      console.debug('>>> ESTABLISHED <<<', this.transport);
      /* runInAction('HardwareWalletsStore:: Connection established',() => {
        this.fetchingDevice = false;
      }); */

      /* this.stopDeviceFetchPoller(true);
      await this._getExtendedPublicKey();
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: 'Hardware Wallet',
        extendedPublicKey: this.extendedPublicKey,
        device: this.transport,
      });
      console.debug('OOOOOOOOOO   START  OOOOOOOOO');
      this._setWalletConnected();
      console.debug('OOOOOOOOOO   DONE  OOOOOOOOO'); */

      this.pollingDeviceInterval = setInterval(
        this._getCardanoAdaApp,
        POLLING_DEVICES_INTERVAL
      );
    } catch (e) {
      console.debug('>>> ESTABLISH CONNECTION - ERROR: ', e);
      this._establishConnection2();
    }
  };

  @action _establishConnection = async () => {
    // Object.assign(this._newWalletDetails, params);
    console.debug('Fetching HW device...');
    const device = await this._getHardwareWalletDevice();
    console.debug('>>>> HW device found: ', device);

    console.debug('Exporting public key...');
    await this._getExtendedPublicKey();
    console.debug('Extended public key Exported: ', this.extendedPublicKey);

    console.debug('Creating HW...');
    this.actions.wallets.createHardwareWallet.trigger({
      walletName: 'Ledger Wallet',
      extendedPublicKey,
      device,
    });
    console.debug('HW Created!');
  };

  @action _getHardwareWalletDevice = async () => {
    console.debug('>>> GET LEDGER <<<');
    let transport = null;
    try {
      transport = await getHardwareWalletTransportChannel.request();
      console.debug('>>> transport: ', transport);
      this._setTransport(transport);
    } catch (e) {
      console.debug('>>>> TRANSPORT ERROR: ', e);
      if (e.statusCode === 28177) {
        throw new Error('Device is locked');
      }
      if (e.statusCode === 28160) {
        throw new Error('Wrong Ledger app');
      }
      if (e.id === 'TransportLocked') {
        console.debug('>>> FAILYRE');
        this.stopDeviceFetchPoller(false);
      }
      throw new Error('Error occured');
    }

    runInAction('HardwareWalletsStore:: HW device connected', () => {
      // this.fetchingDevice = false;
      this.isDeviceConnected = true;
      this.transport = transport;
    });
  };

  @action _getCardanoAdaApp = async () => {
    console.debug('>>> GET Cardano APP <<<');
    this.hwDeviceStatus = HwDeviceStatuses.LAUNCHING_CARDANO_APP;
    try {
      const cardanoAdaAppVersion = await getCardanoAdaAppChannel.request({
        isConnected: true,
      });
      console.debug('>>> GET Cardano APP - DONE <<<: ', cardanoAdaAppVersion);
      this.stopDeviceFetchPoller(true);
    } catch (error) {
      console.debug('>>>> Cardano App error: ', error);
      throw error;
    }

    console.debug('HardwareWalletsStore:: HW Cardano App launched');
    runInAction('HardwareWalletsStore:: HW Cardano App launched', () => {
      this.fetchingDevice = false;
      this.isCardanoAppLaunched = true;
    });
    await this._getExtendedPublicKey();
  };

  @action _getCardanoAdaApp22 = async (isConnected = false) => {
    try {
      console.debug('>>> isConnected: ', isConnected);
      const getCardanoAdaAppVersion = await getCardanoAdaAppChannel.request({
        isConnected,
      });
      console.debug('>>> getCardanoAdaAppVersion: ', getCardanoAdaAppVersion);
    } catch (e) {
      runInAction('HardwareWalletsStore:: HW Disconnected', () => {
        this.isDeviceConnected = false;
      });
      console.debug('>>>> getCardanoAdaAppVersion ERROR: ', e);
    }
  };

  @action _getExtendedPublicKey = async () => {
    console.debug('>>> _getExtendedPublicKey <<<');
    this.isExportingExtendedPublicKey = true;
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY;

    let extendedPublicKey = null;
    const path = [
      utils.HARDENED + 44,
      utils.HARDENED + 1815,
      utils.HARDENED + 0,
    ];
    try {
      extendedPublicKey = await getExtendedPublicKeyChannel.request({
        transport: this.transport,
        path,
      });
      console.debug('>>> extendedPublicKey: ', extendedPublicKey);
      this._setExtendedPublicKey(extendedPublicKey);

      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: 'Hardware Wallet',
        extendedPublicKey: this.extendedPublicKey,
        device: this.transport,
      });
      console.debug('OOOOOOOOOO   START  OOOOOOOOO');
      this._setWalletConnected();
      console.debug('OOOOOOOOOO   DONE  OOOOOOOOO');
    } catch (e) {
      console.debug('>>>> extendedPublicKey ERROR: ', e);
      if (e.statusCode === 28169) {
        this.setExportingPublicKeyToAborted();
      }
      throw e;
    }
  };

  @action _deriveAddress = async () => {
    console.debug('>>> DERIVE ADDRESS <<<');
    try {
      const derivedAddress = await deriveAddressChannel.request({
        derivationPath: "44'/1815'/0'/1/0",
      });
      console.debug('>>> DERIVE ADDRESS - DONE <<<: ', derivedAddress);
      runInAction('HardwareWalletsStore:: set derived address', () => {
        this.derivedAddress = derivedAddress;
      });
    } catch (error) {
      console.debug('>>>> DERIVE ADDRESS error: ', error);
      throw error;
    }
  };

  @action _showAddress = async () => {
    console.debug('>>> SHOW ADDRESS <<<');
    try {
      const address = await showAddressChannel.request({
        derivationPath: "44'/1815'/0'/1/0",
      });
      console.debug('>>> SHOW ADDRESS - DONE <<<: ', address);
    } catch (error) {
      console.debug('>>>> SHOW ADDRESS error: ', error);
      throw error;
    }
  };

  // Coin Selections example (from console):
  // daedalus.stores.hardwareWallets.selectCoins({walletId: "hw_d5184982ea26e8f6335e04b93c8d64cac7b1f678", address: "addr1ssj9c58d76x7v9yulh8wt03t4ksshre2m3wfkul0l43g8pf65ul5u6fal3lnl4y73q8pvvcdt26kp63g8zfsag9edxzjnhx7s6zm82zlt30slw", amount: 700000})
  @action _signTransaction = async () => {
    const { inputs, outputs } = this.txSignRequest;
    console.debug('>>> SIGN TRANSACTION <<< ', { inputs, outputs, txDataHex: this.txDataHex });

    let inputsData;
    let outputsData;
    // Sign transaction with testing data // @TODO - remove
    if (!inputs && !outputs) {
      console.debug('!!! SIGN DUMMY DATA !!!');
      // WORKING EXAMPLE
      inputsData = map(inputs, input => {
        return {
          txDataHex:
            '839f8200d8185824825820918c11e1c041a0cb04baea651b9fb1bdef7ee5295f' +
            '032307e2e57d109de118b8008200d81858248258208f34e4f719effe82c28c8f' +
            'f45e426233651fc03686130cb7e1d4bc6de20e689c01ff9f8282d81858218358' +
            '1cb6f4b193e083530aca83ff03de4a60f4e7a6732b68b4fa6972f42c11a0001a' +
            '907ab5c71a000f42408282d818584283581cb5bacd405a2dcedce19899f8647a' +
            '8c4f45d84c06fb532c63f9479a40a101581e581c6b8487e9d22850b7539db255' +
            'e27dd48dc0a50c7994d678696be64f21001ac5000d871a03dc396fffa0',
          outputIndex: 0,
          path: utils.str_to_path("44'/1815'/0'/0/0"),
        };
      });
      outputsData = [
        {
          amountStr: '700000',
          address58:
            'DdzFFzCqrhsoarXqLakMBEiURCGPCUL7qRvPf2oGknKN2nNix5b9SQKj2YckgXZK6q1Ym7BNLxgEX3RQFjS2C41xt54yJHeE1hhMUfSG',
        },
        {
          amountStr: '100000',
          path: utils.str_to_path("44'/1815'/0'/1/0"),
        },
      ];
    } else {
      console.debug('!!! SIGN   R E A L    DATA !!!');
      inputsData = map(inputs, input => {
        return {
          txDataHex: this.txDataHex,
          outputIndex: input.index,
          path: utils.str_to_path(`44'/1815'/0'/0/0`),
        };
      });

      outputsData = map(outputs, output => {
        if (output.address !== this.txSignRequest.recieverAddress) {
          // ChangeAddress === true
          console.debug('>>> CHANGE ADDRESS: ', output);
          return {
            // amountStr: output.amount.quantity.toString(),
            amountStr: '10000',
            path: utils.str_to_path("44'/1815'/0'/1/0"), // 4th param can be (0 or 1), 1 will say that address is change address
          };
        }
        console.debug('>>> SEND ADDRESS: ', output);
        return {
          amountStr: output.amount.quantity.toString(),
          address58: output.address,
        };
      });
    }

    console.debug('>>> DATA TO SIGN: ', {
      inputsData,
      outputsData,
    })
    // AMOUNT: 9 ADA
    // SEND: 0.7 ADA
    // OUTPUT_AMOUNT: 871162, = 8.7 ADA

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
      });
      console.debug('>>> SIGN TRANSACTION - DONE <<<: ', signedTransaction);
      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
        this.signedTransaction = signedTransaction;
      });
    } catch (error) {
      console.debug('>>>> SIGN TRANSACTION error: ', error);
      runInAction('HardwareWalletsStore:: set Transaction verifying failed', () => {
        this.hwDeviceStatus = HwDeviceStatuses.VERIFYING_FAILED;
      });
      throw error;
    }
  };

  @action _setExtendedPublicKey = extendedPublicKey => {
    this.extendedPublicKey = extendedPublicKey;
    this.isExportingExtendedPublicKey = false;
  };

  @action _setWalletConnected = () => {
    this.hwDeviceStatus = HwDeviceStatuses.READY;
    this.isExtendedPublicKeyExported = true;
  };

  @action setExportingPublicKeyToAborted = () => {
    this.hwDeviceStatus = HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED;
    this.isExportingExtendedPublicKey = false;
    this.isExportingPublicKeyAborted = true;
  };

  @action resetInitializedConnection = () => {
    this.hwDeviceStatus = HwDeviceStatuses.CONNECTING;
    this.isDeviceConnected = false;
    this.fetchingDevice = false;
    this.extendedPublicKey = null;
    this.isExportingExtendedPublicKey = false;
    this.isExtendedPublicKeyExported = false;
    this.isExportingPublicKeyAborted = false;
    this.transport = null;
    this.isLedger = false;
    this.isTrezor = false;
    this.isCardanoAppLaunched = false;
  };

  @action _setTransport = transport => {
    this.transport = transport;
    const deviceModel = get(transport, ['deviceModel', 'id'], null);
    this.isLedger = deviceModel === 'nanoS' || deviceModel === 'nanoX';
    this.isTrezor = deviceModel === 'trezor';
  };

  _resetTxSignRequestData = () => {
    this.selectCoinsRequest.reset();
    this.txSignRequest = {};
  };

  _getXpub = async (path) => {
    try {
      const xPub = await getExtendedPublicKeyChannel.request({
        transport: this.transport,
        path,
      });
      console.debug('>>> xPub: ', xPub);
      return xPub;
    } catch (e) {
      if (e.statusCode === 28169) {
        this.setExportingPublicKeyToAborted();
      }
      throw e;
    }
  };

    // Coin Selections example (from console):
  // daedalus.stores.hardwareWallets.selectCoins({walletId: "hw_d5184982ea26e8f6335e04b93c8d64cac7b1f678", address: "addr1ssj9c58d76x7v9yulh8wt03t4ksshre2m3wfkul0l43g8pf65ul5u6fal3lnl4y73q8pvvcdt26kp63g8zfsag9edxzjnhx7s6zm82zlt30slw", amount: 700000})
  @action _signTransaction22 = async () => {
    console.debug('!!! SIGN   R E A L    DATA !!!');

    const txDataHex =
      '839f8200d8185824825820918c11e1c041a0cb04baea651b9fb1bdef7ee5295f' +
      '032307e2e57d109de118b8008200d81858248258208f34e4f719effe82c28c8f' +
      'f45e426233651fc03686130cb7e1d4bc6de20e689c01ff9f8282d81858218358' +
      '1cb6f4b193e083530aca83ff03de4a60f4e7a6732b68b4fa6972f42c11a0001a' +
      '907ab5c71a000f42408282d818584283581cb5bacd405a2dcedce19899f8647a' +
      '8c4f45d84c06fb532c63f9479a40a101581e581c6b8487e9d22850b7539db255' +
      'e27dd48dc0a50c7994d678696be64f21001ac5000d871a03dc396fffa0';

    const inputsData = [
      {
        txDataHex: txDataHex,
        outputIndex: 0,
        path: utils.str_to_path("44'/1815'/0'/0/0")
      }
    ];

    // const inputsData = [{
    //   txDataHex: this.txDataHex,
    //   outputIndex: 0,
    //   path: utils.str_to_path("44'/1815'/0'/0/0"),
    // }];

    // const outputsData = [
    //   {
    //     amountStr: "4200000",
    //     // address58: "37btjrVyb4KDR9ZYpYSX3T9btsYgLSpzeoyJXpcoTmo4smbpF4PTp8rV9zarEqVoRM4Q9iYTbgK7AR5eLQmr9cTBYkYWq1EdcU1jCU98XTU88cDoes",
    //     address58:
    //       "DdzFFzCqrhsoarXqLakMBEiURCGPCUL7qRvPf2oGknKN2nNix5b9SQKj2YckgXZK6q1Ym7BNLxgEX3RQFjS2C41xt54yJHeE1hhMUfSG"
    //   },
    //   {
    //     amountStr: "4628838",
    //     path: utils.str_to_path("44'/1815'/0'/1/0")
    //   }
    // ];


    const outputsData = [
      {
        amountStr: "700000",
        address58: "DdzFFzCqrhshMav9kXdWuSYDe71zbN625sGXYAeYbUzjzctQB1NDRXrWa8EwbtsGQA4FKQ48H39zsADgdCRJ5g9QZ691Uzr1WXYpteZw"
      },
      {
        amountStr: "100000",
        path: utils.str_to_path("44'/1815'/0'/1/0")
      }
    ];

// 9ADA - AMOUNT

//   8128838 - output from coin selection = 8,128838 ADA
//    871162 - AMOUNT - OUTPUT = 0.871162 ADA
//   8828838 - OUTPUT + AMOUT_TO_SEND = 8,828838 ADA

//   1000000 - my output = 1 ADA
//    700000 - I want to send = 0.7 ADA
//   2928838 - fee = 2.928838 ADA
//  11057676 - SUM output + fee = 11.057676 ADA

    try {
      console.debug('>>> SIGN TRANSACTION <<<', {
        inputsData,
        outputsData,
      })
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
      });
      console.debug('>>> SIGN TRANSACTION - DONE <<<: ', signedTransaction);

// FROM:
// txHashHex: "a6348c8e40948f2726df3ce523de859d0a307e06622e51354c43b9ea9b5b0b4e"
// witnesses: [
//   {
//     path: [2147483692, 2147485463, 2147483648, 0, 0],
//     witnessSignatureHex: "c5d786b715e4a9c1fd370ac6c86e4dadddaf6b9b4858432fed0522f9d3ddd2f50ec0c559550170a21b49954c20a77ffa130844af60c6441c0d863d70e2791809",
//   }
// ]


// --> TO

// {
//   txHashHex: "a6348c8e40948f2726df3ce523de859d0a307e06622e51354c43b9ea9b5b0b4e",
//   witnesses: [
//     {
//       signature: "c5d786b715e4a9c1fd370ac6c86e4dadddaf6b9b4858432fed0522f9d3ddd2f50ec0c559550170a21b49954c20a77ffa130844af60c6441c0d863d70e2791809",
//       xpub: {
//         publicKeyHex: "e3254ba7a2ee9b6bb240913b869931b1476abac70910fd40d9680e0339ff7627",
//         chainCodeHex: "503e1ced54b8f9fcfc23d126ce4586fa2c9c3be6c5c535f7176e899d8c1c2e8f",
//       }
//     }
//   ],
// }

// --> TO Uint8Array(173)

//  Request body
// >>> requestBody:  {"type":"Buffer","data":[130,166,52,140,142,64,148,143,39,38,223,60,229,35,222,133,157,10,48,126,6,98,46,81,53,76,67,185,234,155,91,11,78,129,88,137,130,0,216,24,130,88,64,227,37,75,167,162,238,155,107,178,64,145,59,134,153,49,177,71,106,186,199,9,16,253,64,217,104,14,3,57,255,118,39,80,62,28,237,84,184,249,252,252,35,209,38,206,69,134,250,44,156,59,230,197,197,53,247,23,110,137,157,140,28,46,143,88,64,197,215,134,183,21,228,169,193,253,55,10,198,200,110,77,173,221,175,107,155,72,88,67,47,237,5,34,249,211,221,210,245,14,192,197,89,85,1,112,162,27,73,149,76,32,167,127,250,19,8,68,175,96,198,68,28,13,134,61,112,226,121,24,9]}


// ERROR
//  "code": "malformed_tx_payload",
//  "message": "I couldn't verify that the payload has the correct binary format. Therefore I couldn't send it to the node. Please check the format and try again."



      const witnesses = await Promise.all(
        signedTransaction.witnesses.map(async (witness) => {
          const xPub = await this._getXpub(witness.path);
          console.debug('>>> RESOLVED xPub: ', {xPub, witness});
          return {
            xpub: xPub,
            signature: witness.witnessSignatureHex,
          };
        })
      );




      console.debug('>>> witnesses <<<: ', witnesses);
      const signedTransactionData = {
        txDataHex: txDataHex,
        witnesses: witnesses,
      }

      const encodedSignedTransaction = encodeSignedTransaction(signedTransactionData);

      console.debug('>>> ENCODED: ', encodedSignedTransaction);

      console.debug('>>> signedTransactionData: ', {
        signedTransactionData,
        encodedSignedTransaction,
        test0: encodedSignedTransaction.buffer,
        test1: Buffer.from(encodedSignedTransaction.buffer),
        test2: Buffer.from(encodedSignedTransaction),
      });


      runInAction('HardwareWalletsStore:: set Transaction verified', () => {
        this.signedTransaction = encodedSignedTransaction;
      });
    } catch (error) {
      console.debug('>>>> SIGN TRANSACTION error: ', error);
      throw error;
    }
  };
}
