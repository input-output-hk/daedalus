// @flow
import { observable, action, runInAction, computed } from 'mobx';
import AppAda, { utils } from "@cardano-foundation/ledgerjs-hw-app-cardano"; //"@cardano-foundation/ledgerjs-hw-app-cardano";
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

  pollingDeviceInterval: ?IntervalID = null;

  setup() {
    const {
      hardwareWallets: hardwareWalletsActions,
    } = this.actions;
    hardwareWalletsActions.getHardwareWalletDevice.listen(
      this._getHardwareWalletDevice
    );
    getHardwareWalletConnectionChannel.onReceive(this._checkHardwareWalletConnection);
  };

  selectCoins = async ({
    walletId,
    address,
    amount,
  }: {
    walletId: string,
    address: string,
    amount: string,
  }) => {

    console.debug('>>> WALLET ID: ', walletId);

    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error(
        'Active wallet required before coin selections.'
      );
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
        outputs: coinSelection.outputs
      };
    });

    console.debug('>>> coinSelection RES: ', coinSelection);
  };

  _checkHardwareWalletConnection = async (params: { disconnected: boolean }) => {
    const activeHardwareWalletId = get(this.stores, ['wallets', 'activeHardwareWallet', 'id'], null);
    console.debug('>>>> C H E C K <<<< ', {
      disconnected: params.disconnected,
      activeHardwareWalletId,
      stores: this.stores,
      FN: this.stores.wallets._setHardwareWalletConnectionStatus,
    });
    if (!activeHardwareWalletId) return;
    await this.stores.wallets._setHardwareWalletConnectionStatus({
      disconnected: params.disconnected,
      walletId: activeHardwareWalletId,
    });
    console.debug('>>>> WALLET DISCONNECTED <<<< ');
    this.resetInitializedConnection();
    this.stores.wallets.refreshWalletsData();
    return activeHardwareWalletId;
  };

  @action startDeviceFetchPoller = () => {
    console.debug('!!!!!!!! STORE:: startDeviceFetchPoller !!!!!!!!');
    this.fetchingDevice = true;
    // this.pollingDeviceInterval = setInterval(
    //   this._establishConnection2,
    //   POLLING_DEVICES_INTERVAL
    // );
    this._establishConnection2();
  };

  @action stopDeviceFetchPoller = (isConnected) => {
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
        console.debug('>>> FAILYRE')
        this.stopDeviceFetchPoller(false);
      }
      throw new Error('Error occured');
    }

    runInAction(
      'HardwareWalletsStore:: HW device connected',
      () => {
        // this.fetchingDevice = false;
        this.isDeviceConnected = true;
        this.transport = transport;
      }
    );
  };

  @action _getCardanoAdaApp = async () => {
    console.debug('>>> GET Cardano APP <<<');
    try {
      const cardanoAdaAppVersion = await getCardanoAdaAppChannel.request({
        isConnected: true
      });
      console.debug('>>> GET Cardano APP - DONE <<<: ', cardanoAdaAppVersion);
      this.stopDeviceFetchPoller(true)
    } catch (error) {
      console.debug('>>>> Cardano App error: ', error);
      throw error;
    }

    console.debug('HardwareWalletsStore:: HW Cardano App launched');
    runInAction(
      'HardwareWalletsStore:: HW Cardano App launched',
      () => {
        this.fetchingDevice = false;
        this.isCardanoAppLaunched = true;
      }
    );
    await this._getExtendedPublicKey();
  };

  @action _getCardanoAdaApp22 = async (isConnected = false) => {
    try {
      console.debug('>>> isConnected: ', isConnected);
      const getCardanoAdaAppVersion = await getCardanoAdaAppChannel.request({
        isConnected
      });
      console.debug('>>> getCardanoAdaAppVersion: ', getCardanoAdaAppVersion);
    } catch (e) {
      runInAction(
      'HardwareWalletsStore:: HW Disconnected',
      () => {
        this.isDeviceConnected = false;
      }
    );
      console.debug('>>>> getCardanoAdaAppVersion ERROR: ', e);
    }
  };

  @action _getExtendedPublicKey = async () => {
    console.debug('>>> _getExtendedPublicKey <<<');
    this.isExportingExtendedPublicKey = true
    let extendedPublicKey = null;
    const path = [
      utils.HARDENED + 44,
      utils.HARDENED + 1815,
      utils.HARDENED + 0
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
        this.setExportingPublicKeyToAborted()
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
      runInAction('HardwareWalletsStore:: set derived address',() => {
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
    console.debug('>>> SIGN TRANSACTION <<< ', {inputs, outputs});

    let inputsData;
    let outputsData;
    // Sign transaction with testing data // @TODO - remove
    if (!inputs && !outputs) {
      // WORKING EXAMPLE
      inputsData = map(inputs, input => {
        return {
         txDataHex:
            "839f8200d8185824825820918c11e1c041a0cb04baea651b9fb1bdef7ee5295f" +
            "032307e2e57d109de118b8008200d81858248258208f34e4f719effe82c28c8f" +
            "f45e426233651fc03686130cb7e1d4bc6de20e689c01ff9f8282d81858218358" +
            "1cb6f4b193e083530aca83ff03de4a60f4e7a6732b68b4fa6972f42c11a0001a" +
            "907ab5c71a000f42408282d818584283581cb5bacd405a2dcedce19899f8647a" +
            "8c4f45d84c06fb532c63f9479a40a101581e581c6b8487e9d22850b7539db255" +
            "e27dd48dc0a50c7994d678696be64f21001ac5000d871a03dc396fffa0",
          outputIndex: 0,
          path: utils.str_to_path("44'/1815'/0'/0/0")
        }
      });
      outputsData = [
        {
          amountStr: "700000",
          address58:
            "DdzFFzCqrhsoarXqLakMBEiURCGPCUL7qRvPf2oGknKN2nNix5b9SQKj2YckgXZK6q1Ym7BNLxgEX3RQFjS2C41xt54yJHeE1hhMUfSG"
        },
        {
          amountStr: "100000",
          path: utils.str_to_path("44'/1815'/0'/1/0")
        }
      ];
    } else {
      inputsData = map(inputs, input => {
        return {
          txDataHex: input.id,
          outputIndex: input.index,
          path: utils.str_to_path(`44'/1815'/${input.index}'/0/0`)
        }
      });

      outputsData = map(outputs, output => {
        if (output.address === this.txSignRequest.recieverAddress) {
          // ChangeAddress === true
          return {
            amountStr: output.amount.quantity.toString(),
            path: utils.str_to_path("44'/1815'/0'/1/0") // 4th param can be (0 or 1), 1 will say that address is change address
          }
        }
        return {
          amountStr: output.amount.quantity.toString(),
          address58: output.address,
        }
      });
    }

    try {
      const signedTransaction = await signTransactionChannel.request({
        inputs: inputsData,
        outputs: outputsData,
      });
      console.debug('>>> SIGN TRANSACTION - DONE <<<: ', signedTransaction);
    } catch (error) {
      console.debug('>>>> SIGN TRANSACTION error: ', error);
      throw error;
    }
  };

  @action _setExtendedPublicKey = (extendedPublicKey) => {
    this.extendedPublicKey = extendedPublicKey;
    this.isExportingExtendedPublicKey = false;
  };

  @action _setWalletConnected = () => {
    this.isExtendedPublicKeyExported = true;
  };

  @action setExportingPublicKeyToAborted = () => {
    this.isExportingExtendedPublicKey = false;
    this.isExportingPublicKeyAborted = true;
  };

  @action resetInitializedConnection = () => {
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

  @action _setTransport = (transport) => {
    this.transport = transport;
    const deviceModel = get(transport, ['deviceModel', 'id'], null);
    this.isLedger = deviceModel === 'nanoS' || deviceModel === 'nanoX';
    this.isTrezor = deviceModel === 'trezor';
  };

  _resetTxSignRequestData = () => {
    this.selectCoinsRequest.reset();
    this.txSignRequest = {};
  }
}
