// @flow
import { observable, action, runInAction, computed } from 'mobx';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano'; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { getHardwareWalletTransportChannel, getExtendedPublicKeyChannel, getCardanoAdaAppChannel, getHardwareWalletConnectionChannel } from '../ipc/getHardwareWalletChannel';
import type { WalletStatus } from '../types/walletRestoreTypes';

const POLLING_DEVICES_INTERVAL = 1000;

export default class HardwareWalletsStore extends Store {;
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

  @observable walletStatus: ?WalletStatus = null;

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
  }

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

  }

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
    console.debug('>>>> RESET');
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
  }
}
