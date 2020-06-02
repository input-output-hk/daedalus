// @flow
import { observable, action, runInAction, computed } from 'mobx';
import AppAda, { utils } from "@cardano-foundation/ledgerjs-hw-app-cardano"; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { getHardwareWalletTransportChannel, getExtendedPublicKeyChannel, getCardanoAdaAppChannel, /*getHardwareWalletConnectionChannel*/ } from '../ipc/getHardwareWalletChannel';

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

  pollingDeviceInterval: ?IntervalID = null;

  setup() {
    const {
      hardwareWallets: hardwareWalletsActions,
    } = this.actions;
    hardwareWalletsActions.getHardwareWalletDevice.listen(
      this._getHardwareWalletDevice
    );
    // getHardwareWalletConnectionChannel.onReceive(this._checkHardwareWalletConnection);
  }

  @action _checkHardwareWalletConnection = ({ disconnected }) => {
    console.debug('>>>> C H E C K <<<< ', disconnected);
  }

  @action startDeviceFetchPoller = () => {
    console.debug('!!!!!!!! STORE:: startDeviceFetchPoller !!!!!!!!');
    this.fetchingDevice = true;
    this.pollingDeviceInterval = setInterval(
      this._establishConnection2,
      POLLING_DEVICES_INTERVAL
    );
  }

  @action stopDeviceFetchPoller = (isConnected) => {
    console.debug('!!!!!!!! STORE:: stopDeviceFetchPoller !!!!!!!!');
    if (this.pollingDeviceInterval) clearInterval(this.pollingDeviceInterval);
    this.fetchingDevice = false;
    this.isDeviceConnected = isConnected;
  }

  @action _establishConnection2 = async () => {
    console.debug('>>>> POLL');
    try {
      const device = await this._getHardwareWalletDevice();
      console.debug('>>> ESTABLISHED <<<', device);
      /* runInAction('HardwareWalletsStore:: Connection established',() => {
        this.fetchingDevice = false;
      }); */
      this.stopDeviceFetchPoller(true);
      await this._getExtendedPublicKey();
      await this.actions.wallets.createHardwareWallet.trigger({
        walletName: 'Ledger Wallet',
        extendedPublicKey: this.extendedPublicKey,
        device,
      });
      console.debug('OOOOOOOOOO   START  OOOOOOOOO');
      this._setWalletConnected();
      console.debug('OOOOOOOOOO   DONE  OOOOOOOOO');
    } catch (e) {
      console.debug('>>> POLL - ERROR: ', e);
    }
  }

  @action _establishConnection = async () => {
    // Object.assign(this._newWalletDetails, params);
    console.debug('Fetching HW device...');
    const device = await this._getHardwareWalletDevice();
    console.debug('>>>> HW device found: ', device);


    console.debug('Exporting public key...');
    const extendedPublicKey = await this._getExtendedPublicKey();
    console.debug('Extended public key Exported: ', extendedPublicKey);

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
    this.fetchingDevice = true;
    let transport = null;
    try {
      transport = await getHardwareWalletTransportChannel.request();
      console.debug('>>> transport: ', transport);
      this._setTransport(transport);
      return transport;
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
      'HardwareWalletsStore:: HW fetching finished',
      () => {
        this.fetchingDevice = false;
        this.isDeviceConnected = true;
        this.transport = transport;
      }
    );
  };

  @action _getCardanoAdaApp = async (isConnected = false) => {
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
      return extendedPublicKey;
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
    this.isDeviceConnected = true;
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
  };

  @action _setTransport = (transport) => {
    this.transport = transport;
    const deviceModel = get(transport, ['deviceModel', 'id'], null);
    this.isLedger = deviceModel === 'nanoS' || deviceModel === 'nanoX';
    this.isTrezor = deviceModel === 'trezor';
  }
}
