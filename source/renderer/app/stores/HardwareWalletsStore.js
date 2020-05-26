// @flow
import { observable, action, runInAction, computed } from 'mobx';
import AppAda, { utils } from "@cardano-foundation/ledgerjs-hw-app-cardano"; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { getHardwareWalletTransportChannel, getExtendedPublicKeyChannel, getCardanoAdaAppChannel } from '../ipc/getHardwareWalletChannel';
// import { SOME_TIMER } from '../config/timingConfig';
// import HardwareWallet, { HardwareWalletTypes } from '../domains/HardwareWallet';
// import type { SomeType } from '../api/hardwareWallet/types';

export default class HardwareWalletsStore extends Store {;
  @observable fetchingDevice: boolean = false;
  @observable transport;
  @observable fetchingDevice: boolean = false;
  @observable extendedPublicKey: string = null;

  // Ledger
  @observable isDeviceConnected: boolean = false;
  @observable connectedDevices: Object = {};

  /* @observable getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  ); */
  // pollingLedgerDeviceIntervalId: ?IntervalID = null;
  //
  // setup() {
  //   this.pollingLedgerDeviceIntervalId = setInterval(
  //     this.getHardwareWalletDevice,
  //     2000
  //   );
  // }
  setup() {
    const {
      hardwareWallets: hardwareWalletsActions,
    } = this.actions;
    hardwareWalletsActions.getHardwareWalletDevice.listen(
      this._getHardwareWalletDevice
    );
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
      return transport;
    } catch (e) {
      console.debug('>>>> TRANSPORT ERROR: ', e);
      if (e.statusCode === 28177) {
        throw new Error('Device is locked');
      }
      if (e.statusCode === 28160) {
        throw new Error('Wrong Ledger app');
      }
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

  _getExtendedPublicKey = async () => {
    console.debug('>>> _getExtendedPublicKey <<<');
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
      return extendedPublicKey;
    } catch (e) {
      console.debug('>>>> extendedPublicKey ERROR: ', e);
      if (e.statusCode === 28177) {
        throw new Error('Error occured');
      }
    }
  };
}
