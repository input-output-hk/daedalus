// @flow
import { observable, action, runInAction, computed } from 'mobx';
import '@babel/polyfill'
// import TransportNodeHid from "@ledgerhq/hw-transport-node-hid-noevents";
// import TransportNodeHid from "@ledgerhq/hw-transport-node-hid";
// import TransportNodeHid from "@ledgerhq/hw-transport-node-hid";
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
// import { SOME_TIMER } from '../config/timingConfig';
// import HardwareWallet, { HardwareWalletTypes } from '../domains/HardwareWallet';
// import type { SomeType } from '../api/hardwareWallet/types';

export default class HardwareWalletsStore extends Store {;
  @observable fetchingLedgerDevice = false;
  @observable transport = null;
  /* @observable getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  ); */
  // pollingLedgerDeviceIntervalId: ?IntervalID = null;
  //
  // setup() {
  //   this.pollingLedgerDeviceIntervalId = setInterval(
  //     this.getLedgerDevice,
  //     2000
  //   );
  // }

  @action getLedgerDevice = async () => {
    // const TR = require('@ledgerhq/hw-transport-node-hid').default
    // console.debug('>>> getLedgerDevice: ', TransportNodeHid);
    // const list = await TransportNodeHid.list();
    // console.debug('>>> LIST: ', list);
    this.fetchingLedgerDevice = true;
    // const transport = await TransportNodeHid.create();
    // console.debug('>>> TRANSPORT: ', transport);
  };

  @computed get deviceModel() {
    if (this.transport) {
      console.debug('>>> DEVICE: ', this.transport.model)
    }
    return this.transport && this.transport.model;
  }
}
