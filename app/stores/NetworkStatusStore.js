// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';

export default class NetworkStatusStore extends Store {

  POLLING_INTERVAL = 5000; // How often we poll for network status

  @observable isCardanoConnected = false;
  @observable cardanoStatusRequest = new Request(this.api, 'getWallets');

  constructor(...args) {
    super(...args);
    setInterval(this._updateCardanoStatus, this.POLLING_INTERVAL);
    this._updateCardanoStatus();
  }

  @action _updateCardanoStatus = () => {
    this.cardanoStatusRequest.execute()
      .then(action(() => { this.isCardanoConnected = true; }))
      .catch(action(() => { this.isCardanoConnected = false; }));
  };

}
