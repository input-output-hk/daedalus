// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';

export default class NetworkStatusStore extends Store {

  POLLING_INTERVAL = 5000; // How often we poll for network status
  UPDATE_TIMEOUT = 2000; // Simulates a "connecting" status for the user (we have too low latency)

  @observable isCardanoConnected = false;
  @observable isCardanoConnecting = false;
  @observable cardanoStatusRequest = new Request(this.api, 'getWallets');

  constructor(...args) {
    super(...args);
    setInterval(this._updateCardanoStatus, this.POLLING_INTERVAL);
    this._updateCardanoStatus();
  }

  @computed get cardanoStatus() {
    if (this.isCardanoConnected) return 'connected';
    if (this.isCardanoConnecting) return 'connecting';
    return 'disconnected';
  }

  @action _updateCardanoStatus = () => {
    this.cardanoStatusRequest.execute();
    this.isCardanoConnecting = true;
    // Since we have very low latency we would only be in "connecting" state for a few milliseconds
    setTimeout(action(() => {
      this.cardanoStatusRequest
        .then(action(() => { this.isCardanoConnected = true; }))
        .catch(action(() => { this.isCardanoConnected = false; }));
      this.isCardanoConnecting = false;
    }), this.UPDATE_TIMEOUT);
  };

}
