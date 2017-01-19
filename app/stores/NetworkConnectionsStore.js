// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';

export default class NetworkConnectionsStore extends Store {

  @observable isCardanoConnected = false;
  @observable cardanoConnectionRequest = new Request(this.api, 'getWallets');

  constructor(...args) {
    super(...args);
    setInterval(this._updateCardanoStatus, 5000);
  }

  @computed get isCardanoConnecting() {
    return !this.isCardanoConnected && this.cardanoConnectionRequest.isExecuting;
  }

  _updateCardanoStatus = () => {
    this.cardanoConnectionRequest.execute()
      .then(action(() => { this.isCardanoConnected = true; }))
      .catch(action(() => { this.isCardanoConnected = false; }));
  };

}
