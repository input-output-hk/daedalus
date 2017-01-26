// @flow
import { observable } from 'mobx';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/Request';

export default class AdaRedemptionStore extends Store {

  @observable redeemAdaRequest = new Request(this.api, 'redeemAda');

  constructor(...args) {
    super(...args);
    this.actions.redeemAda.listen(this._redeemAda);
  }

  _redeemAda = (params) => {
    console.log(params);
    const result = ipcRenderer.sendSync('extract-redemption-code-from-pdf', params.certificate.path);
    console.log(result);
  };

}
