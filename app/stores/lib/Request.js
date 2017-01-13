// @flow
import { observable, action, computed } from 'mobx';
import { isEqual } from 'lodash/fp';

export default class Request {

  @observable result = null;
  @observable isExecuting = false;
  @observable wasExecuted = false;

  _promise = null;
  _api = null;
  _method = null;
  _isWaitingForResponse = false;
  _currentApiCall = null;

  constructor(api, method) {
    this._api = api;
    this._method = method;
  }

  execute(...callArgs) {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;

    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(action(() => {
      this.isExecuting = true;
    }), 0);

    // Issue api call & save it as promise that is handled to update the results of the operation
    this._promise = this._api[this._method](...callArgs).then((result) => {
      return new Promise((resolve) => {
        setTimeout(action(() => {
          this.result = result;
          this.isExecuting = false;
          this.wasExecuted = true;
          this._isWaitingForResponse = false;
          resolve(result);
        }), 0);
      });
    });

    this._isWaitingForResponse = true;
    this._currentApiCall = { args: callArgs };
    return this;
  }

  isExecutingWithArgs(...args) {
    return this.isExecuting && this._currentApiCall && isEqual(this._currentApiCall.args, args);
  }

  @computed get isExecutingFirstTime() {
    return !this.wasExecuted && this.isExecuting;
  }

  then(...args) {
    return this._promise.then(...args);
  }

  ['catch'](...args) {
    return this._promise.catch(...args);
  }

}
