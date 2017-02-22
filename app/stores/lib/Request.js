// @flow
import { observable, action, computed } from 'mobx';
import { isEqual } from 'lodash/fp';
import ExtendableError from 'es6-error';

class NotExecutedYetError extends ExtendableError {
  message = 'You have to call Request::execute before you can access it as promise';
}

export default class Request {

  @observable result: any = null;
  @observable error: any = null;
  @observable isExecuting: bool = false;
  @observable isError: bool = false;
  @observable wasExecuted: bool = false;

  promise: ?Promise<any> = null;

  _api: Object = {};
  _method: string = '';
  _isWaitingForResponse: bool = false;
  _currentApiCall: ?ApiCallType = null;

  constructor(api: Object, method: string) {
    this._api = api;
    this._method = method;
  }

  execute(...callArgs: Array<any>): Request {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;

    if (!this._api[this._method]) {
      throw new Error(`Missing method <${this._method}> on api object:`, this._api);
    }

    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(action(() => {
      this.isExecuting = true;
    }), 0);

    // Issue api call & save it as promise that is handled to update the results of the operation
    this.promise = new Promise((resolve, reject) => {
      this._api[this._method](...callArgs)
        .then((result) => {
          setTimeout(action(() => {
            this.result = result;
            if (this._currentApiCall) this._currentApiCall.result = result;
            this.isExecuting = false;
            this.wasExecuted = true;
            this._isWaitingForResponse = false;
            resolve(result);
          }), 1);
          return result;
        })
        .catch(action((error) => {
          setTimeout(action(() => {
            this.error = error;
            this.isExecuting = false;
            this.isError = true;
            this.wasExecuted = true;
            this._isWaitingForResponse = false;
            reject(error);
          }), 1);
        }));
    });

    this._isWaitingForResponse = true;
    this._currentApiCall = { args: callArgs, result: null };
    return this;
  }

  isExecutingWithArgs(...args: Array<any>): bool {
    return (
      this.isExecuting &&
      (this._currentApiCall != null)
      && isEqual(this._currentApiCall.args, args)
    );
  }

  @computed get isExecutingFirstTime(): bool {
    return !this.wasExecuted && this.isExecuting;
  }

  then(...args: Array<any>): Promise<any> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.then(...args);
  }

  catch(...args: Array<any>): Promise<any> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.catch(...args);
  }

  reset() {
    this.result = null;
    this.error = null;
    this.isError = false;
  }

}

export type ApiCallType = {
  args: Array<any>,
  result: any,
};
