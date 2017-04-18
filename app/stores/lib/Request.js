// @flow
import { observable, action, computed } from 'mobx';
import { isEqual } from 'lodash/fp';
import ExtendableError from 'es6-error';

class NotExecutedYetError extends ExtendableError {
  message = 'You have to call Request::execute before you can access it as promise';
}

export type ApiCallType = {
  args: Array<any>,
  result: any,
};

export default class Request<Result, Error> {

  @observable result: ?Result = null;
  @observable error: ?Error = null;
  @observable isExecuting: boolean = false;
  @observable isError: boolean = false;
  @observable wasExecuted: boolean = false;

  promise: ?Promise<Result> = null;

  _method: Function;
  _isWaitingForResponse: boolean = false;
  _currentApiCall: ?ApiCallType = null;

  constructor(method: Function) {
    this._method = method;
  }

  execute(...callArgs: Array<any>): Request<Result, Error> {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;

    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(action(() => {
      this.isExecuting = true;
    }), 0);

    // Issue api call & save it as promise that is handled to update the results of the operation
    this.promise = new Promise((resolve, reject) => {
      this._method(...callArgs)
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

  isExecutingWithArgs(...args: Array<any>): boolean {
    return (
      this.isExecuting &&
      (this._currentApiCall != null)
      && isEqual(this._currentApiCall.args, args)
    );
  }

  @computed get isExecutingFirstTime(): boolean {
    return !this.wasExecuted && this.isExecuting;
  }

  then(...args: Array<any>): Promise<Result> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.then(...args);
  }

  catch(...args: Array<any>): Promise<Error> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.catch(...args);
  }

  reset() {
    this.result = null;
    this.error = null;
    this.isError = false;
  }

}
