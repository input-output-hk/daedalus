import { observable, action, computed, isObservableArray } from 'mobx';
import { isEqual } from 'lodash/fp';
import ExtendableError from 'es6-error';

class NotExecutedYetError extends ExtendableError {
  message =
    'You have to call Request::execute before you can access it as promise';
}

export type ApiCallType = {
  args: Array<any>;
  result: any;
};
export default class Request<Result, Error> {
  @observable
  result: Result | null | undefined = null;
  @observable
  error: Error | null | undefined = null;
  @observable
  isExecuting = false;
  @observable
  isError = false;
  @observable
  wasExecuted = false;
  promise: Promise<Result> | null | undefined = null;
  _method: (...args: Array<any>) => any;
  _isWaitingForResponse = false;
  _currentApiCall: ApiCallType | null | undefined = null;

  constructor(method: (...args: Array<any>) => any) {
    this._method = method;
  }

  execute(...callArgs: Array<any>): Request<Result, Error> {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;
    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(
      action('Request::execute (setting this.isExecuting)', () => {
        this.isExecuting = true;
      }),
      0
    );
    // Issue api call & save it as promise that is handled to update the results of the operation
    this.promise = new Promise((resolve, reject) => {
      if (!this._method)
        reject(new ReferenceError('Request method not defined'));

      this._method(...callArgs)
        .then((result) => {
          setTimeout(
            action('Request::execute/then', () => {
              if (
                this.result != null &&
                isObservableArray(this.result) &&
                Array.isArray(result)
              ) {
                // @ts-ignore
                this.result.replace(result);
              } else {
                this.result = result;
              }

              if (this._currentApiCall) this._currentApiCall.result = result;
              this.isExecuting = false;
              this.wasExecuted = true;
              this._isWaitingForResponse = false;
              resolve(result);
            }),
            1
          );
          return result;
        })
        .catch(
          action('Request::execute/catch', (error) => {
            setTimeout(
              action(() => {
                this.error = error;
                this.isExecuting = false;
                this.isError = true;
                this.wasExecuted = true;
                this._isWaitingForResponse = false;
                reject(error);
              }),
              1
            );
          })
        );
    });
    this._isWaitingForResponse = true;
    this._currentApiCall = {
      args: callArgs,
      result: null,
    };
    return this;
  }

  isExecutingWithArgs(...args: Array<any>): boolean {
    return (
      this.isExecuting &&
      this._currentApiCall != null &&
      isEqual(this._currentApiCall.args, args)
    );
  }

  @computed
  get isExecutingFirstTime(): boolean {
    return !this.wasExecuted && this.isExecuting;
  }

  then(...args: Array<any>): Promise<Result> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.then(...args);
  }

  catch(...args: Array<any>): Promise<any> {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.catch(...args);
  }

  /**
   * Asynchronously patch the result of the request.
   * This can be used for optimistic UI updates before the server has confirmed the change.
   *
   * @param modify {Function} - Custom function to path the result (which gets passed in as
   * only param) You can either change the result directly (e.g: `result.push(something)` or
   * if you need to replace the whole result of the request you need to return it from this
   * function.
   *
   * @returns {Promise}
   */
  patch(modify: (...args: Array<any>) => any): Promise<Request<Result, Error>> {
    return new Promise((resolve) => {
      setTimeout(
        action(() => {
          const override = modify(this.result);
          if (override !== undefined) this.result = override;
          if (this._currentApiCall) this._currentApiCall.result = this.result;
          resolve(this);
        }),
        0
      );
    });
  }

  @action
  reset(): Request<Result, Error> {
    this.result = null;
    this.error = null;
    this.isError = false;
    this.isExecuting = false;
    this.wasExecuted = false;
    this._isWaitingForResponse = false;
    this._currentApiCall = null;
    return this;
  }
}
