// @flow
import { action } from 'mobx';
import { isEqual, remove } from 'lodash';
import Request from './Request';
import type { ApiCallType } from './Request';

export default class CachedRequest extends Request {

  _apiCalls: Array<ApiCallType> = [];
  _isInvalidated: bool = true;

  execute(...callArgs: Array<any>): CachedRequest {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;

    // Very simple caching strategy -> only continue if the call / args changed
    // or the request was invalidated manually from outside
    const existingApiCall = this._findApiCall(callArgs);

    // Invalidate if new or different api call will be done
    if (existingApiCall && existingApiCall !== this._currentApiCall) {
      this._isInvalidated = true;
      this._currentApiCall = existingApiCall;
    } else if (!existingApiCall) {
      this._isInvalidated = true;
      this._currentApiCall = this._addApiCall(callArgs);
    }

    // Do not continue if this request is not invalidated (see above)
    if (!this._isInvalidated) return this;

    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(action(() => {
      this.isExecuting = true;
      // Apply the previous result from this call immediately (cached)
      if (existingApiCall) {
        this.result = existingApiCall.result;
      }
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
            this._isInvalidated = false;
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
    return this;
  }

  invalidate(options: { immediately: bool } = { immediately: false }): CachedRequest {
    this._isInvalidated = true;
    if (options.immediately && this._currentApiCall) {
      return this.execute(...this._currentApiCall.args);
    }
    return this;
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
  patch(modify: Function): Promise<any> {
    return new Promise((resolve) => {
      setTimeout(action(() => {
        const override = modify(this.result);
        if (override !== undefined) this.result = override;
        if (this._currentApiCall) this._currentApiCall.result = this.result;
        resolve(this);
      }), 0);
    });
  }

  removeCacheForCallWith(...args: Array<any>): Array<ApiCallType> {
    return remove(this._apiCalls, c => isEqual(c.args, args));
  }

  _addApiCall(args: Array<any>): ApiCallType {
    const newCall = { args, result: null };
    this._apiCalls.push(newCall);
    return newCall;
  }

  _findApiCall(args: Array<any>): ?ApiCallType {
    return this._apiCalls.find(c => isEqual(c.args, args));
  }

}
