// @flow
import { observable, action } from 'mobx';
import { isEqual, remove } from 'lodash';
import Request from './Request';

export default class CachedRequest extends Request {

  _apiCalls = [];
  _isInvalidated = true;

  execute(...callArgs) {
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
    this._promise = this._api[this._method](...callArgs)
      .then((result) => {
        return new Promise((resolve) => {
          setTimeout(action(() => {
            this.result = this._currentApiCall.result = result;
            this.isExecuting = false;
            this.wasExecuted = true;
            this._isInvalidated = false;
            this._isWaitingForResponse = false;
            resolve(result);
          }), 1);
        })
      .catch(action((error) => {
        return new Promise((_, reject) => {
          setTimeout(action(() => {
            this.result = this._currentApiCall.result = result;
            this.isExecuting = false;
            this.isError = true;
            this.wasExecuted = true;
            this._isWaitingForResponse = false;
            reject(error);
          }));
        });
      }));
    });

    this._isWaitingForResponse = true;
    return this;
  }

  invalidate(options = { immediately: false }) {
    this._isInvalidated = true;
    if (options.immediately) return this.execute(...this._currentApiCall.args);
    return this;
  }

  patch(modify) {
    return new Promise((resolve) => {
      setTimeout(action(() => {
        const override = modify(this.result);
        if (override !== undefined) this.result = override;
        if (this._currentApiCall) this._currentApiCall.result = this.result;
        resolve(this);
      }), 0);
    });
  }

  removeCacheForCallWith(...args) {
    remove(this._apiCalls, c => isEqual(c.args, args));
  }

  _addApiCall(args) {
    const newCall = { args, result: null };
    this._apiCalls.push(newCall);
    return newCall;
  }

  _findApiCall(args) {
    return this._apiCalls.find(c => isEqual(c.args, args));
  }

}
