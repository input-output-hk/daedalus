'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const Request_1 = __importDefault(require('./Request'));
class CachedRequest extends Request_1.default {
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
    setTimeout(
      (0, mobx_1.action)(() => {
        this.isExecuting = true;
        // Apply the previous result from this call immediately (cached)
        if (existingApiCall) {
          this.result = existingApiCall.result;
        }
      }),
      0
    );
    // Issue api call & save it as promise that is handled to update the results of the operation
    this.promise = new Promise((resolve, reject) => {
      this._method(...callArgs)
        .then((result) => {
          setTimeout(
            (0, mobx_1.action)(() => {
              this.result = result;
              if (this._currentApiCall) this._currentApiCall.result = result;
              this.isExecuting = false;
              this.wasExecuted = true;
              this._isInvalidated = false;
              this._isWaitingForResponse = false;
              resolve(result);
            }),
            1
          );
          return result;
        })
        .catch(
          (0, mobx_1.action)((error) => {
            setTimeout(
              (0, mobx_1.action)(() => {
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
    return this;
  }
  invalidate(
    options = {
      immediately: false,
    }
  ) {
    this._isInvalidated = true;
    if (options.immediately && this._currentApiCall) {
      return this.execute(...this._currentApiCall.args);
    }
    return this;
  }
  removeCacheForCallWith(...args) {
    return (0, lodash_1.remove)(this._apiCalls, (c) =>
      (0, lodash_1.isEqual)(c.args, args)
    );
  }
  _addApiCall(args) {
    const newCall = {
      args,
      result: null,
    };
    this._apiCalls.push(newCall);
    return newCall;
  }
  _findApiCall(args) {
    return this._apiCalls.find((c) => (0, lodash_1.isEqual)(c.args, args));
  }
  reset() {
    super.reset();
    this._isInvalidated = true;
    return this;
  }
}
exports.default = CachedRequest;
//# sourceMappingURL=CachedRequest.js.map
