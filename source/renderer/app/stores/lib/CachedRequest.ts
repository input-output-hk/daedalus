import { action } from 'mobx';
import { isEqual, remove } from 'lodash';
import Request from './Request';
import type { ApiCallType } from './Request';

export default class CachedRequest<Result, Error> extends Request<
  Result,
  Error
> {
  _apiCalls: Array<ApiCallType> = [];
  _isInvalidated = true;

  execute(...callArgs: Array<any>): CachedRequest<Result, Error> {
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
      action(() => {
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
            action(() => {
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
          action((error) => {
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
    return this;
  }

  invalidate(
    options: {
      immediately: boolean;
    } = {
      immediately: false,
    }
  ): CachedRequest<Result, Error> {
    this._isInvalidated = true;

    if (options.immediately && this._currentApiCall) {
      return this.execute(...this._currentApiCall.args);
    }

    return this;
  }

  removeCacheForCallWith(...args: Array<any>): Array<ApiCallType> {
    return remove(this._apiCalls, (c) => isEqual(c.args, args));
  }

  _addApiCall(args: Array<any>): ApiCallType {
    const newCall = {
      args,
      result: null,
    };

    this._apiCalls.push(newCall);

    return newCall;
  }

  _findApiCall(args: Array<any>): ApiCallType | null | undefined {
    return this._apiCalls.find((c) => isEqual(c.args, args));
  }

  reset(): CachedRequest<Result, Error> {
    super.reset();
    this._isInvalidated = true;
    return this;
  }
}
