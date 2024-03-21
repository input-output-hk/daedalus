'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const fp_1 = require('lodash/fp');
const es6_error_1 = __importDefault(require('es6-error'));
class NotExecutedYetError extends es6_error_1.default {
  message =
    'You have to call Request::execute before you can access it as promise';
}
class Request {
  result = null;
  error = null;
  isExecuting = false;
  isError = false;
  wasExecuted = false;
  promise = null;
  _method;
  _isWaitingForResponse = false;
  _currentApiCall = null;
  constructor(method) {
    this._method = method;
  }
  execute(...callArgs) {
    // Do not continue if this request is already loading
    if (this._isWaitingForResponse) return this;
    // This timeout is necessary to avoid warnings from mobx
    // regarding triggering actions as side-effect of getters
    setTimeout(
      (0, mobx_1.action)('Request::execute (setting this.isExecuting)', () => {
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
            (0, mobx_1.action)('Request::execute/then', () => {
              if (
                this.result != null &&
                (0, mobx_1.isObservableArray)(this.result) &&
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
          (0, mobx_1.action)('Request::execute/catch', (error) => {
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
    this._currentApiCall = {
      args: callArgs,
      result: null,
    };
    return this;
  }
  isExecutingWithArgs(...args) {
    return (
      this.isExecuting &&
      this._currentApiCall != null &&
      (0, fp_1.isEqual)(this._currentApiCall.args, args)
    );
  }
  get isExecutingFirstTime() {
    return !this.wasExecuted && this.isExecuting;
  }
  then(...args) {
    if (!this.promise) throw new NotExecutedYetError();
    return this.promise.then(...args);
  }
  catch(...args) {
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
  patch(modify) {
    return new Promise((resolve) => {
      setTimeout(
        (0, mobx_1.action)(() => {
          const override = modify(this.result);
          if (override !== undefined) this.result = override;
          if (this._currentApiCall) this._currentApiCall.result = this.result;
          resolve(this);
        }),
        0
      );
    });
  }
  reset() {
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
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Request.prototype,
  'result',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Request.prototype,
  'error',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Request.prototype,
  'isExecuting',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Request.prototype,
  'isError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Request.prototype,
  'wasExecuted',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Request.prototype,
  'isExecutingFirstTime',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', []),
    __metadata('design:returntype', Request),
  ],
  Request.prototype,
  'reset',
  null
);
exports.default = Request;
//# sourceMappingURL=Request.js.map
