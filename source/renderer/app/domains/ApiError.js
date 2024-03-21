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
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const errors_1 = require('../api/common/errors');
const errors_2 = require('../api/errors');
const logging_1 = require('../utils/logging');
const helper_1 = require('../../../common/utils/helper');
class ApiError {
  tempError = '';
  clause;
  forceSet = false;
  additionalValues = {};
  isFinalError = false;
  id;
  defaultMessage;
  values;
  code;
  constructor(error = {}, logging) {
    // Construct Localizable Error
    const errorCode = error.code ? (0, lodash_1.camelCase)(error.code) : null;
    const localizableError = (0, lodash_1.get)(errors_2.messages, errorCode);
    let humanizedError;
    if (localizableError) {
      this.isFinalError = true;
      humanizedError = {
        id: localizableError.id,
        defaultMessage: localizableError.defaultMessage,
        values: error,
      };
    } else {
      const genericApiError = new errors_1.GenericApiError(error);
      humanizedError = {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'GenericApiEr... Remove this comment to see the full error message
        id: genericApiError.id,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'defaultMessage' does not exist on type '... Remove this comment to see the full error message
        defaultMessage: genericApiError.defaultMessage,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'values' does not exist on type 'GenericA... Remove this comment to see the full error message
        values: genericApiError.values,
      };
    }
    Object.assign(this, { ...humanizedError, code: error.code });
    // Set logging
    this._logError(logging);
  }
  set(
    predefinedError,
    // @ts-ignore ts-migrate(1015) FIXME: Parameter cannot have question mark and initialize... Remove this comment to see the full error message
    force = false,
    values
  ) {
    if (
      predefinedError &&
      !this.clause &&
      (!this.isFinalError || (this.isFinalError && force))
    ) {
      this.tempError = predefinedError;
      this.clause = true;
      this.forceSet = force;
    } else {
      this.clause = false;
    }
    if (values && this.clause) {
      const transformedValues = {};
      (0, lodash_1.map)(values, (val, key) => {
        const translated = (0, lodash_1.get)(errors_2.messages, val);
        if (translated) {
          transformedValues[key] = translated;
        } else {
          transformedValues[key] = val;
        }
      });
      this.additionalValues = transformedValues;
      Object.assign(this, {
        values: { ...this.values, ...transformedValues },
      });
    }
    return this;
  }
  where(type, declaration) {
    if (
      this.clause &&
      (!this.isFinalError || (this.isFinalError && this.forceSet))
    ) {
      this.clause = this.values[type] === declaration;
      if (!this.clause) {
        this.tempError = '';
        if (this.additionalValues) {
          const additionalValuesKeys = (0, lodash_1.keys)(
            this.additionalValues
          );
          this.values = (0, lodash_1.omit)(this.values, additionalValuesKeys);
        }
        if (this.forceSet) {
          this.clause = false;
          this.forceSet = false;
        }
      }
    }
    return this;
  }
  inc(type, declaration) {
    const fullMessage = (0, lodash_1.get)(this.values, type, '');
    if (this.clause && !this.isFinalError && fullMessage) {
      this.clause = (0, lodash_1.includes)(fullMessage, declaration);
      if (!this.clause) {
        this.tempError = '';
      }
    }
    return this;
  }
  result(fallbackError) {
    if (this.isFinalError && !this.forceSet) return this;
    if (this.tempError && errors_2.messages[this.tempError]) {
      Object.assign(this, {
        id: errors_2.messages[this.tempError].id,
        defaultMessage: errors_2.messages[this.tempError].defaultMessage,
        values: this.values,
        code: this.values.code,
      });
      return this;
    }
    if (fallbackError) {
      Object.assign(this, {
        id: errors_2.messages[fallbackError].id,
        defaultMessage: errors_2.messages[fallbackError].defaultMessage,
        values: this.values,
        code: (0, lodash_1.snakeCase)(fallbackError),
      });
      return this;
    }
    return new errors_1.GenericApiError(this.values);
  }
  _logError(logging) {
    if (logging && logging.msg) {
      const { logError, msg } = logging;
      logging_1.logger.error(msg, {
        error: logError ? (0, helper_1.toJS)(this.values) : null,
      });
    }
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ApiError.prototype,
  'tempError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  ApiError.prototype,
  'clause',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ApiError.prototype,
  'forceSet',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ApiError.prototype,
  'additionalValues',
  void 0
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [String, Object, Object]),
    __metadata('design:returntype', void 0),
  ],
  ApiError.prototype,
  'set',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [String, String]),
    __metadata('design:returntype', void 0),
  ],
  ApiError.prototype,
  'where',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [String, String]),
    __metadata('design:returntype', void 0),
  ],
  ApiError.prototype,
  'inc',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [String]),
    __metadata('design:returntype', void 0),
  ],
  ApiError.prototype,
  'result',
  null
);
exports.default = ApiError;
//# sourceMappingURL=ApiError.js.map
