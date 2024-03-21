'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const es6_error_1 = __importDefault(require('es6-error'));
class LocalizableError extends es6_error_1.default {
  id;
  defaultMessage;
  values;
  constructor({ id, defaultMessage, values = {} }) {
    if (!id) throw new Error('id:string is required.');
    if (!defaultMessage) throw new Error('defaultMessage:string is required.');
    super(`${id}: ${JSON.stringify(values)}`);
    this.id = id;
    this.defaultMessage = defaultMessage;
    this.values = values;
  }
}
exports.default = LocalizableError;
//# sourceMappingURL=LocalizableError.js.map
