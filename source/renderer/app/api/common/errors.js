'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.ApiMethodNotYetImplementedError = exports.GenericApiError = exports.messages = void 0;
const react_intl_1 = require('react-intl');
const LocalizableError_1 = __importDefault(
  require('../../i18n/LocalizableError')
);
exports.messages = (0, react_intl_1.defineMessages)({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred.',
    description: 'Generic error message.',
  },
  apiMethodNotYetImplementedError: {
    id: 'api.errors.ApiMethodNotYetImplementedError',
    defaultMessage: '!!!This API method is not yet implemented.',
    description: '"This API method is not yet implemented." error message.',
  },
});
class GenericApiError extends LocalizableError_1.default {
  // @ts-ignore ts-migrate(1015) FIXME: Parameter cannot have question mark and initialize... Remove this comment to see the full error message
  constructor(values = {}) {
    super({
      id: exports.messages.genericApiError.id,
      defaultMessage: exports.messages.genericApiError.defaultMessage,
      values,
    });
  }
}
exports.GenericApiError = GenericApiError;
class ApiMethodNotYetImplementedError extends LocalizableError_1.default {
  constructor() {
    super({
      id: exports.messages.apiMethodNotYetImplementedError.id,
      defaultMessage:
        exports.messages.apiMethodNotYetImplementedError.defaultMessage,
    });
  }
}
exports.ApiMethodNotYetImplementedError = ApiMethodNotYetImplementedError;
//# sourceMappingURL=errors.js.map
