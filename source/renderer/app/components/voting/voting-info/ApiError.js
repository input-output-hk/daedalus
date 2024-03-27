'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const ApiError_scss_1 = __importDefault(require('./ApiError.scss'));
const ApiError_messages_1 = require('./ApiError.messages');
function ApiError({ intl }) {
  return react_1.default.createElement(
    'section',
    { className: ApiError_scss_1.default.root },
    react_1.default.createElement(
      'h1',
      { className: ApiError_scss_1.default.title },
      intl.formatMessage(ApiError_messages_1.messages.title)
    ),
    react_1.default.createElement(
      'span',
      null,
      intl.formatMessage(ApiError_messages_1.messages.description1)
    ),
    react_1.default.createElement(
      'span',
      { className: ApiError_scss_1.default.description2 },
      intl.formatMessage(ApiError_messages_1.messages.description2)
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(ApiError);
//# sourceMappingURL=ApiError.js.map
