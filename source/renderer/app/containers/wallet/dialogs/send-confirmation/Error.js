'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.ConfirmationError = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const FormattedHTMLMessageWithLink_1 = require('../../../../components/widgets/FormattedHTMLMessageWithLink');
const styles_scss_1 = __importDefault(require('./styles.scss'));
function ConfirmationError({ error, onExternalLinkClick }) {
  if (!error) {
    return null;
  }
  const errorHasLink = !!error.values.linkLabel;
  return react_1.default.createElement(
    'p',
    { className: styles_scss_1.default.error },
    errorHasLink
      ? react_1.default.createElement(
          FormattedHTMLMessageWithLink_1.FormattedHTMLMessageWithLink,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          {
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            message: error,
            onExternalLinkClick: onExternalLinkClick,
          }
        )
      : react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...error,
        })
  );
}
exports.ConfirmationError = ConfirmationError;
//# sourceMappingURL=Error.js.map
