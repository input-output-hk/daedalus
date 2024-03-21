'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DialogContentWithoutAssets = void 0;
const react_1 = __importDefault(require('react'));
const compose_1 = __importDefault(require('lodash/fp/compose'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const global_messages_1 = __importDefault(
  require('../../../../i18n/global-messages')
);
const messages_1 = require('./messages');
const DialogContentWithoutAssets_scss_1 = __importDefault(
  require('./DialogContentWithoutAssets.scss')
);
function Component({
  intl,
  amount,
  receiver,
  transactionFee,
  formattedTotalAmount,
}) {
  return react_1.default.createElement(
    'div',
    { className: DialogContentWithoutAssets_scss_1.default.root },
    react_1.default.createElement(
      'div',
      {
        className:
          DialogContentWithoutAssets_scss_1.default.addressToLabelWrapper,
      },
      react_1.default.createElement(
        'div',
        { className: DialogContentWithoutAssets_scss_1.default.addressToLabel },
        intl.formatMessage(messages_1.messages.addressToLabel)
      ),
      react_1.default.createElement(
        'div',
        { className: DialogContentWithoutAssets_scss_1.default.addressTo },
        receiver
      )
    ),
    react_1.default.createElement(
      'div',
      {
        className: DialogContentWithoutAssets_scss_1.default.amountFeesWrapper,
      },
      react_1.default.createElement(
        'div',
        { className: DialogContentWithoutAssets_scss_1.default.amountWrapper },
        react_1.default.createElement(
          'div',
          { className: DialogContentWithoutAssets_scss_1.default.amountLabel },
          intl.formatMessage(messages_1.messages.amountLabel)
        ),
        react_1.default.createElement(
          'div',
          { className: DialogContentWithoutAssets_scss_1.default.amount },
          amount,
          react_1.default.createElement(
            'span',
            null,
            '\u00A0',
            intl.formatMessage(global_messages_1.default.adaUnit)
          )
        )
      ),
      react_1.default.createElement(
        'div',
        { className: DialogContentWithoutAssets_scss_1.default.feesWrapper },
        react_1.default.createElement(
          'div',
          { className: DialogContentWithoutAssets_scss_1.default.feesLabel },
          intl.formatMessage(messages_1.messages.feesLabel)
        ),
        react_1.default.createElement(
          'div',
          { className: DialogContentWithoutAssets_scss_1.default.fees },
          '+',
          transactionFee,
          react_1.default.createElement(
            'span',
            null,
            '\u00A0',
            intl.formatMessage(global_messages_1.default.adaUnit)
          )
        )
      )
    ),
    react_1.default.createElement(
      'div',
      { className: DialogContentWithoutAssets_scss_1.default.totalAmountLabel },
      intl.formatMessage(messages_1.messages.totalLabel)
    ),
    react_1.default.createElement(
      'div',
      { className: DialogContentWithoutAssets_scss_1.default.totalAmount },
      formattedTotalAmount,
      react_1.default.createElement(
        'span',
        null,
        '\u00A0',
        intl.formatMessage(global_messages_1.default.adaUnit)
      )
    )
  );
}
exports.DialogContentWithoutAssets = (0, compose_1.default)(
  react_intl_1.injectIntl,
  mobx_react_1.observer
)(Component);
//# sourceMappingURL=DialogContentWithoutAssets.js.map
