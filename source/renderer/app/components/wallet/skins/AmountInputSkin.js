'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.messages = void 0;
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const AmountInputSkin_scss_1 = __importDefault(
  require('./AmountInputSkin.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  feesLabel: {
    id: 'wallet.amountInput.feesLabel',
    defaultMessage: '!!!+ {amount} of fees',
    description:
      'Label for the "+ 12.042481 of fees" message above amount input field.',
  },
  calculatingFeesLabel: {
    id: 'wallet.amountInput.calculatingFeesLabel',
    defaultMessage: '!!!Calculating fees',
    description:
      'Label for the "Calculating fees" message above amount input field.',
  },
});
class AmountInputSkin extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { error, fees, total, currency, isCalculatingFees } = this.props;
    const { intl } = this.context;
    return react_1.default.createElement(
      'div',
      { className: AmountInputSkin_scss_1.default.root },
      react_1.default.createElement(InputSkin_1.InputSkin, { ...this.props }),
      isCalculatingFees &&
        react_1.default.createElement(
          'span',
          { className: AmountInputSkin_scss_1.default.calculatingFees },
          intl.formatMessage(exports.messages.calculatingFeesLabel)
        ),
      fees &&
        !error &&
        !isCalculatingFees &&
        react_1.default.createElement(
          'span',
          { className: AmountInputSkin_scss_1.default.fees },
          intl.formatMessage(exports.messages.feesLabel, {
            amount: fees,
          })
        ),
      react_1.default.createElement(
        'span',
        { className: AmountInputSkin_scss_1.default.total },
        total && !error && `= ${total} `,
        currency
      )
    );
  }
}
exports.default = AmountInputSkin;
//# sourceMappingURL=AmountInputSkin.js.map
