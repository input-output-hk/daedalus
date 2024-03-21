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
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const CancelTransactionButton_scss_1 = __importDefault(
  require('./CancelTransactionButton.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  cancelLabel: {
    id: 'wallet.transaction.pending.cancelTransactionButton',
    defaultMessage: '!!!Cancel pending transaction',
    description: 'Label for the cancel pending transaction button',
  },
  removeLabel: {
    id: 'wallet.transaction.failed.removeTransactionButton',
    defaultMessage: '!!!Remove failed transaction',
    description: 'Label for the remove failed transaction button',
  },
});
class CancelTransactionButton extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { onClick, state } = this.props;
    const label = this.context.intl.formatMessage(messages[`${state}Label`]);
    return react_1.default.createElement(Button_1.Button, {
      className: 'attention',
      disabled: false,
      label: label,
      onClick: onClick,
      skin: ButtonSkin_1.ButtonSkin,
      themeOverrides: CancelTransactionButton_scss_1.default,
    });
  }
}
exports.default = CancelTransactionButton;
//# sourceMappingURL=CancelTransactionButton.js.map
