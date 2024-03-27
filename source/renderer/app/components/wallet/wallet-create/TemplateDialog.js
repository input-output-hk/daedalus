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
const WalletCreateDialog_1 = __importDefault(require('./WalletCreateDialog'));
const WalletCreateStyles_scss_1 = __importDefault(
  require('./WalletCreateStyles.scss')
);
class TemplateDialog extends react_1.Component {
  render() {
    const { onContinue, onClose } = this.props;
    return react_1.default.createElement(
      WalletCreateDialog_1.default,
      {
        stepNumber: 1,
        actions: [
          {
            primary: true,
            label: 'Print template',
            onClick: () => {},
          },
          {
            label: 'Continue without template',
            onClick: onContinue,
          },
        ],
        onClose: onClose,
      },
      react_1.default.createElement(
        'div',
        { className: WalletCreateStyles_scss_1.default.component },
        'TEMPLATE STEP CONTENT'
      )
    );
  }
}
exports.default = TemplateDialog;
//# sourceMappingURL=TemplateDialog.js.map
