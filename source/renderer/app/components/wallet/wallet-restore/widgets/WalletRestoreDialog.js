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
const DialogCloseButton_1 = __importDefault(
  require('../../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../../widgets/Dialog'));
const DialogBackButton_1 = __importDefault(
  require('../../../widgets/DialogBackButton')
);
const WalletRestoreSteps_1 = __importDefault(require('./WalletRestoreSteps'));
const WalletRestoreDialog_scss_1 = __importDefault(
  require('./WalletRestoreDialog.scss')
);
const walletRestoreConfig_1 = require('../../../../config/walletRestoreConfig');
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.restore.dialog.title',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet restore form.',
  },
  dialogTitleSuccess: {
    id: 'wallet.restore.dialog.titleSuccess',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet restore form.',
  },
  stepsCounter: {
    id: 'wallet.restore.dialog.stepsCounter',
    defaultMessage: '!!!Step {currentStep} of {totalSteps}',
    description: 'Step counters in the wallet restore dialog.',
  },
});
class WalletRestoreDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  get filteredSteps() {
    return walletRestoreConfig_1.RESTORE_WALLET_STEPS.filter(
      (stepId) => stepId !== 'success'
    );
  }
  render() {
    const { intl } = this.context;
    const { actions, children, stepNumber, onClose, onBack } = this.props;
    const hasStep = stepNumber !== undefined;
    const title = hasStep
      ? intl.formatMessage(messages.dialogTitle)
      : intl.formatMessage(messages.dialogTitleSuccess);
    const currentStep = (stepNumber || 0) + 1;
    const totalSteps = this.filteredSteps.length;
    const subTitle = react_1.default.createElement(
      react_intl_1.FormattedHTMLMessage,
      {
        ...messages.stepsCounter,
        values: {
          currentStep,
          totalSteps,
        },
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: WalletRestoreDialog_scss_1.default.component,
        title: title,
        subtitle: hasStep && subTitle,
        actions: actions,
        closeOnOverlayClick: false,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
        backButton:
          onBack &&
          react_1.default.createElement(DialogBackButton_1.default, {
            onBack: onBack,
          }),
      },
      hasStep &&
        react_1.default.createElement(WalletRestoreSteps_1.default, {
          stepNumber: stepNumber || 0,
        }),
      children
    );
  }
}
exports.default = WalletRestoreDialog;
//# sourceMappingURL=WalletRestoreDialog.js.map
