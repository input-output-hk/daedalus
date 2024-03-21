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
const Stepper_1 = require('@react-polymorph/components/Stepper');
const StepperSkin_1 = require('@react-polymorph/skins/simple/StepperSkin');
const WalletRestoreSteps_scss_1 = __importDefault(
  require('./WalletRestoreSteps.scss')
);
const walletRestoreConfig_1 = require('../../../../config/walletRestoreConfig');
const messages = (0, react_intl_1.defineMessages)({
  typeStep: {
    id: 'wallet.restore.dialog.typeStep',
    defaultMessage: '!!!Type',
    description: 'Step "Type" in the wallet restore dialog.',
  },
  mnemonicsStep: {
    id: 'wallet.restore.dialog.mnemonicsStep',
    defaultMessage: '!!!Recovery Phrase',
    description: 'Step "Recovery Phrase" in the wallet restore dialog.',
  },
  configurationStep: {
    id: 'wallet.restore.dialog.configurationStep',
    defaultMessage: '!!!Configuration',
    description: 'Step "Configuration" in the wallet restore dialog.',
  },
});
class WalletRestoreSteps extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  get filteredSteps() {
    return walletRestoreConfig_1.RESTORE_WALLET_STEPS.filter(
      (stepId) => stepId !== 'success'
    );
  }
  get stepsList() {
    return this.filteredSteps.map((stepId) =>
      this.context.intl.formatMessage(messages[`${stepId}Step`])
    );
  }
  render() {
    const { stepNumber } = this.props;
    const currentStep = stepNumber + 1;
    return react_1.default.createElement(
      'div',
      { className: WalletRestoreSteps_scss_1.default.component },
      react_1.default.createElement(Stepper_1.Stepper, {
        steps: this.stepsList,
        activeStep: currentStep,
        skin: StepperSkin_1.StepperSkin,
        labelDisabled: true,
      })
    );
  }
}
exports.default = WalletRestoreSteps;
//# sourceMappingURL=WalletRestoreSteps.js.map
