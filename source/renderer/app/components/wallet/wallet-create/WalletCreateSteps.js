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
const WalletCreateSteps_scss_1 = __importDefault(
  require('./WalletCreateSteps.scss')
);
const walletsConfig_1 = require('../../../config/walletsConfig');
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create dialog.',
  },
  stepsCounter: {
    id: 'wallet.create.dialog.stepsCounter',
    defaultMessage: '!!!Step {currentStep} of {totalSteps}',
    description: 'Step counters in the wallet create dialog.',
  },
  instructionsStep: {
    id: 'wallet.create.dialog.instructionsStep',
    defaultMessage: '!!!Instructions',
    description: 'Step "Instructions" in the wallet create dialog.',
  },
  templateStep: {
    id: 'wallet.create.dialog.templateStep',
    defaultMessage: '!!!Template',
    description: 'Step "Template" in the wallet create dialog.',
  },
  mnemonicsStep: {
    id: 'wallet.create.dialog.mnemonicsStep',
    defaultMessage: '!!!Mnemonics',
    description: 'Step "Mnemonics" in the wallet create dialog.',
  },
  validateStep: {
    id: 'wallet.create.dialog.validateStep',
    defaultMessage: '!!!Validate',
    description: 'Step "Validate" in the wallet create dialog.',
  },
  hashImageStep: {
    id: 'wallet.create.dialog.hashImageStep',
    defaultMessage: '!!!Hash & Image',
    description: 'Step "HashImage" in the wallet create dialog.',
  },
  configStep: {
    id: 'wallet.create.dialog.configStep',
    defaultMessage: '!!!Config',
    description: 'Step "Config" in the wallet create dialog.',
  },
});
class WalletCreateSteps extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  get stepsList() {
    return walletsConfig_1.CREATE_WALLET_STEPS.map((stepId) =>
      this.context.intl.formatMessage(messages[`${stepId}Step`])
    );
  }
  render() {
    const { stepNumber } = this.props;
    const currentStep = stepNumber + 1;
    const totalSteps = walletsConfig_1.CREATE_WALLET_STEPS.length;
    return react_1.default.createElement(
      'div',
      null,
      react_1.default.createElement(
        'div',
        { className: WalletCreateSteps_scss_1.default.stepCounter },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.stepsCounter,
          values: {
            currentStep,
            totalSteps,
          },
        })
      ),
      react_1.default.createElement(Stepper_1.Stepper, {
        steps: this.stepsList,
        activeStep: currentStep,
        skin: StepperSkin_1.StepperSkin,
        labelDisabled: true,
      })
    );
  }
}
exports.default = WalletCreateSteps;
//# sourceMappingURL=WalletCreateSteps.js.map
