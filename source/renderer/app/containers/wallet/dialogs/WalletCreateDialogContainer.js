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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const mobx_react_1 = require('mobx-react');
const InstructionsDialogContainer_1 = __importDefault(
  require('./wallet-create/InstructionsDialogContainer')
);
const TemplateDialogContainer_1 = __importDefault(
  require('./wallet-create/TemplateDialogContainer')
);
const MnemonicsDialogContainer_1 = __importDefault(
  require('./wallet-create/MnemonicsDialogContainer')
);
const ValidateDialogContainer_1 = __importDefault(
  require('./wallet-create/ValidateDialogContainer')
);
const HashDialogContainer_1 = __importDefault(
  require('./wallet-create/HashDialogContainer')
);
const ConfigDialogContainer_1 = __importDefault(
  require('./wallet-create/ConfigDialogContainer')
);
const walletsConfig_1 = require('../../../config/walletsConfig');
function CreateWalletAbortConfirmation() {
  return react_1.default.createElement('div', null, 'Are you sure');
}
let WalletCreateDialogContainer = class WalletCreateDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  get containers() {
    return {
      instructions: InstructionsDialogContainer_1.default,
      template: TemplateDialogContainer_1.default,
      mnemonics: MnemonicsDialogContainer_1.default,
      validate: ValidateDialogContainer_1.default,
      hashImage: HashDialogContainer_1.default,
      config: ConfigDialogContainer_1.default,
    };
  }
  get shouldDisplayAbortAlert() {
    return (
      this.currentStep !== null &&
      (this.currentStep < 0 ||
        this.currentStep > walletsConfig_1.CREATE_WALLET_STEPS.length)
    );
  }
  get currentStep() {
    return this.props.stores.wallets.createWalletStep;
  }
  onContinue = () => {
    const {
      createWalletChangeStep,
      createWalletClose,
    } = this.props.actions.wallets;
    if (this.currentStep !== null) {
      if (this.currentStep < walletsConfig_1.CREATE_WALLET_STEPS.length - 1) {
        createWalletChangeStep.trigger();
      } else {
        createWalletClose.trigger();
      }
    }
  };
  onBack = () => {
    this.props.actions.wallets.createWalletChangeStep.trigger(true);
  };
  onClose = () => {
    const { createWalletAbort, createWalletClose } = this.props.actions.wallets;
    if (this.shouldDisplayAbortAlert) {
      createWalletAbort.trigger();
    } else {
      createWalletClose.trigger();
    }
  };
  onAbort = () => this.props.actions.wallets.createWalletAbort.trigger();
  render() {
    const {
      createWalletStep,
      createWalletShowAbortConfirmation,
    } = this.props.stores.wallets;
    if (createWalletStep === null) {
      return null;
    }
    const stepId = walletsConfig_1.CREATE_WALLET_STEPS[createWalletStep];
    const CurrentContainer = this.containers[stepId];
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      createWalletShowAbortConfirmation &&
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ onAbort: () => any; }' is not assignable t... Remove this comment to see the full error message
        react_1.default.createElement(CreateWalletAbortConfirmation, {
          onAbort: this.onAbort,
        }),
      react_1.default.createElement(CurrentContainer, {
        onContinue: this.onContinue,
        onBack: this.onBack,
        onClose: this.onClose,
      })
    );
  }
};
WalletCreateDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletCreateDialogContainer
);
exports.default = WalletCreateDialogContainer;
//# sourceMappingURL=WalletCreateDialogContainer.js.map
