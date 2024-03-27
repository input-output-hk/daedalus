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
const StepWalletTypeContainer_1 = __importDefault(
  require('./wallet-restore/StepWalletTypeContainer')
);
const StepMnemonicsContainer_1 = __importDefault(
  require('./wallet-restore/StepMnemonicsContainer')
);
const StepConfigurationContainer_1 = __importDefault(
  require('./wallet-restore/StepConfigurationContainer')
);
const StepSuccessContainer_1 = __importDefault(
  require('./wallet-restore/StepSuccessContainer')
);
const walletRestoreConfig_1 = require('../../../config/walletRestoreConfig');
const ConfirmationDialog_1 = __importDefault(
  require('../../../components/wallet/wallet-restore/widgets/ConfirmationDialog')
);
let WalletRestoreContainer = class WalletRestoreContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  get containers() {
    return {
      type: StepWalletTypeContainer_1.default,
      mnemonics: StepMnemonicsContainer_1.default,
      configuration: StepConfigurationContainer_1.default,
      success: StepSuccessContainer_1.default,
    };
  }
  render() {
    const { stores, actions } = this.props;
    const {
      restoreWalletStep,
      restoreWalletShowAbortConfirmation,
    } = stores.wallets;
    const {
      restoreWalletClose,
      restoreWalletCancelClose,
      restoreWalletChangeStep,
    } = actions.wallets;
    if (restoreWalletStep === null) {
      return null;
    }
    const stepId =
      walletRestoreConfig_1.RESTORE_WALLET_STEPS[restoreWalletStep];
    const CurrentContainer = this.containers[stepId];
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      restoreWalletShowAbortConfirmation &&
        react_1.default.createElement(ConfirmationDialog_1.default, {
          onConfirm: () => restoreWalletClose.trigger(),
          onCancel: () => restoreWalletCancelClose.trigger(),
        }),
      react_1.default.createElement(CurrentContainer, {
        onContinue: () => restoreWalletChangeStep.trigger(),
        onBack: () => restoreWalletChangeStep.trigger(true),
        onClose: () => restoreWalletClose.trigger(),
      })
    );
  }
};
WalletRestoreContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletRestoreContainer
);
exports.default = WalletRestoreContainer;
//# sourceMappingURL=WalletRestoreDialogContainer.js.map
