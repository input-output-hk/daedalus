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
const InstructionsDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/InstructionsDialog')
);
const InstructionsDialogContainer_1 = __importDefault(
  require('./dialogs/paper-wallet-certificate/InstructionsDialogContainer')
);
const PrintDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/PrintDialog')
);
const PrintDialogContainer_1 = __importDefault(
  require('./dialogs/paper-wallet-certificate/PrintDialogContainer')
);
const SecuringPasswordDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog')
);
const SecuringPasswordDialogContainer_1 = __importDefault(
  require('./dialogs/paper-wallet-certificate/SecuringPasswordDialogContainer')
);
const VerificationDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/VerificationDialog')
);
const VerificationDialogContainer_1 = __importDefault(
  require('./dialogs/paper-wallet-certificate/VerificationDialogContainer')
);
const CompletionDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/CompletionDialog')
);
const CompletionDialogContainer_1 = __importDefault(
  require('./dialogs/paper-wallet-certificate/CompletionDialogContainer')
);
const ConfirmationDialog_1 = __importDefault(
  require('../../components/wallet/paper-wallet-certificate/ConfirmationDialog')
);
let PaperWalletCreateCertificatePage = class PaperWalletCreateCertificatePage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  componentDidUpdate() {
    const stepChanged =
      this.props.stores.wallets.certificateStep !== this.state.currentStep;
    if (this.props.stores.wallets.certificateStep && stepChanged) {
      this.onContinue(this.props.stores.wallets.certificateStep);
    }
  }
  CREATE_CERTIFICATE_DIALOGS = [
    'instructions',
    'print',
    'securingPassword',
    'verification',
    'completion',
  ];
  state = {
    currentStep: 0,
    showConfirmationDialog: false,
  };
  render() {
    const { uiDialogs, app } = this.props.stores;
    const { showConfirmationDialog } = this.state;
    const { environment: network } = app;
    let activeDialog = null;
    if (uiDialogs.isOpen(InstructionsDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        InstructionsDialogContainer_1.default,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        {
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue: this.onContinue,
          onClose: this.onClose,
        }
      );
    }
    if (uiDialogs.isOpen(PrintDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        PrintDialogContainer_1.default,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        {
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue: this.onContinue,
          onClose: this.showConfirmationDialog,
        }
      );
    }
    if (uiDialogs.isOpen(SecuringPasswordDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        SecuringPasswordDialogContainer_1.default,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        {
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue: this.onContinue,
          onClose: this.showConfirmationDialog,
        }
      );
    }
    if (uiDialogs.isOpen(VerificationDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        VerificationDialogContainer_1.default,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        {
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue: this.onContinue,
          onClose: this.showConfirmationDialog,
        }
      );
    }
    if (uiDialogs.isOpen(CompletionDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        CompletionDialogContainer_1.default,
        { onClose: this.onClose }
      );
    }
    return react_1.default.createElement(
      'div',
      null,
      activeDialog,
      showConfirmationDialog &&
        react_1.default.createElement(
          ConfirmationDialog_1.default,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          {
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            network: network,
            onCancel: this.hideConfirmationDialog,
            onConfirm: this.onClose,
          }
        )
    );
  }
  onContinue = (nextStep) => {
    const nextDialog = this.CREATE_CERTIFICATE_DIALOGS[nextStep];
    this.switchDialog(nextDialog);
    this.setState({
      currentStep: nextStep,
    });
  };
  onClose = () => {
    this.setState({
      currentStep: 0,
      showConfirmationDialog: false,
    });
    this.props.actions.wallets.closeCertificateGeneration.trigger();
  };
  showConfirmationDialog = () => {
    this.setState({
      showConfirmationDialog: true,
    });
  };
  hideConfirmationDialog = () => {
    this.setState({
      showConfirmationDialog: false,
    });
  };
  switchDialog = (dialog) => {
    switch (dialog) {
      case 'instructions':
        this.props.actions.dialogs.open.trigger({
          dialog: InstructionsDialog_1.default,
        });
        break;
      case 'print':
        this.props.actions.dialogs.open.trigger({
          dialog: PrintDialog_1.default,
        });
        break;
      case 'securingPassword':
        this.props.actions.dialogs.open.trigger({
          dialog: SecuringPasswordDialog_1.default,
        });
        break;
      case 'verification':
        this.props.actions.dialogs.open.trigger({
          dialog: VerificationDialog_1.default,
        });
        break;
      case 'completion':
        this.props.actions.dialogs.open.trigger({
          dialog: CompletionDialog_1.default,
        });
        break;
      default:
        this.onClose();
    }
  };
};
PaperWalletCreateCertificatePage = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  PaperWalletCreateCertificatePage
);
exports.default = PaperWalletCreateCertificatePage;
//# sourceMappingURL=PaperWalletCreateCertificatePage.js.map
