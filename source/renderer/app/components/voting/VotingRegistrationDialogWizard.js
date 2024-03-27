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
const lodash_1 = require('lodash');
const VotingRegistrationStepsChooseWallet_1 = __importDefault(
  require('./voting-registration-wizard-steps/VotingRegistrationStepsChooseWallet')
);
const VotingRegistrationStepsRegister_1 = __importDefault(
  require('./voting-registration-wizard-steps/VotingRegistrationStepsRegister')
);
const VotingRegistrationStepsConfirm_1 = __importDefault(
  require('./voting-registration-wizard-steps/VotingRegistrationStepsConfirm')
);
const VotingRegistrationStepsEnterPinCode_1 = __importDefault(
  require('./voting-registration-wizard-steps/VotingRegistrationStepsEnterPinCode')
);
const VotingRegistrationStepsQrCode_1 = __importDefault(
  require('./voting-registration-wizard-steps/VotingRegistrationStepsQrCode')
);
let VotingRegistrationDialogWizard = class VotingRegistrationDialogWizard extends react_1.Component {
  render() {
    const {
      stepsList,
      activeStep,
      onContinue,
      onSelectWallet,
      onSetPinCode,
      wallets,
      selectedWallet,
      isWalletAcceptable,
      stakePoolsList,
      minVotingRegistrationFunds,
      getStakePoolById,
      transactionFee,
      transactionFeeError,
      onSubmit,
      qrCode,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      transactionError,
      onDownloadPDF,
      onRestart,
      onExternalLinkClick,
      onClose,
      onBack,
      hwDeviceStatus,
      isTrezor,
      isHardwareWallet,
    } = this.props;
    const selectedWalletId = (0, lodash_1.get)(selectedWallet, 'id', null);
    let content = null;
    switch (activeStep) {
      case 1:
        content = react_1.default.createElement(
          VotingRegistrationStepsChooseWallet_1.default,
          {
            onClose: onClose,
            stepsList: stepsList,
            activeStep: activeStep,
            numberOfStakePools: stakePoolsList.length,
            wallets: wallets,
            minVotingRegistrationFunds: minVotingRegistrationFunds,
            selectedWalletId: selectedWalletId,
            onSelectWallet: onSelectWallet,
            isWalletAcceptable: isWalletAcceptable,
            getStakePoolById: getStakePoolById,
          }
        );
        break;
      case 2:
        content = react_1.default.createElement(
          VotingRegistrationStepsRegister_1.default,
          {
            onClose: onClose,
            stepsList: stepsList,
            activeStep: activeStep,
            transactionFee: transactionFee,
            transactionFeeError: transactionFeeError,
            transactionError: transactionError,
            isSubmitting: isTransactionPending,
            onConfirm: onSubmit,
            onBack: onBack,
            onExternalLinkClick: onExternalLinkClick,
            hwDeviceStatus: hwDeviceStatus,
            selectedWallet: selectedWallet,
            isTrezor: isTrezor,
            isHardwareWallet: isHardwareWallet,
          }
        );
        break;
      case 3:
        content = react_1.default.createElement(
          VotingRegistrationStepsConfirm_1.default,
          {
            onClose: onClose,
            stepsList: stepsList,
            activeStep: activeStep,
            isTransactionPending: isTransactionPending,
            isTransactionConfirmed: isTransactionConfirmed,
            transactionConfirmations: transactionConfirmations,
            transactionError: transactionError,
            onConfirm: onContinue,
            onRestart: onRestart,
          }
        );
        break;
      case 4:
        content = react_1.default.createElement(
          VotingRegistrationStepsEnterPinCode_1.default,
          {
            onSetPinCode: onSetPinCode,
            onClose: onClose,
            stepsList: stepsList,
            activeStep: activeStep,
          }
        );
        break;
      case 5:
        content = react_1.default.createElement(
          VotingRegistrationStepsQrCode_1.default,
          {
            qrCode: qrCode,
            onClose: onClose,
            onDownloadPDF: onDownloadPDF,
            stepsList: stepsList,
            activeStep: activeStep,
          }
        );
        break;
      default:
        content = react_1.default.createElement(react_1.default.Fragment, null);
    }
    return content;
  }
};
VotingRegistrationDialogWizard = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationDialogWizard
);
exports.default = VotingRegistrationDialogWizard;
//# sourceMappingURL=VotingRegistrationDialogWizard.js.map
