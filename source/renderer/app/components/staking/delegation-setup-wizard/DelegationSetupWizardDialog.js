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
const DelegationStepsSuccessDialog_1 = __importDefault(
  require('./DelegationStepsSuccessDialog')
);
const DelegationStepsChooseWalletDialog_1 = __importDefault(
  require('./DelegationStepsChooseWalletDialog')
);
const DelegationStepsConfirmationDialog_1 = __importDefault(
  require('./DelegationStepsConfirmationDialog')
);
const DelegationStepsIntroDialog_1 = __importDefault(
  require('./DelegationStepsIntroDialog')
);
const DelegationStepsNotAvailableDialog_1 = __importDefault(
  require('./DelegationStepsNotAvailableDialog')
);
const DelegationStepsChooseStakePoolDialog_1 = __importDefault(
  require('./DelegationStepsChooseStakePoolDialog')
);
const getOversaturationPercentage = (
  selectedWallet,
  selectedPool,
  maxDelegationFunds
) => {
  if (
    !selectedPool ||
    !selectedWallet ||
    (selectedWallet.lastDelegatedStakePoolId ||
      selectedWallet.delegatedStakePoolId) === selectedPool.id
  )
    return 0;
  const percentageIncrease = Number(
    // @ts-ignore ts-migrate(2363) FIXME: The right-hand side of an arithmetic operation mus... Remove this comment to see the full error message
    (100 / maxDelegationFunds) * selectedWallet.availableAmount
  );
  return selectedPool.saturation + percentageIncrease - 100;
};
let DelegationSetupWizardDialog = class DelegationSetupWizardDialog extends react_1.Component {
  componentDidUpdate(prevProps) {
    // On confirm delegation step, wait for API stake pool "join" endpoint response
    // and redirect to "Ta-Da" step
    if (
      prevProps.isSubmitting &&
      !this.props.isSubmitting &&
      !this.props.error
    ) {
      prevProps.onContinue();
    }
  }
  render() {
    const {
      activeStep,
      isDisabled,
      onBack,
      onClose,
      onConfirm,
      onContinue,
      onLearnMoreClick,
      onSelectWallet,
      onSelectPool,
      stepsList,
      wallets,
      minDelegationFunds,
      recentStakePools,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      selectedWallet,
      selectedPool,
      isWalletAcceptable,
      stakePoolJoinFee,
      futureEpochStartTime,
      currentLocale,
      isSubmitting,
      error,
      getStakePoolById,
      hwDeviceStatus,
      isTrezor,
      maxDelegationFunds,
      onThumbPoolSelect,
    } = this.props;
    const selectedWalletId = (0, lodash_1.get)(selectedWallet, 'id', null);
    const oversaturationPercentage = getOversaturationPercentage(
      selectedWallet,
      selectedPool,
      maxDelegationFunds
    );
    if (isDisabled) {
      return react_1.default.createElement(
        DelegationStepsNotAvailableDialog_1.default,
        { minDelegationFunds: minDelegationFunds, onClose: onClose }
      );
    }
    let content = null;
    switch (activeStep) {
      case 1:
        content = react_1.default.createElement(
          DelegationStepsChooseWalletDialog_1.default,
          {
            numberOfStakePools: stakePoolsList.length,
            stepsList: stepsList,
            wallets: wallets,
            minDelegationFunds: minDelegationFunds,
            selectedWalletId: selectedWalletId,
            onBack: onBack,
            onClose: onClose,
            onSelectWallet: onSelectWallet,
            isWalletAcceptable: isWalletAcceptable,
            getStakePoolById: getStakePoolById,
          }
        );
        break;
      case 2:
        content = react_1.default.createElement(
          DelegationStepsChooseStakePoolDialog_1.default,
          {
            stepsList: stepsList,
            recentStakePools: recentStakePools,
            stakePoolsList: stakePoolsList,
            selectedWallet: selectedWallet,
            onOpenExternalLink: onOpenExternalLink,
            currentTheme: currentTheme,
            selectedPool: selectedPool,
            onClose: onClose,
            onBack: onBack,
            onSelectPool: onThumbPoolSelect,
            onContinue: onSelectPool,
            oversaturationPercentage: oversaturationPercentage,
            onThumbPoolSelect: onThumbPoolSelect,
          }
        );
        break;
      case 3:
        content = react_1.default.createElement(
          DelegationStepsConfirmationDialog_1.default,
          {
            transactionFee: stakePoolJoinFee,
            selectedPool: selectedPool,
            selectedWallet: selectedWallet,
            stepsList: stepsList,
            onClose: onClose,
            onConfirm: onConfirm,
            onBack: onBack,
            isSubmitting: isSubmitting,
            error: error,
            hwDeviceStatus: hwDeviceStatus,
            onExternalLinkClick: onOpenExternalLink,
            isTrezor: isTrezor,
            oversaturationPercentage: oversaturationPercentage,
          }
        );
        break;
      case 4:
        content = react_1.default.createElement(
          DelegationStepsSuccessDialog_1.default,
          {
            delegatedWallet: selectedWallet,
            delegatedStakePool: selectedPool,
            futureEpochStartTime: futureEpochStartTime,
            currentLocale: currentLocale,
            onClose: onClose,
          }
        );
        break;
      default:
        content = react_1.default.createElement(
          DelegationStepsIntroDialog_1.default,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          {
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            onLearnMoreClick: onLearnMoreClick,
            onClose: onClose,
            onContinue: onContinue,
          }
        );
        break;
    }
    return content;
  }
};
DelegationSetupWizardDialog = __decorate(
  [mobx_react_1.observer],
  DelegationSetupWizardDialog
);
exports.default = DelegationSetupWizardDialog;
//# sourceMappingURL=DelegationSetupWizardDialog.js.map
