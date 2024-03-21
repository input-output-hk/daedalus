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
const classnames_1 = __importDefault(require('classnames'));
const Stepper_1 = require('@react-polymorph/components/Stepper');
const StepperSkin_1 = require('@react-polymorph/skins/simple/StepperSkin');
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsChooseWalletD... Remove this comment to see the full error message
const DelegationStepsChooseWalletDialog_scss_1 = __importDefault(
  require('./DelegationStepsChooseWalletDialog.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../widgets/DialogBackButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletsDropdown_1 = __importDefault(
  require('../../widgets/forms/WalletsDropdown')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.title',
    defaultMessage: '!!!Delegate wallet',
    description:
      'Title "Delegate wallet" on the delegation setup "choose wallet" step dialog.',
  },
  description: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.description',
    defaultMessage:
      '!!!Choose a wallet that holds the funds you want to delegate. The selected wallet must contain a <span>minimum amount of {minDelegationFunds} ADA</span> for delegation to be an option.',
    description:
      'Description on the delegation setup "choose wallet" step dialog.',
  },
  selectWalletInputLabel: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.selectWalletInputLabel',
    defaultMessage: '!!!Wallet',
    description:
      'Label "Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  selectWalletInputPlaceholder: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select Wallet',
    description:
      'Placeholder "Select Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFunds: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {minDelegationFunds} ADA which is required for delegation to be available. Please select a wallet with <span>a minimum amount of {minDelegationFunds} ADA</span> and click continue.',
    description:
      'errorMinDelegationFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFundsRewardsOnly: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet contains only rewards balances so it cannot be delegated.',
    description:
      'errorMinDelegationFundsRewardsOnly Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for delegation while it’s being synced.',
    description:
      'RestoringWallet Error Label on the delegation setup "choose wallet" step dialog.',
  },
  continueButtonLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "choose wallet" step dialog.',
  },
});
class DelegationStepsChooseWalletDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    selectedWalletId: this.props.selectedWalletId,
  };
  onWalletChange = (selectedWalletId) => {
    this.setState({
      selectedWalletId,
    });
  };
  onSelectWallet = () => {
    const { selectedWalletId } = this.state;
    this.props.onSelectWallet(selectedWalletId);
  };
  getErrorMessage = (wallet) => {
    if (!wallet) {
      return null;
    }
    const { amount, reward, isRestoring } = wallet;
    if (isRestoring) {
      return messages.errorRestoringWallet;
    }
    if (!this.props.isWalletAcceptable(amount, reward)) {
      // Wallet only has Reward balance
      if (!amount.isZero() && amount.isEqualTo(reward)) {
        return messages.errorMinDelegationFundsRewardsOnly;
      }
      // Wallet balance < min delegation funds
      return messages.errorMinDelegationFunds;
    }
    return null;
  };
  render() {
    const { intl } = this.context;
    const { selectedWalletId } = this.state;
    const {
      wallets,
      stepsList,
      minDelegationFunds,
      onClose,
      onBack,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;
    const selectedWallet = wallets.find(
      (wallet) => wallet && wallet.id === selectedWalletId
    );
    const errorMessage = this.getErrorMessage(selectedWallet);
    const error =
      errorMessage &&
      react_1.default.createElement(
        'p',
        {
          className:
            DelegationStepsChooseWalletDialog_scss_1.default.errorMessage,
        },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...errorMessage,
          values: {
            minDelegationFunds,
          },
        })
      );
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsChooseWalletDialog_scss_1.default
        .delegationStepsChooseWalletDialogWrapper,
    ]);
    const contentClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsChooseWalletDialog_scss_1.default.content,
    ]);
    const walletSelectClasses = (0, classnames_1.default)([
      DelegationStepsChooseWalletDialog_scss_1.default.walletSelect,
      error ? DelegationStepsChooseWalletDialog_scss_1.default.error : null,
    ]);
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: this.onSelectWallet.bind(this),
        primary: true,
        disabled: !selectedWalletId || !!error,
      },
    ];
    const stepsIndicatorLabel = react_1.default.createElement(
      react_intl_1.FormattedMessage,
      {
        ...messages.stepIndicatorLabel,
        values: {
          currentStep: 1,
          totalSteps: stepsList.length,
        },
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: stepsIndicatorLabel,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        className: dialogClassName,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
        backButton: react_1.default.createElement(DialogBackButton_1.default, {
          onBack: onBack,
        }),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            DelegationSteps_scss_1.default.delegationStepsIndicatorWrapper,
        },
        react_1.default.createElement(Stepper_1.Stepper, {
          steps: stepsList,
          activeStep: 1,
          skin: StepperSkin_1.StepperSkin,
          labelDisabled: true,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: contentClassName },
        react_1.default.createElement(
          'p',
          {
            className:
              DelegationStepsChooseWalletDialog_scss_1.default.description,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description,
            values: {
              minDelegationFunds,
            },
          })
        ),
        react_1.default.createElement(WalletsDropdown_1.default, {
          className: walletSelectClasses,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
          label: intl.formatMessage(messages.selectWalletInputLabel),
          numberOfStakePools: numberOfStakePools,
          wallets: wallets,
          onChange: (walletId) => this.onWalletChange(walletId),
          placeholder: intl.formatMessage(
            messages.selectWalletInputPlaceholder
          ),
          value: selectedWalletId,
          getStakePoolById: getStakePoolById,
        }),
        error
      )
    );
  }
}
exports.default = DelegationStepsChooseWalletDialog;
//# sourceMappingURL=DelegationStepsChooseWalletDialog.js.map
