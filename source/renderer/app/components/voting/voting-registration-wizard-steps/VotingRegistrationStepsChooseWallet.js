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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const classnames_1 = __importDefault(require('classnames'));
const WalletsDropdown_1 = __importDefault(
  require('../../widgets/forms/WalletsDropdown')
);
const VotingRegistrationStepsChooseWallet_scss_1 = __importDefault(
  require('./VotingRegistrationStepsChooseWallet.scss')
);
const VotingRegistrationDialog_1 = __importDefault(
  require('./widgets/VotingRegistrationDialog')
);
const messages = (0, react_intl_1.defineMessages)({
  description: {
    id: 'voting.votingRegistration.chooseWallet.step.description',
    defaultMessage:
      '!!!You can only use one wallet when registering. To maximize rewards and voting power, choose the wallet with the largest balance.',
    description: 'Description on the voting registration "choose wallet" step.',
  },
  selectWalletInputLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.selectWalletInputLabel',
    defaultMessage: '!!!Select a wallet',
    description:
      'Label "Wallet" for select input on the voting registration "choose wallet" step.',
  },
  selectWalletInputPlaceholder: {
    id:
      'voting.votingRegistration.chooseWallet.step.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select a wallet',
    description:
      'Placeholder "Select Wallet" for select input on the voting registration "choose wallet" step.',
  },
  errorMinVotingFunds: {
    id: 'voting.votingRegistration.chooseWallet.step.errorMinVotingFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum required amount of <span>{minVotingRegistrationFunds} ADA</span>. Please select a different wallet with <span>a minimum balance of {minVotingRegistrationFunds} ADA</span>.',
    description:
      'errorMinVotingFunds Error Label on the voting registration "choose wallet" step.',
  },
  errorMinVotingFundsRewardsOnly: {
    id:
      'voting.votingRegistration.chooseWallet.step.errorMinVotingFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet cannot be registered for voting as it contains rewards balance only.',
    description:
      'errorMinVotingFundsRewardsOnly Error Label on the voting registration "choose wallet" step.',
  },
  errorLegacyWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorLegacyWallet',
    defaultMessage:
      '!!!This wallet cannot be registered for voting as it is a legacy Byron wallet.',
    description:
      'Byron wallet error message on the voting registration "choose wallet" step.',
  },
  errorRestoringWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorRestoringWallet',
    defaultMessage:
      '!!!The wallet cannot be registered for voting while it is being synced with the blockchain.',
    description:
      'Restoring wallet error message on the voting registration "choose wallet" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "choose wallet" step.',
  },
});
class VotingRegistrationStepsChooseWallet extends react_1.Component {
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
  render() {
    const { intl } = this.context;
    const { selectedWalletId } = this.state;
    const {
      onClose,
      stepsList,
      activeStep,
      wallets,
      minVotingRegistrationFunds,
      isWalletAcceptable,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const selectedWallet = wallets.find(
      (wallet) => wallet && wallet.id === selectedWalletId
    );
    const { amount, reward, isLegacy, isRestoring } = selectedWallet || {};
    let errorMessage;
    if (
      selectedWallet &&
      !isWalletAcceptable(isLegacy, isRestoring, amount, reward)
    ) {
      // Wallet is a legacy wallet
      if (isLegacy) errorMessage = messages.errorLegacyWallet;
      // Wallet is restoring
      else if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.isEqualTo(reward))
        errorMessage = messages.errorMinVotingFundsRewardsOnly;
      // Wallet balance < min voting registration funds
      else errorMessage = messages.errorMinVotingFunds;
    }
    const error =
      errorMessage &&
      react_1.default.createElement(
        'p',
        {
          className:
            VotingRegistrationStepsChooseWallet_scss_1.default.errorMessage,
        },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...errorMessage,
          values: {
            minVotingRegistrationFunds: new bignumber_js_1.default(
              minVotingRegistrationFunds
            ).toFormat(0),
          },
        })
      );
    const walletSelectClasses = (0, classnames_1.default)([
      VotingRegistrationStepsChooseWallet_scss_1.default.walletSelect,
      error ? VotingRegistrationStepsChooseWallet_scss_1.default.error : null,
    ]);
    const actions = [
      {
        label: buttonLabel,
        onClick: this.onSelectWallet,
        disabled: !selectedWalletId || !!error,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      VotingRegistrationDialog_1.default,
      {
        onClose: () => {
          onClose();
        },
        stepsList: stepsList,
        activeStep: activeStep,
        actions: actions,
        containerClassName:
          VotingRegistrationStepsChooseWallet_scss_1.default.component,
      },
      react_1.default.createElement(
        'p',
        {
          className:
            VotingRegistrationStepsChooseWallet_scss_1.default.description,
        },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.description,
        })
      ),
      react_1.default.createElement(WalletsDropdown_1.default, {
        className: walletSelectClasses,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
        label: intl.formatMessage(messages.selectWalletInputLabel),
        numberOfStakePools: numberOfStakePools,
        wallets: wallets,
        onChange: (walletId) => this.onWalletChange(walletId),
        placeholder: intl.formatMessage(messages.selectWalletInputPlaceholder),
        value: selectedWalletId,
        getStakePoolById: getStakePoolById,
      }),
      error
    );
  }
}
exports.default = VotingRegistrationStepsChooseWallet;
//# sourceMappingURL=VotingRegistrationStepsChooseWallet.js.map
