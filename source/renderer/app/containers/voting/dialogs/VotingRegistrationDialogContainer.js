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
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const votingConfig_1 = require('../../../config/votingConfig');
const VotingRegistrationDialogWizard_1 = __importDefault(
  require('../../../components/voting/VotingRegistrationDialogWizard')
);
const ConfirmationDialog_1 = __importDefault(
  require('../../../components/voting/voting-registration-wizard-steps/widgets/ConfirmationDialog')
);
const FormattedHTMLMessageWithLink_1 = require('../../../components/widgets/FormattedHTMLMessageWithLink');
const formatters_1 = require('../../../utils/formatters');
const messages = (0, react_intl_1.defineMessages)({
  votingRegistrationStep1Label: {
    id: 'voting.votingRegistration.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on voting registration.',
  },
  votingRegistrationStep2Label: {
    id: 'voting.votingRegistration.steps.step.2.label',
    defaultMessage: '!!!Register',
    description: 'Step 2 label text on voting registration.',
  },
  votingRegistrationStep3Label: {
    id: 'voting.votingRegistration.steps.step.3.label',
    defaultMessage: '!!!Confirm',
    description: 'Step 3 label text on voting registration.',
  },
  votingRegistrationStep4Label: {
    id: 'voting.votingRegistration.steps.step.4.label',
    defaultMessage: '!!!PIN',
    description: 'Step 4 label text on voting registration.',
  },
  votingRegistrationStep5Label: {
    id: 'voting.votingRegistration.steps.step.5.label',
    defaultMessage: 'QR code',
    description: 'Step 5 label text on voting registration.',
  },
});
let VotingRegistrationDialogContainer = class VotingRegistrationDialogContainer extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  componentDidMount() {
    this._isMounted = true;
  }
  componentWillUnmount() {
    this._isMounted = false;
    this.props.actions.voting.resetRegistration.trigger();
  }
  handleIsWalletAcceptable = (
    isLegacy,
    isRestoring,
    walletAmount,
    // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    walletReward = 0
  ) =>
    !isLegacy &&
    !isRestoring &&
    walletAmount &&
    walletAmount.gte(
      new bignumber_js_1.default(
        votingConfig_1.VOTING_REGISTRATION_MIN_WALLET_FUNDS
      )
    ) &&
    !walletAmount.isEqualTo(walletReward);
  get selectedWalletId() {
    return (0, lodash_1.get)(
      this.props,
      ['stores', 'voting', 'selectedWalletId'],
      null
    );
  }
  state = {
    selectedWalletId: this.selectedWalletId,
    transactionFee: null,
    transactionFeeError: null,
  };
  STEPS_LIST = [
    this.context.intl.formatMessage(messages.votingRegistrationStep1Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep2Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep3Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep4Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep5Label),
  ];
  handleClose = (showConfirmationDialog) => {
    if (showConfirmationDialog) {
      this.props.actions.voting.showConfirmationDialog.trigger();
    } else {
      this.props.actions.dialogs.closeActiveDialog.trigger();
    }
  };
  handleRestart = () => {
    this.props.actions.voting.resetRegistration.trigger();
    this.props.stores.hardwareWallets.sendMoneyRequest.reset();
  };
  handleContinue = () => {
    this.props.actions.voting.nextRegistrationStep.trigger();
  };
  handleBack = () => {
    this.props.actions.voting.previousRegistrationStep.trigger();
  };
  handleSelectWallet = (walletId) => {
    this.setState({
      selectedWalletId: walletId,
    });
    this.props.actions.voting.selectWallet.trigger(walletId);
    this._handleCalculateTransactionFee();
    this.handleContinue();
  };
  handleSetPinCode = (code) => {
    this.props.actions.voting.generateQrCode.trigger(code);
  };
  handleSendTransaction = (spendingPassword) => {
    const amount = (0, formatters_1.formattedAmountToLovelace)(
      `${votingConfig_1.VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
    this.props.actions.voting.sendTransaction.trigger({
      amount,
      passphrase: spendingPassword,
    });
  };
  render() {
    const {
      selectedWalletId,
      transactionFee,
      transactionFeeError,
    } = this.state;
    const {
      wallets,
      staking,
      voting,
      app,
      hardwareWallets,
    } = this.props.stores;
    const { closeConfirmationDialog, saveAsPDF } = this.props.actions.voting;
    const { all } = wallets;
    const { stakePools, getStakePoolById } = staking;
    const {
      isConfirmationDialogOpen,
      registrationStep,
      getWalletPublicKeyRequest,
      createVotingRegistrationTransactionRequest,
      signMetadataRequest,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      qrCode,
    } = voting;
    const { openExternalLink } = app;
    const {
      hwDeviceStatus,
      checkIsTrezorByWalletId,
      sendMoneyRequest,
      isTransactionPending: isHwTransactionPending,
    } = hardwareWallets;
    const selectedWallet = (0, lodash_1.find)(
      all,
      (wallet) => wallet.id === selectedWalletId
    );
    let isTrezor = false;
    let isHardwareWallet = false;
    if (selectedWallet) {
      isTrezor = checkIsTrezorByWalletId(selectedWallet.id);
      isHardwareWallet = selectedWallet.isHardwareWallet;
    }
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(VotingRegistrationDialogWizard_1.default, {
        onClose: this.handleClose,
        stepsList: this.STEPS_LIST,
        activeStep: registrationStep,
        stakePoolsList: stakePools,
        wallets: all,
        minVotingRegistrationFunds:
          votingConfig_1.VOTING_REGISTRATION_MIN_WALLET_FUNDS,
        isWalletAcceptable: this.handleIsWalletAcceptable,
        selectedWallet: selectedWallet,
        getStakePoolById: getStakePoolById,
        onContinue: this.handleContinue,
        onSelectWallet: this.handleSelectWallet,
        onSetPinCode: this.handleSetPinCode,
        onSubmit: this.handleSendTransaction,
        onRestart: this.handleRestart,
        onBack: this.handleBack,
        transactionFee: transactionFee,
        transactionFeeError: transactionFeeError,
        qrCode: qrCode,
        onDownloadPDF: saveAsPDF.trigger,
        isTransactionPending:
          isTransactionPending || (isHardwareWallet && isHwTransactionPending),
        isTransactionConfirmed: isTransactionConfirmed,
        transactionConfirmations: transactionConfirmations,
        hwDeviceStatus: hwDeviceStatus,
        transactionError:
          getWalletPublicKeyRequest.error ||
          createVotingRegistrationTransactionRequest.error ||
          signMetadataRequest.error ||
          (isHardwareWallet && sendMoneyRequest.error),
        onExternalLinkClick: openExternalLink,
        isTrezor: isTrezor,
        isHardwareWallet: isHardwareWallet,
      }),
      isConfirmationDialogOpen &&
        react_1.default.createElement(ConfirmationDialog_1.default, {
          onConfirm: closeConfirmationDialog.trigger,
          onCancel: () => {
            this.props.actions.dialogs.closeActiveDialog.trigger();
          },
        })
    );
  }
  async _handleCalculateTransactionFee() {
    const {
      transactions,
      addresses,
      app,
      wallets,
      hardwareWallets,
      voting,
    } = this.props.stores;
    const { calculateTransactionFee } = transactions;
    const { getAddressesByWalletId } = addresses;
    const { getWalletById } = wallets;
    const {
      selectCoins,
      initiateTransaction,
      updateTxSignRequest,
    } = hardwareWallets;
    const { prepareVotingData } = voting;
    const amount = (0, formatters_1.formattedAmountToLovelace)(
      `${votingConfig_1.VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
    this.setState({
      transactionFee: null,
      transactionFeeError: null,
    });
    try {
      const selectedWallet = getWalletById(this.selectedWalletId);
      const [address] = await getAddressesByWalletId(this.selectedWalletId);
      const isHardwareWallet = (0, lodash_1.get)(
        selectedWallet,
        'isHardwareWallet',
        false
      );
      let fee;
      let votingData;
      if (isHardwareWallet) {
        votingData = await prepareVotingData({
          walletId: this.selectedWalletId,
        });
        const coinSelection = await selectCoins({
          walletId: this.selectedWalletId,
          address: address.id,
          amount,
          metadata: votingData.metadata,
        });
        updateTxSignRequest(coinSelection);
        fee = coinSelection.fee;
      } else {
        ({ fee } = await calculateTransactionFee({
          walletId: this.selectedWalletId,
          address: address.id,
          amount,
        }));
      }
      if (this._isMounted) {
        this.setState({
          transactionFee: fee,
          transactionFeeError: null,
        });
      }
      if (isHardwareWallet) {
        await initiateTransaction({
          walletId: this.selectedWalletId,
          votingData,
        });
      }
    } catch (error) {
      const errorHasLink = !!(0, lodash_1.get)(error, ['values', 'linkLabel']);
      const transactionFeeError = errorHasLink
        ? react_1.default.createElement(
            FormattedHTMLMessageWithLink_1.FormattedHTMLMessageWithLink,
            { message: error, onExternalLinkClick: app.openExternalLink }
          )
        : this.context.intl.formatMessage(error);
      if (this._isMounted) {
        this.setState({
          transactionFee: new bignumber_js_1.default(0),
          transactionFeeError,
        });
      }
    }
  }
};
VotingRegistrationDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  VotingRegistrationDialogContainer
);
exports.default = VotingRegistrationDialogContainer;
//# sourceMappingURL=VotingRegistrationDialogContainer.js.map
