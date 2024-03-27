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
const DelegationSetupWizardDialog_1 = __importDefault(
  require('../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const messages = (0, react_intl_1.defineMessages)({
  learnMoreLinkUrl: {
    id: 'staking.delegationSetup.intro.step.dialog.learnMore.url',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/',
    description:
      '"Learn more" link URL on the delegation setup "intro" dialog.',
  },
  delegationSetupStep1Label: {
    id: 'staking.delegationSetup.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on delegation steps dialog.',
  },
  delegationSetupStep2Label: {
    id: 'staking.delegationSetup.steps.step.2.label',
    defaultMessage: '!!!Stake pool',
    description: 'Step 2 label text on delegation steps dialog.',
  },
  delegationSetupStep3Label: {
    id: 'staking.delegationSetup.steps.step.3.label',
    defaultMessage: '!!!Confirmation',
    description: 'Step 3 label text on delegation steps dialog.',
  },
});
let DelegationSetupWizardDialogContainer = class DelegationSetupWizardDialogContainer extends react_1.Component {
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
  }
  handleIsWalletAcceptable = (
    walletAmount,
    // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    walletReward = 0
  ) =>
    walletAmount &&
    walletAmount.gte(
      new bignumber_js_1.default(stakingConfig_1.MIN_DELEGATION_FUNDS)
    ) &&
    !walletAmount.isEqualTo(walletReward);
  get selectedWalletId() {
    return (0, lodash_1.get)(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }
  get selectedPoolId() {
    return (0, lodash_1.get)(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'poolId'],
      null
    );
  }
  state = {
    activeStep: 0,
    selectedWalletId: this.selectedWalletId,
    selectedPoolId: this.selectedPoolId,
    stakePoolJoinFee: null,
  };
  STEPS_LIST = [
    this.context.intl.formatMessage(messages.delegationSetupStep1Label),
    this.context.intl.formatMessage(messages.delegationSetupStep2Label),
    this.context.intl.formatMessage(messages.delegationSetupStep3Label),
  ];
  handleDialogClose = () => {
    const { stores, actions } = this.props;
    stores.staking.joinStakePoolRequest.reset();
    actions.dialogs.closeActiveDialog.trigger();
    stores.hardwareWallets._resetTransaction({
      cancelDeviceAction: true,
    });
  };
  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({
      activeStep: activeStep + 1,
    });
  };
  onBack = () => {
    const { activeStep } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.setState({
      activeStep: activeStep - 1,
    });
  };
  handleLearnMoreClick = (event) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };
  handleConfirm = (spendingPassword, isHardwareWallet) => {
    const { selectedPoolId, selectedWalletId } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.props.actions.staking.joinStakePool.trigger({
      stakePoolId: selectedPoolId,
      walletId: selectedWalletId,
      passphrase: spendingPassword,
      isHardwareWallet,
    });
  };
  handleSelectWallet = (walletId) => {
    this.setState({
      selectedWalletId: walletId,
    });
    this.props.actions.staking.selectDelegationWallet.trigger(walletId);
    this.handleContinue();
  };
  handleChoosePool = (poolId) => {
    this.setState({
      selectedPoolId: poolId,
    });
  };
  handleSelectPool = (pool) => {
    this._handleCalculateTransactionFee(pool.id);
    this.handleContinue();
  };
  render() {
    const {
      activeStep,
      selectedWalletId,
      selectedPoolId,
      stakePoolJoinFee,
    } = this.state;
    const {
      app,
      staking,
      wallets,
      profile,
      networkStatus,
      hardwareWallets,
    } = this.props.stores;
    const { futureEpoch } = networkStatus;
    const { currentTheme, currentLocale } = profile;
    const {
      hwDeviceStatus,
      sendMoneyRequest,
      selectCoinsRequest,
      checkIsTrezorByWalletId,
    } = hardwareWallets;
    const {
      stakePools,
      recentStakePools,
      joinStakePoolRequest,
      getStakePoolById,
      isDelegationTransactionPending,
      maxDelegationFunds,
    } = staking;
    const futureEpochStartTime = (0, lodash_1.get)(
      futureEpoch,
      'epochStart',
      0
    );
    const selectedPool = (0, lodash_1.find)(
      stakePools,
      (pool) => pool.id === selectedPoolId
    );
    const selectedWallet = (0, lodash_1.find)(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );
    const acceptableWallets = (0, lodash_1.find)(
      wallets.allWallets,
      ({ amount, reward }) => this.handleIsWalletAcceptable(amount, reward)
    );
    let isTrezor = false;
    if (selectedWallet) {
      isTrezor = checkIsTrezorByWalletId(selectedWallet.id);
    }
    return react_1.default.createElement(
      DelegationSetupWizardDialog_1.default,
      {
        wallets: wallets.allWallets,
        stepsList: this.STEPS_LIST,
        activeStep: activeStep,
        minDelegationFunds: stakingConfig_1.MIN_DELEGATION_FUNDS,
        isDisabled: activeStep === 1 && !acceptableWallets,
        isWalletAcceptable: this.handleIsWalletAcceptable,
        maxDelegationFunds: maxDelegationFunds,
        selectedWallet: selectedWallet,
        selectedPool: selectedPool || null,
        stakePoolsList: stakePools,
        recentStakePools: (0, lodash_1.take)(
          recentStakePools,
          stakingConfig_1.RECENT_STAKE_POOLS_COUNT
        ),
        stakePoolJoinFee: stakePoolJoinFee,
        futureEpochStartTime: futureEpochStartTime,
        currentLocale: currentLocale,
        onOpenExternalLink: app.openExternalLink,
        currentTheme: currentTheme,
        onClose: this.handleDialogClose,
        onContinue: this.handleContinue,
        onSelectWallet: this.handleSelectWallet,
        onSelectPool: this.handleSelectPool,
        onThumbPoolSelect: this.handleChoosePool,
        onBack: this.onBack,
        onLearnMoreClick: this.handleLearnMoreClick,
        onConfirm: this.handleConfirm,
        getStakePoolById: getStakePoolById,
        isSubmitting:
          joinStakePoolRequest.isExecuting ||
          sendMoneyRequest.isExecuting ||
          isDelegationTransactionPending,
        error:
          joinStakePoolRequest.error ||
          sendMoneyRequest.error ||
          selectCoinsRequest.error,
        hwDeviceStatus: hwDeviceStatus,
        isTrezor: isTrezor,
      }
    );
  }
  async _handleCalculateTransactionFee(poolId) {
    const { staking, uiDialogs, wallets, hardwareWallets } = this.props.stores;
    const { isOpen } = uiDialogs;
    const { calculateDelegationFee } = staking;
    const { selectedWalletId } = this.state;
    const selectedWallet = (0, lodash_1.find)(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );
    let stakePoolJoinFee;
    if (selectedWallet.isHardwareWallet) {
      // Calculate fee from coins selections
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId: selectedWallet.id,
        poolId,
        delegationAction: stakingConfig_1.DELEGATION_ACTIONS.JOIN,
      });
      const { deposits, depositsReclaimed, fee } = coinsSelection;
      stakePoolJoinFee = {
        deposits,
        depositsReclaimed,
        fee,
      };
      // Initiate Transaction (Delegation)
      hardwareWallets.initiateTransaction({
        walletId: selectedWalletId,
      });
    } else {
      stakePoolJoinFee = await calculateDelegationFee({
        walletId: selectedWalletId,
      });
    }
    // Update state only if DelegationSetupWizardDialog is still mounted and active
    // and fee calculation was successful
    if (
      this._isMounted &&
      isOpen(DelegationSetupWizardDialog_1.default) &&
      stakePoolJoinFee
    ) {
      this.setState({
        stakePoolJoinFee,
      });
    }
  }
};
DelegationSetupWizardDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  DelegationSetupWizardDialogContainer
);
exports.default = DelegationSetupWizardDialogContainer;
//# sourceMappingURL=DelegationSetupWizardDialogContainer.js.map
