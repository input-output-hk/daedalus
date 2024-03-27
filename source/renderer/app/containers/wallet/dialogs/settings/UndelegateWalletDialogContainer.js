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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const UndelegateWalletConfirmationDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/UndelegateWalletConfirmationDialog')
);
const UndelegateWalletSuccessDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/UndelegateWalletSuccessDialog')
);
const stakingConfig_1 = require('../../../../config/stakingConfig');
let UndelegateWalletDialogContainer = class UndelegateWalletDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  state = {
    stakePoolQuitFee: null,
  };
  _isMounted = false;
  componentDidMount() {
    this._isMounted = true;
    this._handleCalculateTransactionFee();
  }
  componentWillUnmount() {
    this._isMounted = false;
  }
  get selectedWalletId() {
    return (0, lodash_1.get)(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }
  async _handleCalculateTransactionFee() {
    const { staking, wallets, hardwareWallets } = this.props.stores;
    const { calculateDelegationFee } = staking;
    const selectedWallet = (0, lodash_1.find)(
      wallets.allWallets,
      (wallet) => wallet.id === this.selectedWalletId
    );
    const { lastDelegatedStakePoolId, delegatedStakePoolId } = selectedWallet;
    const poolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';
    let stakePoolQuitFee;
    if (selectedWallet.isHardwareWallet) {
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId: this.selectedWalletId,
        poolId,
        delegationAction: stakingConfig_1.DELEGATION_ACTIONS.QUIT,
      });
      const { deposits, depositsReclaimed, fee } = coinsSelection;
      stakePoolQuitFee = {
        deposits,
        depositsReclaimed,
        fee,
      };
      hardwareWallets.initiateTransaction({
        walletId: this.selectedWalletId,
      });
    } else {
      stakePoolQuitFee = await calculateDelegationFee({
        walletId: this.selectedWalletId,
      });
      // @TODO Remove this when api returns depositsReclaimed value
      if (stakePoolQuitFee) {
        stakePoolQuitFee.depositsReclaimed = new bignumber_js_1.default(
          stakingConfig_1.DELEGATION_DEPOSIT
        );
      }
    }
    if (this._isMounted && stakePoolQuitFee) {
      this.setState({
        stakePoolQuitFee,
      });
    }
  }
  render() {
    const { actions, stores, onExternalLinkClick } = this.props;
    const {
      wallets,
      staking,
      networkStatus,
      profile,
      hardwareWallets,
    } = stores;
    const { futureEpoch } = networkStatus;
    const { currentLocale } = profile;
    const {
      getStakePoolById,
      quitStakePoolRequest,
      isDelegationTransactionPending,
    } = staking;
    const { getWalletById, undelegateWalletSubmissionSuccess } = wallets;
    const {
      hwDeviceStatus,
      sendMoneyRequest,
      selectCoinsRequest,
      checkIsTrezorByWalletId,
    } = hardwareWallets;
    const { stakePoolQuitFee } = this.state;
    const futureEpochStartTime = (0, lodash_1.get)(
      futureEpoch,
      'epochStart',
      0
    );
    const walletToBeUndelegated = getWalletById(this.selectedWalletId);
    if (!walletToBeUndelegated) return null;
    const isTrezor = checkIsTrezorByWalletId(walletToBeUndelegated.id);
    const { name: walletName } = walletToBeUndelegated;
    const {
      lastDelegatedStakePoolId,
      delegatedStakePoolId,
    } = walletToBeUndelegated;
    const stakePoolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';
    if (
      (!stakePoolId || !isDelegationTransactionPending) &&
      undelegateWalletSubmissionSuccess &&
      !quitStakePoolRequest.error
    ) {
      return react_1.default.createElement(
        UndelegateWalletSuccessDialog_1.default,
        {
          walletName: walletName,
          futureEpochStartTime: futureEpochStartTime,
          currentLocale: currentLocale,
          onClose: () => {
            actions.dialogs.closeActiveDialog.trigger();
            quitStakePoolRequest.reset();
            actions.wallets.setUndelegateWalletSubmissionSuccess.trigger({
              result: false,
            });
          },
        }
      );
    }
    const delegatedStakePool = getStakePoolById(stakePoolId);
    const stakePoolName = (0, lodash_1.get)(delegatedStakePool, 'name', '');
    const stakePoolTicker = (0, lodash_1.get)(delegatedStakePool, 'ticker');
    return react_1.default.createElement(
      UndelegateWalletConfirmationDialog_1.default,
      {
        selectedWallet: walletToBeUndelegated,
        stakePoolName: stakePoolName,
        stakePoolTicker: stakePoolTicker,
        onConfirm: (passphrase, isHardwareWallet) => {
          actions.wallets.undelegateWallet.trigger({
            walletId: this.selectedWalletId,
            passphrase,
            isHardwareWallet,
          });
        },
        onCancel: () => {
          actions.dialogs.closeActiveDialog.trigger();
          quitStakePoolRequest.reset();
          actions.wallets.setUndelegateWalletSubmissionSuccess.trigger({
            result: false,
          });
        },
        onExternalLinkClick: onExternalLinkClick,
        isSubmitting:
          quitStakePoolRequest.isExecuting ||
          sendMoneyRequest.isExecuting ||
          isDelegationTransactionPending,
        error:
          quitStakePoolRequest.error ||
          sendMoneyRequest.error ||
          selectCoinsRequest.error,
        fees: stakePoolQuitFee,
        hwDeviceStatus: hwDeviceStatus,
        isTrezor: isTrezor,
      }
    );
  }
};
UndelegateWalletDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  UndelegateWalletDialogContainer
);
exports.default = UndelegateWalletDialogContainer;
//# sourceMappingURL=UndelegateWalletDialogContainer.js.map
