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
const DelegationCenter_1 = __importDefault(
  require('../../components/staking/delegation-center/DelegationCenter')
);
const DelegationSetupWizardDialogContainer_1 = __importDefault(
  require('./dialogs/DelegationSetupWizardDialogContainer')
);
const UndelegateWalletDialogContainer_1 = __importDefault(
  require('../wallet/dialogs/settings/UndelegateWalletDialogContainer')
);
const UndelegateWalletConfirmationDialog_1 = __importDefault(
  require('../../components/wallet/settings/UndelegateWalletConfirmationDialog')
);
const DelegationSetupWizardDialog_1 = __importDefault(
  require('../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog')
);
const DelegationCenterNoWallets_1 = __importDefault(
  require('../../components/staking/delegation-center/DelegationCenterNoWallets')
);
const routes_config_1 = require('../../routes-config');
const stakingConfig_1 = require('../../config/stakingConfig');
const STAKE_POOLS_DELEGATING_LIST = 'stakePoolsDelegatingList';
const initialState = {
  selectedList: null,
};
let DelegationCenterPage = class DelegationCenterPage extends react_1.Component {
  static defaultProps = {
    stores: null,
  };
  state = { ...initialState };
  handleSetListActive = (selectedList) =>
    this.setState({
      selectedList,
    });
  handleDelegate = (walletId) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({
      dialog: DelegationSetupWizardDialog_1.default,
    });
    updateDataForActiveDialog.trigger({
      data: {
        walletId,
      },
    });
  };
  handleUndelegate = async (walletId) => {
    const { dialogs } = this.props.actions;
    dialogs.open.trigger({
      dialog: UndelegateWalletConfirmationDialog_1.default,
    });
    dialogs.updateDataForActiveDialog.trigger({
      data: {
        walletId,
      },
    });
  };
  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.ADD,
    });
  };
  render() {
    const { stores } = this.props;
    const { app, uiDialogs, staking, wallets, networkStatus, profile } = stores;
    const { stakePools, getStakePoolById, fetchingStakePoolsFailed } = staking;
    const {
      isSynced,
      networkTip,
      nextEpoch,
      futureEpoch,
      isEpochsInfoAvailable,
      epochLength,
    } = networkStatus;
    const { currentLocale, currentTheme } = profile;
    const { selectedList } = this.state;
    const numberOfRankedStakePools = stakePools.filter(
      (stakePool) =>
        stakingConfig_1.IS_RANKING_DATA_AVAILABLE &&
        stakePool.nonMyopicMemberRewards
    ).length;
    if (!wallets.allWallets.length) {
      return react_1.default.createElement(
        DelegationCenterNoWallets_1.default,
        {
          onGoToCreateWalletClick: this.handleGoToCreateWalletClick,
          minDelegationFunds: stakingConfig_1.MIN_DELEGATION_FUNDS,
        }
      );
    }
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      react_1.default.createElement(DelegationCenter_1.default, {
        wallets: wallets.allWallets,
        numberOfStakePools: stakePools.length,
        numberOfRankedStakePools: numberOfRankedStakePools,
        onDelegate: this.handleDelegate,
        onUndelegate: this.handleUndelegate,
        networkTip: networkTip,
        epochLength: epochLength,
        nextEpoch: nextEpoch,
        futureEpoch: futureEpoch,
        getStakePoolById: getStakePoolById,
        isLoading: !isSynced || fetchingStakePoolsFailed || !stakePools.length,
        isEpochsInfoAvailable: isEpochsInfoAvailable,
        currentLocale: currentLocale,
        onOpenExternalLink: app.openExternalLink,
        currentTheme: currentTheme,
        listName: STAKE_POOLS_DELEGATING_LIST,
        isListActive: selectedList === STAKE_POOLS_DELEGATING_LIST,
        containerClassName: 'StakingWithNavigation_page',
        setListActive: this.handleSetListActive,
      }),
      uiDialogs.isOpen(UndelegateWalletConfirmationDialog_1.default)
        ? react_1.default.createElement(
            UndelegateWalletDialogContainer_1.default,
            { onExternalLinkClick: app.openExternalLink }
          )
        : null,
      uiDialogs.isOpen(DelegationSetupWizardDialog_1.default)
        ? react_1.default.createElement(
            DelegationSetupWizardDialogContainer_1.default,
            null
          )
        : null
    );
  }
};
DelegationCenterPage = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  DelegationCenterPage
);
exports.default = DelegationCenterPage;
//# sourceMappingURL=DelegationCenterPage.js.map
