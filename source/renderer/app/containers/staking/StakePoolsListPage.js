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
const StakePools_1 = __importDefault(
  require('../../components/staking/stake-pools/StakePools')
);
const StakePoolsRankingLoader_1 = __importDefault(
  require('../../components/staking/stake-pools/StakePoolsRankingLoader')
);
const DelegationSetupWizardDialogContainer_1 = __importDefault(
  require('./dialogs/DelegationSetupWizardDialogContainer')
);
const DelegationSetupWizardDialog_1 = __importDefault(
  require('../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog')
);
const routes_config_1 = require('../../routes-config');
const withAnalytics_1 = require('../../components/analytics/withAnalytics');
let StakePoolsListPage = class StakePoolsListPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleDelegate = (poolId) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({
      dialog: DelegationSetupWizardDialog_1.default,
    });
    updateDataForActiveDialog.trigger({
      data: {
        poolId,
      },
    });
  };
  onUpdateDelegatingStake = (selectedWalletId, sliderValue) => {
    const {
      actions: { staking: stakingActions },
    } = this.props;
    stakingActions.selectDelegationWallet.trigger(selectedWalletId);
    stakingActions.updateDelegatingStake.trigger(sliderValue);
  };
  onRankStakePools = () => {
    const {
      actions: { staking: stakingActions },
    } = this.props;
    stakingActions.rankStakePools.trigger();
  };
  handleSmashSettingsClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.SETTINGS.STAKE_POOLS,
    });
  };
  render() {
    const {
      uiDialogs,
      staking,
      app,
      networkStatus,
      profile,
      wallets,
    } = this.props.stores;
    const { currentTheme, currentLocale } = profile;
    const { isSynced } = networkStatus;
    const {
      stakePoolsRequest,
      stakePools,
      selectedDelegationWalletId,
      stake,
      fetchingStakePoolsFailed,
      recentStakePools,
      getStakePoolById,
      smashServerUrl,
      maxDelegationFunds,
      isFetchingStakePools,
    } = staking;
    const { all } = wallets;
    const isLoading = !isSynced || fetchingStakePoolsFailed;
    const isRanking =
      !isLoading && staking.isRanking && stakePoolsRequest.isExecuting;
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      react_1.default.createElement(StakePools_1.default, {
        analyticsTracker: this.props.analyticsTracker,
        wallets: all,
        currentLocale: currentLocale,
        stakePoolsList: stakePools,
        stakePoolsDelegatingList: recentStakePools,
        onOpenExternalLink: app.openExternalLink,
        currentTheme: currentTheme,
        updateDelegatingStake: this.onUpdateDelegatingStake,
        rankStakePools: this.onRankStakePools,
        selectedDelegationWalletId: selectedDelegationWalletId,
        stake: stake,
        onDelegate: this.handleDelegate,
        isLoading: isLoading,
        isListViewTooltipVisible: staking.stakePoolsListViewTooltipVisible,
        onListViewVisited: staking.hideStakePoolsListViewTooltip,
        isFetching: isFetchingStakePools,
        isRanking: isRanking,
        getStakePoolById: getStakePoolById,
        smashServerUrl: smashServerUrl,
        onSmashSettingsClick: this.handleSmashSettingsClick,
        maxDelegationFunds: maxDelegationFunds,
      }),
      isRanking &&
        react_1.default.createElement(StakePoolsRankingLoader_1.default, null),
      uiDialogs.isOpen(DelegationSetupWizardDialog_1.default)
        ? react_1.default.createElement(
            DelegationSetupWizardDialogContainer_1.default,
            null
          )
        : null
    );
  }
};
StakePoolsListPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  StakePoolsListPage
);
exports.default = (0, withAnalytics_1.withAnalytics)(StakePoolsListPage);
//# sourceMappingURL=StakePoolsListPage.js.map
