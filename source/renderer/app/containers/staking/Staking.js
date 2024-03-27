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
const MainLayout_1 = __importDefault(require('../MainLayout'));
const VerticalFlexContainer_1 = __importDefault(
  require('../../components/layout/VerticalFlexContainer')
);
const StakingUnavailable_1 = __importDefault(
  require('../../components/staking/StakingUnavailable')
);
const StakingWithNavigation_1 = __importDefault(
  require('../../components/staking/layouts/StakingWithNavigation')
);
const DelegationSetupWizardDialog_1 = __importDefault(
  require('../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog')
);
const UndelegateWalletConfirmationDialog_1 = __importDefault(
  require('../../components/wallet/settings/UndelegateWalletConfirmationDialog')
);
const routes_config_1 = require('../../routes-config');
const routing_1 = require('../../utils/routing');
const stakingConfig_1 = require('../../config/stakingConfig');
let Staking = class Staking extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  componentDidMount() {
    this.handleDelegationRoute();
  }
  handleDelegationRoute = () => {
    const {
      actions,
      stores: { staking },
    } = this.props;
    if (staking.showCountdown() && !staking.isStakingDelegationCountdown) {
      return actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.STAKING.COUNTDOWN,
      });
    }
    if (!staking.showCountdown() && staking.isStakingDelegationCountdown) {
      return actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.STAKING.INFO,
      });
    }
    return true;
  };
  isActiveNavItem = (page, item) => {
    const { app } = this.props.stores;
    const { options } = item;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'length' does not exist on type 'never'.
    if (options && options.length) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'forEach' does not exist on type 'never'.
      options.forEach((option) => {
        if (
          app.currentRoute &&
          app.currentRoute.includes(option.value.toString())
        ) {
          page = option.value.toString();
        }
      });
    }
    const screenRoute = (0, routing_1.buildRoute)(
      routes_config_1.ROUTES.STAKING.PAGE,
      {
        page,
      }
    );
    return app.currentRoute === screenRoute;
  };
  handleNavItemClick = (page) => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.STAKING.PAGE,
      params: {
        page,
      },
    });
  };
  render() {
    const {
      stores: { app, staking, networkStatus, uiDialogs },
      children,
    } = this.props;
    const {
      isSynced,
      syncPercentage,
      isAlonzoPending,
      isAlonzoActivated,
    } = networkStatus;
    const { isStakingDelegationCountdown } = staking;
    const shouldShowInfoTab = isAlonzoPending || isAlonzoActivated;
    const isDelegationWizardOpen = uiDialogs.isOpen(
      DelegationSetupWizardDialog_1.default
    );
    const isUndelegationWizardOpen = uiDialogs.isOpen(
      UndelegateWalletConfirmationDialog_1.default
    );
    if (!isSynced && !(isDelegationWizardOpen || isUndelegationWizardOpen)) {
      return react_1.default.createElement(
        MainLayout_1.default,
        null,
        react_1.default.createElement(
          VerticalFlexContainer_1.default,
          null,
          react_1.default.createElement(StakingUnavailable_1.default, {
            syncPercentage: syncPercentage,
          })
        )
      );
    }
    return react_1.default.createElement(
      MainLayout_1.default,
      null,
      isStakingDelegationCountdown
        ? children
        : react_1.default.createElement(
            StakingWithNavigation_1.default,
            {
              isActiveNavItem: this.isActiveNavItem,
              onNavItemClick: this.handleNavItemClick,
              activeItem: app.currentPage,
              showInfoTab:
                stakingConfig_1.IS_STAKING_INFO_PAGE_AVAILABLE &&
                shouldShowInfoTab,
            },
            children
          )
    );
  }
};
Staking = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  Staking
);
exports.default = Staking;
//# sourceMappingURL=Staking.js.map
