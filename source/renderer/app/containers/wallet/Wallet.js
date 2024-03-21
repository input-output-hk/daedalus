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
const WalletWithNavigation_1 = __importDefault(
  require('../../components/wallet/layouts/WalletWithNavigation')
);
const LoadingSpinner_1 = __importDefault(
  require('../../components/widgets/LoadingSpinner')
);
const RestoreNotification_1 = __importDefault(
  require('../../components/notifications/RestoreNotification')
);
const ChangeSpendingPasswordDialog_1 = __importDefault(
  require('../../components/wallet/settings/ChangeSpendingPasswordDialog')
);
const routing_1 = require('../../utils/routing');
const routes_config_1 = require('../../routes-config');
let Wallet = class Wallet extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  isActiveScreen = (page, item) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
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
      routes_config_1.ROUTES.WALLETS.PAGE,
      {
        id: wallets.active.id,
        page,
      }
    );
    return app.currentRoute === screenRoute;
  };
  handleWalletNavItemClick = (page) => {
    const { wallets } = this.props.stores;
    if (!wallets.active) return;
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.PAGE,
      params: {
        id: wallets.active.id,
        page,
      },
    });
  };
  render() {
    const { actions, stores } = this.props;
    const { app, wallets, walletSettings, uiDialogs } = stores;
    const { isOpen: isDialogOpen } = uiDialogs;
    const { restartNode } = actions.networkStatus;
    const { active: activeWallet } = wallets;
    if (!activeWallet) {
      return react_1.default.createElement(
        MainLayout_1.default,
        null,
        react_1.default.createElement(LoadingSpinner_1.default, null)
      );
    }
    const {
      hasNotification,
    } = walletSettings.getWalletsRecoveryPhraseVerificationData(
      activeWallet.id
    );
    const {
      isRestoring,
      isLegacy,
      isNotResponding,
      isHardwareWallet,
      hasPassword,
    } = activeWallet;
    return react_1.default.createElement(
      MainLayout_1.default,
      null,
      isRestoring
        ? react_1.default.createElement(RestoreNotification_1.default, {
            restoreProgress: activeWallet.restorationProgress,
          })
        : null,
      react_1.default.createElement(
        WalletWithNavigation_1.default,
        {
          activeItem: app.currentPage,
          hasNotification: hasNotification,
          hasPassword: hasPassword,
          isActiveScreen: this.isActiveScreen,
          isLegacy: isLegacy,
          isNotResponding: isNotResponding,
          isHardwareWallet: isHardwareWallet,
          isSetWalletPasswordDialogOpen: isDialogOpen(
            ChangeSpendingPasswordDialog_1.default
          ),
          onOpenExternalLink: (url) => stores.app.openExternalLink(url),
          onRestartNode: () => restartNode.trigger(),
          onSetWalletPassword: () => {
            actions.dialogs.open.trigger({
              dialog: ChangeSpendingPasswordDialog_1.default,
            });
          },
          onWalletNavItemClick: this.handleWalletNavItemClick,
        },
        this.props.children
      )
    );
  }
};
Wallet = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  Wallet
);
exports.default = Wallet;
//# sourceMappingURL=Wallet.js.map
