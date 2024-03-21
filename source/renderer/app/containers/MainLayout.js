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
const Sidebar_1 = __importDefault(require('../components/sidebar/Sidebar'));
const TopBarContainer_1 = __importDefault(require('./TopBarContainer'));
const SidebarLayout_1 = __importDefault(
  require('../components/layout/SidebarLayout')
);
const PaperWalletCreateCertificatePage_1 = __importDefault(
  require('./wallet/PaperWalletCreateCertificatePage')
);
const InstructionsDialog_1 = __importDefault(
  require('../components/wallet/paper-wallet-certificate/InstructionsDialog')
);
const TransferFundsPage_1 = __importDefault(
  require('./wallet/TransferFundsPage')
);
const AssetSettingsDialogContainer_1 = __importDefault(
  require('./assets/AssetSettingsDialogContainer')
);
const routes_config_1 = require('../routes-config');
let MainLayout = class MainLayout extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  handleActivateCategory = (category) => {
    const { actions } = this.props;
    if (category === routes_config_1.ROUTES.PAPER_WALLET_CREATE_CERTIFICATE) {
      actions.dialogs.open.trigger({
        dialog: InstructionsDialog_1.default,
      });
    } else if (category === routes_config_1.ROUTES.NETWORK_INFO) {
      actions.networkStatus.toggleSplash.trigger();
    } else {
      actions.sidebar.activateSidebarCategory.trigger({
        category,
      });
    }
  };
  render() {
    const { actions, stores } = this.props;
    const {
      sidebar,
      profile,
      app,
      wallets: walletsStore,
      networkStatus,
    } = stores;
    const activeWallet = walletsStore.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const { isShelleyActivated } = networkStatus;
    const { currentTheme } = profile;
    const {
      environment: { network },
    } = app;
    const appWallets =
      sidebar.wallets.length > 0
        ? {
            items: sidebar.wallets,
            activeWalletId,
            actions: {
              onWalletItemClick: (walletId) => {
                actions.sidebar.walletSelected.trigger({
                  walletId,
                });
              },
              onWalletSortBy: ({ sortBy }) => {
                sidebar.onChangeWalletSortType(sortBy);
              },
              onSearch: sidebar.onSearchValueUpdated,
            },
            walletSortConfig: {
              sortBy: sidebar.walletSortConfig.sortBy,
              sortOrder: sidebar.walletSortConfig.sortOrder,
            },
            searchValue: sidebar.searchValue,
          }
        : null;
    const sidebarMenus = {
      wallets: appWallets,
    };
    const sidebarComponent = react_1.default.createElement(Sidebar_1.default, {
      menus: sidebarMenus,
      isShowingSubMenus: sidebar.isShowingSubMenus,
      isShelleyActivated: isShelleyActivated,
      categories: sidebar.CATEGORIES,
      activeSidebarCategory: sidebar.activeSidebarCategory,
      onActivateCategory: this.handleActivateCategory,
      onAddWallet: () =>
        actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.WALLETS.ADD,
        }),
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: { wallets: { items: any; activeWall... Remove this comment to see the full error message
      onSubmitSupportRequest: () =>
        actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.SETTINGS.SUPPORT,
        }),
      pathname: this.props.stores.router.location.pathname,
      currentTheme: currentTheme,
      network: network,
    });
    return react_1.default.createElement(
      SidebarLayout_1.default,
      {
        sidebar: sidebarComponent,
        // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
        topbar: react_1.default.createElement(TopBarContainer_1.default, null),
        contentDialogs: [
          react_1.default.createElement(
            PaperWalletCreateCertificatePage_1.default,
            {
              key: 'PaperWalletCreateCertificatePage',
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              certificateStep: this.props.stores.wallets.certificateStep,
            }
          ),
          react_1.default.createElement(TransferFundsPage_1.default, {
            key: 'TransferFundsPage',
          }),
          react_1.default.createElement(
            AssetSettingsDialogContainer_1.default,
            { key: 'AssetSettingsDialogContainer' }
          ),
        ],
      },
      this.props.children
    );
  }
};
MainLayout = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  MainLayout
);
exports.default = MainLayout;
//# sourceMappingURL=MainLayout.js.map
