'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const TopBar_1 = __importDefault(require('../components/layout/TopBar'));
const NodeSyncStatusIcon_1 = __importDefault(
  require('../components/widgets/NodeSyncStatusIcon')
);
const discreet_mode_1 = require('../features/discreet-mode');
const NewsFeedIcon_1 = __importDefault(
  require('../components/widgets/NewsFeedIcon')
);
const TadaButton_1 = __importDefault(
  require('../components/widgets/TadaButton')
);
const WalletTestEnvironmentLabel_1 = __importDefault(
  require('../components/widgets/WalletTestEnvironmentLabel')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/menu-opened-i... Remove this comment to see the full error message
const menu_opened_ic_inline_svg_1 = __importDefault(
  require('../assets/images/menu-opened-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/menu-ic.inlin... Remove this comment to see the full error message
const menu_ic_inline_svg_1 = __importDefault(
  require('../assets/images/menu-ic.inline.svg')
);
const routing_1 = require('../utils/routing');
const routes_config_1 = require('../routes-config');
const topBarConfig_1 = require('../config/topBarConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../components/layout/TopBar.sc... Remove this comment to see the full error message
const TopBar_scss_1 = __importDefault(
  require('../components/layout/TopBar.scss')
);
function TopBarContainer(
  { actions, stores } = {
    actions: null,
    stores: null,
  }
) {
  const {
    sidebar,
    app,
    networkStatus,
    wallets,
    newsFeed,
    appUpdate,
    staking,
  } = stores;
  const {
    isSynced,
    syncPercentage,
    isShelleyActivated,
    isAlonzoActivated,
    isAlonzoPending,
  } = networkStatus;
  const { stakingInfoWasOpen } = staking;
  const shouldShowTadaIconAnimation = isAlonzoActivated && !stakingInfoWasOpen;
  const shouldShowTadaIcon =
    topBarConfig_1.IS_TADA_ICON_AVAILABLE &&
    (isAlonzoPending || isAlonzoActivated);
  const { active, isWalletRoute, hasAnyWallets, hasRewardsWallets } = wallets;
  const {
    currentRoute,
    environment: { isMainnet, network },
    openExternalLink,
  } = app;
  const walletRoutesMatch = (0, routing_1.matchRoute)(
    `${routes_config_1.ROUTES.WALLETS.ROOT}/:id(*page)`,
    currentRoute
  );
  const showSubMenuToggle = isWalletRoute && hasAnyWallets;
  const activeWallet = walletRoutesMatch && active != null ? active : null;
  const leftIconSVG = sidebar.isShowingSubMenus
    ? menu_opened_ic_inline_svg_1.default
    : menu_ic_inline_svg_1.default;
  const leftIcon = showSubMenuToggle ? leftIconSVG : null;
  const testnetLabel = !isMainnet
    ? react_1.default.createElement(WalletTestEnvironmentLabel_1.default, {
        network: network,
      })
    : null;
  const onWalletAdd = () => {
    actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.ADD,
    });
  };
  const onClickTadaButton = () => {
    actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.STAKING.INFO,
    });
  };
  const onTransferFunds = (sourceWalletId) =>
    actions.wallets.transferFundsSetSourceWalletId.trigger({
      sourceWalletId,
    });
  const { unread } = newsFeed.newsFeedData;
  const { displayAppUpdateNewsItem } = appUpdate;
  const hasUnreadNews = unread.length > 0;
  return react_1.default.createElement(
    TopBar_1.default,
    {
      leftIcon: leftIcon,
      onLeftIconClick: actions.sidebar.toggleSubMenus.trigger,
      activeWallet: activeWallet,
      onTransferFunds: onTransferFunds,
      hasRewardsWallets: hasRewardsWallets,
      onWalletAdd: onWalletAdd,
      onLearnMore: openExternalLink,
      isShelleyActivated: isShelleyActivated,
    },
    testnetLabel,
    react_1.default.createElement(NodeSyncStatusIcon_1.default, {
      isSynced: isSynced,
      syncPercentage: syncPercentage,
      hasTadaIcon: shouldShowTadaIcon,
    }),
    react_1.default.createElement('span', {
      className: (0, classnames_1.default)(
        TopBar_scss_1.default.rectangle,
        shouldShowTadaIcon && TopBar_scss_1.default.hasTadaIcon
      ),
    }),
    react_1.default.createElement(discreet_mode_1.DiscreetToggleTopBar, {
      hasTadaIcon: shouldShowTadaIcon,
    }),
    shouldShowTadaIcon &&
      react_1.default.createElement(TadaButton_1.default, {
        onClick: onClickTadaButton,
        shouldAnimate: shouldShowTadaIconAnimation,
      }),
    react_1.default.createElement(NewsFeedIcon_1.default, {
      onNewsFeedIconClick: actions.app.toggleNewsFeed.trigger,
      hasNotification: hasUnreadNews,
      hasUpdate: displayAppUpdateNewsItem,
    })
  );
}
exports.default = (0, mobx_react_1.inject)(
  'stores',
  'actions'
)((0, mobx_react_1.observer)(TopBarContainer));
//# sourceMappingURL=TopBarContainer.js.map
