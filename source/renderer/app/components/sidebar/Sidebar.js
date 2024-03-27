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
const Sidebar_scss_1 = __importDefault(require('./Sidebar.scss'));
const helpers_1 = require('./helpers');
const SidebarCategory_1 = __importDefault(require('./SidebarCategory'));
const SidebarCategoryNetworkInfo_1 = __importDefault(
  require('./SidebarCategoryNetworkInfo')
);
const SidebarWalletsMenu_1 = __importDefault(
  require('./wallets/SidebarWalletsMenu')
);
const sidebarConfig_1 = require('../../config/sidebarConfig');
const routes_config_1 = require('../../routes-config');
const getCategoryContent = (categoryName, network) =>
  categoryName === 'NETWORK_INFO'
    ? react_1.default.createElement(SidebarCategoryNetworkInfo_1.default, {
        network: network,
      })
    : null;
function Sidebar({
  menus,
  categories,
  activeSidebarCategory,
  pathname,
  isShowingSubMenus = false,
  onAddWallet,
  isShelleyActivated,
  onActivateCategory,
  network,
}) {
  const hasSubMenu = (0, helpers_1.shouldShowWalletSubMenu)({
    activeSidebarCategory,
    walletRoute: sidebarConfig_1.CATEGORIES_BY_NAME.WALLETS.route,
    menus,
  });
  const isMinimized = !isShowingSubMenus || !hasSubMenu;
  const sidebarStyles = (0, classnames_1.default)(
    Sidebar_scss_1.default.component,
    isMinimized && Sidebar_scss_1.default.minimized
  );
  return react_1.default.createElement(
    'div',
    { className: sidebarStyles },
    react_1.default.createElement(
      'div',
      { className: Sidebar_scss_1.default.minimized },
      categories.map((category) => {
        const content = getCategoryContent(category.name, network);
        const isActive = activeSidebarCategory === category.route;
        return react_1.default.createElement(SidebarCategory_1.default, {
          key: category.name,
          category: category,
          isActive: isActive,
          onClick: onActivateCategory,
          content: content,
        });
      })
    ),
    hasSubMenu &&
      react_1.default.createElement(SidebarWalletsMenu_1.default, {
        wallets: menus?.wallets?.items || [],
        onAddWallet: onAddWallet,
        onWalletItemClick: menus?.wallets?.actions.onWalletItemClick,
        isActiveWallet: (id) => id === menus?.wallets?.activeWalletId,
        isAddWalletButtonActive:
          pathname === routes_config_1.ROUTES.WALLETS.ADD,
        isShelleyActivated: isShelleyActivated,
        visible: isShowingSubMenus,
        onWalletSortBy: menus?.wallets?.actions.onWalletSortBy,
        sortBy: menus?.wallets?.walletSortConfig?.sortBy,
        sortOrder: menus?.wallets?.walletSortConfig?.sortOrder,
        searchValue: menus?.wallets?.searchValue,
        onSearch: menus?.wallets?.actions.onSearch,
      })
  );
}
exports.default = (0, mobx_react_1.observer)(Sidebar);
//# sourceMappingURL=Sidebar.js.map
