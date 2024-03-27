'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.shouldShowWalletSubMenu = void 0;
const shouldShowWalletSubMenu = ({
  activeSidebarCategory,
  walletRoute,
  menus,
}) => menus?.wallets?.items && activeSidebarCategory === walletRoute;
exports.shouldShowWalletSubMenu = shouldShowWalletSubMenu;
//# sourceMappingURL=helpers.js.map
