// @flow
import type { SidebarWalletType } from '../../types/sidebarTypes';

export type SidebarMenus = {
  wallets: ?{
    items: Array<SidebarWalletType>,
    activeWalletId: ?string,
    actions: {
      onWalletItemClick: Function,
    },
  },
};

export type ShouldShowWalletSubMenu = {
  activeSidebarCategory: string,
  walletRoute: string,
  menus: SidebarMenus,
};
