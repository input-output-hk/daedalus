import type { SidebarWalletType } from '../../types/sidebarTypes';

export type SidebarMenus = {
  wallets:
    | {
        items: Array<SidebarWalletType>;
        activeWalletId: string | null | undefined;
        actions: {
          onWalletItemClick: (...args: Array<any>) => any;
        };
      }
    | null
    | undefined;
};
export type ShouldShowWalletSubMenu = {
  activeSidebarCategory: string;
  walletRoute: string;
  menus: SidebarMenus;
};
