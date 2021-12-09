// @flow
import type {
  SidebarWalletType,
  WalletSortByOptions,
  WalletSortOrderOptions,
  WalletSortConfig,
} from '../../types/sidebarTypes';

export type SidebarMenus = {
  wallets: ?{
    items: Array<SidebarWalletType>,
    activeWalletId: ?string,
    actions: {
      onWalletItemClick: Function,
      onWalletSortBy: ({
        sortBy: WalletSortByOptions,
        sortOrder: WalletSortOrderOptions,
      }) => void,
      onSearch: (term: string) => void,
    },
    walletSortConfig: WalletSortConfig,
    searchValue: string,
  },
};

export type ShouldShowWalletSubMenu = {
  activeSidebarCategory: string,
  walletRoute: string,
  menus: SidebarMenus,
};
