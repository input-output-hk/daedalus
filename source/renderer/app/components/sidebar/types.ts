import type {
  SidebarWalletType,
  WalletSortByOptions,
  WalletSortOrderOptions,
  WalletSortConfig,
} from '../../types/sidebarTypes';

export type SidebarMenus = {
  wallets:
    | {
        items: Array<SidebarWalletType>;
        activeWalletId: string | null | undefined;
        actions: {
          onWalletItemClick: (...args: Array<any>) => any;
          onWalletSortBy: (arg0: {
            sortBy: WalletSortByOptions;
            sortOrder: WalletSortOrderOptions;
          }) => void;
          onSearch: (term: string) => void;
        };
        walletSortConfig: WalletSortConfig;
        searchValue: string;
      }
    | null
    | undefined;
};
export type ShouldShowWalletSubMenu = {
  activeSidebarCategory: string;
  walletRoute: string;
  menus: SidebarMenus;
};
