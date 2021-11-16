// @flow
import type { ShouldShowWalletSubMenu } from './types';

export const isCategoryActive = (
  activeSidebarCategory: string,
  currentRoute: string
) => activeSidebarCategory === currentRoute;

export const shouldShowWalletSubMenu = ({
  activeSidebarCategory,
  walletRoute,
  menus,
}: ShouldShowWalletSubMenu) =>
  menus?.wallets?.items && isCategoryActive(activeSidebarCategory, walletRoute);
