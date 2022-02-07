import type { ShouldShowWalletSubMenu } from './types';

export const shouldShowWalletSubMenu = ({
  activeSidebarCategory,
  walletRoute,
  menus,
}: ShouldShowWalletSubMenu) =>
  menus?.wallets?.items && activeSidebarCategory === walletRoute;
