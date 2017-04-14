// @flow
import { Action } from './lib/actions';

// ======= SIDEBAR ACTIONS =======

export type SidebarActions = {
  toggleSubMenus: Action<any>,
  activateSidebarCategory: Action<{ category: string, showSubMenu?: boolean }>,
  walletSelected: Action<{ walletId: string }>,
};

const sidebarActions: SidebarActions = {
  toggleSubMenus: new Action(),
  activateSidebarCategory: new Action(),
  walletSelected: new Action(),
};

export default sidebarActions;
