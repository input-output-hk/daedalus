// @flow
import { Action } from './lib/actions';

// ======= SIDEBAR ACTIONS =======

export default class SidebarActions {
  toggleSubMenus: Action<any> = new Action();
  activateSidebarCategory: Action<{ category: string, showSubMenu?: boolean }> = new Action();
  walletSelected: Action<{ walletId: string }> = new Action();
}
