// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import WalletsController from './WalletsController';
import SidebarController from './SidebarController';

export default class AppController {

  state: appState;
  wallets: WalletsController;
  sidebar: SidebarController;

  constructor(state: appState) {
    this.state = state;
    this.wallets = new WalletsController(state);
    this.sidebar = new SidebarController(state);
  }

  @action setAppRouter(router: Object) {
    this.state.router = router;
  }

}
