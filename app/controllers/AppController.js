import AppStore from '../stores/AppStore';
import WalletsController from './WalletsController';
import SidebarController from './SidebarController';

export default class AppController {

  constructor(appStore: AppStore) {
    this.store = appStore;
    this.wallets = new WalletsController(appStore);
    this.sidebar = new SidebarController(appStore);
  }

}
