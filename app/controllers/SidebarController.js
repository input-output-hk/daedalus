// @flow
import { action } from 'mobx';
import AppStore from '../stores/AppStore';

export default class SidebarController {

  store: AppStore;

  constructor(store: AppStore) {
    this.store = store;
  }

  @action toggleSidebar() {
    const { sidebar } = this.store;
    sidebar.hidden = !sidebar.hidden;
  }

  @action changeSidebarRoute(route: string) {
    const { sidebar } = this.store;
    if (sidebar.route === route) {
      // Toggle menu if it's the current route
      sidebar.showMenu = !sidebar.showMenu;
    } else {
      sidebar.route = route;
      sidebar.showMenu = true;
    }
  }

}
