// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';

export default class SidebarController {

  state: appState;

  constructor(state: appState) {
    this.state = state;
  }

  @action toggleSidebar() {
    const { sidebar } = this.state;
    sidebar.hidden = !sidebar.hidden;
  }

  @action changeSidebarRoute(route: string) {
    const { sidebar } = this.state;
    if (sidebar.route === route) {
      // Toggle menu if it's the current route
      sidebar.showMenu = !sidebar.showMenu;
    } else {
      sidebar.route = route;
      sidebar.showMenu = true;
      switch (route) {
        case '/settings':
          this.changeApplicationRoute('/settings');
          break;
        default:
          break;
      }
    }
  }

  changeApplicationRoute(route: string) {
    const { router } = this.state;
    if (router) router.transitionTo(route);
  }

}
