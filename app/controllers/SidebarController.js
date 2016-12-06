// @flow
import { action } from 'mobx';
import BaseController from './BaseController';

export default class SidebarController extends BaseController {

  @action toggleSidebar() {
    const { sidebar } = this.state;
    sidebar.hidden = !sidebar.hidden;
  }

  @action changeSidebarRoute(route: string) {
    const { sidebar } = this.state;
    if (sidebar.route === route) {
      // Toggle menu if it's the current route
      sidebar.isMaximized = !sidebar.isMaximized;
    } else {
      sidebar.route = route;
      sidebar.isMaximized = false;
      if (route === '/settings' || route === '/staking') {
        if (this.state.router) this.appController.navigateTo(route);
      }
    }
  }

}
