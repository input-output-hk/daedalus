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
      sidebar.isMaximized = !sidebar.isMaximized;
    } else {
      sidebar.route = route;
      sidebar.isMaximized = false;
      if (route === '/settings') {
        if (this.state.router) this.state.router.transitionTo(route);
      }
    }
  }

}
