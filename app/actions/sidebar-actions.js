import { action } from 'mobx';
import store from '../state';

const sidebar = store.uiStore.sidebar;

export const changeSidebarRoute = action((route) => {
  if (sidebar.route === route) {
    // Toggle menu if it's the current route
    sidebar.showMenu = !sidebar.showMenu;
  } else {
    sidebar.route = route;
    sidebar.showMenu = true;
  }
});
