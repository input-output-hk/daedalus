import { action } from 'mobx';
import store from '../store';

const sidebar = store.sidebar;

export const toggleSidebar = action(() => {
  sidebar.hidden = !store.sidebar.hidden;
});

export const changeSidebarRoute = action((route) => {
  if (sidebar.route === route) {
    // Toggle menu if it's the current route
    sidebar.showMenu = !sidebar.showMenu;
  } else {
    sidebar.route = route;
    sidebar.showMenu = true;
  }
});
