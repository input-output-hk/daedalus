import { action } from 'mobx';
import state from '../state';

const { uiStore } = state;
const { sidebar } = uiStore;

export const toggleSidebar = action(() => {
  sidebar.hidden = !sidebar.hidden;
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
