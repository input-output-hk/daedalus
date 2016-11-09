import { action } from 'mobx';
import store from '../store';

export const toggleSidebar = action(() => {
  store.sidebar.hidden = !store.sidebar.hidden;
});
