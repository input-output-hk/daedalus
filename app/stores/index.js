// @flow
import { observable } from 'mobx';
import AppStore from './AppStore';
import UserStore from './UserStore';
import SettingsStore from './SettingsStore';
import WalletsStore from './WalletsStore';
import TransactionsStore from './TransactionsStore';
import SidebarStore from './SidebarStore';

// Constant that does never change during lifetime
const stores = observable({
  app: null,
  routing: { location: null }, // TODO: replace with mobx-react-router
  user: null,
  settings: null,
  wallets: null,
  transactions: null,
  sidebar: null,
});

// Set up and return the stores for this app
// can also be used to reset all stores to defaults
export default (api, actions): storesType => (
  Object.assign(stores, {
    app: new AppStore(stores, api, actions),
    user: new UserStore(stores, api, actions),
    settings: new SettingsStore(stores, api, actions),
    wallets: new WalletsStore(stores, api, actions),
    transactions: new TransactionsStore(stores, api, actions),
    sidebar: new SidebarStore(stores, api, actions),
  })
);

export type storesType = {
  app: AppStore,
  user: UserStore,
  settings: SettingsStore,
  wallets: WalletsStore,
  transactions: TransactionsStore,
  sidebar: SidebarStore,
};
