// @flow
import { observable, action } from 'mobx';
import AppStore from './AppStore';
import UserStore from './UserStore';
import SettingsStore from './SettingsStore';
import WalletsStore from './WalletsStore';
import TransactionsStore from './TransactionsStore';
import SidebarStore from './SidebarStore';
import WindowStore from './WindowStore';
import WalletBackupStore from './WalletBackupStore';
import NetworkStatusStore from './NetworkStatusStore';
import NodeUpdateStore from './NodeUpdateStore';

// Constant that does never change during lifetime
const stores = observable({
  router: null,
  app: null,
  user: null,
  settings: null,
  wallets: null,
  transactions: null,
  sidebar: null,
  window: null,
  walletBackup: null,
  networkStatus: null,
  nodeUpdate: null
});

// Set up and return the stores for this app -> also used to reset all stores to defaults
export default action((api, actions, router): storesType => {
  const storeNames = Object.keys(stores);
  if (!stores.router) stores.router = router;
  // Teardown existing stores
  storeNames.forEach(name => {
    if (stores[name] && stores[name].teardown) stores[name].teardown();
  });
  // Assign new store instances
  Object.assign(stores, {
    app: new AppStore(stores, api, actions),
    user: new UserStore(stores, api, actions),
    settings: new SettingsStore(stores, api, actions),
    wallets: new WalletsStore(stores, api, actions),
    transactions: new TransactionsStore(stores, api, actions),
    sidebar: new SidebarStore(stores, api, actions),
    window: new WindowStore(stores, api, actions),
    walletBackup: new WalletBackupStore(stores, api, actions),
    networkStatus: new NetworkStatusStore(stores, api, actions),
    nodeUpdate: new NodeUpdateStore(stores, api, actions)
  });
  // Initialize the new stores
  storeNames.forEach(name => {
    if (stores[name] && stores[name].initialize) stores[name].initialize();
  });
  return stores;
});

export type storesType = {
  app: AppStore,
  user: UserStore,
  settings: SettingsStore,
  wallets: WalletsStore,
  transactions: TransactionsStore,
  sidebar: SidebarStore,
  window: WindowStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  nodeUpdate: NodeUpdateStore
};
