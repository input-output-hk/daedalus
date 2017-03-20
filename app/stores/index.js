// @flow
import { observable, action } from 'mobx';
import AppStore from './AppStore';
import SettingsStore from './SettingsStore';
import WalletsStore from './WalletsStore';
import TransactionsStore from './TransactionsStore';
import SidebarStore from './SidebarStore';
import WindowStore from './WindowStore';
import WalletBackupStore from './WalletBackupStore';
import NetworkStatusStore from './NetworkStatusStore';
import AdaRedemptionStore from './AdaRedemptionStore';
import NodeUpdateStore from './NodeUpdateStore';

const storeClasses = {
  app: AppStore,
  settings: SettingsStore,
  wallets: WalletsStore,
  transactions: TransactionsStore,
  sidebar: SidebarStore,
  window: WindowStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
};

// Constant that does never change during lifetime
const stores = observable({
  router: null,
  app: null,
  settings: null,
  wallets: null,
  transactions: null,
  sidebar: null,
  window: null,
  walletBackup: null,
  networkStatus: null,
  adaRedemption: null,
  nodeUpdate: null,
});

// Set up and return the stores for this app -> also used to reset all stores to defaults
export default action((api, actions, router): storesType => {
  // Assign mobx-react-router only once
  if (stores.router == null) stores.router = router;
  // All other stores have our lifecycle
  const storeNames = Object.keys(storeClasses);
  storeNames.forEach(name => { if (stores[name]) stores[name].teardown(); });
  storeNames.forEach(name => { stores[name] = new storeClasses[name](stores, api, actions); });
  storeNames.forEach(name => { if (stores[name]) stores[name].initialize(); });
  return stores;
});

export type storesType = storeClasses;
