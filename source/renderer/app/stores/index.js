// @flow
import { observable, action } from 'mobx';
import AdaRedemptionStore from './AdaRedemptionStore';
import AdaWalletsStore from './AdaWalletsStore';
import AdaWalletSettingsStore from './AdaWalletSettingsStore';
import AddressesStore from './AddressesStore';
import AppStore from './AppStore';
import NetworkStatusStore from './NetworkStatusStore';
import NodeUpdateStore from './NodeUpdateStore';
import ProfileStore from './ProfileStore';
import SidebarStore from './SidebarStore';
import TransactionsStore from './AdaTransactionsStore';
import UiDialogsStore from './UiDialogsStore';
import UiNotificationsStore from './UiNotificationsStore';
import WalletBackupStore from './WalletBackupStore';
import WindowStore from './WindowStore';

export const storeClasses = {
  adaRedemption: AdaRedemptionStore,
  addresses: AddressesStore,
  app: AppStore,
  networkStatus: NetworkStatusStore,
  nodeUpdate: NodeUpdateStore,
  profile: ProfileStore,
  sidebar: SidebarStore,
  transactions: TransactionsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  wallets: AdaWalletsStore,
  walletBackup: WalletBackupStore,
  walletSettings: AdaWalletSettingsStore,
  window: WindowStore,
};

export type StoresMap = {
  adaRedemption: AdaRedemptionStore,
  addresses: AddressesStore,
  app: AppStore,
  networkStatus: NetworkStatusStore,
  nodeUpdate: NodeUpdateStore,
  profile: ProfileStore,
  router: Object,
  sidebar: SidebarStore,
  transactions: TransactionsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  wallets: AdaWalletsStore,
  walletBackup: WalletBackupStore,
  walletSettings: AdaWalletSettingsStore,
  window: WindowStore,
};

// Constant that does never change during lifetime
const stores = observable({
  adaRedemption: null,
  addresses: null,
  app: null,
  networkStatus: null,
  nodeUpdate: null,
  profile: null,
  router: null,
  sidebar: null,
  transactions: null,
  uiDialogs: null,
  uiNotifications: null,
  wallets: null,
  walletBackup: null,
  walletSettings: null,
  window: null,
});

// Set up and return the stores for this app -> also used to reset all stores to defaults
export default action((api, actions, router): StoresMap => {
  // Assign mobx-react-router only once
  if (stores.router == null) stores.router = router;
  // All other stores have our lifecycle
  const storeNames = Object.keys(storeClasses);
  storeNames.forEach(name => { if (stores[name]) stores[name].teardown(); });
  storeNames.forEach(name => { stores[name] = new storeClasses[name](stores, api, actions); });
  storeNames.forEach(name => { if (stores[name]) stores[name].initialize(); });
  return stores;
});
