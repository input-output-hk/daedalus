// @flow
import { observable, action } from 'mobx';
import AppStore from './AppStore';
import SettingsStore from './SettingsStore';
import WalletsStore from './ada/WalletsStore';
import TransactionsStore from './ada/TransactionsStore';
import SidebarStore from './SidebarStore';
import WindowStore from './WindowStore';
import WalletBackupStore from './ada/WalletBackupStore';
import NetworkStatusStore from './ada/NetworkStatusStore';
import AdaRedemptionStore from './ada/AdaRedemptionStore';
import NodeUpdateStore from './ada/NodeUpdateStore';
import WalletSettingsStore from './ada/WalletSettingsStore';
import UiDialogsStore from './UiDialogsStore';
import UiNotificationsStore from './UiNotificationsStore';
import AddressesStore from './ada/AddressesStore';

export const storeClasses = {
  settings: SettingsStore,
  app: AppStore,
  wallets: WalletsStore,
  transactions: TransactionsStore,
  sidebar: SidebarStore,
  window: WindowStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: WalletSettingsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  addresses: AddressesStore,
};

export type StoresMap = {
  settings: SettingsStore,
  app: AppStore,
  router: Object,
  wallets: WalletsStore,
  transactions: TransactionsStore,
  sidebar: SidebarStore,
  window: WindowStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: WalletSettingsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  addresses: AddressesStore,
};

// Constant that does never change during lifetime
const stores = observable({
  settings: null,
  router: null,
  app: null,
  wallets: null,
  transactions: null,
  sidebar: null,
  window: null,
  walletBackup: null,
  networkStatus: null,
  adaRedemption: null,
  nodeUpdate: null,
  walletSettings: null,
  uiDialogs: null,
  uiNotifications: null,
  addresses: null,
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
