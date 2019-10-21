// @flow
import { observable, action } from 'mobx';
import type Store from './lib/Store';
import AddressesStore from './AddressesStore';
import AppStore from './AppStore';
import NetworkStatusStore from './NetworkStatusStore';
import NewsFeedStore from './NewsFeedStore';
import NodeUpdateStore from './NodeUpdateStore';
import ProfileStore from './ProfileStore';
import SidebarStore from './SidebarStore';
import StakingStore from './StakingStore';
import TransactionsStore from './TransactionsStore';
import UiDialogsStore from './UiDialogsStore';
import UiNotificationsStore from './UiNotificationsStore';
import WalletsStore from './WalletsStore';
import WalletSettingsStore from './WalletSettingsStore';
import WalletBackupStore from './WalletBackupStore';
import WindowStore from './WindowStore';

export const storeClasses = {
  addresses: AddressesStore,
  app: AppStore,
  networkStatus: NetworkStatusStore,
  newsFeed: NewsFeedStore,
  nodeUpdate: NodeUpdateStore,
  profile: ProfileStore,
  sidebar: SidebarStore,
  staking: StakingStore,
  transactions: TransactionsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  wallets: WalletsStore,
  walletSettings: WalletSettingsStore,
  walletBackup: WalletBackupStore,
  window: WindowStore,
};

export type StoresMap = {
  addresses: AddressesStore,
  app: AppStore,
  networkStatus: NetworkStatusStore,
  newsFeed: NewsFeedStore,
  nodeUpdate: NodeUpdateStore,
  profile: ProfileStore,
  router: Object,
  sidebar: SidebarStore,
  staking: StakingStore,
  transactions: TransactionsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  wallets: WalletsStore,
  walletSettings: WalletSettingsStore,
  walletBackup: WalletBackupStore,
  window: WindowStore,
};

let stores: ?StoresMap = null;
const storeNames = Object.keys(storeClasses);

// Helpers
function executeOnEveryStore(fn: (store: Store) => void) {
  storeNames.forEach(name => {
    if (stores && stores[name]) fn(stores[name]);
  });
}

// Set up and return the stores for this app -> also used to reset all stores to defaults
export default action(
  (api, actions, router): StoresMap => {
    function createStoreInstanceOf<T: Store>(StoreSubClass: Class<T>): T {
      return new StoreSubClass(api, actions);
    }

    // Teardown existing stores
    if (stores) executeOnEveryStore(store => store.teardown());

    // Create fresh instances of all stores
    stores = observable({
      addresses: createStoreInstanceOf(AddressesStore),
      app: createStoreInstanceOf(AppStore),
      networkStatus: createStoreInstanceOf(NetworkStatusStore),
      newsFeed: createStoreInstanceOf(NewsFeedStore),
      nodeUpdate: createStoreInstanceOf(NodeUpdateStore),
      profile: createStoreInstanceOf(ProfileStore),
      router,
      sidebar: createStoreInstanceOf(SidebarStore),
      staking: createStoreInstanceOf(StakingStore),
      transactions: createStoreInstanceOf(TransactionsStore),
      uiDialogs: createStoreInstanceOf(UiDialogsStore),
      uiNotifications: createStoreInstanceOf(UiNotificationsStore),
      wallets: createStoreInstanceOf(WalletsStore),
      walletBackup: createStoreInstanceOf(WalletBackupStore),
      walletSettings: createStoreInstanceOf(WalletSettingsStore),
      window: createStoreInstanceOf(WindowStore),
    });
    // Configure and initialize all stores
    executeOnEveryStore(store => {
      if (stores) store.configure(stores);
    });
    executeOnEveryStore(store => store.initialize());
    return stores;
  }
);
