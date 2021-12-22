// @ts-ignore ts-migrate(2307) FIXME: Cannot find module 'utility-types' or its correspo... Remove this comment to see the full error message
import { Class } from 'utility-types';
import { observable, action } from 'mobx';
import type Store from './lib/Store';
import AddressesStore from './AddressesStore';
import AppStore from './AppStore';
import AppUpdateStore from './AppUpdateStore';
import AssetsStore from './AssetsStore';
import CurrencyStore from './CurrencyStore';
import HardwareWalletsStore from './HardwareWalletsStore';
import NetworkStatusStore from './NetworkStatusStore';
import NewsFeedStore from './NewsFeedStore';
import ProfileStore from './ProfileStore';
import SidebarStore from './SidebarStore';
import StakingStore from './StakingStore';
import TransactionsStore from './TransactionsStore';
import UiDialogsStore from './UiDialogsStore';
import UiNotificationsStore from './UiNotificationsStore';
import VotingStore from './VotingStore';
import WalletBackupStore from './WalletBackupStore';
import WalletMigrationStore from './WalletMigrationStore';
import WalletSettingsStore from './WalletSettingsStore';
import WalletsLocalStore from './WalletsLocalStore';
import WalletsStore from './WalletsStore';
import WindowStore from './WindowStore';

export const storeClasses = {
  addresses: AddressesStore,
  app: AppStore,
  appUpdate: AppUpdateStore,
  assets: AssetsStore,
  currency: CurrencyStore,
  hardwareWallets: HardwareWalletsStore,
  networkStatus: NetworkStatusStore,
  newsFeed: NewsFeedStore,
  profile: ProfileStore,
  sidebar: SidebarStore,
  staking: StakingStore,
  transactions: TransactionsStore,
  uiDialogs: UiDialogsStore,
  uiNotifications: UiNotificationsStore,
  voting: VotingStore,
  wallets: WalletsStore,
  walletsLocal: WalletsLocalStore,
  walletBackup: WalletBackupStore,
  walletMigration: WalletMigrationStore,
  walletSettings: WalletSettingsStore,
  window: WindowStore,
};
export type StoresMap = {
  addresses: AddressesStore;
  app: AppStore;
  appUpdate: AppUpdateStore;
  currency: CurrencyStore;
  assets: AssetsStore;
  hardwareWallets: HardwareWalletsStore;
  networkStatus: NetworkStatusStore;
  newsFeed: NewsFeedStore;
  profile: ProfileStore;
  router: Record<string, any>;
  sidebar: SidebarStore;
  staking: StakingStore;
  transactions: TransactionsStore;
  uiDialogs: UiDialogsStore;
  uiNotifications: UiNotificationsStore;
  voting: VotingStore;
  wallets: WalletsStore;
  walletsLocal: WalletsLocalStore;
  walletBackup: WalletBackupStore;
  walletMigration: WalletMigrationStore;
  walletSettings: WalletSettingsStore;
  window: WindowStore;
};
let stores: StoresMap | null | undefined = null;
const storeNames = Object.keys(storeClasses);

// Helpers
function executeOnEveryStore(fn: (store: Store) => void) {
  storeNames.forEach((name) => {
    if (stores && stores[name]) fn(stores[name]);
  });
} // Set up and return the stores for this app -> also used to reset all stores to defaults

export default action(
  (api, actions, router): StoresMap => {
    function createStoreInstanceOf<T extends Store>(
      StoreSubClass: Class<T>
    ): T {
      return new StoreSubClass(api, actions);
    }

    // Teardown existing stores
    if (stores) executeOnEveryStore((store) => store.teardown());
    // Create fresh instances of all stores
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ addresses: Store; app: Store; assets: Stor... Remove this comment to see the full error message
    stores = observable({
      addresses: createStoreInstanceOf(AddressesStore),
      app: createStoreInstanceOf(AppStore),
      assets: createStoreInstanceOf(AssetsStore),
      currency: createStoreInstanceOf(CurrencyStore),
      appUpdate: createStoreInstanceOf(AppUpdateStore),
      hardwareWallets: createStoreInstanceOf(HardwareWalletsStore),
      networkStatus: createStoreInstanceOf(NetworkStatusStore),
      newsFeed: createStoreInstanceOf(NewsFeedStore),
      profile: createStoreInstanceOf(ProfileStore),
      router,
      sidebar: createStoreInstanceOf(SidebarStore),
      staking: createStoreInstanceOf(StakingStore),
      transactions: createStoreInstanceOf(TransactionsStore),
      uiDialogs: createStoreInstanceOf(UiDialogsStore),
      uiNotifications: createStoreInstanceOf(UiNotificationsStore),
      voting: createStoreInstanceOf(VotingStore),
      wallets: createStoreInstanceOf(WalletsStore),
      walletsLocal: createStoreInstanceOf(WalletsLocalStore),
      walletBackup: createStoreInstanceOf(WalletBackupStore),
      walletMigration: createStoreInstanceOf(WalletMigrationStore),
      walletSettings: createStoreInstanceOf(WalletSettingsStore),
      window: createStoreInstanceOf(WindowStore),
    });
    // Configure and initialize all stores
    executeOnEveryStore((store) => {
      if (stores) store.configure(stores);
    });
    executeOnEveryStore((store) => store.initialize());
    return stores;
  }
);
