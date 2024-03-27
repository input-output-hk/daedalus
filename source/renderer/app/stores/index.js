'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.setUpStores = exports.storeClasses = void 0;
const mobx_1 = require('mobx');
const AddressesStore_1 = __importDefault(require('./AddressesStore'));
const AppStore_1 = __importDefault(require('./AppStore'));
const AppUpdateStore_1 = __importDefault(require('./AppUpdateStore'));
const AssetsStore_1 = __importDefault(require('./AssetsStore'));
const CurrencyStore_1 = __importDefault(require('./CurrencyStore'));
const HardwareWalletsStore_1 = __importDefault(
  require('./HardwareWalletsStore')
);
const NetworkStatusStore_1 = __importDefault(require('./NetworkStatusStore'));
const NewsFeedStore_1 = __importDefault(require('./NewsFeedStore'));
const ProfileStore_1 = __importDefault(require('./ProfileStore'));
const SidebarStore_1 = __importDefault(require('./SidebarStore'));
const StakingStore_1 = __importDefault(require('./StakingStore'));
const TransactionsStore_1 = __importDefault(require('./TransactionsStore'));
const UiDialogsStore_1 = __importDefault(require('./UiDialogsStore'));
const UiNotificationsStore_1 = __importDefault(
  require('./UiNotificationsStore')
);
const VotingStore_1 = __importDefault(require('./VotingStore'));
const WalletBackupStore_1 = __importDefault(require('./WalletBackupStore'));
const WalletMigrationStore_1 = __importDefault(
  require('./WalletMigrationStore')
);
const WalletSettingsStore_1 = __importDefault(require('./WalletSettingsStore'));
const WalletsLocalStore_1 = __importDefault(require('./WalletsLocalStore'));
const WalletsStore_1 = __importDefault(require('./WalletsStore'));
const WindowStore_1 = __importDefault(require('./WindowStore'));
exports.storeClasses = {
  addresses: AddressesStore_1.default,
  app: AppStore_1.default,
  appUpdate: AppUpdateStore_1.default,
  assets: AssetsStore_1.default,
  currency: CurrencyStore_1.default,
  hardwareWallets: HardwareWalletsStore_1.default,
  networkStatus: NetworkStatusStore_1.default,
  newsFeed: NewsFeedStore_1.default,
  profile: ProfileStore_1.default,
  sidebar: SidebarStore_1.default,
  staking: StakingStore_1.default,
  transactions: TransactionsStore_1.default,
  uiDialogs: UiDialogsStore_1.default,
  uiNotifications: UiNotificationsStore_1.default,
  voting: VotingStore_1.default,
  wallets: WalletsStore_1.default,
  walletsLocal: WalletsLocalStore_1.default,
  walletBackup: WalletBackupStore_1.default,
  walletMigration: WalletMigrationStore_1.default,
  walletSettings: WalletSettingsStore_1.default,
  window: WindowStore_1.default,
};
let stores = null;
const storeNames = Object.keys(exports.storeClasses);
// Helpers
function executeOnEveryStore(fn) {
  storeNames.forEach((name) => {
    if (stores && stores[name]) fn(stores[name]);
  });
} // Set up and return the stores for this app -> also used to reset all stores to defaults
exports.setUpStores = (0, mobx_1.action)(
  (api, actions, router, analyticsTracker) => {
    function createStoreInstanceOf(StoreSubClass) {
      return new StoreSubClass(api, actions, analyticsTracker);
    }
    // Teardown existing stores
    if (stores) executeOnEveryStore((store) => store.teardown());
    // Create fresh instances of all stores
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ addresses: Store; app: Store; assets: Stor... Remove this comment to see the full error message
    stores = (0, mobx_1.observable)({
      addresses: createStoreInstanceOf(AddressesStore_1.default),
      app: createStoreInstanceOf(AppStore_1.default),
      assets: createStoreInstanceOf(AssetsStore_1.default),
      currency: createStoreInstanceOf(CurrencyStore_1.default),
      appUpdate: createStoreInstanceOf(AppUpdateStore_1.default),
      hardwareWallets: createStoreInstanceOf(HardwareWalletsStore_1.default),
      networkStatus: createStoreInstanceOf(NetworkStatusStore_1.default),
      newsFeed: createStoreInstanceOf(NewsFeedStore_1.default),
      profile: createStoreInstanceOf(ProfileStore_1.default),
      router,
      sidebar: createStoreInstanceOf(SidebarStore_1.default),
      staking: createStoreInstanceOf(StakingStore_1.default),
      transactions: createStoreInstanceOf(TransactionsStore_1.default),
      uiDialogs: createStoreInstanceOf(UiDialogsStore_1.default),
      uiNotifications: createStoreInstanceOf(UiNotificationsStore_1.default),
      voting: createStoreInstanceOf(VotingStore_1.default),
      wallets: createStoreInstanceOf(WalletsStore_1.default),
      walletsLocal: createStoreInstanceOf(WalletsLocalStore_1.default),
      walletBackup: createStoreInstanceOf(WalletBackupStore_1.default),
      walletMigration: createStoreInstanceOf(WalletMigrationStore_1.default),
      walletSettings: createStoreInstanceOf(WalletSettingsStore_1.default),
      window: createStoreInstanceOf(WindowStore_1.default),
    });
    // Configure and initialize all stores
    executeOnEveryStore((store) => {
      !!stores && store?.configure(stores);
    });
    executeOnEveryStore((store) => store.initialize());
    return stores;
  }
);
//# sourceMappingURL=index.js.map
