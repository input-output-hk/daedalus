'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const addresses_actions_1 = __importDefault(require('./addresses-actions'));
const app_actions_1 = __importDefault(require('./app-actions'));
const app_update_actions_1 = __importDefault(require('./app-update-actions'));
const assets_actions_1 = __importDefault(require('./assets-actions'));
const currency_actions_1 = __importDefault(require('./currency-actions'));
const dialogs_actions_1 = __importDefault(require('./dialogs-actions'));
const hardware_wallets_actions_1 = __importDefault(
  require('./hardware-wallets-actions')
);
const network_status_actions_1 = __importDefault(
  require('./network-status-actions')
);
const notifications_actions_1 = __importDefault(
  require('./notifications-actions')
);
const profile_actions_1 = __importDefault(require('./profile-actions'));
const router_actions_1 = __importDefault(require('./router-actions'));
const sidebar_actions_1 = __importDefault(require('./sidebar-actions'));
const staking_actions_1 = __importDefault(require('./staking-actions'));
const transactions_actions_1 = __importDefault(
  require('./transactions-actions')
);
const voting_actions_1 = __importDefault(require('./voting-actions'));
const wallets_actions_1 = __importDefault(require('./wallets-actions'));
const wallets_local_actions_1 = __importDefault(
  require('./wallets-local-actions')
);
const wallet_backup_actions_1 = __importDefault(
  require('./wallet-backup-actions')
);
const wallet_migration_actions_1 = __importDefault(
  require('./wallet-migration-actions')
);
const wallet_settings_actions_1 = __importDefault(
  require('./wallet-settings-actions')
);
const window_actions_1 = __importDefault(require('./window-actions'));
const actionsMap = {
  addresses: new addresses_actions_1.default(),
  app: new app_actions_1.default(),
  appUpdate: new app_update_actions_1.default(),
  assets: new assets_actions_1.default(),
  currency: new currency_actions_1.default(),
  dialogs: new dialogs_actions_1.default(),
  hardwareWallets: new hardware_wallets_actions_1.default(),
  networkStatus: new network_status_actions_1.default(),
  notifications: new notifications_actions_1.default(),
  profile: new profile_actions_1.default(),
  router: new router_actions_1.default(),
  sidebar: new sidebar_actions_1.default(),
  staking: new staking_actions_1.default(),
  transactions: new transactions_actions_1.default(),
  voting: new voting_actions_1.default(),
  wallets: new wallets_actions_1.default(),
  walletsLocal: new wallets_local_actions_1.default(),
  walletBackup: new wallet_backup_actions_1.default(),
  walletMigration: new wallet_migration_actions_1.default(),
  walletSettings: new wallet_settings_actions_1.default(),
  window: new window_actions_1.default(),
};
exports.default = actionsMap;
//# sourceMappingURL=index.js.map
