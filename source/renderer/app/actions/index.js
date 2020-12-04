// @flow
import AddressesActions from './addresses-actions';
import AppActions from './app-actions';
import DialogsActions from './dialogs-actions';
import HardwareWalletsActions from './hardware-wallets-actions';
import NetworkStatusActions from './network-status-actions';
import AppUpdateActions from './app-update-actions';
import NotificationsActions from './notifications-actions';
import ProfileActions from './profile-actions';
import RouterActions from './router-actions';
import SidebarActions from './sidebar-actions';
import StakingActions from './staking-actions';
import TransactionsActions from './transactions-actions';
import WalletsActions from './wallets-actions';
import WalletsLocalAction from './wallets-local-actions';
import WalletBackupActions from './wallet-backup-actions';
import WalletMigrationActions from './wallet-migration-actions';
import WalletSettingsActions from './wallet-settings-actions';
import WindowActions from './window-actions';

export type ActionsMap = {
  addresses: AddressesActions,
  app: AppActions,
  dialogs: DialogsActions,
  hardwareWallets: HardwareWalletsActions,
  networkStatus: NetworkStatusActions,
  appUpdate: AppUpdateActions,
  notifications: NotificationsActions,
  profile: ProfileActions,
  router: RouterActions,
  sidebar: SidebarActions,
  staking: StakingActions,
  transactions: TransactionsActions,
  wallets: WalletsActions,
  walletsLocal: WalletsLocalAction,
  walletBackup: WalletBackupActions,
  walletMigration: WalletMigrationActions,
  walletSettings: WalletSettingsActions,
  window: WindowActions,
};

const actionsMap: ActionsMap = {
  addresses: new AddressesActions(),
  app: new AppActions(),
  dialogs: new DialogsActions(),
  hardwareWallets: new HardwareWalletsActions(),
  networkStatus: new NetworkStatusActions(),
  appUpdate: new AppUpdateActions(),
  notifications: new NotificationsActions(),
  profile: new ProfileActions(),
  router: new RouterActions(),
  sidebar: new SidebarActions(),
  staking: new StakingActions(),
  transactions: new TransactionsActions(),
  wallets: new WalletsActions(),
  walletsLocal: new WalletsLocalAction(),
  walletBackup: new WalletBackupActions(),
  walletMigration: new WalletMigrationActions(),
  walletSettings: new WalletSettingsActions(),
  window: new WindowActions(),
};

export default actionsMap;
