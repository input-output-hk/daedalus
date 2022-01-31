import AddressesActions from './addresses-actions';
import AppActions from './app-actions';
import AppUpdateActions from './app-update-actions';
import AssetsActions from './assets-actions';
import CurrencyActions from './currency-actions';
import DialogsActions from './dialogs-actions';
import HardwareWalletsActions from './hardware-wallets-actions';
import NetworkStatusActions from './network-status-actions';
import NotificationsActions from './notifications-actions';
import ProfileActions from './profile-actions';
import RouterActions from './router-actions';
import SidebarActions from './sidebar-actions';
import StakingActions from './staking-actions';
import TransactionsActions from './transactions-actions';
import VotingActions from './voting-actions';
import WalletsActions from './wallets-actions';
import WalletsLocalAction from './wallets-local-actions';
import WalletBackupActions from './wallet-backup-actions';
import WalletMigrationActions from './wallet-migration-actions';
import WalletSettingsActions from './wallet-settings-actions';
import WindowActions from './window-actions';

export type ActionsMap = {
  addresses: AddressesActions;
  app: AppActions;
  appUpdate: AppUpdateActions;
  assets: AssetsActions;
  dialogs: DialogsActions;
  currency: CurrencyActions;
  hardwareWallets: HardwareWalletsActions;
  networkStatus: NetworkStatusActions;
  notifications: NotificationsActions;
  profile: ProfileActions;
  router: RouterActions;
  sidebar: SidebarActions;
  staking: StakingActions;
  transactions: TransactionsActions;
  voting: VotingActions;
  wallets: WalletsActions;
  walletsLocal: WalletsLocalAction;
  walletBackup: WalletBackupActions;
  walletMigration: WalletMigrationActions;
  walletSettings: WalletSettingsActions;
  window: WindowActions;
};
const actionsMap: ActionsMap = {
  addresses: new AddressesActions(),
  app: new AppActions(),
  appUpdate: new AppUpdateActions(),
  assets: new AssetsActions(),
  currency: new CurrencyActions(),
  dialogs: new DialogsActions(),
  hardwareWallets: new HardwareWalletsActions(),
  networkStatus: new NetworkStatusActions(),
  notifications: new NotificationsActions(),
  profile: new ProfileActions(),
  router: new RouterActions(),
  sidebar: new SidebarActions(),
  staking: new StakingActions(),
  transactions: new TransactionsActions(),
  voting: new VotingActions(),
  wallets: new WalletsActions(),
  walletsLocal: new WalletsLocalAction(),
  walletBackup: new WalletBackupActions(),
  walletMigration: new WalletMigrationActions(),
  walletSettings: new WalletSettingsActions(),
  window: new WindowActions(),
};
export default actionsMap;
