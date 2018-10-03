// @flow
import RouterActions from './router-actions';
import SidebarActions from './sidebar-actions';
import WindowActions from './window-actions';
import NetworkStatusActions from './network-status-actions';
import WalletBackupActions from './wallet-backup-actions';
import ProfileActions from './profile-actions';
import DialogsActions from './dialogs-actions';
import NotificationsActions from './notifications-actions';
import AppActions from './app-actions';
import WalletsActions from './wallets-actions';
import AdaRedemptionActions from './ada-redemption-actions';
import TransactionsActions from './transactions-actions';
import NodeUpdateActions from './node-update-actions';
import WalletSettingsActions from './wallet-settings-actions';
import AddressesActions from './addresses-actions';

export type ActionsMap = {
  app: AppActions,
  router: RouterActions,
  sidebar: SidebarActions,
  window: WindowActions,
  networkStatus: NetworkStatusActions,
  walletBackup: WalletBackupActions,
  profile: ProfileActions,
  dialogs: DialogsActions,
  notifications: NotificationsActions,
  wallets: WalletsActions,
  adaRedemption: AdaRedemptionActions,
  transactions: TransactionsActions,
  nodeUpdate: NodeUpdateActions,
  walletSettings: WalletSettingsActions,
  addresses: AddressesActions,
};

const actionsMap: ActionsMap = {
  app: new AppActions(),
  router: new RouterActions(),
  sidebar: new SidebarActions(),
  window: new WindowActions(),
  networkStatus: new NetworkStatusActions(),
  walletBackup: new WalletBackupActions(),
  profile: new ProfileActions(),
  dialogs: new DialogsActions(),
  notifications: new NotificationsActions(),
  wallets: new WalletsActions(),
  adaRedemption: new AdaRedemptionActions(),
  transactions: new TransactionsActions(),
  nodeUpdate: new NodeUpdateActions(),
  walletSettings: new WalletSettingsActions(),
  addresses: new AddressesActions(),
};

export default actionsMap;
