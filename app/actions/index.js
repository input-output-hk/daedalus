// @flow
import WalletsActions from './wallets-actions';
import RouterActions from './router-actions';
import AdaRedemptionActions from './ada-redemption-actions';
import WalletBackupActions from './wallet-backup-actions';
import TransactionsActions from './transactions-actions';
import NodeUpdateActions from './node-update-actions';
import SidebarActions from './sidebar-actions';
import WindowActions from './window-actions';
import NetworkStatusActions from './network-status-actions';
import ProfileActions from './profile-actions';
import WalletSettingsActions from './wallet-settings-actions';
import DialogsActions from './dialogs-actions';
import NotificationsActions from './notifications-actions';
import AddressesActions from './addresses-actions';

export type ActionsMap = {
  router: RouterActions,
  wallets: WalletsActions,
  adaRedemption: AdaRedemptionActions,
  walletBackup: WalletBackupActions,
  transactions: TransactionsActions,
  nodeUpdate: NodeUpdateActions,
  sidebar: SidebarActions,
  window: WindowActions,
  networkStatus: NetworkStatusActions,
  profile: ProfileActions,
  walletSettings: WalletSettingsActions,
  dialogs: DialogsActions,
  notifications: NotificationsActions,
  addresses: AddressesActions,
};

const actionsMap: ActionsMap = {
  router: new RouterActions(),
  wallets: new WalletsActions(),
  adaRedemption: new AdaRedemptionActions(),
  walletBackup: new WalletBackupActions(),
  transactions: new TransactionsActions(),
  nodeUpdate: new NodeUpdateActions(),
  sidebar: new SidebarActions(),
  window: new WindowActions(),
  networkStatus: new NetworkStatusActions(),
  profile: new ProfileActions(),
  walletSettings: new WalletSettingsActions(),
  dialogs: new DialogsActions(),
  notifications: new NotificationsActions(),
  addresses: new AddressesActions(),
};

export default actionsMap;
