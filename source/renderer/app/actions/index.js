// @flow
import AppActions from './app-actions';
import BlockConsolidationActions from './block-consolidation-actions';
import DialogsActions from './dialogs-actions';
import NetworkStatusActions from './network-status-actions';
import NodeUpdateActions from './node-update-actions';
import NotificationsActions from './notifications-actions';
import ProfileActions from './profile-actions';
import RouterActions from './router-actions';
import SidebarActions from './sidebar-actions';
import StakingActions from './staking-actions';
import TransactionsActions from './transactions-actions';
import WalletsActions from './wallets-actions';
import WalletBackupActions from './wallet-backup-actions';
import WalletSettingsActions from './wallet-settings-actions';
import WindowActions from './window-actions';

export type ActionsMap = {
  app: AppActions,
  blockConsolidation: BlockConsolidationActions,
  dialogs: DialogsActions,
  networkStatus: NetworkStatusActions,
  nodeUpdate: NodeUpdateActions,
  notifications: NotificationsActions,
  profile: ProfileActions,
  router: RouterActions,
  sidebar: SidebarActions,
  staking: StakingActions,
  transactions: TransactionsActions,
  wallets: WalletsActions,
  walletBackup: WalletBackupActions,
  walletSettings: WalletSettingsActions,
  window: WindowActions,
};

const actionsMap: ActionsMap = {
  app: new AppActions(),
  blockConsolidation: new BlockConsolidationActions(),
  dialogs: new DialogsActions(),
  networkStatus: new NetworkStatusActions(),
  nodeUpdate: new NodeUpdateActions(),
  notifications: new NotificationsActions(),
  profile: new ProfileActions(),
  router: new RouterActions(),
  sidebar: new SidebarActions(),
  staking: new StakingActions(),
  transactions: new TransactionsActions(),
  wallets: new WalletsActions(),
  walletBackup: new WalletBackupActions(),
  walletSettings: new WalletSettingsActions(),
  window: new WindowActions(),
};

export default actionsMap;
