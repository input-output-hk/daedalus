// @flow
import walletsActions from './wallets-actions';
import type { WalletsActions } from './wallets-actions';
import routerActions from './router-actions';
import type { RouterActions } from './router-actions';
import adaRedemptionActions from './ada-redemption-actions';
import type { AdaRedemptionActions } from './ada-redemption-actions';
import WalletBackupActions from './wallet-backup-actions';
import transactionsActions from './transactions-actions';
import type { TransactionsActions } from './transactions-actions';
import nodeUpdateActions from './node-update-actions';
import type { NodeUpdateActions } from './node-update-actions';
import sidebarActions from './sidebar-actions';
import type { SidebarActions } from './sidebar-actions';
import windowActions from './window-actions';
import networkStatusActions from './network-status-actions';
import type { NetworkStatusActions } from './network-status-actions';
import profileActions from './profile-actions';
import type { ProfileActions } from './profile-actions';
import walletSettingsActions from './wallet-settings-actions';
import dialogsActions from './dialogs-actions';
import type { DialogsActions } from './dialogs-actions';

export type ActionsMap = {
  router: RouterActions,
  wallets: WalletsActions,
  adaRedemption: AdaRedemptionActions,
  walletBackup: WalletBackupActions,
  transactions: TransactionsActions,
  nodeUpdate: NodeUpdateActions,
  sidebar: SidebarActions,
  window: windowActions,
  networkStatus: NetworkStatusActions,
  profile: ProfileActions,
  walletSettings: walletSettingsActions,
  dialogs: DialogsActions,
};

const actionsMap: ActionsMap = {
  router: routerActions,
  wallets: walletsActions,
  adaRedemption: adaRedemptionActions,
  walletBackup: new WalletBackupActions(),
  transactions: transactionsActions,
  nodeUpdate: nodeUpdateActions,
  sidebar: sidebarActions,
  window: windowActions,
  networkStatus: networkStatusActions,
  profile: profileActions,
  walletSettings: walletSettingsActions,
  dialogs: dialogsActions,
};

export default actionsMap;
