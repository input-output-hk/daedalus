// @flow
import walletsActions from './wallets-actions';
import type { WalletsActions } from './wallets-actions';
import routerActions from './router-actions';
import adaRedemptionActions from './ada-redemption-actions';
import type { AdaRedemptionActions } from './ada-redemption-actions';
import walletBackupActions from './wallet-backup-actions';
import transactionsActions from './transactions-actions';
import nodeUpdateActions from './node-update-actions';
import sidebarActions from './sidebar-actions';
import windowActions from './window-actions';
import networkStatusActions from './network-status-actions';
import profileActions from './profile-actions';
import walletSettingsActions from './wallet-settings-actions';
import dialogsActions from './dialogs-actions';
import type { DialogsActions } from './dialogs-actions';

export type ActionsMap = {
  router: routerActions,
  wallets: WalletsActions,
  adaRedemption: AdaRedemptionActions,
  walletBackup: walletBackupActions,
  transactions: transactionsActions,
  nodeUpdate: nodeUpdateActions,
  sidebar: sidebarActions,
  window: windowActions,
  networkStatus: networkStatusActions,
  profile: profileActions,
  walletSettings: walletSettingsActions,
  dialogs: DialogsActions,
};

const actionsMap: ActionsMap = {
  router: routerActions,
  wallets: walletsActions,
  adaRedemption: adaRedemptionActions,
  walletBackup: walletBackupActions,
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
