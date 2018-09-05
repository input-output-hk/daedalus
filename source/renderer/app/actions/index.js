// @flow
import RouterActions from './router-actions';
import SidebarActions from './sidebar-actions';
import WindowActions from './window-actions';
import NetworkStatusActions from './network-status-actions';
import WalletBackupActions from './wallet-backup-actions';
import ProfileActions from './profile-actions';
import DialogsActions from './dialogs-actions';
import NotificationsActions from './notifications-actions';
import adaActionsMap from './ada/index';
import type { AdaActionsMap } from './ada/index';
import AppActions from './app-actions';

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
  ada: AdaActionsMap,
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
  ada: adaActionsMap,
};

export default actionsMap;
