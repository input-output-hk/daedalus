import walletsActions from './wallets-actions';
import routerActions from './router-actions';
import adaRedemptionActions from './ada-redemption-actions';
import walletBackupActions from './wallet-backup-actions';
import transactionsActions from './transactions-actions';
import nodeUpdateActions from './node-update-actions';
import sidebarActions from './sidebar-actions';
import windowActions from './window-actions';

export default {
  router: routerActions,
  wallets: walletsActions,
  adaRedemption: adaRedemptionActions,
  walletBackup: walletBackupActions,
  transactions: transactionsActions,
  nodeUpdate: nodeUpdateActions,
  sidebar: sidebarActions,
  window: windowActions,
};
