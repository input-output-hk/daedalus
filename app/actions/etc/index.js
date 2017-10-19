// @flow
import WalletsActions from './wallets-actions';
import WalletBackupActions from './wallet-backup-actions';

export type EtcActionsMap = {
  wallets: WalletsActions,
  walletBackup: WalletBackupActions,
};

const etcActionsMap: EtcActionsMap = {
  wallets: new WalletsActions(),
  walletBackup: new WalletBackupActions(),
};

export default etcActionsMap;
