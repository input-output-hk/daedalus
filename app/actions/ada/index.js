// @flow
import WalletsActions from './wallets-actions';
import AdaRedemptionActions from './ada-redemption-actions';
import WalletBackupActions from './wallet-backup-actions';
import TransactionsActions from './transactions-actions';
import NodeUpdateActions from './node-update-actions';
import WalletSettingsActions from './wallet-settings-actions';
import AddressesActions from './addresses-actions';

export type AdaActionsMap = {
  wallets: WalletsActions,
  adaRedemption: AdaRedemptionActions,
  walletBackup: WalletBackupActions,
  transactions: TransactionsActions,
  nodeUpdate: NodeUpdateActions,
  walletSettings: WalletSettingsActions,
  addresses: AddressesActions,
};

const adaActionsMap: AdaActionsMap = {
  wallets: new WalletsActions(),
  adaRedemption: new AdaRedemptionActions(),
  walletBackup: new WalletBackupActions(),
  transactions: new TransactionsActions(),
  nodeUpdate: new NodeUpdateActions(),
  walletSettings: new WalletSettingsActions(),
  addresses: new AddressesActions(),
};

export default adaActionsMap;
