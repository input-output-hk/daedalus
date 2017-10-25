// @flow
import WalletsActions from './wallets-actions';
import WalletSettingsActions from './wallet-settings-actions';

export type EtcActionsMap = {
  wallets: WalletsActions,
  walletSettings: WalletSettingsActions,
};

const etcActionsMap: EtcActionsMap = {
  wallets: new WalletsActions(),
  walletSettings: new WalletSettingsActions(),
};

export default etcActionsMap;
