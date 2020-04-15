// @flow
import Action from './lib/Action';

export type WalletExportToFileParams = {
  walletId: string,
  exportType: string,
  filePath: string,
  password: ?string,
};

export default class WalletSettingsActions {
  cancelEditingWalletField: Action<any> = new Action();
  startEditingWalletField: Action<{ field: string }> = new Action();
  stopEditingWalletField: Action<any> = new Action();
  updateWalletField: Action<{ field: string, value: string }> = new Action();
  updateSpendingPassword: Action<{
    walletId: string,
    oldPassword: string,
    newPassword: string,
    isLegacy: boolean,
  }> = new Action();
  exportToFile: Action<WalletExportToFileParams> = new Action();
  startWalletUtxoPolling: Action<any> = new Action();
  stopWalletUtxoPolling: Action<any> = new Action();
  forceWalletResync: Action<{
    walletId: string,
    isLegacy: boolean,
  }> = new Action();
}
