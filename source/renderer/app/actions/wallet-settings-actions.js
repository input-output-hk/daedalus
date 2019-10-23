// @flow
import Action from './lib/Action';

export default class WalletSettingsActions {
  cancelEditingWalletField: Action<any> = new Action();
  startEditingWalletField: Action<{ field: string }> = new Action();
  stopEditingWalletField: Action<any> = new Action();
  updateWalletField: Action<{ field: string, value: string }> = new Action();
  // eslint-disable-next-line max-len
  updateSpendingPassword: Action<{
    walletId: string,
    oldPassword: ?string,
    newPassword: ?string,
  }> = new Action();
  startWalletUtxoPolling: Action<any> = new Action();
  stopWalletUtxoPolling: Action<any> = new Action();
}
