// @flow
import Action from './lib/Action';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, currency: string, password: ?string }> = new Action();
  // eslint-disable-next-line max-len
  restoreWallet: Action<{recoveryPhrase: string, walletName: string, walletPassword: ?string }> = new Action();
  importWalletFromKey: Action<{ filePath: string }> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string }> = new Action();
  setActiveWallet: Action<{ walletId: string }> = new Action();
  // TODO: refactor dialog toggles to use dialog-actions instead
  toggleAddWallet: Action<any> = new Action();
  toggleCreateWalletDialog: Action<any> = new Action();
  toggleWalletRestore: Action<any> = new Action();
  toggleWalletKeyImportDialog: Action<any> = new Action();
  showWalletAddressCopyNotification: Action<any> = new Action();
}
