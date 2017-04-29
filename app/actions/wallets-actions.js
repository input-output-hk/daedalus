// @flow
import Action from './lib/Action';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, currency: string }> = new Action();
  restoreWallet: Action<{ recoveryPhrase: string, walletName: string }> = new Action();
  importWalletFromKey: Action<{ filePath: string }> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string }> = new Action();
  setActiveWallet: Action<{ walletId: string }> = new Action();
  showWalletAddressCopyNotification: Action<any> = new Action();
}
