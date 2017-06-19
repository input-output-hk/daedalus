// @flow
import Action from './lib/Action';
import type { walletExportTypeChoices } from '../types/walletExportTypes';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, password: ?string }> = new Action();
  // eslint-disable-next-line max-len
  restoreWallet: Action<{recoveryPhrase: string, walletName: string, walletPassword: ?string }> = new Action();
  importWalletFromKey: Action<{ filePath: string, walletPassword: ?string }> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string, password: ?string }> = new Action();
  setActiveWallet: Action<{ walletId: string }> = new Action();
  showWalletAddressCopyNotification: Action<any> = new Action();
  chooseWalletExportType: Action<{ walletExportType: walletExportTypeChoices }> = new Action();
}
