// @flow
import { Action } from './lib/actions';

// ======= WALLET ACTIONS =======

export type WalletsActions = {
  createWallet: Action<{ name: string, currency: string }>,
  restoreWallet: Action<{ recoveryPhrase: string, walletName: string }>,
  importWalletFromKey: Action<{ filePath: string }>,
  deleteWallet: Action<{ walletId: string }>,
  sendMoney: Action<{ receiver: string, amount: string }>,
  setActiveWallet: Action<{ walletId: string }>,
  // TODO: refactor dialog toggles to use dialog-actions instead
  toggleAddWallet: Action<any>,
  toggleCreateWalletDialog: Action<any>,
  toggleWalletRestore: Action<any>,
  toggleWalletKeyImportDialog: Action<any>,
  showWalletAddressCopyNotification: Action<any>,
};

const walletActions: WalletsActions = {
  createWallet: new Action(),
  restoreWallet: new Action(),
  importWalletFromKey: new Action(),
  deleteWallet: new Action(),
  sendMoney: new Action(),
  setActiveWallet: new Action(),
  // TODO: refactor dialog toggles to use dialog-actions instead
  toggleAddWallet: new Action(),
  toggleCreateWalletDialog: new Action(),
  toggleWalletRestore: new Action(),
  toggleWalletKeyImportDialog: new Action(),
  showWalletAddressCopyNotification: new Action(),
};

export default walletActions;
