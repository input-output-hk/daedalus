// @flow
import Action from '../lib/Action';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, password: ?string }> = new Action();
  // eslint-disable-next-line max-len
  restoreWallet: Action<{recoveryPhrase: string, walletName: string, walletPassword: ?string }> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string, password: ?string }> = new Action();
}
