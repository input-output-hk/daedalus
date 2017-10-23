// @flow
import Action from '../lib/Action';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, password: ?string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string, password: ?string }> = new Action();
}
