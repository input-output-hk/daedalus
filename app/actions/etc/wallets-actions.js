// @flow
import Action from '../lib/Action';

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, password: ?string }> = new Action();
}
