// @flow
import Action from './lib/Action';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  createByronWalletAddress: Action<{
    walletId: string,
    isLegacy: boolean,
    spendingPassword: ?string,
  }> = new Action();
  resetErrors: Action<any> = new Action();
}
