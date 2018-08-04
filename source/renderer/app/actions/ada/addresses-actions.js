// @flow
import Action from '../lib/Action';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  createAddress: Action<{
    spendingPassword?: string,
    accountIndex?: number,
    walletId: string
  }> = new Action();

  resetErrors: Action<any> = new Action();
}
