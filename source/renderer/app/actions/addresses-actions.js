// @flow
import Action from './lib/Action';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  createAddress: Action<{ walletId: string, spendingPassword: ?string }> = new Action();
  resetErrors: Action<any> = new Action();
}
