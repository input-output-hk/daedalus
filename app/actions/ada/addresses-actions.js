// @flow
import Action from '../lib/Action';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  createAddress: Action<{ walletId: string, password: ?string }> = new Action();
  resetErrors: Action<any> = new Action();
}
