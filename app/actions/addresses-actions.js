// @flow
import Action from './lib/Action';

// ======= ADDRESSES ACTIONS =======

export default class AddressesActions {
  createNew: Action<{ data: Object }> = new Action();
}
