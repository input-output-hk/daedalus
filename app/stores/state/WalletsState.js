// @flow
import { observable, computed } from 'mobx';
import Wallet from '../../domain/Wallet';
import AppStore from '../AppStore';

export default class SidebarState {

  @observable activeWallet: ?Wallet;
  @observable isLoading: boolean;
  @observable errorLoading: ?string;
  @observable errorCreating: ?string;
  @observable errorLoadingTransactions: ?string;
  @observable errorSendingMoney: ?string;
  store: AppStore;

  constructor(store: AppStore) {
    this.store = store;
    Object.assign(this, {
      activeWallet: null,
      isLoading: false,
      wallets: []
    });
  }

  @computed get wallets(): Array<Object> {
    return this.store.account.wallets;
  }
}
