// @flow
import { observable, action } from 'mobx';
import { WalletStore } from './WalletStore';
import { Wallet } from '../../domain/Wallet';

export class UiStore {

  walletStore: WalletStore;
  @observable i18n: Object;
  @observable selectedWallet: Wallet;
  @observable router: Object;
  @observable sidebar: Object;
  @observable walletsForSidebar: Array<Object> = this.walletStore.wallets.map(wallet => ({
    id: wallet.address,
    title: wallet.name,
    info: `${wallet.amount} ${wallet.currency}`
  }));

  constructor(walletStore: WalletStore) {
    this.walletStore = walletStore;
    this.i18n = { locale: 'en-US' };
    this.sidebar = {
      route: '/wallets',
      hidden: false,
      showMenu: true
    };
  }

  @action setRouter(router: Object) {
    this.router = router;
  }

}
