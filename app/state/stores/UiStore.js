import { observable, action } from 'mobx';
import { Wallet } from '../domain/Wallet';

export class UiStore {
  @observable i18n;
  @observable selectedWallet: Wallet;
  @observable router;
  @observable sidebar;

  constructor() {
    this.i18n = { locale: 'en-US' };
    this.sidebar = {
      route: '/wallets',
      hidden: false,
      showMenu: true
    };
  }

  @action setRouter(router) {
    this.router = router;
  }

  @action toggleSidebar() {
    this.sidebar.hidden = !this.sidebar.hidden;
  }

  @action selectWallet(wallet: Wallet) {
    this.selectedWallet = wallet;
  }

}
