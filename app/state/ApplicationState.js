// @flow
import { UiStore } from './stores/UiStore';
import { WalletStore } from './stores/WalletStore';

export class ApplicationState {

  uiStore: UiStore;
  walletStore: WalletStore;

  constructor() {
    this.walletStore = new WalletStore();
    this.uiStore = new UiStore(this.walletStore);
  }

}
