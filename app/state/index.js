// @flow
import { UiStore } from './stores/UiStore';
import { WalletStore } from './stores/WalletStore';

class ApplicationState {

  uiStore: UiStore;
  walletsStore: WalletStore;

  constructor() {
    this.uiStore = new UiStore();
    this.walletsStore = new WalletStore(this.uiStore);
  }

}

const singleton = new ApplicationState();
export default singleton;
