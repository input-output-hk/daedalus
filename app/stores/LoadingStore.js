// @flow
import Store from './lib/Store';

export default class LoadingStore extends Store {

  constructor(...args) {
    super(...args);
    this.registerReactions([
      this._redirectToWalletAfterLoading,
    ]);
  }

  _redirectToWalletAfterLoading = () => {
    // TODO: uncomment commented lines when reintroducing login
    const {router, wallets} = this.stores;
    if (wallets.all.length) {
      const walletToShowAfterLogin = wallets.all[0]; // just pick the first for now
      if (router.location.pathname === '/') {
        router.push(wallets.getWalletRoute(walletToShowAfterLogin.id));
      }
    }
  };

}
