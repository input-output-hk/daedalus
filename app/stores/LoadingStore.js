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
    const { router, wallets } = this.stores;
    if (wallets.all.length && router.location.pathname === '/') {
      router.push(wallets.getWalletRoute(wallets.all[0].id)); // just pick the first for now
    }
  };

}
