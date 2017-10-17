// @flow
import { observable, action, computed, runInAction } from 'mobx';
import Store from '../lib/Store';
import Wallet from '../../domain/Wallet';
import Request from '.././lib/LocalizedRequest';
import { buildRoute, matchRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../routes-config';

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<any> = new Request(this.api.etc.getWallets);
  /* eslint-enable max-len */

  setup() {
    setInterval(this.pollRefresh, this.WALLET_REFRESH_INTERVAL);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
    ]);
  }

  @computed get hasLoadedWallets(): boolean {
    return this.walletsRequest.wasExecuted;
  }

  @computed get hasAnyWallets(): boolean {
    if (this.walletsRequest.result == null) return false;
    return this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0;
  }

  @computed get all(): Array<Wallet> {
    return this.walletsRequest.result ? this.walletsRequest.result : [];
  }

  @computed get first(): ?Wallet {
    return this.all.length > 0 ? this.all[0] : null;
  }

  @computed get hasAnyLoaded(): boolean {
    return this.all.length > 0;
  }

  @action refreshAccountsData = () => this.walletsRequest.execute();

  pollRefresh = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.refreshAccountsData();
    }
  };

  getWalletRoute = (walletId: string, page: string = 'summary'): string => (
    buildRoute(ROUTES.WALLETS.PAGE, { id: walletId, page })
  );

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({ route });
  }

  @action _setActiveWallet = ({ walletId }: { walletId: string }) => {
    if (this.hasAnyWallets) {
      const activeWalletId = this.active ? this.active.id : null;
      const activeWalletChange = activeWalletId !== walletId;
      if (activeWalletChange) this.stores.ada.addresses.lastGeneratedAddress = null;
      this.active = this.all.find(wallet => wallet.id === walletId);
    }
  };

  @action _unsetActiveWallet = () => {
    this.active = null;
    this.stores.ada.addresses.lastGeneratedAddress = null;
  };

  _updateActiveWalletOnRouteChanges = () => {
    const currentRoute = this.stores.app.currentRoute;
    const hasAnyWalletsLoaded = this.hasAnyLoaded;
    runInAction('WalletsStore::_updateActiveWalletOnRouteChanges', () => {
      // There are not wallets loaded (yet) -> unset active and return
      if (!hasAnyWalletsLoaded) return this._unsetActiveWallet();
      const match = matchRoute(`${ROUTES.WALLETS.ROOT}/:id(*page)`, currentRoute);
      if (match) {
        // We have a route for a specific wallet -> lets try to find it
        const walletForCurrentRoute = this.all.find(w => w.id === match.id);
        if (walletForCurrentRoute) {
          // The wallet exists, we are done
          this._setActiveWallet({ walletId: walletForCurrentRoute.id });
        } else if (hasAnyWalletsLoaded) {
          // There is no wallet with given id -> pick first wallet
          this._setActiveWallet({ walletId: this.all[0].id });
          if (this.active) this.goToWalletRoute(this.active.id);
        }
      } else if (this._canRedirectToWallet) {
        // The route does not specify any wallet -> pick first wallet
        if (!this.hasActiveWallet && hasAnyWalletsLoaded) {
          this._setActiveWallet({ walletId: this.all[0].id });
        }
        if (this.active) this.goToWalletRoute(this.active.id);
      }
    });
  };

}
