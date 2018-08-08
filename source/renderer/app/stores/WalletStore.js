// @flow
import { observable, action, computed, runInAction } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Wallet from '../domains/Wallet';
import Request from './lib/LocalizedRequest';
import { buildRoute, matchRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import type { GetWalletRecoveryPhraseResponse } from '../api/common';
import environment from '../../../common/environment';

/**
 * The base wallet store that contains the shared logic
 * dealing with wallets / accounts.
 */

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;
  @observable walletsRequest: Request<any>;
  @observable createWalletRequest: Request<any>;
  @observable deleteWalletRequest: Request<any>;
  @observable getWalletRecoveryPhraseRequest: Request<any>;
  @observable restoreRequest: Request<any>;
  @observable isRestoreActive: boolean = false;
  @observable lastDiscardedAntivirusRestorationSlowdownNotificationWalletId: ?string = null;

  _newWalletDetails: { name: string, mnemonic: string, password: ?string } = {
    name: '',
    mnemonic: '',
    password: null,
  };

  setup() {
    setInterval(this._pollRefresh, this.WALLET_REFRESH_INTERVAL);
    this.actions.ada.wallets.discardAntivirusRestorationSlowdownNotificationForActiveWallet.listen(
      this._discardAntivirusNotificationForRestoration
    );
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._showAddWalletPageWhenNoWallets,
    ]);
  }

  _create = async (params: {
    name: string,
    password: ?string,
  }) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase: ?GetWalletRecoveryPhraseResponse = await (
        this.getWalletRecoveryPhraseRequest.execute().promise
      );
      if (recoveryPhrase != null) {
        this.actions.walletBackup.initiateWalletBackup.trigger({ recoveryPhrase });
      }
    } catch (error) {
      throw error;
    }
  };

  _finishCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails).promise;
    if (wallet) {
      await this.walletsRequest.patch(result => { result.push(wallet); });
      this.actions.dialogs.closeActiveDialog.trigger();
      this.goToWalletRoute(wallet.id);
    } else {
      this.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
    }
  };

  _delete = async (params: { walletId: string }) => {
    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) return;
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
    if (this.hasDiscardedAntivirusRestorationSlowdownNotificationForActiveWallet) {
      this._resetAntivirusNotificationForActiveWallet();
    }
    await this.deleteWalletRequest.execute({ walletId: params.walletId });
    await this.walletsRequest.patch(result => {
      result.splice(indexOfWalletToDelete, 1);
    });
    runInAction('WalletsStore::_delete', () => {
      if (this.hasAnyWallets) {
        const nextIndexInList = Math.max(indexOfWalletToDelete - 1, 0);
        const nextWalletInList = this.all[nextIndexInList];
        this.actions.dialogs.closeActiveDialog.trigger();
        this.goToWalletRoute(nextWalletInList.id);
      } else {
        this.active = null;
        this.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
      }
    });
    this.deleteWalletRequest.reset();
    this.refreshWalletsData();
  };

  _restore = async (params: {
    recoveryPhrase: string,
    walletName: string,
    walletPassword: ?string,
  }) => {
    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  // =================== PUBLIC API ==================== //

  // GETTERS

  @computed get hasActiveWallet(): boolean {
    return !!this.active;
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

  @computed get activeWalletRoute(): ?string {
    if (!this.active) return null;
    return this.getWalletRoute(this.active.id);
  }

  @computed get isWalletRoute(): boolean {
    const { currentRoute } = this.stores.app;
    return matchRoute(ROUTES.WALLETS.ROOT + '(/*rest)', currentRoute);
  }

  @computed get hasDiscardedAntivirusRestorationSlowdownNotificationForActiveWallet(): boolean {
    if (!this.active) return false;
    return this.lastDiscardedAntivirusRestorationSlowdownNotificationWalletId === this.active.id;
  }

  getWalletById = (id: string): (?Wallet) => this.all.find(w => w.id === id);

  getWalletByName = (name: string): (?Wallet) => this.all.find(w => w.name === name);

  getWalletRoute = (walletId: string, page: string = 'summary'): string => (
    buildRoute(ROUTES.WALLETS.PAGE, { id: walletId, page })
  );

  // ACTIONS

  @action refreshWalletsData = async () => {
    if (!this.stores.networkStatus.isConnected) return;
    const result = await this.walletsRequest.execute().promise;
    if (!result) return;
    runInAction('refresh active wallet', () => {
      if (this.active) {
        this._setActiveWallet({ walletId: this.active.id });
      }
    });
    const transactions = this.stores[environment.API].transactions;
    runInAction('refresh transaction data', () => {
      const walletIds = result.map((wallet: Wallet) => wallet.id);
      transactions.transactionsRequests = walletIds.map(walletId => ({
        walletId,
        recentRequest: transactions._getTransactionsRecentRequest(walletId),
        allRequest: transactions._getTransactionsAllRequest(walletId),
      }));
      transactions._refreshTransactionData();
    });
  };

  @action _setActiveWallet = ({ walletId }: { walletId: string }) => {
    if (this.hasAnyWallets) {
      this.active = this.all.find(wallet => wallet.id === walletId);
    }
  };

  @action _unsetActiveWallet = () => { this.active = null; };

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({ route });
  }

  // =================== PRIVATE API ==================== //

  @computed get _canRedirectToWallet(): boolean {
    const currentRoute = this.stores.app.currentRoute;
    const isRootRoute = matchRoute(ROUTES.WALLETS.ROOT, currentRoute);
    const isAddWalletRoute = matchRoute(ROUTES.WALLETS.ADD, currentRoute);
    return isRootRoute || isAddWalletRoute;
  }

  _patchWalletRequestWithNewWallet = async (wallet: Wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    await this.walletsRequest.patch(result => {
      if (!_.find(result, { id: wallet.id })) result.push(wallet);
    });
  };

  _pollRefresh = async () => (
    this.stores.networkStatus.isSynced && await this.refreshWalletsData()
  );

  _showAddWalletPageWhenNoWallets = () => {
    const isRouteThatNeedsWallets = this.isWalletRoute;
    if (isRouteThatNeedsWallets && !this.hasAnyWallets) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
    }
  };

  _updateActiveWalletOnRouteChanges = () => {
    const currentRoute = this.stores.app.currentRoute;
    const hasAnyWalletLoaded = this.hasAnyLoaded;
    const isWalletAddPage = matchRoute(ROUTES.WALLETS.ADD, currentRoute);
    runInAction('WalletsStore::_updateActiveWalletOnRouteChanges', () => {
      // There are not wallets loaded (yet) -> unset active and return
      if (isWalletAddPage || !hasAnyWalletLoaded) return this._unsetActiveWallet();
      const match = matchRoute(`${ROUTES.WALLETS.ROOT}/:id(*page)`, currentRoute);
      if (match) {
        // We have a route for a specific wallet -> lets try to find it
        const walletForCurrentRoute = this.all.find(w => w.id === match.id);
        if (walletForCurrentRoute) {
          // The wallet exists, we are done
          this._setActiveWallet({ walletId: walletForCurrentRoute.id });
        } else if (hasAnyWalletLoaded) {
          // There is no wallet with given id -> pick first wallet
          this._setActiveWallet({ walletId: this.all[0].id });
          if (this.active) this.goToWalletRoute(this.active.id);
        }
      } else if (this._canRedirectToWallet) {
        // The route does not specify any wallet -> pick first wallet
        if (!this.hasActiveWallet && hasAnyWalletLoaded) {
          this._setActiveWallet({ walletId: this.all[0].id });
        }
        if (this.active) {
          this.goToWalletRoute(this.active.id);
        }
      }
    });
  };

  @action _discardAntivirusNotificationForRestoration = () => {
    if (!this.active) return;
    this.lastDiscardedAntivirusRestorationSlowdownNotificationWalletId = this.active.id;
  };

  @action _resetAntivirusNotificationForActiveWallet = () => {
    this.lastDiscardedAntivirusRestorationSlowdownNotificationWalletId = null;
  }
}
