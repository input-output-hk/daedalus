// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import Wallet from '../domain/Wallet';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';
import Request from './lib/Request';
import environment from '../environment';

export default class WalletsStore extends Store {

  BASE_ROUTE = '/wallets';
  WALLET_REFRESH_INTERVAL = 5000;

  @observable active = null;
  @observable walletsRequest = new CachedRequest(this.api, 'getWallets');
  @observable createWalletRequest = new Request(this.api, 'createWallet');
  @observable sendMoneyRequest = new Request(this.api, 'createTransaction');
  @observable getWalletRecoveryPhraseRequest = new Request(this.api, 'getWalletRecoveryPhrase');
  @observable restoreRequest = new Request(this.api, 'restoreWallet');
  // DIALOGUES
  @observable isAddWalletDialogOpen = false;
  @observable isCreateWalletDialogOpen = false;
  @observable isWalletRestoreDialogOpen = false;

  _newWalletDetails = null;

  constructor(...args) {
    super(...args);
    this.actions.createPersonalWallet.listen(this._createPersonalWallet);
    this.actions.sendMoney.listen(this._sendMoney);
    this.actions.toggleAddWallet.listen(this._toggleAddWallet);
    this.actions.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
    this.actions.toggleWalletRestore.listen(this._toggleWalletRestore);
    this.actions.finishWalletBackup.listen(this._finishWalletCreation);
    this.actions.restoreWallet.listen(this._restoreWallet);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._openAddWalletIfNoWallets,
    ]);
    if (environment.CARDANO_API) {
      setInterval(this.refreshWalletsData, this.WALLET_REFRESH_INTERVAL);
    }
  }

  _createPersonalWallet = async (params) => {
    this._newWalletDetails = params;
    try {
      const recoveryPhrase = await this.getWalletRecoveryPhraseRequest.execute();
      this.actions.initiateWalletBackup({ recoveryPhrase });
    } catch (error) {
      throw error;
    }
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails);
    await this.walletsRequest.patch(result => { result.push(wallet); });
    this.goToWalletRoute(wallet.id);
    this.isAddWalletDialogOpen = false;
  };

  _sendMoney = async (transactionDetails) => {
    const wallet = this.active;
    await this.sendMoneyRequest.execute({
      ...transactionDetails,
      walletId: wallet.id,
      amount: parseFloat(transactionDetails.amount),
      sender: wallet.address,
      currency: wallet.currency,
    });
    this.refreshWalletsData();
    this.goToWalletRoute(wallet.id);
  };

  @computed get hasLoadedWallets() {
    return this.walletsRequest.wasExecuted;
  }

  @computed get hasAnyWallets() {
    return this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0;
  }

  @computed get all() {
    return this.walletsRequest.result ? this.walletsRequest.result : [];
  }

  @computed get activeWalletRoute() {
    if (!this.active) return null;
    return this.getWalletRoute(this.active);
  }

  @computed get hasAnyLoaded() {
    return this.all.length > 0;
  }

  @computed get first() {
    return this.all.length > 0 ? this.all[0] : null;
  }

  getWalletRoute(walletId: ?string, screen = 'home') {
    return `${this.BASE_ROUTE}/${walletId}/${screen}`;
  }

  isValidAddress(address: string) {
    return this.api.isValidAddress('ADA', address);
  }

  isValidMnemonic(mnemonic: string) {
    return this.api.isValidMnemonic(mnemonic);
  }

  @action refreshWalletsData = () => {
    if (this.stores.networkStatus.isConnected) {
      this.walletsRequest.invalidate({ immediately: true });
      this.walletsRequest.execute();
      if (!this.walletsRequest.result) return;
      const walletIds = this.walletsRequest.result.map((wallet: Wallet) => wallet.id);
      this.stores.transactions.transactionsRequests = walletIds.map(walletId => ({
        walletId,
        recentRequest: this.stores.transactions._getTransactionsRecentRequest(walletId),
        allRequest: this.stores.transactions._getTransactionsAllRequest(walletId)
      }));
      this.stores.transactions._refreshTransactionData();
    }
  };

  @action _toggleAddWallet = () => {
    if (this.hasAnyWallets) this.isAddWalletDialogOpen = !this.isAddWalletDialogOpen;
  };

  @action _toggleCreateWalletDialog = () => {
    if (!this.isCreateWalletDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isCreateWalletDialogOpen = true;
    } else {
      this.isCreateWalletDialogOpen = false;
      if (!this.hasAnyWallets) {
        this.isAddWalletDialogOpen = true;
      }
    }
  };

  @action _toggleWalletRestore = () => {
    if (!this.isWalletRestoreDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isWalletRestoreDialogOpen = true;
    } else {
      this.isWalletRestoreDialogOpen = false;
      if (!this.hasAnyWallets) {
        this.isAddWalletDialogOpen = true;
      }
      this.restoreRequest.reset();
    }
  };

  @action _restoreWallet = async (params) => {
    const restoredWallet = await this.restoreRequest.execute(params);
    await this.walletsRequest.patch(result => { result.push(restoredWallet); });
    this._toggleWalletRestore();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  goToWalletRoute(walletId) {
    const route = this.getWalletRoute(walletId);
    this.actions.goToRoute({ route });
  }

  _openAddWalletIfNoWallets = () => {
    if (this.hasLoadedWallets && !this.hasAnyWallets) this.isAddWalletDialogOpen = true;
    // TODO: investigate why hasLoadedWallets is needed here, it is in hasAnyWallets
  };

  _updateActiveWalletOnRouteChanges = () => {
    const currentRoute = this.stores.router.location.pathname;
    const hasActiveWallet = !!this.active;
    const hasAnyWalletsLoaded = this.hasAnyLoaded;
    const match = matchRoute(`${this.BASE_ROUTE}/:id(*page)`, currentRoute);
    if (match) {
      // We have a route for a specific wallet -> lets try to find it
      const walletForCurrentRoute = this.all.find(w => w.id === match.id);
      if (walletForCurrentRoute) {
        // The wallet exists, we are done
        this.active = walletForCurrentRoute;
      } else if (hasAnyWalletsLoaded) {
        // There is no wallet with given id -> pick first wallet
        this.active = this.all[0];
        this.goToWalletRoute(this.active.id);
      }
    } else if (matchRoute(this.BASE_ROUTE, currentRoute)) {
      // The route does not specify any wallet -> pick first wallet
      if (!hasActiveWallet && hasAnyWalletsLoaded) this.active = this.all[0];
      if (this.active) this.goToWalletRoute(this.active.id);
    }
  }

}
