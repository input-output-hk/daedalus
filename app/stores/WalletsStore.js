// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';
import Request from './lib/Request';
import environment from '../environment';

export default class WalletsStore extends Store {

  BASE_ROUTE = '/wallets';
  WALLET_REFRESH_INTERVAL = 5000;

  @observable walletsRequest = new CachedRequest(this.api, 'getWallets');
  @observable createWalletRequest = new Request(this.api, 'createWallet');
  @observable sendMoneyRequest = new Request(this.api, 'createTransaction');
  @observable getWalletRecoveryPhraseRequest = new Request(this.api, 'getWalletRecoveryPhrase');
  // DIALOGUES
  @observable isAddWalletDialogOpen = false;
  @observable isCreateWalletDialogOpen = false;
  @observable isWalletImportDialogOpen = false;

  _newWalletDetails = null;

  constructor(...args) {
    super(...args);
    this.actions.createPersonalWallet.listen(this._createPersonalWallet);
    this.actions.sendMoney.listen(this._sendMoney);
    this.actions.toggleAddWallet.listen(this._toggleAddWallet);
    this.actions.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
    this.actions.toggleWalletImport.listen(this._toggleWalletImport);
    this.actions.finishWalletBackup.listen(this._finishWalletCreation);
    if (environment.CARDANO_API) {
      setInterval(this._refreshWalletsData, this.WALLET_REFRESH_INTERVAL);
    }
  }

  _createPersonalWallet = async (params) => {
    this._newWalletDetails = params;
    try {
      const recoveryPhrase = await this.getWalletRecoveryPhraseRequest.execute();
      this.actions.initiateWalletBackup({ recoveryPhrase });
    } catch(error) {
      throw error;
    }
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails);
    await this.walletsRequest.patch(result => { result.push(wallet); });
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
    this._refreshWalletsData();
    this.actions.goToRoute({ route: this.getWalletRoute(wallet.id) });
  };

  @computed get all() {
    return this.walletsRequest.execute(this.stores.user.active.id).result || [];
  }

  @computed get active() {
    const currentRoute = this.stores.router.location.pathname;
    const match = matchRoute(`${this.BASE_ROUTE}/:id(*page)`, currentRoute);
    if (match) return this.all.find(w => w.id === match.id) || null;
    return null;
  }

  getWalletRoute(walletId: ?string, screen = 'home') {
    return `${this.BASE_ROUTE}/${walletId}/${screen}`;
  }

  isValidAddress(address: string) {
    return this.api.isValidAddress('ADA', address);
  }

  _refreshWalletsData = () => {
    if (this.stores.networkStatus.isCardanoConnected) {
      this.walletsRequest.invalidate({ immediately: true });
      this.stores.transactions.searchRequest.invalidate({ immediately: true });
    }
  };

  @action _toggleAddWallet = () => {
    this.isAddWalletDialogOpen = !this.isAddWalletDialogOpen;
  };

  @action _toggleCreateWalletDialog = () => {
    if (!this.isCreateWalletDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isCreateWalletDialogOpen = true;
    } else {
      this.isCreateWalletDialogOpen = false;
    }
  };

  @action _toggleWalletImport = () => {
    if (!this.isWalletImportDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isWalletImportDialogOpen = true;
    } else {
      this.isWalletImportDialogOpen = false;
    }
  };

}
