// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';
import Request from './lib/Request';
import environment from '../environment';

export default class WalletsStore extends Store {

  BASE_ROUTE = '/wallets';

  @observable walletsRequest = new CachedRequest(this.api, 'getWallets');
  @observable createWalletRequest = new Request(this.api, 'createWallet');
  @observable sendMoneyRequest = new Request(this.api, 'createTransaction');
  @observable getWalletRecoveryPhraseRequest = new Request(this.api, 'getWalletRecoveryPhrase');
  @observable walletBackup = { inProgress: false };

  constructor(...args) {
    super(...args);
    this.actions.createPersonalWallet.listen(this._createPersonalWallet);
    this.actions.sendMoney.listen(this._sendMoney);
    this.actions.initiateWalletBackup.listen(this._initiateWalletBackup);
    this.actions.acceptWalletBackupStart.listen(this._acceptWalletBackupStart);
    if (environment.CARDANO_API) setInterval(this._refreshWalletsData, 5000);
  }

  _createPersonalWallet = async (params) => {
    const wallet = await this.createWalletRequest.execute(params);
    await this.walletsRequest.patch(result => { result.push(wallet); });
    const walletRecovery = await this.getWalletRecoveryPhraseRequest.execute({ walletId: wallet.id });
    this.actions.initiateWalletBackup(walletRecovery);
    // TODO: When wallet backup is complete
    // this.actions.goToRoute({ route: this.getWalletRoute(wallet.id) });
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
    this.walletsRequest.invalidate({ immediately: true });
    this.stores.transactions.searchRequest.invalidate({ immediately: true });
  };

  @action _initiateWalletBackup = (params) => {
    this.actions.toggleCreateWalletDialog();
    const { walletId, recoveryPhrase } = params;
    this.walletBackup = {
      inProgress: true,
      walletId,
      recoveryPhrase,
      completed: false,
      enteredPhrase:[],
      isEntering: false,
      isValid: false,
      isWalletBackupStartAccepted: false,
      countdownRemaining: 10,
      countdownTimer: null
    };
    this.walletBackup.countdownTimer = setInterval(() => {
      if (this.walletBackup.countdownRemaining > 0) {
        action(() => this.walletBackup.countdownRemaining--)();
      } else {
        clearInterval(this.walletBackup.countdownTimer);
      }
    }, 1000);
  };

  @action _acceptWalletBackupStart = () => {
    this.walletBackup.isWalletBackupStartAccepted = true;
  };

}
