// @flow
import { observable, computed, action } from 'mobx';
import _ from 'lodash';
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
  @observable importFromKeyRequest = new Request(this.api, 'importWalletFromKey');
  @observable createWalletRequest = new Request(this.api, 'createWallet');
  @observable sendMoneyRequest = new Request(this.api, 'createTransaction');
  @observable getWalletRecoveryPhraseRequest = new Request(this.api, 'getWalletRecoveryPhrase');
  @observable restoreRequest = new Request(this.api, 'restoreWallet');
  // DIALOGUES
  @observable isAddWalletDialogOpen = false;
  @observable isCreateWalletDialogOpen = false;
  @observable isWalletRestoreDialogOpen = false;
  @observable isWalletKeyImportDialogOpen = false;

  _newWalletDetails: { name: string, currency: string, mnemonic: string, } = {
    name: '',
    currency: '',
    mnemonic: ''
  };

  setup() {
    const { wallets, walletBackup } = this.actions;
    wallets.create.listen(this._create);
    wallets.sendMoney.listen(this._sendMoney);
    wallets.toggleAddWallet.listen(this._toggleAddWallet);
    wallets.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
    wallets.toggleWalletRestore.listen(this._toggleWalletRestore);
    wallets.restoreWallet.listen(this._restoreWallet);
    wallets.importWalletFromKey.listen(this._importWalletFromKey);
    wallets.toggleWalletKeyImportDialog.listen(this._toggleWalletKeyImportDialog);
    walletBackup.finishWalletBackup.listen(this._finishWalletCreation);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._openAddWalletIfNoWallets,
    ]);
    if (environment.CARDANO_API) {
      setInterval(this.refreshWalletsData, this.WALLET_REFRESH_INTERVAL);
    }
  }

  _create = async (params: {
    name: string,
    currency: string,
  }) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase = await this.getWalletRecoveryPhraseRequest.execute();
      this.actions.walletBackup.initiateWalletBackup({ recoveryPhrase });
    } catch (error) {
      throw error;
    }
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails).promise;
    if (wallet) {
      await this.walletsRequest.patch(result => {
        result.push(wallet);
      });
      this.goToWalletRoute(wallet.id);
      this.isAddWalletDialogOpen = false;
    }
  };

  _sendMoney = async (transactionDetails: {
    receiver: string,
    amount: string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
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

  @computed get hasLoadedWallets(): bool {
    return this.walletsRequest.wasExecuted;
  }

  @computed get hasAnyWallets(): bool {
    return this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0;
  }

  @computed get all(): Array<Wallet> {
    return this.walletsRequest.result ? this.walletsRequest.result : [];
  }

  @computed get activeWalletRoute(): ?string {
    if (!this.active) return null;
    return this.getWalletRoute(this.active.id);
  }

  @computed get hasAnyLoaded(): bool {
    return this.all.length > 0;
  }

  @computed get first(): ?Wallet {
    return this.all.length > 0 ? this.all[0] : null;
  }


  getWalletRoute = (walletId: string, screen: string = 'summary'): string => {
    return `${this.BASE_ROUTE}/${walletId}/${screen}`;
  };

  isValidAddress = (address: string) => this.api.isValidAddress('ADA', address);

  isValidMnemonic = (mnemonic: string) => this.api.isValidMnemonic(mnemonic);

  @action refreshWalletsData = async () => {
    if (this.stores.networkStatus.isConnected) {
      this.walletsRequest.invalidate();
      const result = await this.walletsRequest.execute();
      if (!result) return;
      const walletIds = result.map((wallet: Wallet) => wallet.id);
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

  @action _toggleWalletKeyImportDialog = () => {
    if (!this.isWalletKeyImportDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isWalletKeyImportDialogOpen = true;
    } else {
      this.isWalletKeyImportDialogOpen = false;
      if (!this.hasAnyWallets) {
        this.isAddWalletDialogOpen = true;
      }
      this.importFromKeyRequest.reset();
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

  @action _restoreWallet = async (params: {
    recoveryPhrase: string,
    walletName: string,
  }) => {
    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this._toggleWalletRestore();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  @action _importWalletFromKey = async (params: {
    filePath: string,
  }) => {
    const importedWallet = await this.importFromKeyRequest.execute(params).promise;
    if (!importedWallet) throw new Error('Imported wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(importedWallet);
    this._toggleWalletKeyImportDialog();
    this.goToWalletRoute(importedWallet.id);
    this.refreshWalletsData();
  };

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute({ route });
  }

  _openAddWalletIfNoWallets = () => {
    // TODO: investigate why hasLoadedWallets is needed here, it is in hasAnyWallets
    if (this.hasLoadedWallets) {
      if (!this.hasAnyWallets) {
        this.isAddWalletDialogOpen = true;
      } else {
        this.isAddWalletDialogOpen = false;
      }
    }
  };

  _updateActiveWalletOnRouteChanges = () => {
    const currentRoute = this.stores.app.currentRoute;
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
  };

  _patchWalletRequestWithNewWallet = async (wallet: Wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    await this.walletsRequest.patch(result => {
      if (!_.find(result, { id: wallet.id })) result.push(wallet);
    });
  };

}
