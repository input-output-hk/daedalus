// @flow
import { observable, computed, action, runInAction } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Wallet from '../domain/Wallet';
import { matchRoute, buildRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';
import Request from './lib/Request';
import environment from '../environment';
import config from '../config';
import { ROUTES } from '../Routes';

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;
  @observable walletsRequest = new CachedRequest(this.api, 'getWallets');
  @observable importFromKeyRequest = new Request(this.api, 'importWalletFromKey');
  @observable createWalletRequest = new Request(this.api, 'createWallet');
  @observable deleteWalletRequest = new Request(this.api, 'deleteWallet');
  @observable sendMoneyRequest = new Request(this.api, 'createTransaction');
  @observable getWalletRecoveryPhraseRequest = new Request(this.api, 'getWalletRecoveryPhrase');
  @observable restoreRequest = new Request(this.api, 'restoreWallet');
  // DIALOGUES
  @observable isAddWalletDialogOpen = false;
  @observable isCreateWalletDialogOpen = false;
  @observable isWalletRestoreDialogOpen = false;
  @observable isWalletKeyImportDialogOpen = false;
  @observable isWalletAddressCopyNotificationVisible = false;

  _hideWalletAddressCopyNotificationTimeout = false;

  _newWalletDetails: { name: string, currency: string, mnemonic: string, } = {
    name: '',
    currency: '',
    mnemonic: ''
  };

  setup() {
    const { wallets, walletBackup, router } = this.actions;
    wallets.createWallet.listen(this._create);
    wallets.deleteWallet.listen(this._delete);
    wallets.sendMoney.listen(this._sendMoney);
    wallets.toggleAddWallet.listen(this._toggleAddWallet);
    wallets.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
    wallets.toggleWalletRestore.listen(this._toggleWalletRestore);
    wallets.restoreWallet.listen(this._restoreWallet);
    wallets.importWalletFromKey.listen(this._importWalletFromKey);
    wallets.toggleWalletKeyImportDialog.listen(this._toggleWalletKeyImportDialog);
    wallets.setActiveWallet.listen(this._setActiveWallet);
    wallets.showWalletAddressCopyNotification.listen(this._onShowWalletAddressCopyNotification);
    router.goToRoute.listen(this._onRouteChange);
    walletBackup.finishWalletBackup.listen(this._finishWalletCreation);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._openAddWalletDialogWhenThereAreNoWallets,
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

  _delete = async (params: { walletId: string }) => {
    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) return;
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
    await this.deleteWalletRequest.execute({ walletId: params.walletId });
    await this.walletsRequest.patch(result => {
      result.splice(indexOfWalletToDelete, 1);
    });
    runInAction(() => {
      if (this.hasAnyWallets) {
        const nextIndexInList = Math.max(indexOfWalletToDelete - 1, 0);
        const nextWalletInList = this.all[nextIndexInList];
        this.goToWalletRoute(nextWalletInList.id);
      } else {
        this.active = null;
        this.actions.router.goToRoute({ route: ROUTES.NO_WALLETS });
      }
    });
    this.refreshWalletsData();
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails).promise;
    if (wallet) {
      await this.walletsRequest.patch(result => { result.push(wallet); });
      this.goToWalletRoute(wallet.id);
      runInAction(() => { this.isAddWalletDialogOpen = false; });
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
      amount: transactionDetails.amount,
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
    if (this.walletsRequest.result == null) return false;
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

  getWalletRoute = (walletId: string, page: string = 'summary'): string => (
    buildRoute(ROUTES.WALLETS.PAGE, { id: walletId, page })
  );

  getWalletById = (id: string): ?Wallet => this.all.find(w => w.id === id);

  isValidAddress = (address: string) => this.api.isValidAddress('ADA', address);

  isValidMnemonic = (mnemonic: string) => this.api.isValidMnemonic(mnemonic);

  @action refreshWalletsData = async () => {
    if (this.stores.networkStatus.isConnected) {
      this.walletsRequest.invalidate();
      const result = await this.walletsRequest.execute().promise;
      if (!result) return;
      runInAction('refresh wallet data', () => {
        const walletIds = result.map((wallet: Wallet) => wallet.id);
        this.stores.transactions.transactionsRequests = walletIds.map(walletId => ({
          walletId,
          recentRequest: this.stores.transactions._getTransactionsRecentRequest(walletId),
          allRequest: this.stores.transactions._getTransactionsAllRequest(walletId)
        }));
        this.stores.transactions._refreshTransactionData();
      });
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

  @action _setActiveWallet = ({ walletId }: { walletId: string }) => {
    if (this.hasAnyWallets) {
      this.active = this.all.find(wallet => wallet.id === walletId);
    }
  };

  @action _unsetActiveWallet = () => {
    this.active = null;
  };

  @action _setIsWalletDialogOpen = (isOpen: boolean) => {
    this.isAddWalletDialogOpen = isOpen;
  };

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute({ route });
  }

  _openAddWalletDialogWhenThereAreNoWallets = () => {
    const isOpenWhenThereAreNoWallets = !this.hasAnyWallets;
    this._setIsWalletDialogOpen(isOpenWhenThereAreNoWallets);
  };

  _updateActiveWalletOnRouteChanges = () => {
    const currentRoute = this.stores.app.currentRoute;
    const hasActiveWallet = !!this.active;
    const hasAnyWalletsLoaded = this.hasAnyLoaded;

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
    } else if (matchRoute(ROUTES.WALLETS.ROOT, currentRoute)) {
      // The route does not specify any wallet -> pick first wallet
      if (!hasActiveWallet && hasAnyWalletsLoaded) {
        this._setActiveWallet({ walletId: this.all[0].id });
      }
      if (this.active) this.goToWalletRoute(this.active.id);
    }
  };

  _patchWalletRequestWithNewWallet = async (wallet: Wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    await this.walletsRequest.patch(result => {
      if (!_.find(result, { id: wallet.id })) result.push(wallet);
    });
  };

  @action _hideWalletAddressCopyNotification = () => {
    this.isWalletAddressCopyNotificationVisible = false;
  };

  @action _onShowWalletAddressCopyNotification = () => {
    if (this._hideWalletAddressCopyNotificationTimeout) {
      clearTimeout(this._hideWalletAddressCopyNotificationTimeout);
    }
    this._hideWalletAddressCopyNotificationTimeout = setTimeout(
      this._hideWalletAddressCopyNotification,
      config.wallets.ADDRESS_COPY_NOTIFICATION_DURATION
    );
    this.isWalletAddressCopyNotificationVisible = true;
  };

  @action _onRouteChange = (options: { route: string, params: ?Object }) => {
    // Reset the send request anytime we visit the send page (e.g: to remove any previous errors)
    if (matchRoute(ROUTES.WALLETS.SEND, buildRoute(options.route, options.params))) {
      this.sendMoneyRequest.reset();
    }
  };

}
