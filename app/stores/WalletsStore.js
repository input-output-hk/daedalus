// @flow
import { observable, computed, action, runInAction } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Wallet from '../domain/Wallet';
import { matchRoute, buildRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/LocalizedCachedRequest';
import Request from './lib/LocalizedRequest';
import environment from '../environment';
import { ROUTES } from '../Routes';
import WalletAddDialog from '../components/wallet/WalletAddDialog';
import type {
  GetWalletsResponse,
  ImportKeyResponse,
  CreateWalletResponse,
  DeleteWalletResponse,
  CreateTransactionResponse,
  GetWalletRecoveryPhraseResponse,
  RestoreWalletResponse
} from '../api';

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: CachedRequest<GetWalletsResponse> = new CachedRequest(this.api.getWallets);
  @observable importFromKeyRequest: Request<ImportKeyResponse> = new Request(this.api.importWalletFromKey);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.createWallet);
  @observable deleteWalletRequest: Request<DeleteWalletResponse> = new Request(this.api.deleteWallet);
  @observable sendMoneyRequest: Request<CreateTransactionResponse> = new Request(this.api.createTransaction);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.getWalletRecoveryPhrase);
  @observable restoreRequest: Request<RestoreWalletResponse> = new Request(this.api.restoreWallet);
  /* eslint-enable max-len */

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
    wallets.restoreWallet.listen(this._restoreWallet);
    wallets.importWalletFromKey.listen(this._importWalletFromKey);
    wallets.setActiveWallet.listen(this._setActiveWallet);
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
        this.actions.router.goToRoute.trigger({ route: ROUTES.NO_WALLETS });
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
    } else {
      this.actions.dialogs.open.trigger({
        dialog: WalletAddDialog,
      });
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

  @computed get activeWalletRoute(): ?string {
    if (!this.active) return null;
    return this.getWalletRoute(this.active.id);
  }

  @computed get hasAnyLoaded(): boolean {
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

  @action _restoreWallet = async (params: {
    recoveryPhrase: string,
    walletName: string,
  }) => {
    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  @action _importWalletFromKey = async (params: {
    filePath: string,
  }) => {
    const importedWallet = await this.importFromKeyRequest.execute(params).promise;
    if (!importedWallet) throw new Error('Imported wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(importedWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.importFromKeyRequest.reset();
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

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({ route });
  }

  _openAddWalletDialogWhenThereAreNoWallets = () => {
    if (this.hasLoadedWallets && !this.hasAnyWallets) {
      this.actions.dialogs.open.trigger({
        dialog: WalletAddDialog,
      });
    }
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

  @action _onRouteChange = (options: { route: string, params: ?Object }) => {
    // Reset the send request anytime we visit the send page (e.g: to remove any previous errors)
    if (matchRoute(ROUTES.WALLETS.SEND, buildRoute(options.route, options.params))) {
      this.sendMoneyRequest.reset();
    }
  };

}
