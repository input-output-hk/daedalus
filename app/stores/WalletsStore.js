// @flow
import { observable, computed, action, runInAction, untracked } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Wallet from '../domain/Wallet';
import { matchRoute, buildRoute } from '../lib/routing-helpers';
import Request from './lib/LocalizedRequest';
import environment from '../environment';
import { ROUTES } from '../routes-config';
import WalletAddDialog from '../components/wallet/WalletAddDialog';
import type { walletExportTypeChoices } from '../types/walletExportTypes';
import type {
  GetWalletsResponse,
  ImportWalletFromFileResponse,
  CreateWalletResponse,
  DeleteWalletResponse,
  CreateTransactionResponse,
  GetWalletRecoveryPhraseResponse,
  RestoreWalletResponse
} from '../api';
import type { WalletImportFromFileParams } from '../actions/wallets-actions';

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;
  @observable isImportActive: boolean = false;
  @observable isRestoreActive: boolean = false;

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<GetWalletsResponse> = new Request(this.api.getWallets);
  @observable importFromFileRequest: Request<ImportWalletFromFileResponse> = new Request(this.api.importWalletFromFile);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.createWallet);
  @observable deleteWalletRequest: Request<DeleteWalletResponse> = new Request(this.api.deleteWallet);
  @observable sendMoneyRequest: Request<CreateTransactionResponse> = new Request(this.api.createTransaction);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.getWalletRecoveryPhrase);
  @observable restoreRequest: Request<RestoreWalletResponse> = new Request(this.api.restoreWallet);
  /* eslint-enable max-len */

  @observable walletExportType: walletExportTypeChoices = 'paperWallet';
  @observable walletExportMnemonic = 'marine joke dry silk ticket thing sugar stereo aim';

  _newWalletDetails: { name: string, mnemonic: string, password: ?string, } = {
    name: '',
    mnemonic: '',
    password: null,
  };

  setup() {
    const { wallets, walletBackup, router } = this.actions;
    wallets.createWallet.listen(this._create);
    wallets.deleteWallet.listen(this._delete);
    wallets.sendMoney.listen(this._sendMoney);
    wallets.restoreWallet.listen(this._restoreWallet);
    wallets.importWalletFromFile.listen(this._importWalletFromFile);
    wallets.setActiveWallet.listen(this._setActiveWallet);
    wallets.chooseWalletExportType.listen(this._chooseWalletExportType);
    router.goToRoute.listen(this._onRouteChange);
    walletBackup.finishWalletBackup.listen(this._finishWalletCreation);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._toggleAddWalletDialogOnWalletsLoaded,
    ]);
    if (environment.CARDANO_API) {
      setInterval(this.pollRefresh, this.WALLET_REFRESH_INTERVAL);
    }
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

  _delete = async (params: { walletId: string }) => {
    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) return;
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
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
        this.actions.router.goToRoute.trigger({ route: ROUTES.NO_WALLETS });
      }
    });
    this.deleteWalletRequest.reset();
    this.refreshWalletsData();
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails).promise;
    if (wallet) {
      await this.walletsRequest.patch(result => { result.push(wallet); });
      this.actions.dialogs.closeActiveDialog.trigger();
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
    password: ?string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    const accountId = this.stores.addresses._getAccountIdByWalletId(wallet.id);
    if (!accountId) throw new Error('Active account required before sending.');
    await this.sendMoneyRequest.execute({
      ...transactionDetails,
      sender: accountId,
    });
    this.refreshWalletsData();
    this.actions.dialogs.closeActiveDialog.trigger();
    this.goToWalletRoute(wallet.id);
  };

  @computed get hasLoadedWallets(): boolean {
    return this.walletsRequest.wasExecuted;
  }

  @computed get hasAnyWallets(): boolean {
    if (this.walletsRequest.result == null) return false;
    return this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0;
  }

  @computed get hasActiveWallet(): boolean {
    return !!this.active;
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

  getWalletById = (id: string): (?Wallet) => this.all.find(w => w.id === id);

  getWalletByName = (name: string): (?Wallet) => this.all.find(w => w.name === name);

  isValidAddress = (address: string) => this.api.isValidAddress(address);

  isValidMnemonic = (mnemonic: string) => this.api.isValidMnemonic(mnemonic);

  // TODO - call endpoint to check if private key is valid
  isValidPrivateKey = () => { return true; }; // eslint-disable-line

  @action refreshWalletsData = async () => {
    if (this.stores.networkStatus.isConnected) {
      const result = await this.walletsRequest.execute().promise;
      if (!result) return;
      runInAction('refresh active wallet', () => {
        if (this.active) {
          this._setActiveWallet({ walletId: this.active.id });
        }
      });
      runInAction('refresh address data', () => {
        const walletIds = result.map((wallet: Wallet) => wallet.id);
        this.stores.addresses.addressesRequests = walletIds.map(walletId => ({
          walletId,
          allRequest: this.stores.addresses._getAddressesAllRequest(walletId),
        }));
        this.stores.addresses._refreshAddresses();
      });
      runInAction('refresh transaction data', () => {
        const walletIds = result.map((wallet: Wallet) => wallet.id);
        this.stores.transactions.transactionsRequests = walletIds.map(walletId => ({
          walletId,
          recentRequest: this.stores.transactions._getTransactionsRecentRequest(walletId),
          allRequest: this.stores.transactions._getTransactionsAllRequest(walletId),
        }));
        this.stores.transactions._refreshTransactionData();
      });
    }
  };

  pollRefresh = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.refreshWalletsData();
    }
  };

  @action _setIsRestoreActive = (active: boolean) => {
    this.isRestoreActive = active;
  };

  @action _restoreWallet = async (params: {
    recoveryPhrase: string,
    walletName: string,
    walletPassword: ?string,
  }) => {
    this.restoreRequest.reset();

    // Hide restore wallet dialog 500ms after restore has been started
    // ...or keep it open in case it has errored out (so that error message can be shown)
    setTimeout(() => {
      if (!this.restoreRequest.isError) {
        this.actions.dialogs.closeActiveDialog.trigger();
        if (this.restoreRequest.isExecuting) this._setIsRestoreActive(true);
      }
    }, 500);

    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    this._setIsRestoreActive(false);
    this.restoreRequest.reset();
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.refreshWalletsData();
  };

  @action _setIsImportActive = (active: boolean) => {
    this.isImportActive = active;
  };

  @action _importWalletFromFile = async (params: WalletImportFromFileParams) => {
    this.importFromFileRequest.reset();

    // Hide import wallet dialog 500ms after import has been started
    // ...or keep it open in case it has errored out (so that error message can be shown)
    setTimeout(() => {
      if (!this.importFromFileRequest.isError) {
        this.actions.dialogs.closeActiveDialog.trigger();
        if (this.importFromFileRequest.isExecuting) this._setIsImportActive(true);
      }
    }, 500);

    const { filePath, walletName, walletPassword } = params;
    const importedWallet = await this.importFromFileRequest.execute({
      filePath, walletName, walletPassword,
    }).promise;
    if (!importedWallet) throw new Error('Imported wallet was not received correctly');
    this._setIsImportActive(false);
    this.importFromFileRequest.reset();
    await this._patchWalletRequestWithNewWallet(importedWallet);
    this.refreshWalletsData();
  }

  @action _setActiveWallet = ({ walletId }: { walletId: string }) => {
    if (this.hasAnyWallets) {
      const activeWalletId = this.active ? this.active.id : null;
      const activeWalletChange = activeWalletId !== walletId;
      if (activeWalletChange) this.stores.addresses.lastGeneratedAddress = null;
      this.active = this.all.find(wallet => wallet.id === walletId);
    }
  };

  @action _unsetActiveWallet = () => {
    this.active = null;
    this.stores.addresses.lastGeneratedAddress = null;
  };

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({ route });
  }

  _toggleAddWalletDialogOnWalletsLoaded = () => {
    // Register mobx observers for active import and restore in order to trigger reaction on change
    this.isImportActive; // eslint-disable-line
    this.isRestoreActive; // eslint-disable-line
    if (this.hasLoadedWallets && !this.hasAnyWallets) {
      this.actions.dialogs.open.trigger({ dialog: WalletAddDialog });
    } else if (untracked(() => this.stores.uiDialogs.isOpen(WalletAddDialog))) {
      this.actions.dialogs.closeActiveDialog.trigger();
    }
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

  _patchWalletRequestWithNewWallet = async (wallet: Wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    await this.walletsRequest.patch(result => {
      if (!_.find(result, { id: wallet.id })) result.push(wallet);
    });
  };

  @computed get _canRedirectToWallet(): boolean {
    const currentRoute = this.stores.app.currentRoute;
    const isRootRoute = matchRoute(ROUTES.WALLETS.ROOT, currentRoute);
    const isNoWalletsRoute = matchRoute(ROUTES.NO_WALLETS, currentRoute);
    return isRootRoute || isNoWalletsRoute;
  }

  @action _onRouteChange = (options: { route: string, params: ?Object }) => {
    // Reset the send request anytime we visit the send page (e.g: to remove any previous errors)
    if (matchRoute(ROUTES.WALLETS.SEND, buildRoute(options.route, options.params))) {
      this.sendMoneyRequest.reset();
    }
  };

  @action _chooseWalletExportType = (params: {
    walletExportType: walletExportTypeChoices,
  }) => {
    if (this.walletExportType !== params.walletExportType) {
      this.walletExportType = params.walletExportType;
    }
  };

}
