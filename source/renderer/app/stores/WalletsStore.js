// @flow
import { observable, action, computed, runInAction } from 'mobx';
import { get, chunk, find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import WalletTransaction from '../domains/WalletTransaction';
import { MAX_ADA_WALLETS_COUNT } from '../config/numbersConfig';
import { i18nContext } from '../utils/i18nContext';
import { mnemonicToSeedHex } from '../utils/crypto';
import { downloadPaperWalletCertificate } from '../utils/paperWalletPdfGenerator';
import { buildRoute, matchRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import type { walletExportTypeChoices } from '../types/walletExportTypes';
import type { WalletImportFromFileParams } from '../actions/wallets-actions';

/**
 * The base wallet store that contains the shared logic
 * dealing with wallets / accounts.
 */

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  // REQUESTS
  /* eslint-disable max-len */
  @observable active: ?Wallet = null;
  @observable isRestoreActive: boolean = false;
  @observable walletsRequest: Request<Array<Wallet>> = new Request(this.api.ada.getWallets);
  @observable importFromFileRequest: Request<Wallet> = new Request(this.api.ada.importWalletFromFile);
  @observable createWalletRequest: Request<Wallet> = new Request(this.api.ada.createWallet);
  @observable getWalletAddressesRequest: Request<any> = new Request(this.api.ada.getAddresses);
  @observable deleteWalletRequest: Request<boolean> = new Request(this.api.ada.deleteWallet);
  @observable sendMoneyRequest: Request<WalletTransaction> = new Request(this.api.ada.createTransaction);
  @observable getWalletRecoveryPhraseRequest: Request<Array<string>> = new Request(this.api.ada.getWalletRecoveryPhrase);
  @observable getWalletCertificateAdditionalMnemonicsRequest: Request<Array<string>> = new Request(this.api.ada.getWalletCertificateAdditionalMnemonics);
  @observable getWalletCertificateRecoveryPhraseRequest: Request<Array<string>> = new Request(this.api.ada.getWalletCertificateRecoveryPhrase);
  @observable getWalletRecoveryPhraseFromCertificateRequest: Request<Array<string>> = new Request(this.api.ada.getWalletRecoveryPhraseFromCertificate);
  @observable restoreRequest: Request<Wallet> = new Request(this.api.ada.restoreWallet);
  /* eslint-enable max-len */

  @observable walletExportType: walletExportTypeChoices = 'paperWallet';
  @observable walletExportMnemonic = 'marine joke dry silk ticket thing sugar stereo aim';
  @observable createPaperWalletCertificateStep = 0;
  @observable walletCertificatePassword = null;
  @observable walletCertificateAddress = null;
  @observable walletCertificateRecoveryPhrase = null;
  @observable generatingCertificateInProgress = false;
  @observable certificateStep = null;
  @observable certificateTemplate = null;
  @observable additionalMnemonicWords = null;

  _newWalletDetails: { name: string, mnemonic: string, spendingPassword: ?string } = {
    name: '',
    mnemonic: '',
    spendingPassword: null,
  };
  _pollingBlocked = false;

  setup() {
    setInterval(this._pollRefresh, this.WALLET_REFRESH_INTERVAL);

    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
    ]);

    const { router, walletBackup, wallets, app } = this.actions;
    wallets.createWallet.listen(this._create);
    wallets.deleteWallet.listen(this._deleteWallet);
    wallets.sendMoney.listen(this._sendMoney);
    wallets.restoreWallet.listen(this._restoreWallet);
    wallets.importWalletFromFile.listen(this._importWalletFromFile);
    wallets.chooseWalletExportType.listen(this._chooseWalletExportType);
    wallets.generateCertificate.listen(this._generateCertificate);
    wallets.updateCertificateStep.listen(this._updateCertificateStep);
    wallets.closeCertificateGeneration.listen(this._closeCertificateGeneration);
    wallets.setCertificateTemplate.listen(this._setCertificateTemplate);
    wallets.finishCertificate.listen(this._finishCertificate);
    router.goToRoute.listen(this._onRouteChange);
    walletBackup.finishWalletBackup.listen(this._finishCreation);
    app.initAppEnvironment.listen(() => {});
  }

  _create = async (params: {
    name: string,
    spendingPassword: ?string,
  }) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase: ?Array<string> = await (
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
    }
  };

  _deleteWallet = async (params: { walletId: string }) => {
    // Pause polling in order to avoid fetching data for wallet we are about to delete
    this._pausePolling();

    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) return;
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
    await this.deleteWalletRequest.execute({ walletId: params.walletId });
    await this.walletsRequest.patch(result => {
      result.splice(indexOfWalletToDelete, 1);
    });
    runInAction('AdaWalletsStore::_deleteWallet', () => {
      if (this.hasAnyWallets) {
        const nextIndexInList = Math.max(indexOfWalletToDelete - 1, 0);
        const nextWalletInList = this.all[nextIndexInList];
        this.actions.dialogs.closeActiveDialog.trigger();
        this.goToWalletRoute(nextWalletInList.id);
      } else {
        this.active = null;
      }
    });
    this._resumePolling();
    this.deleteWalletRequest.reset();
    this.refreshWalletsData();
  };

  _restore = async (params: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: ?string,
  }) => {
    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  _sendMoney = async ({ receiver, amount, password }: {
    receiver: string,
    amount: string,
    password: ?string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    const accountIndex = await this.stores.addresses.getAccountIndexByWalletId(wallet.id);

    await this.sendMoneyRequest.execute({
      address: receiver,
      amount: parseInt(amount, 10),
      spendingPassword: password,
      accountIndex,
      walletId: wallet.id,
    });
    this.refreshWalletsData();
    this.actions.dialogs.closeActiveDialog.trigger();
    this.sendMoneyRequest.reset();
    this.goToWalletRoute(wallet.id);
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

  @computed get hasMaxWallets(): boolean {
    return this.all.length >= MAX_ADA_WALLETS_COUNT;
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
    const transactions = this.stores.transactions;
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
      if (!find(result, { id: wallet.id })) result.push(wallet);
    });
  };

  _pollRefresh = async () => {
    const { isSynced, isSystemTimeCorrect } = this.stores.networkStatus;
    return isSynced && isSystemTimeCorrect && await this.refreshWalletsData();
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

  isValidAddress = (address: string) => this.api.ada.isValidAddress(address);

  isValidMnemonic = (mnemonic: string) => this.api.ada.isValidMnemonic(mnemonic);

  isValidCertificateMnemonic = (
    mnemonic: string,
  ) => this.api.ada.isValidCertificateMnemonic(mnemonic);

  // TODO - call endpoint to check if private key is valid
  isValidPrivateKey = () => { return true; }; // eslint-disable-line

  @action refreshWalletsData = async () => {
    // Prevent wallets data refresh if polling is blocked
    if (this._pollingBlocked) return;

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
      runInAction('refresh active wallet restore', () => {
        const restoringWallet = typeof find(result, ['syncState.tag', 'restoring']) !== 'undefined';
        this._setIsRestoreActive(restoringWallet);
      });
    }
  };

  @action _setIsRestoreActive = (active: boolean) => {
    this.isRestoreActive = active;
  };

  @action _restoreWallet = async (params: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: ?string,
    type?: string,
  }) => {
    // reset getWalletRecoveryPhraseFromCertificateRequest to clear previous errors
    this.getWalletRecoveryPhraseFromCertificateRequest.reset();

    const data = {
      recoveryPhrase: params.recoveryPhrase,
      walletName: params.walletName,
      spendingPassword: params.spendingPassword,
    };

    if (params.type === 'certificate') {
      // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
      const recoveryPhraseArray = params.recoveryPhrase.split(' ');
      const chunked = chunk(recoveryPhraseArray, 18);
      const scrambledInput = chunked[0]; // first 18 mnemonics
      const certificatePassword = chunked[1]; // last 9 mnemonics
      const spendingPassword = mnemonicToSeedHex(certificatePassword.join(' '));

      // Unscramble 18-word wallet certificate mnemonic to 12-word mnemonic
      const unscrambledRecoveryPhrase: Array<string> = await (
        this.getWalletRecoveryPhraseFromCertificateRequest.execute({
          passphrase: spendingPassword,
          scrambledInput: scrambledInput.join(' '),
        }).promise
      );
      data.recoveryPhrase = unscrambledRecoveryPhrase.join(' ');
      this.getWalletRecoveryPhraseFromCertificateRequest.reset();
    }

    const restoredWallet = await this.restoreRequest.execute(data).promise;
    if (!restoredWallet) throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  @action _importWalletFromFile = async (params: WalletImportFromFileParams) => {
    const { filePath, walletName, spendingPassword } = params;
    const importedWallet = await this.importFromFileRequest.execute({
      filePath, walletName, spendingPassword,
    }).promise;
    if (!importedWallet) throw new Error('Imported wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(importedWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.importFromFileRequest.reset();
    this.goToWalletRoute(importedWallet.id);
    this.refreshWalletsData();
  };

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

  @action _pausePolling = () => {
    this._pollingBlocked = true;
  };

  @action _resumePolling = () => {
    this._pollingBlocked = false;
  };

  @action _generateCertificate = async (params: {
    filePath: string,
  }) => {
    try {
      // Pause polling in order not to show Paper wallet in the UI
      this._pausePolling();

      // Set inProgress state to show spinner if is needed
      this._updateCertificateCreationState(true);

      // Generate wallet recovery phrase
      const recoveryPhrase: Array<string> = await (
        this.getWalletRecoveryPhraseRequest.execute().promise
      );

      // Generate 9-words (additional) mnemonic
      const additionalMnemonicWords: Array<string> = await (
        this.getWalletCertificateAdditionalMnemonicsRequest.execute().promise
      );
      this.additionalMnemonicWords = additionalMnemonicWords.join(' ');

      // Generate spending password from 9-word mnemonic and save to store
      const spendingPassword = mnemonicToSeedHex(this.additionalMnemonicWords);
      this.walletCertificatePassword = spendingPassword;

      // Generate paper wallet scrambled mnemonic
      const walletCertificateRecoveryPhrase: Array<string> = await (
        this.getWalletCertificateRecoveryPhraseRequest.execute({
          passphrase: spendingPassword,
          input: recoveryPhrase.join(' '),
        }).promise
      );
      this.walletCertificateRecoveryPhrase = walletCertificateRecoveryPhrase.join(' ');

      // Create temporary wallet
      const walletData = {
        name: 'Paper Wallet',
        mnemonic: recoveryPhrase.join(' '),
      };
      const wallet = await this.createWalletRequest.execute(walletData).promise;

      // Get temporary wallet address
      let walletAddresses;
      if (wallet) {
        walletAddresses = await this.getWalletAddressesRequest.execute({
          walletId: wallet.id,
        }).promise;

        // delete temporary wallet
        await this.deleteWalletRequest.execute({ walletId: wallet.id });
      }

      // Set wallet certificate address
      const walletAddress = get(walletAddresses, ['addresses', '0', 'id'], null);
      this.walletCertificateAddress = walletAddress;

      // download pdf certificate
      await this._downloadCertificate(
        walletAddress,
        walletCertificateRecoveryPhrase,
        params.filePath,
      );
    } catch (error) {
      throw error;
    } finally {
      this._resumePolling();
    }
  };

  _downloadCertificate = async (
    address: string,
    recoveryPhrase: Array<string>,
    filePath: string,
  ) => {
    const locale = this.stores.profile.currentLocale;
    const intl = i18nContext(locale);
    const isMainnet = this.environment.isMainnet;
    const buildLabel = this.environment.buildLabel;
    try {
      await downloadPaperWalletCertificate({
        address,
        mnemonics: recoveryPhrase,
        intl,
        filePath,
        isMainnet,
        buildLabel
      });
      runInAction('handle successful certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false);
        // Update certificate generator step
        this._updateCertificateStep();
      });
    } catch (error) {
      console.log(error);
      runInAction('handle failed certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false);
      });
    }
  };

  _updateCertificateCreationState = action((state: boolean) => {
    this.generatingCertificateInProgress = state;
  });

  @action _setCertificateTemplate = (params: {
    selectedTemplate: string,
  }) => {
    this.certificateTemplate = params.selectedTemplate;
    this._updateCertificateStep();
  };

  @action _finishCertificate = () => {
    this._closeCertificateGeneration();
  };

  @action _updateCertificateStep = (isBack: boolean = false) => {
    const currrentCertificateStep = this.certificateStep || 0;
    this.certificateStep = isBack ? currrentCertificateStep - 1 : currrentCertificateStep + 1;
  };

  @action _closeCertificateGeneration = () => {
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetCertificateData();
  };

  @action _resetCertificateData = () => {
    this.walletCertificatePassword = null;
    this.walletCertificateAddress = null;
    this.walletCertificateRecoveryPhrase = null;
    this.generatingCertificateInProgress = false;
    this.certificateTemplate = false;
    this.certificateStep = null;
  };

}
