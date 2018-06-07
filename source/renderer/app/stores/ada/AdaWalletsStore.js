// @flow
import { observable, action, computed, runInAction } from 'mobx';
import { get, chunk, find } from 'lodash';
import WalletStore from '../WalletStore';
import Wallet from '../../domains/Wallet';
import { MAX_ADA_WALLETS_COUNT } from '../../config/numbersConfig';
import { matchRoute, buildRoute } from '../../utils/routing';
import { i18nContext } from '../../utils/i18nContext';
import Request from '.././lib/LocalizedRequest';
import { ROUTES } from '../../routes-config';
import { mnemonicToSeedHex } from '../../utils/crypto';
import { downloadPaperWalletCertificate } from '../../utils/paperWalletPdfGenerator';
import type { walletExportTypeChoices } from '../../types/walletExportTypes';
import type { WalletImportFromFileParams } from '../../actions/ada/wallets-actions';
import type { ImportWalletFromFileResponse } from '../../api/ada/index';
import type {
  CreateTransactionResponse, CreateWalletResponse, DeleteWalletResponse,
  GetWalletsResponse, RestoreWalletResponse,
  GetWalletRecoveryPhraseResponse,
} from '../../api/common';
import type {
  GetWalletCertificateAdditionalMnemonicsResponse,
  GetWalletCertificateRecoveryPhraseResponse,
  GetWalletRecoveryPhraseFromCertificateResponse,
} from '../../api/ada/types';

export default class AdaWalletsStore extends WalletStore {

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<GetWalletsResponse> = new Request(this.api.ada.getWallets);
  @observable importFromFileRequest: Request<ImportWalletFromFileResponse> = new Request(this.api.ada.importWalletFromFile);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.ada.createWallet);
  @observable getWalletAddressesRequest: Request<any> = new Request(this.api.ada.getAddresses);
  @observable deleteWalletRequest: Request<DeleteWalletResponse> = new Request(this.api.ada.deleteWallet);
  @observable sendMoneyRequest: Request<CreateTransactionResponse> = new Request(this.api.ada.createTransaction);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.ada.getWalletRecoveryPhrase);
  @observable getWalletCertificateAdditionalMnemonicsRequest: Request<GetWalletCertificateAdditionalMnemonicsResponse> = new Request(this.api.ada.getWalletCertificateAdditionalMnemonics);
  @observable getWalletCertificateRecoveryPhraseRequest: Request<GetWalletCertificateRecoveryPhraseResponse> = new Request(this.api.ada.getWalletCertificateRecoveryPhrase);
  @observable getWalletRecoveryPhraseFromCertificateRequest: Request<GetWalletRecoveryPhraseFromCertificateResponse> = new Request(this.api.ada.getWalletRecoveryPhraseFromCertificate);
  @observable restoreRequest: Request<RestoreWalletResponse> = new Request(this.api.ada.restoreWallet);
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

  _pollingBlocked = false;

  setup() {
    super.setup();
    const { router, walletBackup, ada } = this.actions;
    const { wallets } = ada;
    wallets.createWallet.listen(this._create);
    wallets.deleteWallet.listen(this._delete);
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
  }

  _sendMoney = async (transactionDetails: {
    receiver: string,
    amount: string,
    password: ?string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    const accountId = await this.stores.ada.addresses.getAccountIdByWalletId(wallet.id);
    if (!accountId) throw new Error('Active account required before sending.');
    await this.sendMoneyRequest.execute({
      ...transactionDetails,
      sender: accountId,
    });
    this.refreshWalletsData();
    this.actions.dialogs.closeActiveDialog.trigger();
    this.sendMoneyRequest.reset();
    this.goToWalletRoute(wallet.id);
  };

  isValidAddress = (address: string) => this.api.ada.isValidAddress(address);

  isValidMnemonic = (mnemonic: string) => this.api.ada.isValidMnemonic(mnemonic);

  isValidCertificateMnemonic = (
    mnemonic: string,
  ) => this.api.ada.isValidCertificateMnemonic(mnemonic);

  // TODO - call endpoint to check if private key is valid
  isValidPrivateKey = () => { return true; }; // eslint-disable-line

  @computed get hasMaxWallets(): boolean {
    return this.all.length >= MAX_ADA_WALLETS_COUNT;
  }

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
        this.stores.ada.addresses.addressesRequests = walletIds.map(walletId => ({
          walletId,
          allRequest: this.stores.ada.addresses._getAddressesAllRequest(walletId),
        }));
        this.stores.ada.addresses._refreshAddresses();
      });
      runInAction('refresh transaction data', () => {
        const walletIds = result.map((wallet: Wallet) => wallet.id);
        this.stores.ada.transactions.transactionsRequests = walletIds.map(walletId => ({
          walletId,
          recentRequest: this.stores.ada.transactions._getTransactionsRecentRequest(walletId),
          allRequest: this.stores.ada.transactions._getTransactionsAllRequest(walletId),
        }));
        this.stores.ada.transactions._refreshTransactionData();
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
    walletPassword: ?string,
    type?: string,
  }) => {
    // reset getWalletRecoveryPhraseFromCertificateRequest to clear previous errors
    this.getWalletRecoveryPhraseFromCertificateRequest.reset();

    const data = {
      recoveryPhrase: params.recoveryPhrase,
      walletName: params.walletName,
      walletPassword: params.walletPassword,
    };

    if (params.type === 'certificate') {
      // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
      const recoveryPhraseArray = params.recoveryPhrase.split(' ');
      const chunked = chunk(recoveryPhraseArray, 18);
      const scrambledInput = chunked[0]; // first 18 mnemonics
      const certificatePassword = chunked[1]; // last 9 mnemonics
      const spendingPassword = mnemonicToSeedHex(certificatePassword.join(' '));

      // Unscramble 18-word wallet certificate mnemonic to 12-word mnemonic
      const unscrambledRecoveryPhrase: GetWalletRecoveryPhraseFromCertificateResponse = await (
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
    const { filePath, walletName, walletPassword } = params;
    const importedWallet = await this.importFromFileRequest.execute({
      filePath, walletName, walletPassword,
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
      if (activeWalletChange) this.stores.ada.addresses.lastGeneratedAddress = null;
      this.active = this.all.find(wallet => wallet.id === walletId);
    }
  };

  @action _unsetActiveWallet = () => {
    this.active = null;
    this.stores.ada.addresses.lastGeneratedAddress = null;
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

  _pausePolling = () => {
    this._pollingBlocked = true;
  };

  _resumePolling = () => {
    this._pollingBlocked = false;
  };

  _generateCertificate = async (params: {
    filePath: string,
  }) => {
    try {
      // Pause polling in order not to show Paper wallet in the UI
      this._pausePolling();

      // Set inProgress state to show spinner if is needed
      this._updateCertificateCreationState(true);

      // Generate wallet recovery phrase
      const recoveryPhrase: GetWalletRecoveryPhraseResponse = await (
        this.getWalletRecoveryPhraseRequest.execute().promise
      );

      // Generate 9-words (additional) mnemonic
      const additionalMnemonicWords: GetWalletCertificateAdditionalMnemonicsResponse = await (
        this.getWalletCertificateAdditionalMnemonicsRequest.execute().promise
      );
      this.additionalMnemonicWords = additionalMnemonicWords.join(' ');

      // Generate spending password from 9-word mnemonic and save to store
      const spendingPassword = mnemonicToSeedHex(this.additionalMnemonicWords);
      this.walletCertificatePassword = spendingPassword;

      // Generate paper wallet scrambled mnemonic
      const walletCertificateRecoveryPhrase: GetWalletCertificateRecoveryPhraseResponse = await (
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
        password: spendingPassword,
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
    try {
      await downloadPaperWalletCertificate({
        address,
        mnemonics: recoveryPhrase,
        intl,
        filePath
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
