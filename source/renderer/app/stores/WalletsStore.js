// @flow
import { observable, action, computed, runInAction, flow } from 'mobx';
import { get, chunk, find, findIndex, isEqual } from 'lodash';
import moment from 'moment';
import { BigNumber } from 'bignumber.js';
import { Util } from 'cardano-js';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import WalletAddress from '../domains/WalletAddress';
import { WalletTransaction } from '../domains/WalletTransaction';
import { MAX_ADA_WALLETS_COUNT } from '../config/numbersConfig';
import { i18nContext } from '../utils/i18nContext';
import { mnemonicToSeedHex } from '../utils/crypto';
import { downloadPaperWalletCertificate } from '../utils/paperWalletPdfGenerator';
import { downloadRewardsCsv } from '../utils/rewardsCsvGenerator';
import { buildRoute, matchRoute } from '../utils/routing';
import { asyncForEach } from '../utils/asyncForEach';
import { ROUTES } from '../routes-config';
import { formattedWalletAmount } from '../utils/formatters';
import {
  WalletPaperWalletOpenPdfError,
  WalletRewardsOpenCsvError,
} from '../i18n/errors';
import {
  RECOVERY_PHRASE_VERIFICATION_NOTIFICATION,
  RECOVERY_PHRASE_VERIFICATION_WARNING,
  WALLET_RESTORE_TYPES,
} from '../config/walletsConfig';
import { TESTNET } from '../../../common/types/environment.types';
import type { CsvRecord } from '../../../common/types/rewards-csv-request.types';
import type { walletExportTypeChoices } from '../types/walletExportTypes';
import type { WalletImportFromFileParams } from '../actions/wallets-actions';
import type LocalizableError from '../i18n/LocalizableError';
import type {
  WalletLocalData,
  WalletsLocalData,
} from '../api/utils/localStorage';
/* eslint-disable consistent-return */

export const WalletRecoveryPhraseVerificationStatuses = {
  OK: 'ok',
  WARNING: 'warning',
  NOTIFICATION: 'notification',
};

export const WalletRecoveryPhraseVerificationTypes = {
  NEVER_CHECKED: 'neverChecked',
  ALREADY_CHECKED: 'alreadyChecked',
};

type WalletRecoveryPhraseVerificationData = {
  creationDate: ?Date,
  recoveryPhraseVerificationDate: ?Date,
  recoveryPhraseVerificationStatus: string,
  recoveryPhraseVerificationStatusType: string,
};

type RecoveryPhraseVerificationData = {
  [key: string]: WalletRecoveryPhraseVerificationData,
};

/**
 * The base wallet store that contains logic for dealing with wallets
 */

export default class WalletsStore extends Store {
  WALLET_REFRESH_INTERVAL = 5000;

  WALLET_RECOVERY_PHRASE_VERIFICATION_STATUSES = {
    OK: 'ok',
    WARNING: 'warning',
    NOTIFICATION: 'notification',
  };

  WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES = {
    NEVER_CHECKED: 'neverChecked',
    ALREADY_CHECKED: 'alreadyChecked',
  };

  // REQUESTS
  /* eslint-disable max-len */
  @observable active: ?Wallet = null;
  @observable activeValue: ?BigNumber = null;
  @observable walletsRequest: Request<Array<Wallet>> = new Request(
    this.api.ada.getWallets
  );
  @observable importFromFileRequest: Request<Wallet> = new Request(
    this.api.ada.importWalletFromFile
  );
  @observable createWalletRequest: Request<Wallet> = new Request(
    this.api.ada.createWallet
  );
  @observable
  getWalletAddressesRequest: Request<Array<WalletAddress>> = new Request(
    this.api.ada.getAddresses
  );
  @observable deleteWalletRequest: Request<boolean> = new Request(
    this.api.ada.deleteWallet
  );
  @observable sendMoneyRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createTransaction
  );
  @observable getWalletRecoveryPhraseRequest: Request<
    Array<string>
  > = new Request(this.api.ada.getWalletRecoveryPhrase);
  @observable getWalletCertificateAdditionalMnemonicsRequest: Request<
    Array<string>
  > = new Request(this.api.ada.getWalletCertificateAdditionalMnemonics);
  @observable getWalletCertificateRecoveryPhraseRequest: Request<
    Array<string>
  > = new Request(this.api.ada.getWalletCertificateRecoveryPhrase);
  @observable getWalletRecoveryPhraseFromCertificateRequest: Request<
    Array<string>
  > = new Request(this.api.ada.getWalletRecoveryPhraseFromCertificate);
  @observable restoreRequest: Request<Wallet> = new Request(
    this.api.ada.restoreWallet
  );
  @observable restoreLegacyRequest: Request<Wallet> = new Request(
    this.api.ada.restoreLegacyWallet
  );
  @observable transferFundsCalculateFeeRequest: Request<any> = new Request(
    this.api.ada.transferFundsCalculateFee
  );
  @observable transferFundsRequest: Request<any> = new Request(
    this.api.ada.transferFunds
  );
  @observable
  getWalletsLocalDataRequest: Request<WalletsLocalData> = new Request(
    this.api.localStorage.getWalletsLocalData
  );
  @observable setWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.setWalletLocalData
  );
  @observable updateWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.updateWalletLocalData
  );
  @observable unsetWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.unsetWalletLocalData
  );

  /* eslint-enable max-len */

  /* ----------  Create Wallet  ---------- */
  @observable createWalletStep = null;
  @observable createWalletShowAbortConfirmation = false;
  // TODO: Remove once the new wallet creation process is ready
  @observable useNewWalletCreationProcess = false;

  /* ----------  Export Wallet  ---------- */
  @observable walletExportType: walletExportTypeChoices = 'paperWallet';
  @observable walletExportMnemonic =
    'marine joke dry silk ticket thing sugar stereo aim';

  /* ----------  Paper Wallet  ---------- */
  @observable createPaperWalletCertificateStep = 0;
  @observable walletCertificatePassword = null;
  @observable walletCertificateAddress = null;
  @observable walletCertificateRecoveryPhrase = null;
  @observable generatingCertificateInProgress = false;
  @observable generatingCertificateError: ?LocalizableError = null;
  @observable generatingRewardsCsvInProgress = false;
  @observable generatingRewardsCsvError: ?LocalizableError = null;
  @observable certificateStep = null;
  @observable certificateTemplate = null;
  @observable additionalMnemonicWords = null;

  /* ----------  Transfer Funds  ---------- */
  @observable transferFundsSourceWalletId: string = '';
  @observable transferFundsTargetWalletId: string = '';
  @observable transferFundsStep: number = 0;
  @observable transferFundsFee: ?number = null;

  /* ----------  Other  ---------- */
  @observable
  recoveryPhraseVerificationData: RecoveryPhraseVerificationData = {};

  _newWalletDetails: {
    name: string,
    mnemonic: string,
    spendingPassword: string,
  } = {
    name: '',
    mnemonic: '',
    spendingPassword: '',
  };
  _pollingBlocked = false;

  setup() {
    setInterval(this._pollRefresh, this.WALLET_REFRESH_INTERVAL);

    this.registerReactions([this._updateActiveWalletOnRouteChanges]);

    const {
      router,
      walletBackup,
      wallets: walletsActions,
      app,
      networkStatus,
    } = this.actions;
    // Create Wallet Actions ---
    walletsActions.createWallet.listen(this._create);
    walletsActions.createWalletBegin.listen(this._createWalletBegin);
    walletsActions.createWalletChangeStep.listen(this._createWalletChangeStep);
    walletsActions.createWalletAbort.listen(this._createWalletAbort);
    walletsActions.createWalletClose.listen(this._createWalletClose);
    // ---
    walletsActions.deleteWallet.listen(this._deleteWallet);
    walletsActions.sendMoney.listen(this._sendMoney);
    walletsActions.restoreWallet.listen(this._restoreWallet);
    walletsActions.importWalletFromFile.listen(this._importWalletFromFile);
    walletsActions.chooseWalletExportType.listen(this._chooseWalletExportType);

    walletsActions.generateCertificate.listen(this._generateCertificate);
    walletsActions.updateCertificateStep.listen(this._updateCertificateStep);
    walletsActions.closeCertificateGeneration.listen(
      this._closeCertificateGeneration
    );

    walletsActions.generateRewardsCsv.listen(this._generateRewardsCsv);
    walletsActions.closeRewardsCsvGeneration.listen(
      this._closeRewardsCsvGeneration
    );

    walletsActions.setCertificateTemplate.listen(this._setCertificateTemplate);
    walletsActions.finishCertificate.listen(this._finishCertificate);
    walletsActions.finishRewardsCsv.listen(this._finishRewardsCsv);
    router.goToRoute.listen(this._onRouteChange);
    walletBackup.finishWalletBackup.listen(this._finishWalletBackup);
    app.initAppEnvironment.listen(() => {});
    networkStatus.restartNode.listen(this._updateGeneratingCertificateError);
    networkStatus.restartNode.listen(this._updateGeneratingRewardsCsvError);
    walletsActions.updateRecoveryPhraseVerificationDate.listen(
      this._updateRecoveryPhraseVerificationDate
    );
    walletsActions.transferFundsNextStep.listen(this._transferFundsNextStep);
    walletsActions.transferFundsPrevStep.listen(this._transferFundsPrevStep);
    walletsActions.transferFunds.listen(this._transferFunds);
    walletsActions.transferFundsSetSourceWalletId.listen(
      this._transferFundsSetSourceWalletId
    );
    walletsActions.transferFundsSetTargetWalletId.listen(
      this._transferFundsSetTargetWalletId
    );
    walletsActions.transferFundsRedeem.listen(this._transferFundsRedeem);
    walletsActions.transferFundsClose.listen(this._transferFundsClose);
    walletsActions.transferFundsCalculateFee.listen(
      this._transferFundsCalculateFee
    );
  }

  _create = async (params: { name: string, spendingPassword: string }) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase: ?Array<string> = await this.getWalletRecoveryPhraseRequest.execute()
        .promise;
      if (recoveryPhrase != null) {
        this.actions.walletBackup.initiateWalletBackup.trigger({
          recoveryPhrase,
        });
      }
    } catch (error) {
      throw error;
    }
  };

  // TODO: Remove once the new wallet creation process is ready
  @action _toggleUseNewWalletCreationProcess = () => {
    this.useNewWalletCreationProcess = !this.useNewWalletCreationProcess;
  };

  @action _createWalletBegin = () => {
    this.createWalletStep = 0;
    this.createWalletShowAbortConfirmation = false;
  };

  @action _createWalletChangeStep = (isBack: boolean = false) => {
    const currrentCreateWalletStep = this.createWalletStep || 0;
    this.createWalletStep =
      isBack === true
        ? currrentCreateWalletStep - 1
        : currrentCreateWalletStep + 1;
    this.createWalletShowAbortConfirmation = false;
  };

  @action _createWalletClose = () => {
    this.createWalletStep = null;
    this.createWalletShowAbortConfirmation = false;
  };

  @action _createWalletAbort = () => {
    this.createWalletShowAbortConfirmation = true;
  };

  _finishWalletBackup = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(
      ' '
    );
    const wallet = await this.createWalletRequest.execute(
      this._newWalletDetails
    ).promise;
    if (wallet) {
      await this._createWalletLocalData(wallet.id);
      await this._patchWalletRequestWithNewWallet(wallet);
      this.actions.dialogs.closeActiveDialog.trigger();
      this.goToWalletRoute(wallet.id);
    }
  };

  _deleteWallet = async (params: { walletId: string, isLegacy: boolean }) => {
    // Pause polling in order to avoid fetching data for wallet we are about to delete
    this._pausePolling();

    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) return;
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
    await this.deleteWalletRequest.execute({
      walletId: params.walletId,
      isLegacy: params.isLegacy,
    });
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
        this.activeValue = null;
      }
    });
    this._unsetWalletLocalData(params.walletId);
    this._resumePolling();
    this.deleteWalletRequest.reset();
    this.refreshWalletsData();
  };

  _restore = async (params: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: string,
  }) => {
    const restoredWallet = await this.restoreRequest.execute(params).promise;
    if (!restoredWallet)
      throw new Error('Restored wallet was not received correctly');
    await this._createWalletLocalData(restoredWallet.id);
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  _createWalletLocalData = async (id: string) => {
    const walletLocalData = {
      id,
      creationDate: new Date(),
    };
    await this.setWalletLocalDataRequest.execute(walletLocalData);
  };

  _sendMoney = async ({
    receiver,
    amount,
    passphrase,
  }: {
    receiver: string,
    amount: string,
    passphrase: string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    await this.sendMoneyRequest.execute({
      address: receiver,
      amount: parseInt(amount, 10),
      passphrase,
      walletId: wallet.id,
    });
    this.refreshWalletsData();
    this.actions.dialogs.closeActiveDialog.trigger();
    this.sendMoneyRequest.reset();
    this.goToWalletRoute(wallet.id);
  };

  @action _transferFundsNextStep = () => {
    const {
      transferFundsStep,
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
    } = this;
    let nextStep = 0;
    if (transferFundsStep === 0 && transferFundsSourceWalletId) {
      nextStep = 1;
    }
    if (
      transferFundsStep === 1 &&
      transferFundsSourceWalletId &&
      transferFundsTargetWalletId
    ) {
      nextStep = 2;
      this._transferFundsCalculateFee({
        sourceWalletId: transferFundsSourceWalletId,
      });
    }
    this.transferFundsStep = nextStep;
  };

  @action _transferFundsPrevStep = () => {
    const { transferFundsStep } = this;
    const prevStep = transferFundsStep > 0 ? transferFundsStep - 1 : 0;
    this.transferFundsStep = prevStep;
  };

  @action _transferFunds = async (spendingPassword: string) => {
    const { transferFundsSourceWalletId, transferFundsTargetWalletId } = this;
    await this.transferFundsRequest.execute({
      sourceWalletId: transferFundsSourceWalletId,
      targetWalletId: transferFundsTargetWalletId,
      passphrase: spendingPassword,
    });
    this.refreshWalletsData();
    this._transferFundsClose();
    this.transferFundsRequest.reset();
  };

  @action _transferFundsSetSourceWalletId = ({
    sourceWalletId,
  }: {
    sourceWalletId: string,
  }) => {
    this.transferFundsSourceWalletId = sourceWalletId;
    // Sets the target wallet to the first wallet
    const { allWallets } = this;
    this.transferFundsTargetWalletId = allWallets[0].id;
    // Sets to first step
    this.transferFundsStep = 1;
  };

  @action _transferFundsSetTargetWalletId = ({
    targetWalletId,
  }: {
    targetWalletId: string,
  }) => {
    this.transferFundsTargetWalletId = targetWalletId;
  };

  @action _transferFundsRedeem = () => {
    this.transferFundsStep = 0;
    // TODO: Call API method
  };

  @action _transferFundsClose = () => {
    this.transferFundsStep = 0;
    this.transferFundsFee = null;
  };

  @action _transferFundsCalculateFee = async ({
    sourceWalletId,
  }: {
    sourceWalletId: string,
  }) => {
    const fee = await this.transferFundsCalculateFeeRequest.execute({
      sourceWalletId,
    }).promise;
    runInAction('set migration fee', () => {
      this.transferFundsFee = fee;
    });
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
    return (
      this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0
    );
  }

  @computed get hasMaxWallets(): boolean {
    return this.all.length >= MAX_ADA_WALLETS_COUNT;
  }

  @computed get all(): Array<Wallet> {
    return [...this.allWallets, ...this.allLegacyWallets];
  }

  @computed get allWallets(): Array<Wallet> {
    return this.walletsRequest.result
      ? this.walletsRequest.result.filter(({ isLegacy }: Wallet) => !isLegacy)
      : [];
  }

  @computed get allLegacyWallets(): Array<Wallet> {
    return this.walletsRequest.result
      ? this.walletsRequest.result.filter(({ isLegacy }: Wallet) => isLegacy)
      : [];
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
    return matchRoute(`${ROUTES.WALLETS.ROOT}(/*rest)`, currentRoute);
  }

  getWalletById = (id: string): ?Wallet => this.all.find(w => w.id === id);

  getWalletByName = (name: string): ?Wallet =>
    this.all.find(w => w.name === name);

  getWalletRoute = (walletId: string, page: string = 'summary'): string =>
    buildRoute(ROUTES.WALLETS.PAGE, { id: walletId, page });

  // ACTIONS

  goToWalletRoute(walletId: string) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({ route });
  }

  // =================== PRIVATE API ==================== //

  @computed get _canRedirectToWallet(): boolean {
    const { currentRoute } = this.stores.app;
    const isRootRoute = matchRoute(ROUTES.WALLETS.ROOT, currentRoute);
    const isAddWalletRoute = matchRoute(ROUTES.WALLETS.ADD, currentRoute);
    return isRootRoute || isAddWalletRoute;
  }

  _patchWalletRequestWithNewWallet = async (wallet: Wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    await this.walletsRequest.patch(result => {
      if (!find(result, { id: wallet.id })) {
        const index = findIndex(result, 'isLegacy');
        if (index >= 0) {
          result.splice(index, 0, wallet);
        } else {
          result.push(wallet);
        }
      }
    });
  };

  _pollRefresh = async () => {
    const { isSynced } = this.stores.networkStatus;
    return isSynced && this.refreshWalletsData();
  };

  _updateActiveWalletOnRouteChanges = () => {
    const { currentRoute } = this.stores.app;
    const hasAnyWalletLoaded = this.hasAnyLoaded;
    const isWalletAddPage = matchRoute(ROUTES.WALLETS.ADD, currentRoute);
    runInAction('WalletsStore::_updateActiveWalletOnRouteChanges', () => {
      // There are not wallets loaded (yet) -> unset active and return
      if (isWalletAddPage || !hasAnyWalletLoaded)
        return this._unsetActiveWallet();
      const match = matchRoute(
        `${ROUTES.WALLETS.ROOT}/:id(*page)`,
        currentRoute
      );
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

  isValidAddress = (address: string) => {
    const { app, networkStatus } = this.stores;
    const { environment } = app;
    const { network } = environment;
    const { nodeImplementation } = networkStatus;
    try {
      const result = Util.introspectAddress(address);
      if (
        result.network === network ||
        (nodeImplementation === 'jormungandr' && result.network === TESTNET)
      ) {
        return true;
      }
      return false;
    } catch (error) {
      return false;
    }
  };

  isValidMnemonic = (mnemonic: string) =>
    this.api.ada.isValidMnemonic(mnemonic);

  isValidCertificateMnemonic = (mnemonic: string) =>
    this.api.ada.isValidCertificateMnemonic(mnemonic);

  @action refreshWalletsData = async () => {
    // Prevent wallets data refresh if polling is blocked
    if (this._pollingBlocked) return;

    if (this.stores.networkStatus.isConnected) {
      const result = await this.walletsRequest.execute().promise;
      if (!result) return;
      const walletIds = result.map((wallet: Wallet) => wallet.id);
      await this._setWalletsRecoveryPhraseVerificationData(walletIds);
      runInAction('refresh active wallet', () => {
        if (this.active) {
          this._setActiveWallet({ walletId: this.active.id });
        }
      });
      runInAction('refresh address data', () => {
        this.stores.addresses.addressesRequests = walletIds.map(walletId => ({
          walletId,
          allRequest: this.stores.addresses._getAddressesAllRequest(walletId),
        }));
        this.stores.addresses._refreshAddresses();
      });
      runInAction('refresh transaction data', () => {
        this.stores.transactions.transactionsRequests = walletIds.map(
          walletId => ({
            walletId,
            recentRequest: this.stores.transactions._getTransactionsRecentRequest(
              walletId
            ),
            allRequest: this.stores.transactions._getTransactionsAllRequest(
              walletId
            ),
          })
        );
        this.stores.transactions._refreshTransactionData();
      });
    }
  };

  @action resetWalletsData = () => {
    this.walletsRequest.reset();
    this.stores.addresses.addressesRequests = [];
    this.stores.transactions.transactionsRequests = [];
  };

  @action _restoreWallet = async (params: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: string,
    type?: string,
  }) => {
    // reset getWalletRecoveryPhraseFromCertificateRequest to clear previous errors
    this.getWalletRecoveryPhraseFromCertificateRequest.reset();

    const data = {
      recoveryPhrase: params.recoveryPhrase,
      walletName: params.walletName,
      spendingPassword: params.spendingPassword,
    };

    if (params.type === WALLET_RESTORE_TYPES.CERTIFICATE) {
      // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
      const recoveryPhraseArray = params.recoveryPhrase.split(' ');
      const chunked = chunk(recoveryPhraseArray, 18);
      const scrambledInput = chunked[0]; // first 18 mnemonics
      const certificatePassword = chunked[1]; // last 9 mnemonics
      const spendingPassword = mnemonicToSeedHex(certificatePassword.join(' '));

      // Unscramble 18-word wallet certificate mnemonic to 12-word mnemonic
      const unscrambledRecoveryPhrase: Array<string> = await this.getWalletRecoveryPhraseFromCertificateRequest.execute(
        {
          passphrase: spendingPassword,
          scrambledInput: scrambledInput.join(' '),
        }
      ).promise;
      data.recoveryPhrase = unscrambledRecoveryPhrase.join(' ');
      this.getWalletRecoveryPhraseFromCertificateRequest.reset();
    }

    const request =
      params.type === WALLET_RESTORE_TYPES.LEGACY ||
      params.type === WALLET_RESTORE_TYPES.CERTIFICATE
        ? this.restoreLegacyRequest
        : this.restoreRequest;

    const restoredWallet = await request.execute(data).promise;
    if (!restoredWallet)
      throw new Error('Restored wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(restoredWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.restoreRequest.reset();
    this.goToWalletRoute(restoredWallet.id);
    this.refreshWalletsData();
  };

  @action _importWalletFromFile = async (
    params: WalletImportFromFileParams
  ) => {
    const { filePath, walletName, spendingPassword } = params;
    const importedWallet = await this.importFromFileRequest.execute({
      filePath,
      walletName,
      spendingPassword,
    }).promise;
    if (!importedWallet)
      throw new Error('Imported wallet was not received correctly');
    await this._patchWalletRequestWithNewWallet(importedWallet);
    this.actions.dialogs.closeActiveDialog.trigger();
    this.importFromFileRequest.reset();
    this.goToWalletRoute(importedWallet.id);
    this.refreshWalletsData();
  };

  @action _setActiveWallet = ({ walletId }: { walletId: string }) => {
    if (this.hasAnyWallets) {
      const activeWalletId = this.active ? this.active.id : null;
      const newActiveWallet = this.all.find(wallet => wallet.id === walletId);
      const hasActiveWalletBeenChanged = activeWalletId !== walletId;
      const hasActiveWalletBeenUpdated = !isEqual(this.active, newActiveWallet);
      if (hasActiveWalletBeenChanged) {
        // Active wallet has been replaced or removed
        this.active = newActiveWallet || null;
        if (this.active) {
          this.activeValue = formattedWalletAmount(this.active.amount);
        }
      } else if (hasActiveWalletBeenUpdated) {
        // Active wallet has been updated
        if (this.active && newActiveWallet) this.active.update(newActiveWallet);
      }
    }
  };

  @action _unsetActiveWallet = () => {
    this.active = null;
    this.activeValue = null;
  };

  @action _onRouteChange = (options: { route: string, params: ?Object }) => {
    // Reset the send request anytime we visit the send page (e.g: to remove any previous errors)
    if (
      matchRoute(ROUTES.WALLETS.SEND, buildRoute(options.route, options.params))
    ) {
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

  /**
   * Generates a paper wallet certificate by creating a temporary wallet
   * and extracting its data before deleting it. Saves the certificate
   * as PDF to the user selected file location.
   *
   * Using mobx flows: https://mobx.js.org/best/actions.html#flows
   * @private
   */
  _generateCertificate = flow(function* generateCertificate(params: {
    filePath: string,
    timestamp: string,
  }): Generator<any, any, any> {
    try {
      // Pause polling in order not to show Paper wallet in the UI
      this._pausePolling();

      // Set inProgress state to show spinner if is needed
      this._updateCertificateCreationState(true);

      // Generate wallet recovery phrase
      const recoveryPhrase: Array<string> = yield this.getWalletRecoveryPhraseRequest.execute()
        .promise;

      // Generate 9-words (additional) mnemonic
      const additionalMnemonicWords: Array<string> = yield this.getWalletCertificateAdditionalMnemonicsRequest.execute()
        .promise;
      this.additionalMnemonicWords = additionalMnemonicWords.join(' ');

      // Generate spending password from 9-word mnemonic and save to store
      const spendingPassword = mnemonicToSeedHex(this.additionalMnemonicWords);
      this.walletCertificatePassword = spendingPassword;

      // Generate paper wallet scrambled mnemonic
      const walletCertificateRecoveryPhrase: Array<string> = yield this.getWalletCertificateRecoveryPhraseRequest.execute(
        {
          passphrase: spendingPassword,
          input: recoveryPhrase.join(' '),
        }
      ).promise;
      this.walletCertificateRecoveryPhrase = walletCertificateRecoveryPhrase.join(
        ' '
      );

      // Create temporary wallet
      const walletData = {
        name: 'Paper Wallet',
        mnemonic: recoveryPhrase.join(' '),
      };
      const wallet = yield this.createWalletRequest.execute(walletData).promise;

      // Get temporary wallet address
      let walletAddresses;
      if (wallet) {
        walletAddresses = yield this.getWalletAddressesRequest.execute({
          walletId: wallet.id,
        }).promise;

        // delete temporary wallet
        yield this.deleteWalletRequest.execute({
          walletId: wallet.id,
          isLegacy: wallet.isLegacy,
        });
      }

      // Set wallet certificate address
      const walletAddress = get(
        walletAddresses,
        ['addresses', '0', 'id'],
        null
      );
      this.walletCertificateAddress = walletAddress;

      // download pdf certificate
      yield this._downloadCertificate(
        walletAddress,
        walletCertificateRecoveryPhrase,
        params.filePath,
        params.timestamp
      );
    } catch (error) {
      throw error;
    } finally {
      this._resumePolling();
    }
  }).bind(this);

  _downloadCertificate = async (
    address: string,
    recoveryPhrase: Array<string>,
    filePath: string,
    timestamp: string
  ) => {
    const locale = this.stores.profile.currentLocale;
    const intl = i18nContext(locale);
    const { isMainnet, buildLabel } = this.environment;
    try {
      await downloadPaperWalletCertificate({
        address,
        mnemonics: recoveryPhrase,
        intl,
        filePath,
        isMainnet,
        buildLabel,
        timestamp,
      });
      runInAction('handle successful certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false);
        // Update certificate generator step
        this._updateCertificateStep();
      });
    } catch (error) {
      runInAction('handle failed certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false, error);
      });
    }
  };

  _updateCertificateCreationState = action(
    (state: boolean, error?: ?Object) => {
      this.generatingCertificateInProgress = state;
      this._updateGeneratingCertificateError(error);
    }
  );

  _updateGeneratingCertificateError = action((error?: ?Object) => {
    if (error && error.syscall && error.syscall === 'open') {
      // User tries to replace a file that is open
      this.generatingCertificateError = new WalletPaperWalletOpenPdfError();
    } else {
      this.generatingCertificateError = null;
    }
  });

  /**
   * Generates a rewards csv and saves it to the user selected file location.
   *
   * Using mobx flows: https://mobx.js.org/best/actions.html#flows
   * @private
   */
  _generateRewardsCsv = flow(function* generateRewardsCsv(params: {
    rewards: Array<CsvRecord>,
    filePath: string,
  }) {
    try {
      this._pausePolling();

      // Set inProgress state to show spinner if is needed
      this._updateRewardsCsvCreationState(true);

      // download rewards csv
      yield this._downloadRewardsCsv(params.rewards, params.filePath);
    } catch (error) {
      throw error;
    } finally {
      this._resumePolling();
    }
  }).bind(this);

  _downloadRewardsCsv = async (rewards: Array<CsvRecord>, filePath: string) => {
    try {
      await downloadRewardsCsv({
        rewards,
        filePath,
      });
      runInAction('handle successful rewards csv download', () => {
        this._updateRewardsCsvCreationState(false);
      });
    } catch (error) {
      runInAction('handle failed rewards csv download', () => {
        this._updateRewardsCsvCreationState(false, error);
      });
    }
  };

  _updateRewardsCsvCreationState = action((state: boolean, error?: ?Object) => {
    this.generatingRewardsCsvInProgress = state;
    this._updateGeneratingRewardsCsvError(error);
  });

  _updateGeneratingRewardsCsvError = action((error?: ?Object) => {
    if (error && error.syscall && error.syscall === 'open') {
      // User tries to replace a file that is open
      this.generatingRewardsCsvError = new WalletRewardsOpenCsvError();
    } else {
      this.generatingRewardsCsvError = null;
    }
  });

  /**
   * - Receives a walet local data
   * - Returns the wallet's recovery phrase verification status
   */
  _setWalletRecoveryPhraseVerificationData = ({
    recoveryPhraseVerificationDate,
    creationDate,
  }: WalletLocalData) => {
    const dateToCheck =
      recoveryPhraseVerificationDate || creationDate || new Date();
    const daysSinceDate = moment().diff(moment(dateToCheck), 'days');
    let recoveryPhraseVerificationStatus =
      WalletRecoveryPhraseVerificationStatuses.OK;
    if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_NOTIFICATION)
      recoveryPhraseVerificationStatus =
        WalletRecoveryPhraseVerificationStatuses.NOTIFICATION;
    else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_WARNING)
      recoveryPhraseVerificationStatus =
        WalletRecoveryPhraseVerificationStatuses.WARNING;
    const recoveryPhraseVerificationStatusType = recoveryPhraseVerificationDate
      ? WalletRecoveryPhraseVerificationTypes.ALREADY_CHECKED
      : WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED;
    return {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    };
  };

  /**
   * - Receives a list of wallets Ids
   * - Retrieves the wallets local data from localStorage
   * - Populates `recoveryPhraseVerificationData` with
   *   the wallets recovery phrase verification data
   * - Updates localStorage in case of missing information
   */
  _setWalletsRecoveryPhraseVerificationData = async (
    walletIds: Array<string>
  ): RecoveryPhraseVerificationData => {
    const walletsLocalData = await this._getWalletsLocalData();
    const recoveryPhraseVerificationData = {};
    await asyncForEach(walletIds, async id => {
      let walletLocalData = walletsLocalData[id];
      // In case a wallet is not in the localStorage yet, it adds it
      if (!walletLocalData) {
        walletLocalData = {
          id,
          creationDate: new Date(),
        };
        await this.setWalletLocalDataRequest.execute(walletLocalData);
      }
      // In case a wallet doesn't have creationDate in the localStorage yet, it adds it
      if (!walletLocalData.creationDate) {
        walletLocalData.creationDate = new Date();
        await this._updateWalletLocalData({
          id,
          creationDate: walletLocalData.creationDate,
        });
      }
      recoveryPhraseVerificationData[
        id
      ] = this._setWalletRecoveryPhraseVerificationData(walletLocalData);
    });
    runInAction('refresh recovery phrase verification data', async () => {
      this.recoveryPhraseVerificationData = recoveryPhraseVerificationData;
    });
  };

  getWalletRecoveryPhraseVerification = (
    walletId: string
  ): WalletRecoveryPhraseVerificationData => {
    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = this.recoveryPhraseVerificationData[walletId] || {};

    return {
      creationDate: creationDate || new Date(),
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus:
        recoveryPhraseVerificationStatus ||
        this.WALLET_RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
      recoveryPhraseVerificationStatusType:
        recoveryPhraseVerificationStatusType ||
        this.WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED,
    };
  };

  @action _setCertificateTemplate = (params: { selectedTemplate: string }) => {
    this.certificateTemplate = params.selectedTemplate;
    this._updateCertificateStep();
  };

  @action _finishCertificate = () => {
    this._updateGeneratingCertificateError();
    this._closeCertificateGeneration();
  };

  @action _finishRewardsCsv = () => {
    this._updateGeneratingRewardsCsvError();
    this._closeRewardsCsvGeneration();
  };

  @action _updateCertificateStep = (isBack: boolean = false) => {
    this._updateGeneratingCertificateError();
    const currrentCertificateStep = this.certificateStep || 0;
    this.certificateStep = isBack
      ? currrentCertificateStep - 1
      : currrentCertificateStep + 1;
  };

  @action _closeCertificateGeneration = () => {
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetCertificateData();
  };

  @action _closeRewardsCsvGeneration = () => {
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetRewardsCsvData();
  };

  @action _resetCertificateData = () => {
    this.walletCertificatePassword = null;
    this.walletCertificateAddress = null;
    this.walletCertificateRecoveryPhrase = null;
    this.generatingCertificateInProgress = false;
    this.certificateTemplate = false;
    this.certificateStep = null;
    this._updateGeneratingCertificateError();
  };

  @action _resetRewardsCsvData = () => {
    this.generatingRewardsCsvInProgress = false;
    this._updateGeneratingRewardsCsvError();
  };

  _getWalletsLocalData = async () => {
    const walletsLocalData: WalletsLocalData = await this.getWalletsLocalDataRequest.execute();
    return walletsLocalData;
  };
  _updateWalletLocalData = async (updatedWalletData: Object): Object => {
    const { id } = updatedWalletData;
    const walletLocalData = await this.updateWalletLocalDataRequest.execute(
      updatedWalletData
    );
    runInAction('Update wallet verification date', () => {
      this.recoveryPhraseVerificationData[
        id
      ] = this._setWalletRecoveryPhraseVerificationData(walletLocalData);
    });
  };
  _updateRecoveryPhraseVerificationDate = async () => {
    if (!this.active) return;
    const { id } = this.active;
    const recoveryPhraseVerificationDate = new Date();
    await this._updateWalletLocalData({
      id,
      recoveryPhraseVerificationDate,
    });
  };
  _unsetWalletLocalData = async (walletId: string) => {
    await this.unsetWalletLocalDataRequest.execute(walletId);
  };
}
