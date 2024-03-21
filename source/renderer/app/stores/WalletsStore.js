'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const bignumber_js_1 = require('bignumber.js');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const Wallet_1 = __importStar(require('../domains/Wallet'));
const numbersConfig_1 = require('../config/numbersConfig');
const i18nContext_1 = require('../utils/i18nContext');
const crypto_1 = require('../utils/crypto');
const paperWalletPdfGenerator_1 = require('../utils/paperWalletPdfGenerator');
const addressPDFGenerator_1 = require('../utils/addressPDFGenerator');
const csvGenerator_1 = require('../utils/csvGenerator');
const routing_1 = require('../utils/routing');
const logging_1 = require('../utils/logging');
const routes_config_1 = require('../routes-config');
const formatters_1 = require('../utils/formatters');
const strings_1 = require('../utils/strings');
const hardwareWalletUtils_1 = require('../utils/hardwareWalletUtils');
const errors_1 = require('../i18n/errors');
const walletRestoreConfig_1 = require('../config/walletRestoreConfig');
const walletsConfig_1 = require('../config/walletsConfig');
const introspect_address_1 = require('../ipc/introspect-address');
const saveQRCodeImageChannel_1 = require('../ipc/saveQRCodeImageChannel');
const cardano_node_types_1 = require('../../../common/types/cardano-node.types');
const analytics_1 = require('../analytics');
const getEventNameFromWallet_1 = require('../analytics/utils/getEventNameFromWallet');
/* eslint-disable consistent-return */
/**
 * The base wallet store that contains logic for dealing with wallets
 */
class WalletsStore extends Store_1.default {
  WALLET_REFRESH_INTERVAL = 5000;
  undelegateWalletSubmissionSuccess = null;
  isAddressFromSameWallet = false;
  // REQUESTS
  walletsRequest = new LocalizedRequest_1.default(this.api.ada.getWallets);
  accountPublicKeyRequest = new LocalizedRequest_1.default(
    this.api.ada.getAccountPublicKey
  );
  icoPublicKeyRequest = new LocalizedRequest_1.default(
    this.api.ada.getICOPublicKey
  );
  importFromFileRequest = new LocalizedRequest_1.default(
    this.api.ada.importWalletFromFile
  );
  createWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.createWallet
  );
  getWalletAddressesRequest = new LocalizedRequest_1.default(
    this.api.ada.getAddresses
  );
  deleteWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.deleteWallet
  );
  sendMoneyRequest = new LocalizedRequest_1.default(
    this.api.ada.createTransaction
  );
  getWalletRecoveryPhraseRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletRecoveryPhrase
  );
  getWalletCertificateAdditionalMnemonicsRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletCertificateAdditionalMnemonics
  );
  getWalletCertificateRecoveryPhraseRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletCertificateRecoveryPhrase
  );
  getWalletRecoveryPhraseFromCertificateRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletRecoveryPhraseFromCertificate
  );
  restoreDaedalusRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreWallet
  );
  restoreLegacyRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreLegacyWallet
  );
  restoreByronRandomWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreByronRandomWallet
  );
  restoreByronIcarusWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreByronIcarusWallet
  );
  restoreByronTrezorWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreByronTrezorWallet
  );
  restoreByronLedgerWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.restoreByronLedgerWallet
  );
  transferFundsCalculateFeeRequest = new LocalizedRequest_1.default(
    this.api.ada.transferFundsCalculateFee
  );
  transferFundsRequest = new LocalizedRequest_1.default(
    this.api.ada.transferFunds
  );
  createHardwareWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.createHardwareWallet
  );
  /* ----------  Active Wallet  ---------- */
  active = null;
  activeValue = null;
  activePublicKey = null;
  icoPublicKey = null;
  /* ----------  Create Wallet  ---------- */
  createWalletStep = null;
  createWalletShowAbortConfirmation = false;
  // TODO: Remove once the new wallet creation process is ready
  createWalletUseNewProcess = false;
  /* ----------  Restore Wallet  ---------- */
  restoreWalletStep = null;
  restoreWalletShowAbortConfirmation = false;
  // STEP: WALLET TYPE
  walletKind = null;
  walletKindDaedalus = null;
  walletKindYoroi = null;
  walletKindHardware = null;
  // STEP: RECOVERY PHRASE
  mnemonics = [];
  // STEP: CONFIGURATION
  walletName = '';
  spendingPassword = '';
  repeatPassword = '';
  // TODO: Remove once the new restore creation process is ready
  restoreWalletUseNewProcess = true;
  restoredWallet = null;
  /* ----------  Export Wallet  ---------- */
  walletExportType = 'paperWallet';
  walletExportMnemonic = 'marine joke dry silk ticket thing sugar stereo aim';
  /* ----------  Delete Wallet  ---------- */
  isDeleting = false;
  /* ----------  Restore Wallet  ---------- */
  isRestoring = false;
  /* ----------  Paper Wallet  ---------- */
  createPaperWalletCertificateStep = 0;
  walletCertificatePassword = null;
  walletCertificateAddress = null;
  walletCertificateRecoveryPhrase = null;
  generatingCertificateInProgress = false;
  generatingCertificateError = null;
  generatingRewardsCsvInProgress = false;
  generatingRewardsCsvError = null;
  certificateStep = null;
  certificateTemplate = null;
  additionalMnemonicWords = null;
  /* ----------  Transfer Funds  ---------- */
  transferFundsSourceWalletId = '';
  transferFundsTargetWalletId = '';
  transferFundsStep = 0;
  transferFundsFee = null;
  transferFundsLeftovers = null;
  /* ----------  Other  ---------- */
  _newWalletDetails = {
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
    walletsActions.createHardwareWallet.listen(this._createHardwareWallet);
    // ---
    // Restore Wallet Actions ---
    walletsActions.restoreWallet.listen(this._restore);
    walletsActions.restoreWalletBegin.listen(this._restoreWalletBegin);
    walletsActions.restoreWalletEnd.listen(this._restoreWalletEnd);
    walletsActions.restoreWalletChangeStep.listen(
      this._restoreWalletChangeStep
    );
    walletsActions.restoreWalletClose.listen(this._restoreWalletClose);
    walletsActions.restoreWalletCancelClose.listen(
      this._restoreWalletCancelClose
    );
    walletsActions.restoreWalletSetKind.listen(this._restoreWalletSetKind);
    walletsActions.restoreWalletSetMnemonics.listen(
      this._restoreWalletSetMnemonics
    );
    walletsActions.restoreWalletSetConfig.listen(this._restoreWalletSetConfig);
    walletsActions.deleteWallet.listen(this._deleteWallet);
    walletsActions.undelegateWallet.listen(this._undelegateWallet);
    walletsActions.setUndelegateWalletSubmissionSuccess.listen(
      this._setUndelegateWalletSubmissionSuccess
    );
    walletsActions.sendMoney.listen(this._sendMoney);
    walletsActions.importWalletFromFile.listen(this._importWalletFromFile);
    walletsActions.chooseWalletExportType.listen(this._chooseWalletExportType);
    walletsActions.getAccountPublicKey.listen(this._getAccountPublicKey);
    walletsActions.getICOPublicKey.listen(this._getICOPublicKey);
    walletsActions.generateCertificate.listen(this._generateCertificate);
    walletsActions.generateAddressPDF.listen(this._generateAddressPDF);
    walletsActions.saveQRCodeImage.listen(this._saveQRCodeImage);
    walletsActions.updateCertificateStep.listen(this._updateCertificateStep);
    walletsActions.closeCertificateGeneration.listen(
      this._closeCertificateGeneration
    );
    walletsActions.generateCsv.listen(this._generateCsv);
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
  _getAccountPublicKey = async ({ spendingPassword: passphrase }) => {
    if (!this.active || !walletsConfig_1.IS_WALLET_PUBLIC_KEY_SHARING_ENABLED) {
      return;
    }
    const walletId = this.active.id;
    const index = '0H';
    const extended = true;
    try {
      const accountPublicKey = await this.accountPublicKeyRequest.execute({
        walletId,
        index,
        passphrase,
        extended,
      }).promise;
      (0, mobx_1.runInAction)('update account public key', () => {
        this.activePublicKey = accountPublicKey;
      });
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Revealed wallet public key'
      );
    } catch (error) {
      throw error;
    }
  };
  _getICOPublicKey = async ({ spendingPassword: passphrase }) => {
    if (!this.active || !walletsConfig_1.IS_WALLET_PUBLIC_KEY_SHARING_ENABLED) {
      return;
    }
    const walletId = this.active.id;
    const index = '0H';
    const format = 'extended';
    const purpose = '1854H';
    try {
      const icoPublicKey = await this.icoPublicKeyRequest.execute({
        walletId,
        index,
        data: {
          passphrase,
          format,
          purpose,
        },
      }).promise;
      (0, mobx_1.runInAction)('update ICO public key', () => {
        this.icoPublicKey = icoPublicKey;
      });
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Revealed wallet multi-signature public key'
      );
    } catch (error) {
      throw error;
    }
  };
  _create = async (params) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase = await this.getWalletRecoveryPhraseRequest.execute()
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
  _togglecreateWalletUseNewProcess = () => {
    this.createWalletUseNewProcess = !this.createWalletUseNewProcess;
  };
  _createWalletBegin = () => {
    this.createWalletStep = 0;
    this.createWalletShowAbortConfirmation = false;
  };
  _createWalletChangeStep = (isBack = false) => {
    const currentCreateWalletStep = this.createWalletStep || 0;
    this.createWalletStep =
      isBack === true
        ? currentCreateWalletStep - 1
        : currentCreateWalletStep + 1;
    this.createWalletShowAbortConfirmation = false;
  };
  _createWalletClose = () => {
    this.createWalletStep = null;
    this.createWalletShowAbortConfirmation = false;
  };
  _createWalletAbort = () => {
    this.createWalletShowAbortConfirmation = true;
  };
  _restoreWalletBegin = () => {
    this.restoreWalletStep = 0;
    this.restoreWalletShowAbortConfirmation = false;
  };
  _restoreWalletEnd = async () => {
    this._resumePolling();
    const { restoredWallet } = this;
    if (restoredWallet) {
      await this._patchWalletRequestWithNewWallet(restoredWallet);
      this.goToWalletRoute(restoredWallet.id);
      this.refreshWalletsData();
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Restored a software wallet',
        this.walletKind
      );
      this._restoreWalletResetRequests();
      this._restoreWalletResetData();
    }
  };
  _restoreWalletChangeStep = (isBack = false) => {
    // Reset restore requests to clear previous errors
    const currentRestoreWalletStep = this.restoreWalletStep || 0;
    this._restoreWalletResetRequests();
    if (this.restoreWalletStep === null) {
      this._restoreWalletResetData();
    }
    this.restoreWalletStep =
      isBack === true
        ? currentRestoreWalletStep - 1
        : currentRestoreWalletStep + 1;
    this.restoreWalletShowAbortConfirmation = false;
  };
  _restoreWalletClose = () => {
    this._resumePolling();
    const { mnemonics, walletName, spendingPassword } = this;
    const shouldDisplayAbortAlert =
      (mnemonics.length || walletName.length || spendingPassword.length) &&
      this.restoreWalletStep !== null &&
      this.restoreWalletStep <
        walletRestoreConfig_1.RESTORE_WALLET_STEPS.length - 1;
    if (shouldDisplayAbortAlert && !this.restoreWalletShowAbortConfirmation) {
      this.restoreWalletShowAbortConfirmation = true;
    } else {
      this._restoreWalletResetRequests();
      this._restoreWalletResetData();
      this.actions.dialogs.closeActiveDialog.trigger();
    }
  };
  _restoreWalletCancelClose = () => {
    this.restoreWalletShowAbortConfirmation = false;
  };
  _restoreWalletResetRequests = () => {
    this.restoreDaedalusRequest.reset();
    this.restoreByronIcarusWalletRequest.reset();
    this.restoreByronLedgerWalletRequest.reset();
    this.restoreByronRandomWalletRequest.reset();
    this.restoreByronTrezorWalletRequest.reset();
    this.getWalletRecoveryPhraseFromCertificateRequest.reset();
  };
  _restoreWalletResetData = () => {
    this.restoreWalletStep = null;
    this.restoreWalletShowAbortConfirmation = false;
    this.restoredWallet = null;
    this.walletKind = null;
    this.walletKindDaedalus = null;
    this.walletKindYoroi = null;
    this.walletKindHardware = null;
    this.mnemonics = [];
    this.walletName = '';
    this.spendingPassword = '';
    this.repeatPassword = '';
  };
  _restoreWalletSetKind = ({ param, kind }) => {
    this[`walletKind${param || ''}`] = kind;
    this.mnemonics = [];
  };
  _restoreWalletSetMnemonics = ({ mnemonics }) => {
    this.mnemonics = mnemonics;
  };
  _restoreWalletSetConfig = ({ param, value }) => {
    if (param === 'walletName') {
      this.walletName = value;
    } else if (param === 'spendingPassword') {
      this.spendingPassword = value;
    } else if (param === 'repeatPassword') {
      this.repeatPassword = value;
    }
  };
  _createHardwareWallet = async (params) => {
    const { walletName, extendedPublicKey, device } = params;
    const { deviceId, deviceType, deviceModel, deviceName, path } = device;
    const accountPublicKey =
      extendedPublicKey.publicKeyHex + extendedPublicKey.chainCodeHex;
    logging_1.logger.debug('[HW-DEBUG] HWStore - Execute HW create / restore', {
      deviceId,
      deviceType,
      deviceModel,
      deviceName,
      path,
      walletName,
    });
    try {
      await this._pausePolling();
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const wallet = await this.createHardwareWalletRequest.execute({
        walletName,
        accountPublicKey,
      });
      await this.stores.hardwareWallets._setHardwareWalletLocalData({
        walletId: wallet.id,
        data: {
          device,
          extendedPublicKey,
          disconnected: false,
        },
      });
      await this.stores.hardwareWallets._setHardwareWalletDevice({
        deviceId,
        data: {
          deviceType,
          deviceModel,
          deviceName,
          path,
          paired: wallet.id,
          // device paired with software wallet
          disconnected: false, // device physically disconnected
        },
      });
      if (wallet) {
        await this._patchWalletRequestWithNewWallet(wallet);
        this.goToWalletRoute(wallet.id);
        this.refreshWalletsData();
        this.actions.dialogs.closeActiveDialog.trigger();
        this.analytics.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Created a new hardware wallet'
        );
      }
    } catch (error) {
      throw error;
    } finally {
      this._resumePolling();
    }
  };
  _finishWalletBackup = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(
      ' '
    );
    const wallet = await this.createWalletRequest.execute(
      this._newWalletDetails
    ).promise;
    if (wallet) {
      await this._patchWalletRequestWithNewWallet(wallet);
      this.actions.dialogs.closeActiveDialog.trigger();
      this.goToWalletRoute(wallet.id);
      this.refreshWalletsData();
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Created a new software wallet'
      );
    }
  };
  _deleteWallet = async (params) => {
    // Pause polling in order to avoid fetching data for wallet we are about to delete
    (0, mobx_1.runInAction)('AdaWalletsStore::isDeleting set', () => {
      this.isDeleting = true;
    });
    await this._pausePolling();
    const walletToDelete = this.getWalletById(params.walletId);
    if (!walletToDelete) {
      (0, mobx_1.runInAction)('AdaWalletsStore::isDeleting reset', () => {
        this.isDeleting = false;
      });
      return;
    }
    const indexOfWalletToDelete = this.all.indexOf(walletToDelete);
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.deleteWalletRequest.execute({
      walletId: params.walletId,
      isLegacy: params.isLegacy || false,
    });
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.walletsRequest.patch((result) => {
      result.splice(indexOfWalletToDelete, 1);
    });
    (0, mobx_1.runInAction)('AdaWalletsStore::_deleteWallet', () => {
      this.isDeleting = false;
      if (this.hasAnyWallets) {
        const nextIndexInList = Math.max(indexOfWalletToDelete - 1, 0);
        const nextWalletInList = this.all[nextIndexInList];
        this.goToWalletRoute(nextWalletInList.id);
      } else {
        this.active = null;
        this.activeValue = null;
        this.actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.WALLETS.ADD,
        });
      }
    });
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Wallet deleted',
      (0, getEventNameFromWallet_1.getEventNameFromWallet)(walletToDelete)
    );
    this.actions.dialogs.closeActiveDialog.trigger();
    this.actions.walletsLocal.unsetWalletLocalData.trigger({
      walletId: params.walletId,
    });
    await this.stores.hardwareWallets._unsetHardwareWalletLocalData({
      walletId: params.walletId,
    });
    this._resumePolling();
    this.deleteWalletRequest.reset();
    this.refreshWalletsData();
  };
  _undelegateWallet = async (params) => {
    const { quitStakePoolRequest } = this.stores.staking;
    const { quitStakePool } = this.actions.staking;
    const walletToUndelegate = this.getWalletById(params.walletId);
    if (!walletToUndelegate) {
      return;
    }
    await quitStakePool.trigger(params);
    this._setUndelegateWalletSubmissionSuccess({
      result: true,
    });
    quitStakePoolRequest.reset();
    this.refreshWalletsData();
  };
  _setUndelegateWalletSubmissionSuccess = ({ result }) => {
    (0, mobx_1.runInAction)(
      'AdaWalletsStore::_setUndelegateWalletSubmissionSuccess',
      () => {
        this.undelegateWalletSubmissionSuccess = result;
      }
    );
  };
  _getUnscrambledMnemonics = async (
    mnemonics
    // @ts-ignore ts-migrate(1064) FIXME: The return type of an async function or method mus... Remove this comment to see the full error message
  ) => {
    // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
    const { passphrase, scrambledInput } = (0, crypto_1.getScrambledInput)(
      mnemonics
    );
    // Unscramble 18-word wallet certificate mnemonic to 12-word mnemonic
    const unscrambledRecoveryPhrase = await this.getWalletRecoveryPhraseFromCertificateRequest.execute(
      {
        passphrase,
        scrambledInput,
      }
    ).promise;
    this.getWalletRecoveryPhraseFromCertificateRequest.reset();
    return unscrambledRecoveryPhrase;
  };
  _restore = async () => {
    this.isRestoring = true;
    // Pause polling in order to avoid fetching data for wallet we are about to restore
    // so that we remain on the "Add wallet" screen until user closes the TADA screen
    await this._pausePolling();
    // Reset restore requests to clear previous errors
    this._restoreWalletResetRequests();
    const data = {
      recoveryPhrase: this.mnemonics,
      walletName: this.walletName,
      spendingPassword: this.spendingPassword,
    };
    const request = this.restoreRequest;
    if (
      this.walletKind === walletRestoreConfig_1.WALLET_KINDS.DAEDALUS &&
      this.walletKindDaedalus ===
        walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.BYRON_27_WORD
    ) {
      // Reset getWalletRecoveryPhraseFromCertificateRequest to clear previous errors
      this.getWalletRecoveryPhraseFromCertificateRequest.reset();
      data.recoveryPhrase = await this._getUnscrambledMnemonics(this.mnemonics);
    }
    try {
      const restoredWallet = await request.execute(data).promise;
      if (!restoredWallet)
        throw new Error('Restored wallet was not received correctly');
      (0, mobx_1.runInAction)('set restoredWallet', () => {
        this.restoredWallet = restoredWallet;
        this.restoreWalletStep = 3;
      });
    } finally {
      (0, mobx_1.runInAction)('end wallet restore', () => {
        this.isRestoring = false;
      });
    }
  };
  _sendMoney = async ({
    receiver,
    amount,
    passphrase,
    assets,
    assetsAmounts: assetsAmountsStr,
    hasAssetsRemainingAfterTransaction,
  }) => {
    const assetsAmounts = assetsAmountsStr
      ? assetsAmountsStr.map(
          (assetAmount) => new bignumber_js_1.BigNumber(assetAmount)
        )
      : null;
    const formattedAssets =
      assets && assets.length
        ? assets.map(
            // eslint-disable-next-line
            ({ policyId: policy_id, assetName: asset_name }, index) => ({
              policy_id,
              asset_name,
              quantity: (0, lodash_1.get)(assetsAmounts, index, 0),
            })
          )
        : null;
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.sendMoneyRequest.execute({
      address: receiver,
      amount: parseInt(amount, 10),
      passphrase,
      walletId: wallet.id,
      isLegacy: wallet.isLegacy,
      assets: formattedAssets,
      hasAssetsRemainingAfterTransaction,
    });
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Transaction made',
      'Software wallet'
    );
    this.refreshWalletsData();
    this.actions.dialogs.closeActiveDialog.trigger();
    this.sendMoneyRequest.reset();
    this.goToWalletRoute(wallet.id);
  };
  _transferFundsNextStep = async () => {
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
      await this._transferFundsCalculateFee({
        sourceWalletId: transferFundsSourceWalletId,
      });
      nextStep = 2;
    }
    (0, mobx_1.runInAction)('update transfer funds step', () => {
      this.transferFundsStep = nextStep;
    });
  };
  _transferFundsPrevStep = () => {
    const { transferFundsStep } = this;
    const prevStep = transferFundsStep > 0 ? transferFundsStep - 1 : 0;
    this.transferFundsStep = prevStep;
  };
  _transferFunds = async ({ spendingPassword }) => {
    const { transferFundsSourceWalletId, transferFundsTargetWalletId } = this;
    const targetWalletAddresses = await this.getWalletAddressesRequest.execute({
      walletId: transferFundsTargetWalletId,
      queryParams: {
        state: 'unused',
      },
    }).promise;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.transferFundsRequest.execute({
      sourceWalletId: transferFundsSourceWalletId,
      targetWalletAddresses: targetWalletAddresses
        ? targetWalletAddresses.map((address) => address.id).slice(0, 20)
        : null,
      passphrase: spendingPassword,
    });
    this.refreshWalletsData();
    this._transferFundsClose();
    this.transferFundsRequest.reset();
    this.goToWalletRoute(transferFundsSourceWalletId);
  };
  _transferFundsSetSourceWalletId = ({ sourceWalletId }) => {
    this.transferFundsSourceWalletId = sourceWalletId;
    // Sets the target wallet to the first wallet
    const { allWallets } = this;
    this.transferFundsTargetWalletId = (0, lodash_1.get)(
      allWallets,
      [0, 'id'],
      ''
    );
    // Sets to first step
    this.transferFundsStep = 1;
  };
  _transferFundsSetTargetWalletId = ({ targetWalletId }) => {
    this.transferFundsTargetWalletId = targetWalletId;
  };
  _transferFundsRedeem = () => {
    this.transferFundsStep = 0; // TODO: Call API method
  };
  _transferFundsClose = () => {
    this.transferFundsStep = 0;
    this.transferFundsFee = null;
    this.transferFundsCalculateFeeRequest.reset();
  };
  _transferFundsCalculateFee = async ({ sourceWalletId }) => {
    const {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'fee' does not exist on type 'TransferFun... Remove this comment to see the full error message
      fee,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'leftovers' does not exist on type 'Trans... Remove this comment to see the full error message
      leftovers,
    } = await this.transferFundsCalculateFeeRequest.execute({
      sourceWalletId,
    }).promise;
    (0, mobx_1.runInAction)('set migration fee and leftovers', () => {
      this.transferFundsFee = fee;
      this.transferFundsLeftovers = leftovers;
    });
  };
  // =================== PUBLIC API ==================== //
  // GETTERS
  get hasActiveWallet() {
    return !!this.active;
  }
  get hasLoadedWallets() {
    return this.walletsRequest.wasExecuted;
  }
  get hasAnyWallets() {
    if (this.walletsRequest.result == null) return false;
    return (
      this.walletsRequest.wasExecuted && this.walletsRequest.result.length > 0
    );
  }
  get hasRewardsWallets() {
    return this.allWallets.length > 0;
  }
  get hasMaxWallets() {
    return this.all.length >= numbersConfig_1.MAX_ADA_WALLETS_COUNT;
  }
  get all() {
    return [...this.allWallets, ...this.allLegacyWallets];
  }
  get allWallets() {
    return this.walletsRequest.result
      ? this.walletsRequest.result.filter(({ isLegacy }) => !isLegacy)
      : [];
  }
  get allLegacyWallets() {
    return this.walletsRequest.result
      ? this.walletsRequest.result.filter(({ isLegacy }) => isLegacy)
      : [];
  }
  get first() {
    return this.all.length > 0 ? this.all[0] : null;
  }
  get hasAnyLoaded() {
    return this.all.length > 0;
  }
  get activeWalletRoute() {
    if (!this.active) return null;
    return this.getWalletRoute(this.active.id);
  }
  get isWalletRoute() {
    const { currentRoute } = this.stores.app;
    return (0, routing_1.matchRoute)(
      `${routes_config_1.ROUTES.WALLETS.ROOT}(/*rest)`,
      currentRoute
    );
  }
  get restoreRequest() {
    switch (this.walletKind) {
      case walletRestoreConfig_1.WALLET_KINDS.DAEDALUS:
        if (
          this.walletKindDaedalus ===
            walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.SHELLEY_15_WORD ||
          this.walletKindDaedalus ===
            walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.SHELLEY_24_WORD
        ) {
          return this.restoreDaedalusRequest;
        }
        return this.restoreByronRandomWalletRequest;
      case walletRestoreConfig_1.WALLET_KINDS.YOROI:
        if (
          this.walletKindYoroi ===
          walletRestoreConfig_1.WALLET_YOROI_KINDS.BYRON_15_WORD
        ) {
          return this.restoreByronIcarusWalletRequest;
        }
        return this.restoreDaedalusRequest;
      case walletRestoreConfig_1.WALLET_KINDS.HARDWARE:
        if (
          this.walletKindHardware ===
          walletRestoreConfig_1.WALLET_HARDWARE_KINDS.LEDGER
        ) {
          return this.restoreByronLedgerWalletRequest;
        }
        return this.restoreByronTrezorWalletRequest;
      default:
        return this.restoreDaedalusRequest;
    }
  }
  getWalletById = (id) => this.all.find((w) => w.id === id);
  getWalletByName = (name) => this.all.find((w) => w.name === name);
  getWalletRoute = (walletId, page = 'summary') =>
    (0, routing_1.buildRoute)(routes_config_1.ROUTES.WALLETS.PAGE, {
      id: walletId,
      page,
    });
  // ACTIONS
  goToWalletRoute(walletId) {
    const route = this.getWalletRoute(walletId);
    this.actions.router.goToRoute.trigger({
      route,
    });
  }
  // =================== PRIVATE API ==================== //
  get _canRedirectToWallet() {
    const { currentRoute } = this.stores.app;
    const isRootRoute = (0, routing_1.matchRoute)(
      routes_config_1.ROUTES.WALLETS.ROOT,
      currentRoute
    );
    const isAddWalletRoute = (0, routing_1.matchRoute)(
      routes_config_1.ROUTES.WALLETS.ADD,
      currentRoute
    );
    return isRootRoute || isAddWalletRoute;
  }
  _patchWalletRequestWithNewWallet = async (wallet) => {
    // Only add the new wallet if it does not exist yet in the result!
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.walletsRequest.patch((result) => {
      if (
        !(0, lodash_1.find)(result, {
          id: wallet.id,
        })
      ) {
        if (wallet.isLegacy) {
          // Legacy wallets are always added to the end of the list!
          result.push(wallet);
        } else {
          const index = (0, lodash_1.findIndex)(result, 'isLegacy');
          if (index >= 0) {
            result.splice(index, 0, wallet);
          } else {
            result.push(wallet);
          }
        }
      }
    });
  };
  _pollRefresh = async () => {
    const { isConnected } = this.stores.networkStatus;
    return isConnected && this.refreshWalletsData();
  };
  _updateActiveWalletOnRouteChanges = () => {
    const { currentRoute } = this.stores.app;
    const hasAnyWalletLoaded = this.hasAnyLoaded;
    const isWalletAddPage = (0, routing_1.matchRoute)(
      routes_config_1.ROUTES.WALLETS.ADD,
      currentRoute
    );
    (0, mobx_1.runInAction)(
      'WalletsStore::_updateActiveWalletOnRouteChanges',
      () => {
        // There are not wallets loaded (yet) -> unset active and return
        if (isWalletAddPage || !hasAnyWalletLoaded)
          return this._unsetActiveWallet();
        const match = (0, routing_1.matchRoute)(
          `${routes_config_1.ROUTES.WALLETS.ROOT}/:id(*page)`,
          currentRoute
        );
        if (match) {
          // We have a route for a specific wallet -> let's try to find it
          const walletForCurrentRoute = this.all.find((w) => w.id === match.id);
          if (walletForCurrentRoute) {
            // The wallet exists, we are done
            this._setActiveWallet({
              walletId: walletForCurrentRoute.id,
            });
          } else if (hasAnyWalletLoaded) {
            // There is no wallet with given id -> pick first wallet
            this._setActiveWallet({
              walletId: this.all[0].id,
            });
            if (this.active) this.goToWalletRoute(this.active.id);
          }
        } else if (this._canRedirectToWallet) {
          // The route does not specify any wallet -> pick first wallet
          if (!this.hasActiveWallet && hasAnyWalletLoaded) {
            this._setActiveWallet({
              walletId: this.all[0].id,
            });
          }
          if (this.active) {
            this.goToWalletRoute(this.active.id);
          }
        }
      }
    );
  };
  isValidAddress = async (address) => {
    const { network } = this.environment;
    const expectedNetworkTag = (0,
    lodash_1.get)(cardano_node_types_1.NetworkMagics, [network]);
    const validAddressStyles = ['Byron', 'Icarus', 'Shelley'];
    this.isAddressFromSameWallet = false;
    if (!expectedNetworkTag) {
      throw new Error('Unexpected environment');
    }
    try {
      const response = await introspect_address_1.introspectAddressChannel.send(
        {
          input: address,
        }
      );
      if (
        response === 'Invalid' ||
        !(0, hardwareWalletUtils_1.isReceiverAddressType)(
          response.introspection.address_type
        )
      ) {
        return false;
      }
      (0, mobx_1.runInAction)(
        'check if address is from the same wallet',
        () => {
          const walletAddresses = this.stores.addresses.all
            .slice()
            .map((addr) => addr.id);
          this.isAddressFromSameWallet = !!walletAddresses.filter(
            (addr) => addr === address
          ).length;
        }
      );
      return (
        validAddressStyles.includes(response.introspection.address_style) &&
        ((Array.isArray(expectedNetworkTag) &&
          (0, lodash_1.includes)(
            expectedNetworkTag,
            response.introspection.network_tag
          )) ||
          expectedNetworkTag === response.introspection.network_tag)
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(error);
    }
  };
  isValidCertificateMnemonic = (mnemonic) =>
    this.api.ada.isValidCertificateMnemonic(mnemonic);
  refreshWalletsData = async () => {
    // Prevent wallets data refresh if polling is blocked
    if (this._pollingBlocked) return;
    if (this.stores.networkStatus.isConnected) {
      const result = await this.walletsRequest.execute().promise;
      if (!result) return;
      const walletIds = result
        .filter(
          ({ syncState }) =>
            syncState.status !== Wallet_1.WalletSyncStateStatuses.NOT_RESPONDING
        )
        .map((wallet) => wallet.id);
      await this.actions.walletsLocal.refreshWalletsLocalData.trigger();
      (0, mobx_1.runInAction)('refresh active wallet', () => {
        if (this.active) {
          this._setActiveWallet({
            walletId: this.active.id,
          });
        }
      });
      (0, mobx_1.runInAction)('refresh address data', () => {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'WalletsS... Remove this comment to see the full error message
        this.stores.addresses.addressesRequests = walletIds.map((walletId) => ({
          walletId,
          allRequest: this.stores.addresses._getAddressesAllRequest(walletId),
        }));
        this.stores.addresses._refreshAddresses();
      });
      (0, mobx_1.runInAction)('refresh transaction data', () => {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'WalletsS... Remove this comment to see the full error message
        this.stores.transactions.transactionsRequests = walletIds.map(
          (walletId) => ({
            walletId,
            recentRequest: this.stores.transactions._getTransactionsRecentRequest(
              walletId
            ),
            allRequest: this.stores.transactions._getTransactionsAllRequest(
              walletId
            ),
            withdrawalsRequest: this.stores.transactions._getWithdrawalsRequest(
              walletId
            ),
          })
        );
        this.stores.transactions._refreshTransactionData();
      });
      this.actions.wallets.refreshWalletsDataSuccess.trigger();
    }
  };
  resetWalletsData = () => {
    this.walletsRequest.reset();
    this.stores.addresses.addressesRequests = [];
    this.stores.transactions.transactionsRequests = [];
    this.isAddressFromSameWallet = false;
  };
  _importWalletFromFile = async (params) => {
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
  _setActiveWallet = ({ walletId }) => {
    if (this.hasAnyWallets) {
      const activeWalletId = this.active ? this.active.id : null;
      const newActiveWallet = this.all.find((wallet) => wallet.id === walletId);
      if (
        (!this.active || !this.active.isNotResponding) &&
        newActiveWallet &&
        newActiveWallet.isNotResponding
      ) {
        this.actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.WALLETS.PAGE,
          params: {
            id: newActiveWallet.id,
            page: 'summary',
          },
        });
      }
      const hasActiveWalletBeenChanged = activeWalletId !== walletId;
      const hasActiveWalletBeenUpdated = !(0, lodash_1.isEqual)(
        this.active,
        newActiveWallet
      );
      if (hasActiveWalletBeenChanged) {
        // Active wallet has been replaced or removed
        this.active = newActiveWallet || null;
        this.stores.addresses.lastGeneratedAddress = null;
        if (this.active) {
          // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
          this.activeValue = (0, formatters_1.formattedWalletAmount)(
            this.active.amount
          );
          if (this.active && this.active.isHardwareWallet) {
            const {
              hardwareWalletsConnectionData,
            } = this.stores.hardwareWallets;
            const hardwareWalletConnectionData = (0, lodash_1.get)(
              hardwareWalletsConnectionData,
              this.active.id
            );
            if (hardwareWalletConnectionData) {
              const { extendedPublicKey } = hardwareWalletConnectionData;
              const extendedPublicKeyHex = `${extendedPublicKey.publicKeyHex}${extendedPublicKey.chainCodeHex}`;
              const xpub = Buffer.from(extendedPublicKeyHex, 'hex');
              const activePublicKey = (0,
              hardwareWalletUtils_1.bech32EncodePublicKey)(xpub);
              this.activePublicKey = activePublicKey || null;
            }
          } else {
            this.activePublicKey = null;
            this.icoPublicKey = null;
          }
        }
      } else if (hasActiveWalletBeenUpdated) {
        // Active wallet has been updated
        if (this.active && newActiveWallet) this.active.update(newActiveWallet);
      }
    }
  };
  _unsetActiveWallet = () => {
    this.active = null;
    this.activeValue = null;
    this.activePublicKey = null;
    this.icoPublicKey = null;
    this.stores.addresses.lastGeneratedAddress = null;
  };
  _onRouteChange = (options) => {
    // Reset the send request anytime we visit the send page (e.g: to remove any previous errors)
    if (
      (0, routing_1.matchRoute)(
        routes_config_1.ROUTES.WALLETS.SEND,
        (0, routing_1.buildRoute)(options.route, options.params)
      )
    ) {
      this.sendMoneyRequest.reset();
      this.isAddressFromSameWallet = false;
    }
  };
  _chooseWalletExportType = (params) => {
    if (this.walletExportType !== params.walletExportType) {
      this.walletExportType = params.walletExportType;
    }
  };
  _pausePolling = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    if (this.walletsRequest.isExecuting) await this.walletsRequest;
    (0, mobx_1.runInAction)('AdaWalletsStore::_pausePolling', () => {
      this._pollingBlocked = true;
    });
  };
  _resumePolling = () => {
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
  _generateCertificate = (0, mobx_1.flow)(function* generateCertificate(
    params
  ) {
    try {
      // Pause polling in order not to show Paper wallet in the UI
      yield this._pausePolling();
      // Set inProgress state to show spinner if is needed
      this._updateCertificateCreationState(true);
      // Generate wallet recovery phrase
      const recoveryPhrase = yield this.getWalletRecoveryPhraseRequest.execute()
        .promise;
      // Generate 9-words (additional) mnemonic
      const additionalMnemonicWords = yield this.getWalletCertificateAdditionalMnemonicsRequest.execute()
        .promise;
      this.additionalMnemonicWords = additionalMnemonicWords.join(' ');
      // Generate spending password from 9-word mnemonic and save to store
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      const spendingPassword = (0, crypto_1.mnemonicToSeedHex)(
        this.additionalMnemonicWords
      );
      this.walletCertificatePassword = spendingPassword;
      // Generate paper wallet scrambled mnemonic
      const walletCertificateRecoveryPhrase = yield this.getWalletCertificateRecoveryPhraseRequest.execute(
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
      const walletAddress = (0, lodash_1.get)(
        walletAddresses,
        ['0', 'id'],
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
    address,
    recoveryPhrase,
    filePath,
    timestamp
  ) => {
    const locale = this.stores.profile.currentLocale;
    const intl = (0, i18nContext_1.i18nContext)(locale);
    const { isMainnet } = this.environment;
    const {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'buildLabel' does not exist on type 'type... Remove this comment to see the full error message
      buildLabel,
    } = global;
    try {
      await (0, paperWalletPdfGenerator_1.paperWalletPdfGenerator)({
        address,
        mnemonics: recoveryPhrase,
        intl,
        filePath,
        isMainnet,
        buildLabel,
        timestamp,
      });
      (0, mobx_1.runInAction)('handle successful certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false);
        // Update certificate generator step
        this._updateCertificateStep();
      });
    } catch (error) {
      (0, mobx_1.runInAction)('handle failed certificate download', () => {
        // Reset progress
        this._updateCertificateCreationState(false, error);
      });
    }
  };
  _generateAddressPDF = async ({ note, address, filePath, wallet }) => {
    const {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    } = this.stores.profile;
    const { network, isMainnet } = this.environment;
    const intl = (0, i18nContext_1.i18nContext)(currentLocale);
    try {
      await (0, addressPDFGenerator_1.addressPDFGenerator)({
        address,
        note,
        filePath,
        currentLocale,
        currentDateFormat,
        currentTimeFormat,
        network,
        isMainnet,
        intl,
      });
      const walletAddress = (0, strings_1.ellipsis)(address, 15, 15);
      this.actions.wallets.generateAddressPDFSuccess.trigger({
        walletAddress,
      });
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Saved wallet address as PDF',
        (0, getEventNameFromWallet_1.getEventNameFromWallet)(wallet)
      );
    } catch (error) {
      throw new Error(error);
    }
  };
  _saveQRCodeImage = async ({ address, filePath, wallet }) => {
    try {
      await saveQRCodeImageChannel_1.saveQRCodeImageChannel.send({
        address,
        filePath,
      });
      const walletAddress = (0, strings_1.ellipsis)(address, 15, 15);
      this.actions.wallets.saveQRCodeImageSuccess.trigger({
        walletAddress,
      });
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Saved wallet address as QR code',
        (0, getEventNameFromWallet_1.getEventNameFromWallet)(wallet)
      );
    } catch (error) {
      throw new Error(error);
    }
  };
  _updateCertificateCreationState = (0, mobx_1.action)((state, error) => {
    this.generatingCertificateInProgress = state;
    this._updateGeneratingCertificateError(error);
  });
  _updateGeneratingCertificateError = (0, mobx_1.action)((error) => {
    if (error && error.syscall && error.syscall === 'open') {
      // User tries to replace a file that is open
      this.generatingCertificateError = new errors_1.WalletPaperWalletOpenPdfError();
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
  _generateCsv = (0, mobx_1.flow)(function* generateCsv({
    fileContent,
    filePath,
  }) {
    try {
      yield this._pausePolling();
      // Set inProgress state to show spinner if is needed
      this._updateRewardsCsvCreationState(true);
      // download rewards csv
      yield this._downloadRewardsCsv(fileContent, filePath);
    } catch (error) {
      throw error;
    } finally {
      this._resumePolling();
    }
  }).bind(this);
  _downloadRewardsCsv = async (fileContent, filePath) => {
    try {
      await (0, csvGenerator_1.downloadCsv)({
        fileContent,
        filePath,
      });
      (0, mobx_1.runInAction)('handle successful rewards csv download', () => {
        this._updateRewardsCsvCreationState(false);
      });
    } catch (error) {
      (0, mobx_1.runInAction)('handle failed rewards csv download', () => {
        this._updateRewardsCsvCreationState(false, error);
      });
    }
  };
  _updateRewardsCsvCreationState = (0, mobx_1.action)((state, error) => {
    this.generatingRewardsCsvInProgress = state;
    this._updateGeneratingRewardsCsvError(error);
  });
  _updateGeneratingRewardsCsvError = (0, mobx_1.action)((error) => {
    if (error && error.syscall && error.syscall === 'open') {
      // User tries to replace a file that is open
      this.generatingRewardsCsvError = new errors_1.WalletRewardsOpenCsvError();
    } else {
      this.generatingRewardsCsvError = null;
    }
  });
  _setCertificateTemplate = (params) => {
    this.certificateTemplate = params.selectedTemplate;
    this._updateCertificateStep();
  };
  _finishCertificate = () => {
    this._updateGeneratingCertificateError();
    this._closeCertificateGeneration();
  };
  _finishRewardsCsv = () => {
    this._updateGeneratingRewardsCsvError();
    this._closeRewardsCsvGeneration();
  };
  _updateCertificateStep = (isBack = false) => {
    this._updateGeneratingCertificateError();
    const currentCertificateStep = this.certificateStep || 0;
    this.certificateStep = isBack
      ? currentCertificateStep - 1
      : currentCertificateStep + 1;
  };
  _closeCertificateGeneration = () => {
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetCertificateData();
  };
  _closeRewardsCsvGeneration = () => {
    this.actions.dialogs.closeActiveDialog.trigger();
    this._resetRewardsCsvData();
  };
  _resetCertificateData = () => {
    this.walletCertificatePassword = null;
    this.walletCertificateAddress = null;
    this.walletCertificateRecoveryPhrase = null;
    this.generatingCertificateInProgress = false;
    this.certificateTemplate = false;
    this.certificateStep = null;
    this._updateGeneratingCertificateError();
  };
  _resetRewardsCsvData = () => {
    this.generatingRewardsCsvInProgress = false;
    this._updateGeneratingRewardsCsvError();
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  WalletsStore.prototype,
  'undelegateWalletSubmissionSuccess',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'isAddressFromSameWallet',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'walletsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'accountPublicKeyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'icoPublicKeyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'importFromFileRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'createWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'getWalletAddressesRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'deleteWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'sendMoneyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'getWalletRecoveryPhraseRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'getWalletCertificateAdditionalMnemonicsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'getWalletCertificateRecoveryPhraseRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'getWalletRecoveryPhraseFromCertificateRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreDaedalusRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreLegacyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreByronRandomWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreByronIcarusWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreByronTrezorWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'restoreByronLedgerWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'transferFundsCalculateFeeRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'transferFundsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsStore.prototype,
  'createHardwareWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Wallet_1.default)],
  WalletsStore.prototype,
  'active',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.BigNumber)],
  WalletsStore.prototype,
  'activeValue',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'activePublicKey',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'icoPublicKey',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'createWalletStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'createWalletShowAbortConfirmation',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'createWalletUseNewProcess',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'restoreWalletStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'restoreWalletShowAbortConfirmation',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'walletKind',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'walletKindDaedalus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'walletKindYoroi',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'walletKindHardware',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  WalletsStore.prototype,
  'mnemonics',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'walletName',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'spendingPassword',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'repeatPassword',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'restoreWalletUseNewProcess',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Wallet_1.default)],
  WalletsStore.prototype,
  'restoredWallet',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletsStore.prototype,
  'walletExportType',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'walletExportMnemonic',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'isDeleting',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'isRestoring',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'createPaperWalletCertificateStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'walletCertificatePassword',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'walletCertificateAddress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'walletCertificateRecoveryPhrase',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'generatingCertificateInProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Function)],
  WalletsStore.prototype,
  'generatingCertificateError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'generatingRewardsCsvInProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Function)],
  WalletsStore.prototype,
  'generatingRewardsCsvError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'certificateStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'certificateTemplate',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'additionalMnemonicWords',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'transferFundsSourceWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'transferFundsTargetWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'transferFundsStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.BigNumber)],
  WalletsStore.prototype,
  'transferFundsFee',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.BigNumber)],
  WalletsStore.prototype,
  'transferFundsLeftovers',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_getAccountPublicKey',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_getICOPublicKey',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_togglecreateWalletUseNewProcess',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_createWalletBegin',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_createWalletChangeStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_createWalletClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_createWalletAbort',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletBegin',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletEnd',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletChangeStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletCancelClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletResetData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletSetKind',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletSetMnemonics',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restoreWalletSetConfig',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_createHardwareWallet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_restore',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsNextStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsPrevStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFunds',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsSetSourceWalletId',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsSetTargetWalletId',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsRedeem',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_transferFundsCalculateFee',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasActiveWallet',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasLoadedWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasAnyWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasRewardsWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasMaxWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'all',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'allWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'allLegacyWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Wallet_1.default),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'first',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'hasAnyLoaded',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'activeWalletRoute',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'isWalletRoute',
  null
);
__decorate(
  [
    mobx_1.computed,
    // @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
    __metadata('design:type', LocalizedRequest_1.default),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  'restoreRequest',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletsStore.prototype,
  '_canRedirectToWallet',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'refreshWalletsData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  'resetWalletsData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_importWalletFromFile',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_setActiveWallet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_unsetActiveWallet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_onRouteChange',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_chooseWalletExportType',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_pausePolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_resumePolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_setCertificateTemplate',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_finishCertificate',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_finishRewardsCsv',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_updateCertificateStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_closeCertificateGeneration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_closeRewardsCsvGeneration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_resetCertificateData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletsStore.prototype,
  '_resetRewardsCsvData',
  void 0
);
exports.default = WalletsStore;
//# sourceMappingURL=WalletsStore.js.map
