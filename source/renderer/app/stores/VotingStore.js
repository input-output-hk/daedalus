'use strict';
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
exports.FundPhase = void 0;
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const routes_config_1 = require('../routes-config');
const WalletTransaction_1 = require('../domains/WalletTransaction');
const formatters_1 = require('../utils/formatters');
const walletUtils_1 = __importDefault(require('../utils/walletUtils'));
const votingConfig_1 = require('../config/votingConfig');
const votingPDFGenerator_1 = require('../utils/votingPDFGenerator');
const i18nContext_1 = require('../utils/i18nContext');
const analytics_1 = require('../analytics');
var FundPhase;
(function (FundPhase) {
  FundPhase['SNAPSHOT'] = 'snapshot';
  FundPhase['VOTING'] = 'voting';
  FundPhase['TALLYING'] = 'tallying';
  FundPhase['RESULTS'] = 'results';
})((FundPhase = exports.FundPhase || (exports.FundPhase = {})));
class VotingStore extends Store_1.default {
  registrationStep = 1;
  selectedWalletId = null;
  transactionId = null;
  transactionConfirmations = 0;
  isTransactionPending = false;
  isTransactionConfirmed = false;
  votingRegistrationKey = null;
  qrCode = null;
  isConfirmationDialogOpen = false;
  fundPhase;
  catalystFund;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  transactionPollingInterval = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  fundPhaseInterval = null;
  setup() {
    const { voting: votingActions } = this.actions;
    votingActions.selectWallet.listen(this._setSelectedWalletId);
    votingActions.sendTransaction.listen(this._sendTransaction);
    votingActions.generateQrCode.listen(this._generateQrCode);
    votingActions.saveAsPDF.listen(this._saveAsPDF);
    votingActions.nextRegistrationStep.listen(this._nextRegistrationStep);
    votingActions.previousRegistrationStep.listen(
      this._previousRegistrationStep
    );
    votingActions.resetRegistration.listen(this._resetRegistration);
    votingActions.showConfirmationDialog.listen(this._showConfirmationDialog);
    votingActions.closeConfirmationDialog.listen(this._closeConfirmationDialog);
    this._setupFund();
  }
  _setupFund = async () => {
    await this.getCatalystFundRequest.execute().promise;
    this._initializeFundPhaseInterval();
    (0, mobx_1.runInAction)('Initialize fund', () => {
      this.catalystFund = this.getCatalystFundRequest.result;
      this._checkFundPhase(new Date());
    });
  };
  // REQUESTS
  getWalletPublicKeyRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletPublicKey
  );
  createVotingRegistrationTransactionRequest = new LocalizedRequest_1.default(
    this.api.ada.createVotingRegistrationTransaction
  );
  signMetadataRequest = new LocalizedRequest_1.default(
    this.api.ada.createWalletSignature
  );
  getTransactionRequest = new LocalizedRequest_1.default(
    this.api.ada.getTransaction
  );
  getCatalystFundRequest = new LocalizedRequest_1.default(
    this.api.ada.getCatalystFund
  );
  // ACTIONS
  _showConfirmationDialog = () => {
    this.isConfirmationDialogOpen = true;
  };
  _closeConfirmationDialog = () => {
    this.isConfirmationDialogOpen = false;
  };
  _setSelectedWalletId = (walletId) => {
    this.selectedWalletId = walletId;
  };
  _nextRegistrationStep = () => {
    this.registrationStep++;
  };
  _previousRegistrationStep = () => {
    this.registrationStep--;
  };
  _resetRegistration = () => {
    this.isConfirmationDialogOpen = false;
    this.registrationStep = 1;
    this.selectedWalletId = null;
    this.transactionId = null;
    this.transactionConfirmations = 0;
    this.isTransactionPending = false;
    this.isTransactionConfirmed = false;
    this.votingRegistrationKey = null;
    this.qrCode = null;
    this.getWalletPublicKeyRequest.reset();
    this.createVotingRegistrationTransactionRequest.reset();
    this.signMetadataRequest.reset();
    if (this.transactionPollingInterval) {
      clearInterval(this.transactionPollingInterval);
    }
    if (this.fundPhaseInterval) {
      clearInterval(this.fundPhaseInterval);
    }
  };
  _startTransactionPolling = () => {
    if (this.transactionPollingInterval)
      clearInterval(this.transactionPollingInterval);
    this.transactionPollingInterval = setInterval(() => {
      this._checkVotingRegistrationTransaction();
    }, votingConfig_1.VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL);
  };
  _initializeFundPhaseInterval = () => {
    if (this.fundPhaseInterval) {
      clearInterval(this.fundPhaseInterval);
    }
    this.fundPhaseInterval = setInterval(() => {
      this._checkFundPhase(new Date());
    }, votingConfig_1.VOTING_PHASE_CHECK_INTERVAL);
  };
  _setVotingRegistrationKey = (value) => {
    this.votingRegistrationKey = value;
  };
  _setTransactionId = (transactionId) => {
    this.transactionId = transactionId;
  };
  _setTransactionConfirmations = (confirmations) => {
    this.transactionConfirmations = confirmations;
  };
  _setIsTransactionPending = (value) => {
    this.isTransactionPending = value;
  };
  _setIsTransactionConfirmed = (value) => {
    this.isTransactionConfirmed = value;
  };
  _setQrCode = (value) => {
    this.qrCode = value;
  };
  prepareVotingData = async ({ walletId }) => {
    try {
      const [address] = await this.stores.addresses.getAddressesByWalletId(
        walletId
      );
      const addressHex = await this._getHexFromBech32(address.id);
      await this._generateVotingRegistrationKey();
      if (!this.votingRegistrationKey)
        throw new Error('Failed to generate voting registration key.');
      const votingKey = (0, formatters_1.formattedArrayBufferToHexString)(
        this.votingRegistrationKey.public().bytes()
      );
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const stakeKeyBech32 = await this.getWalletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      const stakeKey = await this._getHexFromBech32(stakeKeyBech32);
      const { absoluteSlotNumber } = this.stores.networkStatus;
      const metadata = {
        [61284]: {
          map: [
            {
              k: {
                int: 1,
              },
              v: {
                bytes: votingKey,
              },
            },
            {
              k: {
                int: 2,
              },
              v: {
                bytes: stakeKey,
              },
            },
            {
              k: {
                int: 3,
              },
              v: {
                bytes: addressHex,
              },
            },
            {
              k: {
                int: 4,
              },
              v: {
                int: absoluteSlotNumber,
              },
            },
          ],
        },
        [61285]: {
          map: [
            {
              k: {
                int: 1,
              },
              v: {
                bytes:
                  '00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000',
              },
            },
          ],
        },
      };
      const votingData = {
        address,
        addressHex,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
        metadata,
        nonce: absoluteSlotNumber,
      };
      return votingData;
    } catch (e) {
      throw e;
    }
  };
  _sendTransaction = async ({ amount, passphrase }) => {
    const walletId = this.selectedWalletId;
    if (!walletId)
      throw new Error(
        'Selected wallet required before send voting registration.'
      );
    const [address] = await this.stores.addresses.getAddressesByWalletId(
      walletId
    );
    const selectedWallet = this.stores.wallets.getWalletById(walletId);
    const isHardwareWallet = (0, lodash_1.get)(
      selectedWallet,
      'isHardwareWallet',
      false
    );
    const { absoluteSlotNumber } = this.stores.networkStatus;
    // Reset voting registration transaction state
    this._setIsTransactionPending(true);
    this._setIsTransactionConfirmed(false);
    // Reset voting registration requests
    this.getWalletPublicKeyRequest.reset();
    this.createVotingRegistrationTransactionRequest.reset();
    this.signMetadataRequest.reset();
    let transaction;
    try {
      if (isHardwareWallet) {
        transaction = await this.stores.hardwareWallets._sendMoney({
          isVotingRegistrationTransaction: true,
          selectedWalletId: walletId,
        });
      } else {
        const votingData = await this.prepareVotingData({
          walletId,
        });
        const { addressHex, votingKey, stakeKey, role, index } = votingData;
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        const signature = await this.signMetadataRequest.execute({
          addressHex,
          walletId,
          passphrase,
          votingKey,
          stakeKey,
          role,
          index,
          absoluteSlotNumber,
        });
        transaction =
          // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
          await this.createVotingRegistrationTransactionRequest.execute({
            address: address.id,
            addressHex,
            amount,
            passphrase,
            walletId,
            votingKey,
            stakeKey,
            signature: signature.toString('hex'),
            absoluteSlotNumber,
          });
      }
      this._setTransactionId(transaction.id);
      if (!isHardwareWallet) {
        this._startTransactionPolling();
        this._nextRegistrationStep();
      }
    } catch (error) {
      if (error.code === 'wrong_encryption_passphrase') {
        // In case of a invalid spending password we stay on the same screen
        this._setIsTransactionPending(false);
      } else {
        // For any other error code we proceed to the next screen
        this._nextRegistrationStep();
      }
      throw error;
    }
  };
  _generateQrCode = async (pinCode) => {
    const { symmetric_encrypt: symmetricEncrypt } = await walletUtils_1.default;
    const password = new Uint8Array(4);
    pinCode
      .toString()
      .split('')
      .forEach((value, index) => {
        password[index] = parseInt(value, 10);
      });
    if (!this.votingRegistrationKey)
      throw new Error(
        'Failed to generate QR code due to missing voting registration key.'
      );
    const encrypt = symmetricEncrypt(
      password,
      this.votingRegistrationKey.bytes()
    );
    this._setQrCode((0, formatters_1.formattedArrayBufferToHexString)(encrypt));
    this._nextRegistrationStep();
    this.analytics.sendEvent(
      analytics_1.EventCategories.VOTING,
      'Registered for voting'
    );
  };
  _saveAsPDF = async () => {
    const { qrCode, selectedWalletId } = this;
    if (!qrCode || !selectedWalletId) return;
    const selectedWallet = this.stores.wallets.getWalletById(selectedWalletId);
    if (!selectedWallet) return;
    const { name: walletName } = selectedWallet;
    const { desktopDirectoryPath } = this.stores.profile;
    const {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    } = this.stores.profile;
    const { network, isMainnet } = this.environment;
    const intl = (0, i18nContext_1.i18nContext)(currentLocale);
    const result = await (0, votingPDFGenerator_1.votingPDFGenerator)({
      qrCode,
      walletName,
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
      desktopDirectoryPath,
      network,
      isMainnet,
      intl,
    });
    if (result === votingPDFGenerator_1.VotingPDFGeneratorResult.FileSaved) {
      this.actions.voting.saveAsPDFSuccess.trigger();
    }
  };
  _checkVotingRegistrationTransaction = async () => {
    const { confirmations, state } = await this.getTransactionRequest.execute({
      walletId: this.selectedWalletId,
      transactionId: this.transactionId,
    });
    // Update voting registration confirmations count
    if (this.transactionConfirmations !== confirmations) {
      this._setTransactionConfirmations(confirmations);
    }
    // Update voting registration pending state
    if (
      this.isTransactionPending &&
      state === WalletTransaction_1.TransactionStates.OK
    ) {
      this._setIsTransactionPending(false);
    }
    // Update voting registration confirmed state
    if (
      !this.isTransactionConfirmed &&
      confirmations >=
        votingConfig_1.VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS
    ) {
      this._setIsTransactionConfirmed(true);
      if (this.transactionPollingInterval)
        clearInterval(this.transactionPollingInterval);
    }
  };
  _checkFundPhase = (now) => {
    const phaseValidation = {
      [FundPhase.SNAPSHOT]: (date) =>
        date < this.catalystFund?.current?.startTime,
      [FundPhase.VOTING]: (date) =>
        date >= this.catalystFund?.current?.startTime &&
        date < this.catalystFund?.current?.endTime,
      [FundPhase.TALLYING]: (date) =>
        date >= this.catalystFund?.current?.endTime &&
        date < this.catalystFund?.current?.resultsTime,
      [FundPhase.RESULTS]: (date) =>
        date >= this.catalystFund?.current?.resultsTime,
    };
    this.fundPhase =
      Object.values(FundPhase).find((phase) => phaseValidation[phase](now)) ||
      null;
  };
  _generateVotingRegistrationKey = async () => {
    const {
      Ed25519ExtendedPrivate: extendedPrivateKey,
    } = await walletUtils_1.default;
    this._setVotingRegistrationKey(extendedPrivateKey.generate());
  };
  _getHexFromBech32 = async (key) => {
    const {
      bech32_decode_to_bytes: decodeBech32ToBytes,
    } = await walletUtils_1.default;
    return (0, formatters_1.formattedArrayBufferToHexString)(
      decodeBech32ToBytes(key)
    );
  };
  // GETTERS
  get currentRoute() {
    return this.stores.router.location.pathname;
  }
  get isVotingPage() {
    return (
      this.currentRoute.indexOf(routes_config_1.ROUTES.VOTING.REGISTRATION) > -1
    );
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'registrationStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  VotingStore.prototype,
  'selectedWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  VotingStore.prototype,
  'transactionId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'transactionConfirmations',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'isTransactionPending',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'isTransactionConfirmed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'votingRegistrationKey',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  VotingStore.prototype,
  'qrCode',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'isConfirmationDialogOpen',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  VotingStore.prototype,
  'fundPhase',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  VotingStore.prototype,
  'catalystFund',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setupFund',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  VotingStore.prototype,
  'getWalletPublicKeyRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  VotingStore.prototype,
  'createVotingRegistrationTransactionRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  VotingStore.prototype,
  'signMetadataRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  VotingStore.prototype,
  'getTransactionRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  VotingStore.prototype,
  'getCatalystFundRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_showConfirmationDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_closeConfirmationDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setSelectedWalletId',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_nextRegistrationStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_previousRegistrationStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_resetRegistration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_startTransactionPolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_initializeFundPhaseInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setVotingRegistrationKey',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setTransactionId',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setTransactionConfirmations',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setIsTransactionPending',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setIsTransactionConfirmed',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_setQrCode',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  VotingStore.prototype,
  '_checkFundPhase',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  VotingStore.prototype,
  'currentRoute',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  VotingStore.prototype,
  'isVotingPage',
  null
);
exports.default = VotingStore;
//# sourceMappingURL=VotingStore.js.map
