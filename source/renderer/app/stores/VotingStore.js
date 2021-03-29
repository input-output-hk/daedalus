// @flow
import { action, computed, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import {
  TransactionStates,
  WalletTransaction,
} from '../domains/WalletTransaction';
import { formattedArrayBufferToHexString } from '../utils/formatters';
import walletUtils from '../utils/walletUtils';
import {
  VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL,
  VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
  VOTING_FUND_NUMBER,
  VOTING_REGISTRATION_END_DATE,
  VOTING_REGISTRATION_END_CHECK_INTERVAL,
} from '../config/votingConfig';
import { votingPDFGenerator } from '../utils/votingPDFGenerator';
import { i18nContext } from '../utils/i18nContext';

export type VotingRegistrationKeyType = { bytes: Function, public: Function };

export default class VotingStore extends Store {
  @observable registrationStep: number = 1;
  @observable selectedWalletId: ?string = null;
  @observable transactionId: ?string = null;
  @observable transactionConfirmations: number = 0;
  @observable isTransactionPending: boolean = false;
  @observable isTransactionConfirmed: boolean = false;
  @observable votingRegistrationKey: ?VotingRegistrationKeyType = null;
  @observable qrCode: ?string = null;
  @observable isConfirmationDialogOpen: boolean = false;
  @observable isRegistrationEnded: boolean = false;

  transactionPollingInterval: ?IntervalID = null;
  registrationEndCheckInterval: ?IntervalID = null;

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
    this._initializeRegistrationEndCheckInterval();
  }

  // REQUESTS

  @observable getWalletPublicKeyRequest: Request<string> = new Request(
    this.api.ada.getWalletPublicKey
  );

  @observable
  createVotingRegistrationTransactionRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createVotingRegistrationTransaction
  );

  @observable
  signMetadataRequest: Request<Buffer> = new Request(
    this.api.ada.createWalletSignature
  );

  // ACTIONS

  @action _showConfirmationDialog = () => {
    this.isConfirmationDialogOpen = true;
  };

  @action _closeConfirmationDialog = () => {
    this.isConfirmationDialogOpen = false;
  };

  @action _setSelectedWalletId = (walletId: string) => {
    this.selectedWalletId = walletId;
  };

  @action _nextRegistrationStep = () => {
    this.registrationStep++;
  };

  @action _previousRegistrationStep = () => {
    this.registrationStep--;
  };

  @action _resetRegistration = () => {
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
    if (this.transactionPollingInterval)
      clearInterval(this.transactionPollingInterval);
    if (this.registrationEndCheckInterval)
      clearInterval(this.registrationEndCheckInterval);
  };

  @action _startTransactionPolling = () => {
    if (this.transactionPollingInterval)
      clearInterval(this.transactionPollingInterval);
    this.transactionPollingInterval = setInterval(() => {
      this._checkVotingRegistrationTransaction();
    }, VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL);
  };

  @action _initializeRegistrationEndCheckInterval = () => {
    if (this.registrationEndCheckInterval)
      clearInterval(this.registrationEndCheckInterval);
    this.registrationEndCheckInterval = setInterval(() => {
      this._checkVotingRegistrationEnd();
    }, VOTING_REGISTRATION_END_CHECK_INTERVAL);
  };

  @action _setVotingRegistrationKey = (value: VotingRegistrationKeyType) => {
    this.votingRegistrationKey = value;
  };

  @action _setTransactionId = (transactionId: string) => {
    this.transactionId = transactionId;
  };

  @action _setTransactionConfirmations = (confirmations: number) => {
    this.transactionConfirmations = confirmations;
  };

  @action _setIsTransactionPending = (value: boolean) => {
    this.isTransactionPending = value;
  };

  @action _setIsTransactionConfirmed = (value: boolean) => {
    this.isTransactionConfirmed = value;
  };

  @action _setQrCode = (value: ?string) => {
    this.qrCode = value;
  };

  _sendTransaction = async ({
    amount,
    passphrase,
  }: {
    amount: number,
    passphrase: string,
  }) => {
    const walletId = this.selectedWalletId;
    if (!walletId)
      throw new Error(
        'Selected wallet required before send voting registration.'
      );
    const [address] = await this.stores.addresses.getAddressesByWalletId(
      walletId
    );

    const { absoluteSlotNumber } = this.stores.networkStatus;

    // Reset voting registration transaction state
    this._setIsTransactionPending(true);
    this._setIsTransactionConfirmed(false);

    // Reset voting registration requests
    this.getWalletPublicKeyRequest.reset();
    this.createVotingRegistrationTransactionRequest.reset();
    this.signMetadataRequest.reset();

    try {
      const addressHex = await this._getHexFromBech32(address.id);

      await this._generateVotingRegistrationKey();
      if (!this.votingRegistrationKey)
        throw new Error('Failed to generate voting registration key.');
      const votingKey = formattedArrayBufferToHexString(
        this.votingRegistrationKey.public().bytes()
      );

      const stakeKeyBech32 = await this.getWalletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      const stakeKey = await this._getHexFromBech32(stakeKeyBech32);

      const signature = await this.signMetadataRequest.execute({
        addressHex,
        walletId,
        passphrase,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
        absoluteSlotNumber,
      });

      const transaction = await this.createVotingRegistrationTransactionRequest.execute(
        {
          address: address.id,
          addressHex,
          amount,
          passphrase,
          walletId,
          votingKey,
          stakeKey,
          signature: signature.toString('hex'),
          absoluteSlotNumber,
        }
      );

      this._setTransactionId(transaction.id);
      this._startTransactionPolling();
      this._nextRegistrationStep();
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

  _generateQrCode = async (pinCode: number) => {
    const { symmetric_encrypt: symmetricEncrypt } = await walletUtils;
    const password = new Uint8Array(4);
    pinCode
      .toString()
      .split('')
      .forEach((value: string, index: number) => {
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
    this._setQrCode(formattedArrayBufferToHexString(encrypt));
    this._nextRegistrationStep();
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
    const fundNumber = VOTING_FUND_NUMBER;
    const { network, isMainnet } = this.environment;
    const intl = i18nContext(currentLocale);

    try {
      await votingPDFGenerator({
        fundNumber,
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
      this.actions.voting.saveAsPDFSuccess.trigger();
    } catch (error) {
      throw new Error(error);
    }
  };

  _checkVotingRegistrationTransaction = async () => {
    const {
      confirmations,
      state,
    }: WalletTransaction = await this.stores.transactions
      ._getTransactionRequest()
      .execute({
        walletId: this.selectedWalletId,
        transactionId: this.transactionId,
      });

    // Update voting registration confirmations count
    if (this.transactionConfirmations !== confirmations) {
      this._setTransactionConfirmations(confirmations);
    }

    // Update voting registration pending state
    if (this.isTransactionPending && state === TransactionStates.OK) {
      this._setIsTransactionPending(false);
    }

    // Update voting registration confirmed state
    if (
      !this.isTransactionConfirmed &&
      confirmations >= VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS
    ) {
      this._setIsTransactionConfirmed(true);
      if (this.transactionPollingInterval)
        clearInterval(this.transactionPollingInterval);
    }
  };

  @action _checkVotingRegistrationEnd = () => {
    this.isRegistrationEnded = new Date() >= VOTING_REGISTRATION_END_DATE;
  };

  _generateVotingRegistrationKey = async () => {
    const { Ed25519ExtendedPrivate: extendedPrivateKey } = await walletUtils;
    this._setVotingRegistrationKey(extendedPrivateKey.generate());
  };

  _getHexFromBech32 = async (key: string): Promise<string> => {
    const { bech32_decode_to_bytes: decodeBech32ToBytes } = await walletUtils;
    return formattedArrayBufferToHexString(decodeBech32ToBytes(key));
  };

  // GETTERS

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isVotingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.VOTING.REGISTRATION) > -1;
  }
}
