// @flow
import { action, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  TransactionStates,
  WalletTransaction,
} from '../domains/WalletTransaction';
import { formattedArrayBufferToHexString } from '../utils/formatters';
import walletUtils from '../utils/walletUtils';
import {
  VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL,
  VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
} from '../config/votingConfig';

export type VotingRegistrationKeyType = { bytes: Function, public: Function };

export default class VotingStore extends Store {
  @observable selectedWalletId: ?string = null;
  @observable transactionId: ?string = null;
  @observable transactionConfirmations: number = 0;
  @observable isTransactionPending: boolean = false;
  @observable isTransactionConfirmed: boolean = false;
  @observable votingRegistrationKey: ?VotingRegistrationKeyType = null;
  @observable qrCode: ?string = null;

  transactionPollingInterval: ?IntervalID = null;

  setup() {
    const { voting: votingActions } = this.actions;
    votingActions.selectWallet.listen(this._setSelectedWalletId);
    votingActions.sendTransaction.listen(this._sendTransaction);
    votingActions.generateQrCode.listen(this._generateQrCode);
    votingActions.resetRegistration.listen(this._resetRegistration);
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
  @action _setSelectedWalletId = (walletId: string) => {
    this.selectedWalletId = walletId;
  };

  @action _resetRegistration = () => {
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
  };

  @action _startTransactionPolling = () => {
    if (this.transactionPollingInterval)
      clearInterval(this.transactionPollingInterval);
    this.transactionPollingInterval = setInterval(() => {
      this._checkVotingRegistrationTransaction();
    }, VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL);
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

      let stakeKey = await this.getWalletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      stakeKey = await this._getHexFromBech32(stakeKey);

      const signature = await this.signMetadataRequest.execute({
        addressHex,
        walletId,
        passphrase,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
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
        }
      );

      this._setTransactionId(transaction.id);
      this._startTransactionPolling();
    } catch (error) {
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
    this._setTransactionConfirmations(confirmations);

    // Update voting registration pending state
    if (state === TransactionStates.OK) {
      this._setIsTransactionPending(false);
    }

    // Update voting registration confirmed state
    if (confirmations >= VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS) {
      this._setIsTransactionConfirmed(true);
    }
  };

  _generateVotingRegistrationKey = async () => {
    const { Ed25519ExtendedPrivate: extendedPrivateKey } = await walletUtils;
    this._setVotingRegistrationKey(extendedPrivateKey.generate());
  };

  _getHexFromBech32 = async (key: string): Promise<string> => {
    const { bech32_decode_to_bytes: decodeBech32ToBytes } = await walletUtils;
    return formattedArrayBufferToHexString(decodeBech32ToBytes(key));
  };
}
