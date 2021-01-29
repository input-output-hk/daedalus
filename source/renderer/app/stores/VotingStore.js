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
  VOTING_REGISTRATION_CONFIRMATION_DURATION,
  VOTING_REGISTRATION_CONFIRMATION_CHECK_INTERVAL,
} from '../config/votingConfig';

export type VotingRegistrationKeyType = { bytes: Function, public: Function };

export default class VotingStore extends Store {
  @observable selectedVotingWalletId: ?string = null;
  @observable transactionId: ?string = null;
  @observable isVotingRegistrationTransactionPending: boolean = false;
  @observable isVotingRegistrationTransactionApproved: boolean = false;
  @observable votingRegistrationKey: ?VotingRegistrationKeyType = null;
  @observable qrCode: ?string = null;
  @observable countdownRemaining = 0;

  countdownTimerInterval: ?IntervalID = null;

  setup() {
    const { voting: votingActions } = this.actions;
    votingActions.selectVotingWallet.listen(this._setSelectedVotingWalletId);
    votingActions.generateQrCode.listen(this._generateQrCode);
    votingActions.sendTransaction.listen(this._sendTransaction);
    votingActions.resetVotingRegistration.listen(this._resetVotingRegistration);
    votingActions.initializeCountdownInterval.listen(this._initializeCountdown);
  }

  // REQUESTS
  @observable walletPublicKeyRequest: Request<string> = new Request(
    this.api.ada.getWalletPublicKey
  );

  @observable
  votingSendTransactionRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createVotingRegistrationTransaction
  );

  @observable
  signMetadataRequest: Request<Buffer> = new Request(
    this.api.ada.createWalletSignature
  );

  // ACTIONS
  @action _setSelectedVotingWalletId = (walletId: string) => {
    this.selectedVotingWalletId = walletId;
  };

  @action _resetVotingRegistration = () => {
    this.selectedVotingWalletId = null;
    this.transactionId = null;
    this.isVotingRegistrationTransactionPending = false;
    this.isVotingRegistrationTransactionApproved = false;
    this.votingRegistrationKey = null;
    this.qrCode = null;
  };

  @action _initializeCountdown = async () => {
    this.countdownRemaining = VOTING_REGISTRATION_CONFIRMATION_DURATION;
    if (this.countdownTimerInterval) clearInterval(this.countdownTimerInterval);
    this.countdownTimerInterval = setInterval(() => {
      if (this.countdownRemaining > 0) {
        action(() => this.countdownRemaining--)();
      } else if (this.countdownTimerInterval != null) {
        clearInterval(this.countdownTimerInterval);
        this.checkVotingRegistrationTransaction();
      }
    }, VOTING_REGISTRATION_CONFIRMATION_CHECK_INTERVAL);
  };

  @action setIsVotingRegistrationTransactionPending = (value: boolean) => {
    this.isVotingRegistrationTransactionPending = value;
  };

  @action setIsVotingRegistrationTransactionApproved = (value: boolean) => {
    this.isVotingRegistrationTransactionApproved = value;
  };

  @action setVotingRegistrationKey = (value: VotingRegistrationKeyType) => {
    this.votingRegistrationKey = value;
  };

  @action setQrCode = (value: ?string) => {
    this.qrCode = value;
  };

  @action setTransactionId = (transactionId: string) => {
    this.transactionId = transactionId;
  };

  /* ====  Private methods  ===== */

  _sendTransaction = async ({
    amount,
    passphrase,
  }: {
    amount: number,
    passphrase: string,
  }) => {
    const walletId = this.selectedVotingWalletId;
    if (!walletId)
      throw new Error(
        'Selected wallet required before send voting registration.'
      );
    const [address] = await this.stores.addresses.getAddressesByWalletId(
      walletId
    );

    // Set join transaction in "PENDING" state
    this.setIsVotingRegistrationTransactionPending(true);
    this.setIsVotingRegistrationTransactionApproved(false);

    try {
      await this.generateVotingRegistrationKey();

      if (!this.votingRegistrationKey)
        throw new Error('Failed to generate voting registration key.');
      const votingKey = formattedArrayBufferToHexString(
        this.votingRegistrationKey.public().bytes()
      );

      let stakeKey = await this.walletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      stakeKey = await this.getHexFromBech32(stakeKey);
      let addressHex = await this.getHexFromBech32(address.id);

      const signature = await this.signMetadataRequest.execute({
        addressHex: addressHex,
        walletId,
        passphrase,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
      });

      const transaction = await this.votingSendTransactionRequest.execute({
        address: address.id,
        addressHex: addressHex,
        amount,
        passphrase,
        walletId,
        votingKey,
        stakeKey,
        signature: signature.toString('hex'),
      });

      this.setTransactionId(transaction.id);
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
    this.setQrCode(formattedArrayBufferToHexString(encrypt));
  };

  // Check voting registration transaction state and reset pending state when transaction is "in_ledger"
  checkVotingRegistrationTransaction = async () => {
    const transaction = await this.stores.transactions
      ._getTransactionRequest()
      .execute({
        walletId: this.selectedVotingWalletId,
        transactionId: this.transactionId,
      });

    // Return voting transaction when state is not "PENDING"
    if (transaction.state === TransactionStates.OK) {
      this.setIsVotingRegistrationTransactionApproved(true);
      this.setIsVotingRegistrationTransactionPending(false);
    }
  };

  generateVotingRegistrationKey = async () => {
    const { Ed25519ExtendedPrivate: extendedPrivateKey } = await walletUtils;
    this.setVotingRegistrationKey(extendedPrivateKey.generate());
  };

  getHexFromBech32 = async (key: string): Promise<string> => {
    const { bech32_decode_to_bytes: decodeBech32ToBytes } = await walletUtils;
    return formattedArrayBufferToHexString(decodeBech32ToBytes(key));
  };
}
