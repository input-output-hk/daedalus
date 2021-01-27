// @flow
import { action, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  TransactionStates,
  WalletTransaction,
} from '../domains/WalletTransaction';
import { formattedArrayBufferToHexString } from '../utils/formatters';
import wallet from '../utils/wallet';
import { VOTING_COUNTDOWN_INTERVAL } from '../config/votingConfig';

export default class VotingStore extends Store {
  @observable selectedVotingWalletId: ?string = null;
  @observable transactionId: ?string = null;
  @observable isVotingRegistrationTransactionPending: boolean = false;
  @observable isVotingRegistrationTransactionApproved: boolean = false;
  @observable votingRegistrationKey: any = null;
  @observable qrCode: string | null = null;
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
    this.countdownRemaining = VOTING_COUNTDOWN_INTERVAL;
    if (this.countdownTimerInterval) clearInterval(this.countdownTimerInterval);
    this.countdownTimerInterval = setInterval(() => {
      if (this.countdownRemaining > 0) {
        action(() => this.countdownRemaining--)();
      } else if (this.countdownTimerInterval != null) {
        clearInterval(this.countdownTimerInterval);
        this.checkVotingRegistrationTransaction();
      }
    }, 1000);
  };

  @action setIsVotingRegistrationTransactionPending = (value: boolean) => {
    this.isVotingRegistrationTransactionPending = value;
  };

  @action setIsVotingRegistrationTransactionApproved = (value: boolean) => {
    this.isVotingRegistrationTransactionApproved = value;
  };

  @action setVotingRegistrationKey = (value: any) => {
    this.votingRegistrationKey = value;
  };

  @action setQrCode = (value: string | null) => {
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
    amount: number,
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

      const votingKey = formattedArrayBufferToHexString(
        this.votingRegistrationKey.public().bytes()
      );

      let stakeKey = await this.walletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      stakeKey = await this.getHexFromBech32(stakeKey);

      const signature = await this.signMetadataRequest.execute({
        walletId,
        passphrase,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
      });

      const transaction = await this.votingSendTransactionRequest.execute({
        address: address.id,
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
    const Modules = await wallet;
    const PASSWORD = new Uint8Array(4);
    pinCode
      .toString()
      .split('')
      .forEach((value: string, index: number) => {
        PASSWORD[index] = parseInt(value, 10);
      });
    const encrypt = Modules.symmetric_encrypt(
      PASSWORD,
      this.votingRegistrationKey.bytes()
    );
    this.setQrCode(formattedArrayBufferToHexString(encrypt));
  };

  // Check voting registration transaction state and reset pending state when transaction is "in_ledger"
  checkVotingRegistrationTransaction = async () => {
    const transaction = await this.stores.transactions
      ._getTransaction()
      .execute({
        walletId: this.selectedVotingWalletId,
        transactionId: this.transactionId,
      });

    // Return voting transaction when state is not "PENDING"
    if (transaction.state === TransactionStates.OK) {
      this.setIsVotingRegistrationTransactionApproved(true);
    }
    this.setIsVotingRegistrationTransactionPending(false);
  };

  generateVotingRegistrationKey = async () => {
    const Modules = await wallet;
    const key = Modules.Ed25519ExtendedPrivate;
    this.setVotingRegistrationKey(key.generate());
  };

  getHexFromBech32 = async (key: string) => {
    const Modules = await wallet;
    return formattedArrayBufferToHexString(Modules.bech32_decode_to_bytes(key));
  };
}
