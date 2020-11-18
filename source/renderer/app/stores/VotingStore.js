// @flow
import { action, observable } from 'mobx';
import { find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  TransactionStates,
  WalletTransaction,
} from '../domains/WalletTransaction';
import { formattedArrayBufferToHexString } from '../utils/formatters';
import wallet from '../utils/wallet';
import {
  VOTING_REGISTRATION_TRANSACTION_CHECK_INTERVAL,
  VOTING_REGISTRATION_TRANSACTION_CHECKER_TIMEOUT,
  VOTING_COUNTDOWN_INTERVAL,
} from '../config/votingConfig';

// Voting Types
import type {
  GetWalletKeyRequest,
  CreateWalletSignatureRequest,
} from '../api/voting/types';
import type { GetTransactionRequest } from '../api/transactions/types';

export default class VotingStore extends Store {
  @observable selectedVotingWalletId: ?string = null;
  @observable isVotingRegistrationTransactionPending: boolean = false;
  @observable votingRegistrationKey: any = null;
  @observable qrCode: string | null = null;
  @observable countdownRemaining = 0;

  votingCheckTimeInterval: IntervalID | null = null;
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

  @observable
  votingSendTransactionRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createVotingRegistrationTransaction
  );

  // ACTIONS

  @action _setSelectedVotingWalletId = (walletId: string) => {
    this.selectedVotingWalletId = walletId;
  };

  @action _resetVotingRegistration = () => {
    this.selectedVotingWalletId = null;
    this.isVotingRegistrationTransactionPending = false;
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
      }
    }, 1000);
  };

  @action setVotingCheckTimeInterval = (value: any) => {
    this.votingCheckTimeInterval = value;
  };

  @action setIsVotingRegistrationTransactionPending = (value: boolean) => {
    this.isVotingRegistrationTransactionPending = value;
  };

  @action setVotingRegistrationKey = (value: any) => {
    this.votingRegistrationKey = value;
  };

  @action setQrCode = (value: string | null) => {
    this.qrCode = value;
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

    try {
      await this.generateVotingRegistrationKey();

      const votingKey = formattedArrayBufferToHexString(
        this.votingRegistrationKey.public().bytes()
      );

      let stakeKey = await this._getWalletKeyRequest({
        walletId,
        role: 'mutable_account',
        index: '0',
      });

      stakeKey = await this.getHexFromBech32(stakeKey);

      const signature = await this._createWalletSignatureRequest({
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

      // Start interval to check transaction state every second
      this.setVotingCheckTimeInterval(
        setInterval(
          this.checkVotingRegistrationTransaction,
          VOTING_REGISTRATION_TRANSACTION_CHECK_INTERVAL,
          { transactionId: transaction.id, walletId }
        )
      );

      // Reset transaction state check interval after 30 seconds
      setTimeout(() => {
        this.resetVotingRegistrationTransactionChecker();
      }, VOTING_REGISTRATION_TRANSACTION_CHECKER_TIMEOUT);
    } catch (error) {
      this.resetVotingRegistrationTransactionChecker();
      throw error;
    }
  };

  // Get wallet stake key
  _getWalletKeyRequest = async (
    request: GetWalletKeyRequest
  ): Promise<string> => {
    const response = await this.api.ada.getWalletKey(request);
    if (!response)
      throw new Error('Could not get the public key of the wallet.');
    return response;
  };

  // Create wallet signature
  _createWalletSignatureRequest = async (
    request: CreateWalletSignatureRequest
  ): Promise<Buffer> => {
    const response = await this.api.ada.createWalletSignature(request);
    if (!response) throw new Error('Could not generate a wallet signature.');
    return response;
  };

  _getTransaction = async (
    request: GetTransactionRequest
  ): Request<WalletTransaction> => {
    const transaction = await this.api.ada.getTransaction(request);
    return transaction || {};
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
  checkVotingRegistrationTransaction = (request: {
    transactionId: string,
    walletId: string,
  }) => {
    const { transactionId, walletId } = request;
    const recentTransactionsResponse = this.stores.transactions._getTransactionsRecentRequest(
      walletId
    ).result;
    const recentTransactions = recentTransactionsResponse
      ? recentTransactionsResponse.transactions
      : [];

    // Return stake pool transaction when state is not "PENDING"
    const votingRegistrationTransaction = find(
      recentTransactions,
      (transaction) =>
        transaction.id === transactionId &&
        transaction.state === TransactionStates.OK
    );

    if (votingRegistrationTransaction) {
      this.resetVotingRegistrationTransactionChecker();
    }
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

  // Reset voting registration interval and refresh wallet data
  resetVotingRegistrationTransactionChecker = () => {
    if (this.votingCheckTimeInterval) {
      clearInterval(this.votingCheckTimeInterval);
      this.setVotingCheckTimeInterval(null);
    }
    this.setIsVotingRegistrationTransactionPending(false);
  };
}
