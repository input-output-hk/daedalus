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
import { find } from 'lodash';
import {
  VOTING_REGISTRATION_TRANSACTION_CHECK_INTERVAL,
  VOTING_REGISTRATION_TRANSACTION_CHECKER_TIMEOUT,
} from '../config/votingConfig';

export default class VotingStore extends Store {
  @observable selectedVotingWalletId: ?string = null;
  @observable isVotingRegistrationTransactionPending: boolean = false;
  @observable votingRegistrationKey: any = null;
  @observable qrCode: ?string = null;

  pinCode: ?number = null;
  votingCheckTimeInterval: ?IntervalID = null;

  setup() {
    const { voting: votingActions } = this.actions;

    votingActions.selectVotingWallet.listen(this._setSelectedVotingWalletId);
    votingActions.setPinCode.listen(this._setPinCode);
    votingActions.sendTransaction.listen(this._sendTransaction);
    votingActions.resetVotingRegistration.listen(this._resetVotingRegistration);
  }

  // REQUESTS

  @observable
  votingSendTransactionRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createVotingRegistrationTransaction
  );

  // ACTIONS

  @action _setSelectedVotingWalletId = (walletId: string) =>
    (this.selectedVotingWalletId = walletId);

  @action _setPinCode = (pinCode: number) => (this.pinCode = pinCode);

  @action setVotingCheckTimeInterval = (value: any) => {
    this.votingCheckTimeInterval = value;
  };

  @action setIsVotingRegistrationTransactionPending = (value: boolean) => {
    this.isVotingRegistrationTransactionPending = value;
  };

  @action setVotingRegistrationKey = (value: any) => {
    this.votingRegistrationKey = value;
  };

  @action setQrCode = (value: ?string) => {
    this.qrCode = value;
  };

  @action _resetVotingRegistration = () => {
    this.selectedVotingWalletId = null;
    this.isVotingRegistrationTransactionPending = false;
    this.votingRegistrationKey = null;
    this.qrCode = null;
    this.pinCode = null;
  };

  /* ====  Private methods  ===== */

  _sendTransaction = async ({
    amount,
    passphrase,
  }: {
    amount: string,
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

      const key = this.votingRegistrationKey;

      const transaction = await this.votingSendTransactionRequest.execute({
        address: address.id,
        amount: amount,
        passphrase,
        walletId: walletId,
        votingKey: formattedArrayBufferToHexString(key.public().bytes()),
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
      this.generateQrCode();
    }
  };

  generateVotingRegistrationKey = async () => {
    const Modules = await wallet;
    const key = Modules.Ed25519ExtendedPrivate;
    this.setVotingRegistrationKey(key.generate());
  };

  generateQrCode = async () => {
    const Modules = await wallet;
    const pinCode = this.pinCode;
    const key = this.votingRegistrationKey;
    const PASSWORD = new Uint8Array(4);
    pinCode
      .toString()
      .split('')
      .forEach((value, index) => {
        PASSWORD[index] = value;
      });
    const encrypt = Modules.symmetric_encrypt(PASSWORD, key.bytes());
    this.setQrCode(formattedArrayBufferToHexString(encrypt));
  };

  // Reset voting registration interval and refresh wallet data
  resetVotingRegistrationTransactionChecker = () => {
    if (this.votingCheckTimeInterval) {
      clearInterval(this.votingCheckTimeInterval);
      this.setIsVotingRegistrationTransactionPending(null);
    }
    this.stores.wallets.refreshWalletsData();
    this.setIsVotingRegistrationTransactionPending(false);
  };
}
