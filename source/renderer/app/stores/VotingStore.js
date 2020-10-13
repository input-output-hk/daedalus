// @flow
import { action, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';

import { WalletTransaction } from '../domains/WalletTransaction';

export default class VotingStore extends Store {
  @observable selectedVotingWalletId = null;

  setup() {
    const { voting: votingActions } = this.actions;

    votingActions.selectVotingWallet.listen(this._setSelectedVotingWalletId);

    votingActions.sendTransaction.listen(this._sendTransaction);

    // ========== MOBX REACTIONS =========== //
    this.registerReactions([this._pollOnSync]);
  }

  // REQUESTS
  @observable
  votingSendTransactionRequest: Request<WalletTransaction> = new Request(
    this.api.ada.createTransaction
  );

  @action _setSelectedVotingWalletId = (walletId: string) =>
    (this.selectedVotingWalletId = walletId);

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
    await this.votingSendTransactionRequest.execute({
      address: address.id,
      amount: amount,
      passphrase,
      walletId: walletId,
    });
    await this.stores.wallets.refreshWalletsData();
    this.votingSendTransactionRequest.reset();
  };
}
