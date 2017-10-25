// @flow
import { observable } from 'mobx';
import WalletStore from '../WalletStore';
import Request from '.././lib/LocalizedRequest';
import type {
  CreateWalletResponse, GetWalletsResponse,
  DeleteWalletResponse, RestoreWalletResponse
} from '../../api/etc/index';
import type { SendEtcTransactionResponse } from '../../api/etc/sendEtcTransaction';
import type { GetWalletRecoveryPhraseResponse } from '../../api/common';

export default class EtcWalletsStore extends WalletStore {

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<GetWalletsResponse> = new Request(this.api.etc.getWallets);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.etc.createWallet);
  @observable deleteWalletRequest: Request<DeleteWalletResponse> = new Request(this.api.etc.deleteWallet);
  @observable sendMoneyRequest: Request<SendEtcTransactionResponse> = new Request(this.api.etc.createTransaction);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.etc.getWalletRecoveryPhrase);
  @observable restoreRequest: Request<RestoreWalletResponse> = new Request(this.api.etc.restoreWallet);
  /* eslint-disable max-len */

  setup() {
    const { walletBackup, etc } = this.actions;
    const { wallets } = etc;
    wallets.createWallet.listen(this._create);
    wallets.deleteWallet.listen(this._delete);
    wallets.sendMoney.listen(this._sendMoney);
    wallets.restoreWallet.listen(this._restore);
    walletBackup.finishWalletBackup.listen(this._finishCreation);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._toggleAddWalletDialogOnWalletsLoaded,
    ]);
    setInterval(this._pollRefresh, this.WALLET_REFRESH_INTERVAL);
  }

  _sendMoney = async (transactionDetails: {
    receiver: string,
    amount: string,
    password: ?string,
  }) => {
    const wallet = this.active;
    if (!wallet) throw new Error('Active wallet required before sending.');
    const { receiver, amount, password } = transactionDetails;
    const transaction = await this.sendMoneyRequest.execute([
      { from: wallet.id, to: receiver, amount },
      password !== null ? password : '',
    ]);
    // TODO: fetch transactions, refresh wallet balances etc.
    console.log(transaction);
  };

  isValidMnemonic = (mnemonic: string) => this.api.etc.isValidMnemonic(mnemonic);

}
