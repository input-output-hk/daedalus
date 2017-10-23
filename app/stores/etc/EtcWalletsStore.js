// @flow
import { observable } from 'mobx';
import WalletStore from '../WalletStore';
import Request from '.././lib/LocalizedRequest';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import type {
  CreateWalletResponse, GetWalletsResponse,
  GetWalletRecoveryPhraseResponse,
} from '../../api/etc/index';
import type { SendEtcTransactionResponse } from '../../api/etc/sendEtcTransaction';

export default class EtcWalletsStore extends WalletStore {

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<GetWalletsResponse> = new Request(this.api.etc.getWallets);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.etc.createWallet);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.etc.getWalletRecoveryPhrase);
  @observable sendMoneyRequest: Request<SendEtcTransactionResponse> = new Request(this.api.etc.createTransaction);
  /* eslint-disable max-len */

  setup() {
    const { walletBackup, etc } = this.actions;
    const { wallets } = etc;
    wallets.createWallet.listen(this._create);
    wallets.sendMoney.listen(this._sendMoney);
    walletBackup.finishWalletBackup.listen(this._finishWalletCreation);
    this.registerReactions([
      this._updateActiveWalletOnRouteChanges,
      this._toggleAddWalletDialogOnWalletsLoaded,
    ]);
    setInterval(this._pollRefresh, this.WALLET_REFRESH_INTERVAL);
  }

  _create = async (params: {
    name: string,
    password: ?string,
  }) => {
    Object.assign(this._newWalletDetails, params);
    try {
      const recoveryPhrase: ?GetWalletRecoveryPhraseResponse = await (
        this.getWalletRecoveryPhraseRequest.execute().promise
      );
      if (recoveryPhrase != null) {
        this.actions.walletBackup.initiateWalletBackup.trigger({ recoveryPhrase });
      }
    } catch (error) {
      throw error;
    }
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.walletBackup.recoveryPhrase.join(' ');
    const wallet = await this.createWalletRequest.execute(this._newWalletDetails).promise;
    if (wallet) {
      await this.walletsRequest.patch(result => { result.push(wallet); });
      this.actions.dialogs.closeActiveDialog.trigger();
      this.goToWalletRoute(wallet.id);
    } else {
      this.actions.dialogs.open.trigger({
        dialog: WalletAddDialog,
      });
    }
  };

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
  }

}
