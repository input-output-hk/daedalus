// @flow
import { observable } from 'mobx';
import WalletStore from '../WalletStore';
import Request from '.././lib/LocalizedRequest';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import type { CreateWalletResponse, GetWalletsResponse } from '../../api/etc/index';
import type { GetWalletRecoveryPhraseResponse } from '../../api/ada/index';

export default class EtcWalletsStore extends WalletStore {

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<GetWalletsResponse> = new Request(this.api.etc.getWallets);
  @observable createWalletRequest: Request<CreateWalletResponse> = new Request(this.api.etc.createWallet);
  @observable getWalletRecoveryPhraseRequest: Request<GetWalletRecoveryPhraseResponse> = new Request(this.api.etc.getWalletRecoveryPhrase);
  /* eslint-disable max-len */

  setup () {
    const { router, etc } = this.actions;
    const { wallets, walletBackup } = etc;
    wallets.createWallet.listen(this._create);
    walletBackup.finishWalletBackup.listen(this._finishWalletCreation);
    router.goToRoute.listen(this._onRouteChange);
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
        this.actions.ada.walletBackup.initiateWalletBackup.trigger({ recoveryPhrase });
      }
    } catch (error) {
      throw error;
    }
  };

  _finishWalletCreation = async () => {
    this._newWalletDetails.mnemonic = this.stores.ada.walletBackup.recoveryPhrase.join(' ');
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

}
