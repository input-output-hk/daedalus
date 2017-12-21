// @flow
import { observable, computed, action } from 'mobx';
import { find } from 'lodash';
import Store from '../lib/Store';
import CachedRequest from '../lib/LocalizedCachedRequest';
import WalletAccount from '../../domain/WalletAccount';
import type { GetAccountsResponse } from '../../api/ada/index';

export default class AdaAccountsStore extends Store {

  @observable active: ?WalletAccount = null;
  @observable accountsRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetAccountsResponse>
  }> = [];

  @computed get all(): Array<WalletAccount> {
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return [];
    const result = this._getAccountsAllRequest(wallet.id).result;
    return result || [];
  }

  @action _refreshAccounts = () => {
    if (this.stores.networkStatus.isConnected) {
      const allWallets = this.stores.ada.wallets.all;
      for (const wallet of allWallets) {
        const allRequest = this._getAccountsAllRequest(wallet.id);
        allRequest.invalidate({ immediately: false });
        allRequest.execute({ walletId: wallet.id });
      }
    }
  };

  _getAccountsAllRequest = (walletId: string): CachedRequest<GetAccountsResponse> => {
    const foundRequest = find(this.accountsRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.ada.getAccounts);
  };

}
