// @flow
import { observable, computed, action, runInAction, untracked } from 'mobx';
import Store from '../lib/Store';
import Wallet from '../../domain/Wallet';
import Request from '.././lib/LocalizedRequest';

export default class WalletsStore extends Store {

  WALLET_REFRESH_INTERVAL = 5000;

  @observable active: ?Wallet = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable walletsRequest: Request<any> = new Request(this.api.etc.getAccounts);
  /* eslint-enable max-len */

  setup() {
    setInterval(this.pollRefresh, this.WALLET_REFRESH_INTERVAL);
  }

  @action refreshAccountsData = async () => {
    const result = await this.walletsRequest.execute().promise;
    console.log(result);
  };

  pollRefresh = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.refreshAccountsData();
    }
  };

}
