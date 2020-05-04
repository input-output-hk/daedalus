// @flow
import { observable, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { asyncForEach } from '../utils/asyncForEach';
import type { WalletsLocalData } from '../api/utils/localStorage';

export default class WalletsLocalStore extends Store {
  @observable localWalletsRequest: Request<WalletsLocalData> = new Request(
    this.api.localStorage.getWalletsLocalData
  );
  @observable setWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.setWalletLocalData
  );
  @observable unsetWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.unsetWalletLocalData
  );

  setup() {
    const { walletsLocal: actions } = this.actions;
    actions.refreshWalletsLocalData.listen(this._refreshWalletsLocalData);
    actions.setWalletLocalData.listen(this._setWalletLocalData);
    actions.unsetWalletLocalData.listen(this._unsetWalletLocalData);

    this.localWalletsRequest.execute();
  }

  // =================== PUBLIC API ==================== //

  // GETTERS

  @computed get all(): WalletsLocalData {
    return this.localWalletsRequest.result
      ? this.localWalletsRequest.result
      : {};
  }

  // =================== PRIVATE API ==================== //

  _refreshWalletsLocalData = async () => {
    const currentLocalWallets: WalletsLocalData = this.all;
    const { all: wallets } = this.stores.wallets;
    await asyncForEach(wallets, async ({ id: walletId }) => {
      const walletLocalData = currentLocalWallets[walletId];
      // Adds missing wallets & data
      if (!walletLocalData || !walletLocalData.creationDate) {
        await this._setWalletLocalData({ walletId, skipRefresh: true });
      }
    });
    await this.localWalletsRequest.execute();
  };

  _setWalletLocalData = async ({
    walletId,
    updatedWalletData,
    skipRefresh,
  }: {
    walletId: string,
    updatedWalletData?: Object,
    skipRefresh?: boolean,
  }) => {
    await this.setWalletLocalDataRequest.execute(walletId, updatedWalletData);
    if (!skipRefresh) {
      this._refreshWalletsLocalData();
    }
  };

  _unsetWalletLocalData = async ({ walletId }: { walletId: string }) => {
    await this.unsetWalletLocalDataRequest.execute(walletId);
    this._refreshWalletsLocalData();
  };
}
