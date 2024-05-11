import { observable, computed, makeObservable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { asyncForEach } from '../utils/asyncForEach';
import { WalletsLocalData } from '../types/localDataTypes';
import { Api } from '../api';
import { ActionsMap } from '../actions';
import { AnalyticsTracker } from '../analytics';

export default class WalletsLocalStore extends Store {
  localWalletsRequest: Request<WalletsLocalData> = new Request(
    this.api.localStorage.getWalletsLocalData
  );
  setWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.setWalletLocalData
  );
  unsetWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.unsetWalletLocalData
  );

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      localWalletsRequest: observable,
      setWalletLocalDataRequest: observable,
      unsetWalletLocalDataRequest: observable,
      all: computed,
    });
  }

  setup() {
    const { walletsLocal: actions } = this.actions;
    actions.refreshWalletsLocalData.listen(this._refreshWalletsLocalData);
    actions.setWalletLocalData.listen(this._setWalletLocalData);
    actions.unsetWalletLocalData.listen(this._unsetWalletLocalData);
    this.localWalletsRequest.execute();
  }

  // =================== PUBLIC API ==================== //
  // GETTERS
  get all(): WalletsLocalData {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'WalletsLocalData | {}' is not assignable to ... Remove this comment to see the full error message
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
        await this._setWalletLocalData({
          walletId,
          skipRefresh: true,
        });
      }
    });
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.localWalletsRequest.execute();
  };
  _setWalletLocalData = async ({
    walletId,
    updatedWalletData,
    skipRefresh,
  }: {
    walletId: string;
    updatedWalletData?: Record<string, any>;
    skipRefresh?: boolean;
  }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setWalletLocalDataRequest.execute(walletId, updatedWalletData);

    if (!skipRefresh) {
      this._refreshWalletsLocalData();
    }
  };
  _unsetWalletLocalData = async ({ walletId }: { walletId: string }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.unsetWalletLocalDataRequest.execute(walletId);

    this._refreshWalletsLocalData();
  };
}
