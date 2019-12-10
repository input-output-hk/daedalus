// @flow
import { computed, action, observable } from 'mobx';
import BigNumber from 'bignumber.js';
import { orderBy, take } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import { RECENT_STAKE_POOLS_COUNT } from '../config/stakingConfig';
import type {
  Reward,
  RewardForIncentivizedTestnet,
  JoinStakePoolRequest,
  GetDelegationFeeRequest,
} from '../api/staking/types';
import Wallet from '../domains/Wallet';
import StakePool from '../domains/StakePool';
import REWARDS from '../config/stakingRewards.dummy.json';

export default class StakingStore extends Store {
  STAKE_POOLS_INITIAL_INTERVAL = 1000; // 1000 milliseconds
  STAKE_POOLS_REFRESH_INTERVAL = 30 * 60 * 1000; // 30 minutes | unit: milliseconds;

  initialPooling: ?IntervalID = null;
  refreshPooling: ?IntervalID = null;

  startDateTime: string = '2019-12-09T00:00:00.161Z';
  decentralizationProgress: number = 10;
  adaValue: BigNumber = new BigNumber(82650.15);
  percentage: number = 14;

  setup() {
    this.initialPooling = setInterval(
      this.refreshStakePoolsData,
      this.STAKE_POOLS_INITIAL_INTERVAL
    );
    const { staking } = this.actions;
    staking.goToStakingInfoPage.listen(this._goToStakingInfoPage);
    staking.goToStakingDelegationCenterPage.listen(
      this._goToStakingDelegationCenterPage
    );
    staking.joinStakePool.listen(this._joinStakePool);
    this.refreshStakePoolsData();
  }

  // REQUESTS
  @observable joinStakePoolRequest: Request<JoinStakePoolRequest> = new Request(
    this.api.ada.joinStakePool
  );
  @observable stakePoolsRequest: Request<Array<StakePool>> = new Request(
    this.api.ada.getStakePools
  );

  // =================== PUBLIC API ==================== //

  @action _joinStakePool = async (request: JoinStakePoolRequest) => {
    const { walletId, stakePoolId, passphrase } = request;
    await this.joinStakePoolRequest.execute({
      walletId,
      stakePoolId,
      passphrase,
    });
    this.stores.wallets.refreshWalletsData();
  };

  calculateDelegationFee = async (
    delegationFeeRequest: GetDelegationFeeRequest
  ) => {
    const { walletId } = delegationFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);

    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }

    return this.api.ada.calculateDelegationFee({
      ...delegationFeeRequest,
    });
  };

  // GETTERS

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed get stakePools(): Array<StakePool> {
    return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
  }

  @computed get recentStakePools(): Array<StakePool> {
    const orderedStakePools = orderBy(this.stakePools, 'ranking', 'asc');
    return take(orderedStakePools, RECENT_STAKE_POOLS_COUNT);
  }

  @computed get isStakingDelegationCountdown(): boolean {
    return this.currentRoute === ROUTES.STAKING.COUNTDOWN;
  }

  @computed get rewards(): Array<Reward> {
    return REWARDS;
  }

  @computed
  get rewardsForIncentivizedTestnet(): Array<RewardForIncentivizedTestnet> {
    const { wallets } = this.stores;
    return wallets.allWallets.map(
      this._transformWalletToRewardForIncentivizedTestnet
    );
  }

  @action showCountdown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }

  @action refreshStakePoolsData = async () => {
    const { isSynced, isConnected } = this.stores.networkStatus;
    if (this.stores.wallets._pollingBlocked || !isSynced || !isConnected)
      return;
    if (this.initialPooling && !this.refreshPooling) {
      clearInterval(this.initialPooling);
      this.initialPooling = null;
      this.refreshPooling = setInterval(
        this.refreshStakePoolsData,
        this.STAKE_POOLS_REFRESH_INTERVAL
      );
    }
    await this.stakePoolsRequest.execute().promise;
  };

  _goToStakingInfoPage = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.INFO,
    });
  };

  _goToStakingDelegationCenterPage = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.DELEGATION_CENTER,
    });
  };

  _transformWalletToRewardForIncentivizedTestnet = (inputWallet: Wallet) => {
    const { name: wallet, reward } = inputWallet;
    return { wallet, reward };
  };

  getStakePoolById = (stakePoolId: string) =>
    this.stakePools.find(({ id }: StakePool) => id === stakePoolId);
}
