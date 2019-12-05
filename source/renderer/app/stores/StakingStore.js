// @flow
import { computed, action, observable } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import type {
  Reward,
  RewardForIncentivizedTestnet,
  JoinStakePoolRequest,
  EstimateJoinFeeRequest,
} from '../api/staking/types';
import Wallet from '../domains/Wallet';
import StakePool from '../domains/StakePool';
import REWARDS from '../config/stakingRewards.dummy.json';
import STAKE_POOLS from '../config/stakingStakePools.dummy.json';

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

  // @API TODO - integrate real API V2 endpoint once is available
  estimateJoinFee = async (estimateJoinFeeRequest: EstimateJoinFeeRequest) => {
    const { walletId } = estimateJoinFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);

    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }

    return this.api.ada.estimateJoinFee({
      ...estimateJoinFeeRequest,
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
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return [
      STAKE_POOLS[1],
      STAKE_POOLS[25],
      STAKE_POOLS[100],
      STAKE_POOLS[150],
      STAKE_POOLS[200],
      STAKE_POOLS[250],
      STAKE_POOLS[275],
      STAKE_POOLS[299],
    ];
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
