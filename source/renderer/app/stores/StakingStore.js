// @flow
import { computed, action } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';
import type {
  StakePool,
  Reward,
  RewardForIncentivizedTestnet,
} from '../api/staking/types';
import Wallet from '../domains/Wallet';

import STAKE_POOLS from '../config/stakingStakePools.dummy.json';
import REWARDS from '../config/stakingRewards.dummy.json';

export default class StakingStore extends Store {
  startDateTime: string = '2019-12-09T00:00:00.161Z';
  decentralizationProgress: number = 10;
  adaValue: BigNumber = new BigNumber(82650.15);
  percentage: number = 14;

  setup() {
    const { staking } = this.actions;
    staking.goToStakingInfoPage.listen(this._goToStakingInfoPage);
    staking.goToStakingDelegationCenterPage.listen(
      this._goToStakingDelegationCenterPage
    );
  }

  // =================== PUBLIC API ==================== //

  // GETTERS

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed get stakePools(): Array<StakePool> {
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return STAKE_POOLS;
  }

  @computed get delegatingStakePools(): Array<StakePool> {
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return [
      STAKE_POOLS[1],
      STAKE_POOLS[50],
      STAKE_POOLS[100],
      STAKE_POOLS[200],
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
}
