// @flow
import { computed, action } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';
import type { StakePoolProps } from '../api/staking/types';

import STAKE_POOLS from '../config/stakingStakePools.dummy.json';

export default class StakingStore extends Store {
  startDateTime: string = '2019-09-26T00:00:00.161Z';
  decentralizationProgress: number = 10;

  setup() {
    const { staking } = this.actions;
    staking.goToStakingPage.listen(this._goToStakingPage);
  }

  // =================== PUBLIC API ==================== //

  // GETTERS

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed get stakePools(): Array<StakePoolProps> {
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return STAKE_POOLS;
  }

  @computed get delegatingStakePools(): Array<StakePoolProps> {
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return [STAKE_POOLS[1], STAKE_POOLS[3], STAKE_POOLS[20], STAKE_POOLS[36]];
  }
  @computed
  get isStakingDelegationCountdown(): boolean {
    return this.currentRoute === ROUTES.STAKING.DELEGATION_COUNTDOWN;
  }

  @action showCountdown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }

  _goToStakingPage = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.ROOT,
    });
  };
}
