// @flow
import { computed } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';
import type { StakePoolProps } from '../api/staking/types';

import STAKE_POOLS from '../config/stakingStakePools.dummy.json';

export default class StakingStore extends Store {
  showCountdown: boolean = false;
  startDateTime: string = '20191201T000000Z';

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
    return STAKE_POOLS.slice(0, 29);
  }

  @computed get delegatingStakePools(): Array<StakePoolProps> {
    // return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
    return [STAKE_POOLS[1], STAKE_POOLS[3], STAKE_POOLS[20], STAKE_POOLS[36]];
  }
}
