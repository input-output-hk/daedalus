// @flow
import Action from './lib/Action';
import type { JoinStakePoolRequest } from '../api/staking/types';
// ======= STAKING ACTIONS =======

export default class StakingActions {
  goToStakingInfoPage: Action<any> = new Action();
  goToStakingDelegationCenterPage: Action<any> = new Action();
  joinStakePool: Action<JoinStakePoolRequest> = new Action();
}
