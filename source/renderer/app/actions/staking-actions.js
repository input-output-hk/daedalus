// @flow
import Action from './lib/Action';
import type {
  JoinStakePoolRequest,
  QuitStakePoolRequest,
} from '../api/staking/types';
import type { RedeemItnRewardsStep } from '../types/stakingTypes';
// ======= STAKING ACTIONS =======

export default class StakingActions {
  fakeStakePoolsLoading: Action<any> = new Action();
  goToStakingInfoPage: Action<any> = new Action();
  goToStakingDelegationCenterPage: Action<any> = new Action();
  joinStakePool: Action<JoinStakePoolRequest> = new Action();
  quitStakePool: Action<QuitStakePoolRequest> = new Action();
  /* ----------  Redeem ITN Rewards  ---------- */
  goToRedeemStep: Action<{ step: RedeemItnRewardsStep }> = new Action();
  closeRedeemDialog: Action<any> = new Action();
}
