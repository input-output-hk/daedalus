// @flow
import Action from './lib/Action';
import type {
  JoinStakePoolRequest,
  QuitStakePoolRequest,
} from '../api/staking/types';
// ======= STAKING ACTIONS =======

export default class StakingActions {
  fakeStakePoolsLoading: Action<any> = new Action();
  goToStakingInfoPage: Action<any> = new Action();
  goToStakingDelegationCenterPage: Action<any> = new Action();
  joinStakePool: Action<JoinStakePoolRequest> = new Action();
  quitStakePool: Action<QuitStakePoolRequest> = new Action();
  updateDelegatingStake: Action<number> = new Action();
  rankStakePools: Action<any> = new Action();
  selectDelegationWallet: Action<string> = new Action();
  /* ----------  Redeem ITN Rewards  ---------- */
  onRedeemStart: Action<any> = new Action();
  onConfigurationContinue: Action<{
    recoveryPhrase: Array<string>,
    isRedeemRewards?: boolean,
  }> = new Action();
  onSelectRedeemWallet: Action<{ walletId: string }> = new Action();
  onConfirmationContinue: Action<{ spendingPassword: string }> = new Action();
  onResultContinue: Action<any> = new Action();
  closeRedeemDialog: Action<any> = new Action();
}
