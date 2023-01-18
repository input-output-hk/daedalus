import Action from './lib/Action';
import type { DelegationAction } from '../types/stakingTypes';

export default class HardwareWalletsActions {
  /* ----------  Get hardware wallet device  ---------- */
  selectCoins: Action<{
    walletId: string;
    address: string;
    amount: number;
  }> = new Action();
  selectDelegationCoins: Action<{
    walletId: string;
    poolId: string;
    delegationAction: DelegationAction;
  }> = new Action();
  sendMoney: Action<any> = new Action();
  refreshHardwareWalletsLocalData: Action<any> = new Action();
}
