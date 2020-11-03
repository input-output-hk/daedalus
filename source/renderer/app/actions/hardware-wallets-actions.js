// @flow
import Action from './lib/Action';
import type { SetHardwareWalletLocalDataRequestType, SetHardwareWalletDeviceRequestType} from '../api/utils/localStorage';
import type { DelegationAction } from '../types/stakingTypes';

export default class HardwareWalletsActions {
  /* ----------  Get hardware wallet device  ---------- */
  selectCoins: Action<{
    walletId: string,
    address: string,
    amount: number,
  }> = new Action();
  selectDelegationCoins: Action<{
    walletId: string,
    poolId: string,
    delegationAction: DelegationAction,
  }> = new Action();
  sendMoney: Action<any> = new Action();
  refreshHardwareWalletsLocalData: Action<any> = new Action();
  setHardwareWalletLocalData: Action<SetHardwareWalletLocalDataRequestType> = new Action();
  unsetHardwareWalletLocalData: Action<{ walletId: string }> = new Action();
  setHardwareWalletDevice: Action<SetHardwareWalletDeviceRequestType> = new Action();
  unsetHardwareWalletDevice: Action<{ deviceId: ?string }> = new Action();
}
