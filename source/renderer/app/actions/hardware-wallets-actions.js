// @flow
import Action from './lib/Action';
import type { SetHardwareWalletLocalDataRequestType } from '../api/utils/localStorage';

export default class HardwareWalletsActions {
  /* ----------  Get hardware wallet device  ---------- */
  selectCoins: Action<{
    walletId: string,
    address: string,
    amount: string,
  }> = new Action();
  refreshHardwareWalletsLocalData: Action<any> = new Action();
  setHardwareWalletLocalData: Action<SetHardwareWalletLocalDataRequestType> = new Action();
  unsetHardwareWalletLocalData: Action<{ walletId: string }> = new Action();
  setHardwareWalletDevice: Action<SetHardwareWalletLocalDataRequestType> = new Action();
  unsetHardwareWalletDevice: Action<{ deviceId: string }> = new Action();
}
