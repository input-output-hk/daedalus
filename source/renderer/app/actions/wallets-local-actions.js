// @flow
import Action from './lib/Action';

export default class WalletSettingsActions {
  refreshWalletsLocalData: Action<any> = new Action();
  setWalletLocalData: Action<{
    walletId: string,
    updatedWalletData?: Object,
    skipRefresh?: boolean,
  }> = new Action();
  unsetWalletLocalData: Action<{ walletId: string }> = new Action();
}
