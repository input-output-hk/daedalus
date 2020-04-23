// @flow
import Action from './lib/Action';

export default class WalletSettingsActions {
  refreshWalletsLocalData: Action<{ walletIds: Array<string> }> = new Action();
  setWalletLocalData: Action<{
    walletId: string,
    updatedWalletData?: Object,
  }> = new Action();
  unsetWalletLocalData: Action<{ walletId: string }> = new Action();
}
