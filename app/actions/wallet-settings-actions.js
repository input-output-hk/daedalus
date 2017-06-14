// @flow
import Action from './lib/Action';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class WalletSettingsActions {
  changeWalletPassword: Action<{
    walletId: string,
    oldPassword: string,
    newPassword: ?string,
  }> = new Action();
  setWalletPassword: Action<{ walletId: string, password: string }> = new Action();
  updateWalletAssuranceLevel: Action<{ assurance: AssuranceMode }> = new Action();
  startEditingWalletField: Action<{ field: string }> = new Action();
  stopEditingWalletField: Action<any> = new Action();
  cancelEditingWalletField: Action<any> = new Action();
  updateWalletField: Action<{ field: string, value: string }> = new Action();
}
