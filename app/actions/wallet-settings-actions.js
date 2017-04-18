// @flow
import Action from './lib/Action';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class WalletSettingsActions {
  changeWalletPassword: Action<{
    walletId: string,
    oldPassword: string,
    newPassword: string,
  }> = new Action();
  setWalletPassword: Action<{ walletId: string, password: string, }> = new Action();
  updateWalletAssuranceLevel: Action<{ assurance: AssuranceMode }> = new Action();
}
