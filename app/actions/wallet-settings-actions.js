// @flow
import Action from './lib/Action';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class WalletSettingsActions {
  cancelEditingWalletField: Action<any> = new Action();
  startEditingWalletField: Action<{ field: string }> = new Action();
  stopEditingWalletField: Action<any> = new Action();
  updateWalletAssuranceLevel: Action<{ assurance: AssuranceMode }> = new Action();
  updateWalletField: Action<{ field: string, value: string }> = new Action();
  // eslint-disable-next-line max-len
  updateWalletPassword: Action<{ walletId: string, oldPassword: ?string, newPassword: ?string }> = new Action();
}
