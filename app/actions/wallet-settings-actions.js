// @flow
import { Action } from './lib/actions';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class WalletSettingsActions {
  updateWalletAssuranceLevel: Action<{ assurance: AssuranceMode }> = new Action();
}
