// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type { AssuranceMode, AssuranceLevel } from '../types/transactionAssuranceTypes';
import { assuranceLevels } from '../config/transactionAssuranceConfig';
import type { TransactionCondition } from 'daedalus-client-api';

export type TransactionState = 'pending' | 'failed' | 'ok';
export type TrasactionAddresses = { from: Array<string>, to: Array<string> };
export type TransactionType = 'card' | 'adaExpend' | 'adaIncome' | 'exchange';

export const transactionStates: {
  PENDING: TransactionState, FAILED: TransactionState, OK: TransactionState,
} = {
  PENDING: 'pending', FAILED: 'failed', OK: 'ok',
};

export default class WalletTransaction {

  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable amount: BigNumber;
  @observable date: Date;
  @observable description: string = '';
  @observable numberOfConfirmations: number = 0;
  @observable addresses: TrasactionAddresses = { from: [], to: [] };
  @observable condition: TransactionCondition;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    date: Date,
    description: string,
    numberOfConfirmations: number,
    addresses: TrasactionAddresses,
    condition: TransactionCondition,
  }) {
    Object.assign(this, data);
  }

  getAssuranceLevelForMode(mode: AssuranceMode): AssuranceLevel {
    if (this.numberOfConfirmations < mode.low) {
      return assuranceLevels.LOW;
    } else if (this.numberOfConfirmations < mode.medium) {
      return assuranceLevels.MEDIUM;
    }
    return assuranceLevels.HIGH;
  }

  getState(): TransactionState {
    switch(this.condition) {
      case 'CPtxApplying': return 'pending';
      case 'CPtxWontApply': return 'failed';
      default: return 'ok'; // CPtxInBlocks && CPtxNotTracked
    }
  }

}
