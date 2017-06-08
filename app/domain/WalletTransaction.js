// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type { AssuranceMode, AssuranceLevel } from '../types/transactionAssuranceTypes';
import { assuranceLevels } from '../config/transactionAssuranceConfig';

export type TransactionType = 'card' | 'adaExpend' | 'adaIncome' | 'exchange';

export default class WalletTransaction {

  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable amount: BigNumber;
  @observable date: Date;
  @observable description: string = '';
  @observable numberOfConfirmations: number = 0;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    date: Date,
    description: string,
    numberOfConfirmations: number
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

}
