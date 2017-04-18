// @flow
import { observable, action, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import WalletTransaction from './WalletTransaction';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';
import { assuranceModes, assuranceModeOptions } from '../config/transactionAssuranceConfig';

export default class Wallet {

  id: string = '';
  type: string;
  address: string = '';
  currency: string = '';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable assurance: AssuranceMode;
  @observable hasPassword: bool;
  @observable passwordUpdateDate: ?Date;
  @observable transactions: Array<WalletTransaction> = [];

  constructor(data: {
    id: string,
    type: string,
    name: string,
    address: string,
    currency: string,
    amount: BigNumber,
    assuranceMode: AssuranceMode,
    hasPassword: bool,
    passwordUpdateDate: ?Date,
  }) {
    Object.assign(this, data);
  }

  @action addTransaction(transaction: WalletTransaction) {
    this.transactions.push(transaction);
  }

  @computed get hasFunds(): boolean {
    return this.amount > 0;
  }

  @computed get assuranceMode(): AssuranceMode {
    switch (this.assurance) {
      case assuranceModeOptions.NORMAL: return assuranceModes.NORMAL;
      case assuranceModeOptions.STRICT: return assuranceModes.STRICT;
      default: return assuranceModes.NORMAL;
    }
  }

}
