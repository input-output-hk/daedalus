// @flow
import { observable, action, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import WalletTransaction from './WalletTransaction';
import type { AssuranceMode, AssuranceModeOption } from '../types/transactionAssuranceTypes';
import { assuranceModes, assuranceModeOptions } from '../config/transactionAssuranceConfig';

export default class Wallet {

  id: string = '';
  currency: string = 'ADA';
  address: string = 'current address';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable assurance: AssuranceModeOption;
  @observable hasPassword: boolean;
  @observable passwordUpdateDate: ?Date;

  // TODO: we are not using this?
  @observable transactions: Array<WalletTransaction> = [];

  constructor(data: {
    id: string,
    name: string,
    amount: BigNumber,
    assurance: AssuranceModeOption,
    hasPassword: boolean,
    passwordUpdateDate: ?Date,
  }) {
    Object.assign(this, data);
  }

  // TODO: we are not using this?
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
