// @flow
import { observable, action, computed } from 'mobx';
import WalletTransaction from './WalletTransaction';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class Wallet {

  id: string = '';
  type: string;
  address: string = '';
  currency: string = '';
  @observable name: string = '';
  @observable amount: number;
  @observable assuranceMode: AssuranceMode;
  @observable transactions: Array<WalletTransaction> = [];

  constructor(data: {
    id: string,
    type: string,
    name: string,
    address: string,
    currency: string,
    amount: number,
    assuranceMode: AssuranceMode,
  }) {
    Object.assign(this, data);
  }

  @action addTransaction(transaction: WalletTransaction) {
    this.transactions.push(transaction);
  }

  @computed get hasFunds(): boolean {
    return this.amount > 0;
  }

}
