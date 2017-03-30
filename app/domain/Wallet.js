// @flow
import { observable, action, computed } from 'mobx';
import WalletTransaction from './WalletTransaction';

export default class Wallet {

  id: string = '';
  type: string;
  address: string = '';
  currency: string = '';
  @observable name: string = '';
  @observable amount: number;
  @observable assurance: string;
  @observable transactions: Array<WalletTransaction> = [];

  constructor(data: {
    id: string,
    type: string,
    name: string,
    address: string,
    currency: string,
    amount: number,
    assurance: string,
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
