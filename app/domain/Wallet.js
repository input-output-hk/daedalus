// @flow
import { observable, action } from 'mobx';
import WalletTransaction from './WalletTransaction';

export default class Wallet {

  id: string = '';
  type: string;
  address: string = '';
  currency: string = '';
  @observable name: string = '';
  @observable amount: number;
  @observable transactions: Array<WalletTransaction> = [];

  constructor(data: {
    id: string,
    type: string,
    name: string,
    address: string,
    currency: string,
    amount: number
  }) {
    Object.assign(this, data);
  }

  @action addTransaction(transaction: WalletTransaction) {
    this.transactions.push(transaction);
  }

}
