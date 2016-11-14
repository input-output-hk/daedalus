// @flow
import { observable, action } from 'mobx';
import WalletTransaction from './WalletTransaction';

export type WalletType = 'personal' | 'shared';

export default class Wallet {

  type: WalletType;
  address: string = '';
  currency: string = '';
  @observable name: string = '';
  @observable amount: number;
  @observable transactions: Array<WalletTransaction> = [];
  @observable lastUsed: boolean;

  constructor(data: {
    type: WalletType,
    name: string,
    address: string,
    currency: string,
    amount: number,
    lastUsed: boolean
  }) {
    Object.assign(this, data);
  }

  @action addTransaction(transaction: WalletTransaction) {
    this.transactions.push(transaction);
  }

}
