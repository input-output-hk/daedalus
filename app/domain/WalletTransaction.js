// @flow
import { observable } from 'mobx';

export type TransactionType = 'card' | 'adaExpend' | 'adaIncome' | 'exchange';

export default class WalletTransaction {

  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable currency: string = '';
  @observable amount: number;
  @observable date: Date;
  @observable description: string = '';

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    currency: string,
    amount: number,
    date: Date,
    description: string,
  }) {
    Object.assign(this, data);
  }

}
