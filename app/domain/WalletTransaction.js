// @flow
export type TransactionType = 'card' | 'adaExpend' | 'adaIncome' | 'exchange';

export class WalletTransaction {

  id: string = '';
  type: TransactionType;
  title: string = '';
  currency: string = '';
  amount: number;
  date: Date;
  // TODO: Add other attributes for transaction details

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    currency: string,
    amount: number,
    date: Date
  }) {
    Object.assign(this, data);
  }

}
