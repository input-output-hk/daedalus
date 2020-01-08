// @flow
import moment from 'moment';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';

export const generateFilterEdgesOfTransactions = (
  transactions: Array<WalletTransaction>
) => {
  const dates = transactions.map(({ date }) =>
    date ? date.getTime() : new Date().getTime()
  );
  const amounts = transactions.map(({ amount }) => amount.toNumber());
  const minDate = dates.length > 0 ? Math.min(...dates) : null;
  const maxDate = dates.length > 0 ? Math.max(...dates) : null;
  const minAmount = amounts.length > 0 ? Math.min(...amounts) : null;
  const maxAmount = amounts.length > 0 ? Math.max(...amounts) : null;

  return { minDate, maxDate, minAmount, maxAmount };
};

export const isTransactionDateInFilterRange = (
  fromDate: string,
  toDate: string,
  transaction: WalletTransaction
) => {
  const { date } = transaction;
  if ((!fromDate && !toDate) || !date) {
    return true;
  }
  const fromMoment = moment(fromDate)
    .startOf('day')
    .valueOf();
  const toMoment = moment(toDate)
    .endOf('day')
    .valueOf();
  const compareFrom = date.getTime() >= fromMoment;
  const compareTo = date.getTime() <= toMoment;
  if (!fromDate) {
    return compareTo;
  }
  if (!toDate) {
    return compareFrom;
  }
  return compareFrom && compareTo;
};

export const isTransactionAmountInFilterRange = (
  fromAmount: number,
  toAmount: number,
  transaction: WalletTransaction
) => {
  const { amount } = transaction;
  if (!fromAmount && !toAmount) {
    return true;
  }
  const compareFrom = amount.toNumber() >= fromAmount;
  const compareTo = amount.toNumber() <= toAmount;
  if (!fromAmount) {
    return compareTo;
  }
  if (!toAmount) {
    return compareFrom;
  }
  return compareFrom && compareTo;
};

export const isTransactionTypeInFilterRange = (
  incomingChecked: boolean,
  outgoingChecked: boolean,
  transaction: WalletTransaction
) => {
  const { type } = transaction;
  if (
    (!incomingChecked && type === TransactionTypes.INCOME) ||
    (!outgoingChecked && type === TransactionTypes.EXPEND)
  ) {
    return false;
  }

  return true;
};

export const isTransactionTitleInFilterRange = (
  searchTerm: string,
  transaction: WalletTransaction
) => {
  if (!searchTerm) {
    return true;
  }

  return transaction.title.search(new RegExp(searchTerm, 'i')) !== -1;
};
