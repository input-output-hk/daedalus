// @flow
import moment from 'moment';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';
import type { TransactionFilterOptionsStruct } from '../stores/TransactionsStore';
import { DateRangeTypes } from '../stores/TransactionsStore';

export const generateFilterOptions = (
  transactions: Array<WalletTransaction>
) => {
  const dates = transactions.map(({ date }) =>
    date ? date.getTime() : new Date().getTime()
  );
  const amounts = transactions.map(({ amount }) => amount.toNumber());
  const dateRange = DateRangeTypes.THIS_WEEK;
  const fromDate =
    dates.length > 0 ? moment(Math.min(...dates)).format('YYYY-MM-DD') : '';
  const toDate =
    dates.length > 0 ? moment(Math.max(...dates)).format('YYYY-MM-DD') : '';
  const fromAmount = amounts.length > 0 ? Math.min(...amounts) : 0;
  const toAmount = amounts.length > 0 ? Math.max(...amounts) : 0;
  const incomingChecked = true;
  const outgoingChecked = true;

  return {
    dateRange,
    fromDate,
    toDate,
    fromAmount,
    toAmount,
    incomingChecked,
    outgoingChecked,
  };
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

export const isTransactionInFilterRange = (
  filterOptions: ?TransactionFilterOptionsStruct,
  transaction: WalletTransaction
) => {
  const {
    searchTerm = '',
    fromDate = '',
    toDate = '',
    fromAmount = 0,
    toAmount = 0,
    incomingChecked = true,
    outgoingChecked = true,
  } = filterOptions || {};

  return !!(
    isTransactionTitleInFilterRange(searchTerm, transaction) &&
    isTransactionDateInFilterRange(fromDate, toDate, transaction) &&
    isTransactionAmountInFilterRange(fromAmount, toAmount, transaction) &&
    isTransactionTypeInFilterRange(
      incomingChecked,
      outgoingChecked,
      transaction
    )
  );
};

export const isFilterApplied = (
  filterOptions: ?TransactionFilterOptionsStruct
) => {
  const {
    searchTerm,
    fromDate,
    toDate,
    fromAmount,
    toAmount,
    incomingChecked = true,
    outgoingChecked = true,
  } = filterOptions || {};

  return !!(
    searchTerm ||
    fromDate ||
    toDate ||
    fromAmount ||
    toAmount ||
    !incomingChecked ||
    !outgoingChecked
  );
};
