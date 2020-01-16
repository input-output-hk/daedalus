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
  const dates = transactions
    .filter(({ date }) => !!date)
    .map(({ date }) => (date ? date.getTime() : 0));
  const amounts = transactions.map(({ amount }) => amount.toNumber());
  const dateRange = DateRangeTypes.ALL;
  const fromDate =
    dates.length > 0 ? moment(Math.min(...dates)).format('YYYY-MM-DD') : '';
  const toDate =
    dates.length > 0 ? moment(Math.max(...dates)).format('YYYY-MM-DD') : '';
  const fromAmount = amounts.length > 0 ? Math.min(...amounts).toString() : '';
  const toAmount = amounts.length > 0 ? Math.max(...amounts).toString() : '';
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
  if (!date) {
    return true;
  }

  const compareFrom = fromDate
    ? date.getTime() >=
      moment(fromDate)
        .startOf('day')
        .valueOf()
    : true;
  const compareTo = toDate
    ? date.getTime() <=
      moment(toDate)
        .endOf('day')
        .valueOf()
    : true;

  return compareFrom && compareTo;
};

export const isTransactionAmountInFilterRange = (
  fromAmount: string,
  toAmount: string,
  transaction: WalletTransaction
) => {
  const { amount } = transaction;
  const compareFrom = fromAmount
    ? amount.toNumber() >= Number(fromAmount)
    : true;
  const compareTo = toAmount ? amount.toNumber() <= Number(toAmount) : true;

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
    fromAmount = '',
    toAmount = '',
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

export const getNumberOfFilterDimensionsApplied = (
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
  let result = 0;

  if (searchTerm) {
    result++;
  }
  if (fromDate || toDate) {
    result++;
  }
  if (fromAmount || toAmount) {
    result++;
  }
  if (!incomingChecked || !outgoingChecked) {
    result++;
  }

  return result;
};
