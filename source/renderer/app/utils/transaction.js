// @flow
import React from 'react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';
import { formattedWalletAmount } from './formatters';
import type {
  DateRangeType,
  TransactionFilterOptionsType,
} from '../stores/TransactionsStore';
import { DateRangeTypes } from '../stores/TransactionsStore';

const AMOUNT_RAW_LENGTH_LIMIT = 10;

export const generateFilterOptions = (
  transactions: Array<WalletTransaction>
) => {
  const dates = transactions
    .filter(({ date }) => !!date)
    .map(({ date }) => (date ? date.getTime() : 0));
  const amounts = transactions.map(({ amount }) => Math.abs(amount.toNumber()));
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
  const min =
    fromAmount === '.' || fromAmount === ''
      ? new BigNumber(0)
      : new BigNumber(fromAmount);
  const max =
    toAmount === '.' || toAmount === ''
      ? new BigNumber(0)
      : new BigNumber(toAmount);
  const compareFrom = fromAmount
    ? amount.absoluteValue().greaterThanOrEqualTo(min)
    : true;
  const compareTo = toAmount
    ? amount.absoluteValue().lessThanOrEqualTo(max)
    : true;

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
  filterOptions: ?TransactionFilterOptionsType,
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
  filterOptions: ?TransactionFilterOptionsType
) => {
  const {
    searchTerm,
    dateRange,
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
  if (dateRange !== DateRangeTypes.ALL && (fromDate || toDate)) {
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

export const calculateDateRange = (
  dateRange: string,
  customDateRange: { customFromDate: string, customToDate: string }
) => {
  const { customFromDate, customToDate } = customDateRange;
  let fromDate = null;
  let toDate = null;

  if (dateRange === DateRangeTypes.ALL) {
    fromDate = '';
    toDate = '';
  } else if (dateRange === DateRangeTypes.CUSTOM_DATE_RANGE) {
    fromDate = customFromDate;
    toDate = customToDate;
  } else {
    if (dateRange === DateRangeTypes.THIS_WEEK) {
      fromDate = moment().startOf('week');
    } else if (dateRange === DateRangeTypes.THIS_MONTH) {
      fromDate = moment().startOf('month');
    } else if (dateRange === DateRangeTypes.THIS_YEAR) {
      fromDate = moment().startOf('year');
    } else {
      fromDate = moment();
    }
    fromDate = fromDate.format('YYYY-MM-DD');
    toDate = moment().format('YYYY-MM-DD');
  }

  return { fromDate, toDate };
};

export const formatDateValue = (
  date: string,
  defaultDate: string,
  dateFormat: string
) => {
  if (!date) {
    const formattedDefaultDate = moment(defaultDate).format(dateFormat);

    return <span className="undefined">{formattedDefaultDate}</span>;
  }

  return moment(date).format(dateFormat);
};

export const formatAmountValue = (
  amount: string,
  defaultAmount: string,
  shrinkIfLong?: boolean
) => {
  let inputAmount = amount || defaultAmount;
  if (inputAmount === '.') {
    inputAmount = '0';
  } else if (inputAmount[0] === '.') {
    inputAmount = `0${inputAmount}`;
  } else if (inputAmount[inputAmount.length - 1] === '.') {
    inputAmount = `${inputAmount}0`;
  }

  const amountBigNumber = new BigNumber(inputAmount);
  const amountClassName = amount ? '' : 'undefined';
  const content =
    shrinkIfLong && inputAmount.length > AMOUNT_RAW_LENGTH_LIMIT
      ? formattedWalletAmount(amountBigNumber, false, false)
      : amountBigNumber.toFormat();

  return <span className={amountClassName}>{content}</span>;
};

export const validateFilterForm = (values: {
  dateRange: DateRangeType,
  customFromDate: string,
  customToDate: string,
  fromAmount: string,
  toAmount: string,
}) => {
  const {
    dateRange,
    customFromDate,
    customToDate,
    fromAmount,
    toAmount,
  } = values;

  if (
    (dateRange === DateRangeTypes.CUSTOM_DATE_RANGE &&
      customFromDate &&
      customToDate &&
      moment(customFromDate).valueOf() > moment(customToDate).valueOf()) ||
    (fromAmount && toAmount && Number(fromAmount) > Number(toAmount))
  ) {
    return false;
  }

  return true;
};
