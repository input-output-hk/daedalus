import BigNumber from 'bignumber.js';
import moment from 'moment';
import React from 'react';
import type { CoinSelectionsResponse } from '../api/transactions/types';
import {
  TransactionTypes,
  WalletTransaction,
} from '../domains/WalletTransaction';
import type {
  ByronEncodeSignedTransactionRequest,
  ByronSignedTransactionWitnesses,
} from '../stores/HardwareWalletsStore';
import type { TransactionFilterOptionsType } from '../stores/TransactionsStore';
import { DateRangeTypes } from '../stores/TransactionsStore';
import { formattedWalletAmount } from './formatters';

const cbor = require('cbor');

const bs58 = require('bs58');

const AMOUNT_RAW_LENGTH_LIMIT = 10;
export const generateFilterOptions = (
  transactions: Array<WalletTransaction>
) => {
  const dates = transactions
    .filter(({ date }) => !!date)
    .map(({ date }) => (date ? date.getTime() : 0));
  const amounts = transactions.map(({ amount }) => amount.absoluteValue());
  const dateRange = DateRangeTypes.CUSTOM;
  const fromDate =
    dates.length > 0 ? moment(Math.min(...dates)).format('YYYY-MM-DD') : '';
  const toDate =
    dates.length > 0 ? moment(Math.max(...dates)).format('YYYY-MM-DD') : '';
  const fromAmount =
    amounts.length > 0 ? BigNumber.min(...amounts).toString() : '';
  const toAmount =
    amounts.length > 0 ? BigNumber.max(...amounts).toString() : '';
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
    ? date.getTime() >= moment(fromDate).startOf('day').valueOf()
    : true;
  const compareTo = toDate
    ? date.getTime() <= moment(toDate).endOf('day').valueOf()
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
    ? amount.absoluteValue().isGreaterThanOrEqualTo(min)
    : true;
  const compareTo = toAmount
    ? amount.absoluteValue().isLessThanOrEqualTo(max)
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
  filterOptions: TransactionFilterOptionsType | null | undefined,
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
  filterOptions: TransactionFilterOptionsType | null | undefined
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

  if (dateRange && (fromDate || toDate)) {
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
  dateRangeFromTo: {
    fromDate: string;
    toDate: string;
  }
) => {
  const { fromDate: fromValue, toDate: toValue } = dateRangeFromTo;
  let fromDate = null;
  let toDate = null;

  if (!dateRange) {
    fromDate = '';
    toDate = '';
  } else if (dateRange === DateRangeTypes.CUSTOM) {
    fromDate = fromValue;
    toDate = toValue;
  } else {
    if (dateRange === DateRangeTypes.LAST_7_DAYS) {
      fromDate = moment().subtract(6, 'days');
    } else if (dateRange === DateRangeTypes.LAST_30_DAYS) {
      fromDate = moment().subtract(29, 'days');
    } else if (dateRange === DateRangeTypes.LAST_90_DAYS) {
      fromDate = moment().subtract(89, 'days');
    } else if (dateRange === DateRangeTypes.THIS_YEAR) {
      fromDate = moment().startOf('year');
    } else {
      fromDate = moment();
    }

    fromDate = fromDate.format('YYYY-MM-DD');
    toDate = moment().format('YYYY-MM-DD');
  }

  return {
    fromDate,
    toDate,
  };
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
  fromDate: string;
  toDate: string;
  fromAmount: string;
  toAmount: string;
}) => {
  const { fromDate, toDate, fromAmount, toAmount } = values;
  const invalidFields = {
    toDate: false,
    toAmount: false,
  };

  if (
    fromDate &&
    toDate &&
    moment(fromDate).valueOf() > moment(toDate).valueOf()
  ) {
    invalidFields.toDate = true;
  }

  if (
    fromAmount &&
    toAmount &&
    new BigNumber(fromAmount).isGreaterThan(new BigNumber(toAmount))
  ) {
    invalidFields.toAmount = true;
  }

  return {
    isValid: !invalidFields.toDate && !invalidFields.toAmount,
    invalidFields,
  };
};

class List {
  constructor(xs) {
    // @ts-ignore
    this.elems = xs;
  }

  encodeCBOR(encoder) {
    encoder.push(rawBuffer('9F')); // Begin Indefinite list

    // @ts-ignore
    this.elems.forEach((el) => encoder.pushAny(el));
    encoder.push(rawBuffer('FF')); // Break Indefinite list

    return true;
  }
}

export const thDataHexGenerator = (txData: CoinSelectionsResponse) => {
  const txDataHex = encodeTransaction(txData).toString('hex');
  return txDataHex;
};
export const encodeSignedTransaction = ({
  txDataHex,
  witnesses,
}: ByronEncodeSignedTransactionRequest) => {
  //  {
  //    txDataHex: '01f54c866c778568c01b9e4c0a2cbab29e0af285623404e0ef922c6b63f9b222',
  //    witnesses: [
  //      {
  //        signature: 'f89f0d3e2ad34a29c36d9eebdceb951088b52d33638d0f55d49ba2f8baff6e29056720be55fd2eb7198c05b424ce4308eaeed7195310e5879c41c1743245b000'
  //        xpub: { // see 'getExtendedPublicKey'
  //          publicKeyHex: '...', // hex-encoded string
  //          chainCodeHex: '...', // hex-encoded string
  //        }
  //      }
  //    ]
  //  }
  return Buffer.concat([
    rawBuffer('82'),
    rawBuffer(txDataHex),
    cbor.encode(witnesses.map(encodeWitness)),
  ]).toString('hex');
};

const rawBuffer = (str) => {
  return Buffer.from(str, 'hex');
};

const encodeWitness = ({
  signature,
  xpub,
}: ByronSignedTransactionWitnesses) => {
  const witness = [
    Buffer.concat([rawBuffer(xpub.publicKeyHex), rawBuffer(xpub.chainCodeHex)]),
    rawBuffer(signature),
  ];
  return [0, new cbor.Tagged(24, cbor.encode(witness))];
};

const encodeTransaction = (data) => {
  return cbor.encode([
    encodeTransactionInputs(data.inputs),
    encodeTransactionOutputs(data.outputs),
    {}, // always empty in Byron
  ]);
};

const encodeTransactionInputs = (inps) => {
  return new List(
    inps.map((i) => [0, new cbor.Tagged(24, encodeTransactionInput(i))])
  );
};

const encodeTransactionInput = ({ id, index }) => {
  return cbor.encode([rawBuffer(id), index]);
};

const encodeTransactionOutputs = (outs) => {
  return new List(
    outs.map((o) => [encodeTransactionAddress(o.address), o.amount.quantity])
  );
};

const encodeTransactionAddress = (addr) => {
  const bytes = bs58.decode(addr);
  return cbor.decodeFirstSync(bytes);
};
