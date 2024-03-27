'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.encodeSignedTransaction = exports.thDataHexGenerator = exports.validateFilterForm = exports.formatAmountValue = exports.formatDateValue = exports.calculateDateRange = exports.getNumberOfFilterDimensionsApplied = exports.isTransactionInFilterRange = exports.isTransactionTitleInFilterRange = exports.isTransactionTypeInFilterRange = exports.isTransactionAmountInFilterRange = exports.isTransactionDateInFilterRange = exports.generateFilterOptions = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const moment_1 = __importDefault(require('moment'));
const react_1 = __importDefault(require('react'));
const WalletTransaction_1 = require('../domains/WalletTransaction');
const TransactionsStore_1 = require('../stores/TransactionsStore');
const formatters_1 = require('./formatters');
const cbor = require('cbor');
const bs58 = require('bs58');
const AMOUNT_RAW_LENGTH_LIMIT = 10;
const generateFilterOptions = (transactions) => {
  const dates = transactions
    .filter(({ date }) => !!date)
    .map(({ date }) => (date ? date.getTime() : 0));
  const amounts = transactions.map(({ amount }) => amount.absoluteValue());
  const dateRange = TransactionsStore_1.DateRangeTypes.CUSTOM;
  const fromDate =
    dates.length > 0
      ? (0, moment_1.default)(Math.min(...dates)).format('YYYY-MM-DD')
      : '';
  const toDate =
    dates.length > 0
      ? (0, moment_1.default)(Math.max(...dates)).format('YYYY-MM-DD')
      : '';
  const fromAmount =
    amounts.length > 0 ? bignumber_js_1.default.min(...amounts).toString() : '';
  const toAmount =
    amounts.length > 0 ? bignumber_js_1.default.max(...amounts).toString() : '';
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
exports.generateFilterOptions = generateFilterOptions;
const isTransactionDateInFilterRange = (fromDate, toDate, transaction) => {
  const { date } = transaction;
  if (!date) {
    return true;
  }
  const compareFrom = fromDate
    ? date.getTime() >= (0, moment_1.default)(fromDate).startOf('day').valueOf()
    : true;
  const compareTo = toDate
    ? date.getTime() <= (0, moment_1.default)(toDate).endOf('day').valueOf()
    : true;
  return compareFrom && compareTo;
};
exports.isTransactionDateInFilterRange = isTransactionDateInFilterRange;
const isTransactionAmountInFilterRange = (
  fromAmount,
  toAmount,
  transaction
) => {
  const { amount } = transaction;
  const min =
    fromAmount === '.' || fromAmount === ''
      ? new bignumber_js_1.default(0)
      : new bignumber_js_1.default(fromAmount);
  const max =
    toAmount === '.' || toAmount === ''
      ? new bignumber_js_1.default(0)
      : new bignumber_js_1.default(toAmount);
  const compareFrom = fromAmount
    ? amount.absoluteValue().isGreaterThanOrEqualTo(min)
    : true;
  const compareTo = toAmount
    ? amount.absoluteValue().isLessThanOrEqualTo(max)
    : true;
  return compareFrom && compareTo;
};
exports.isTransactionAmountInFilterRange = isTransactionAmountInFilterRange;
const isTransactionTypeInFilterRange = (
  incomingChecked,
  outgoingChecked,
  transaction
) => {
  const { type } = transaction;
  if (
    (!incomingChecked &&
      type === WalletTransaction_1.TransactionTypes.INCOME) ||
    (!outgoingChecked && type === WalletTransaction_1.TransactionTypes.EXPEND)
  ) {
    return false;
  }
  return true;
};
exports.isTransactionTypeInFilterRange = isTransactionTypeInFilterRange;
const isTransactionTitleInFilterRange = (searchTerm, transaction) => {
  if (!searchTerm) {
    return true;
  }
  return transaction.title.search(new RegExp(searchTerm, 'i')) !== -1;
};
exports.isTransactionTitleInFilterRange = isTransactionTitleInFilterRange;
const isTransactionInFilterRange = (filterOptions, transaction) => {
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
    (0, exports.isTransactionTitleInFilterRange)(searchTerm, transaction) &&
    (0, exports.isTransactionDateInFilterRange)(
      fromDate,
      toDate,
      transaction
    ) &&
    (0, exports.isTransactionAmountInFilterRange)(
      fromAmount,
      toAmount,
      transaction
    ) &&
    (0, exports.isTransactionTypeInFilterRange)(
      incomingChecked,
      outgoingChecked,
      transaction
    )
  );
};
exports.isTransactionInFilterRange = isTransactionInFilterRange;
const getNumberOfFilterDimensionsApplied = (filterOptions) => {
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
exports.getNumberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied;
const calculateDateRange = (dateRange, dateRangeFromTo) => {
  const { fromDate: fromValue, toDate: toValue } = dateRangeFromTo;
  let fromDate = null;
  let toDate = null;
  if (!dateRange) {
    fromDate = '';
    toDate = '';
  } else if (dateRange === TransactionsStore_1.DateRangeTypes.CUSTOM) {
    fromDate = fromValue;
    toDate = toValue;
  } else {
    if (dateRange === TransactionsStore_1.DateRangeTypes.LAST_7_DAYS) {
      fromDate = (0, moment_1.default)().subtract(6, 'days');
    } else if (dateRange === TransactionsStore_1.DateRangeTypes.LAST_30_DAYS) {
      fromDate = (0, moment_1.default)().subtract(29, 'days');
    } else if (dateRange === TransactionsStore_1.DateRangeTypes.LAST_90_DAYS) {
      fromDate = (0, moment_1.default)().subtract(89, 'days');
    } else if (dateRange === TransactionsStore_1.DateRangeTypes.THIS_YEAR) {
      fromDate = (0, moment_1.default)().startOf('year');
    } else {
      fromDate = (0, moment_1.default)();
    }
    fromDate = fromDate.format('YYYY-MM-DD');
    toDate = (0, moment_1.default)().format('YYYY-MM-DD');
  }
  return {
    fromDate,
    toDate,
  };
};
exports.calculateDateRange = calculateDateRange;
const formatDateValue = (date, defaultDate, dateFormat) => {
  if (!date) {
    const formattedDefaultDate = (0, moment_1.default)(defaultDate).format(
      dateFormat
    );
    return react_1.default.createElement(
      'span',
      { className: 'undefined' },
      formattedDefaultDate
    );
  }
  return (0, moment_1.default)(date).format(dateFormat);
};
exports.formatDateValue = formatDateValue;
const formatAmountValue = (amount, defaultAmount, shrinkIfLong) => {
  let inputAmount = amount || defaultAmount;
  if (inputAmount === '.') {
    inputAmount = '0';
  } else if (inputAmount[0] === '.') {
    inputAmount = `0${inputAmount}`;
  } else if (inputAmount[inputAmount.length - 1] === '.') {
    inputAmount = `${inputAmount}0`;
  }
  const amountBigNumber = new bignumber_js_1.default(inputAmount);
  const amountClassName = amount ? '' : 'undefined';
  const content =
    shrinkIfLong && inputAmount.length > AMOUNT_RAW_LENGTH_LIMIT
      ? (0, formatters_1.formattedWalletAmount)(amountBigNumber, false, false)
      : amountBigNumber.toFormat();
  return react_1.default.createElement(
    'span',
    { className: amountClassName },
    content
  );
};
exports.formatAmountValue = formatAmountValue;
const validateFilterForm = (values) => {
  const { fromDate, toDate, fromAmount, toAmount } = values;
  const invalidFields = {
    toDate: false,
    toAmount: false,
  };
  if (
    fromDate &&
    toDate &&
    (0, moment_1.default)(fromDate).valueOf() >
      (0, moment_1.default)(toDate).valueOf()
  ) {
    invalidFields.toDate = true;
  }
  if (
    fromAmount &&
    toAmount &&
    new bignumber_js_1.default(fromAmount).isGreaterThan(
      new bignumber_js_1.default(toAmount)
    )
  ) {
    invalidFields.toAmount = true;
  }
  return {
    isValid: !invalidFields.toDate && !invalidFields.toAmount,
    invalidFields,
  };
};
exports.validateFilterForm = validateFilterForm;
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
const thDataHexGenerator = (txData) => {
  const txDataHex = encodeTransaction(txData).toString('hex');
  return txDataHex;
};
exports.thDataHexGenerator = thDataHexGenerator;
const encodeSignedTransaction = ({ txDataHex, witnesses }) => {
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
exports.encodeSignedTransaction = encodeSignedTransaction;
const rawBuffer = (str) => {
  return Buffer.from(str, 'hex');
};
const encodeWitness = ({ signature, xpub }) => {
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
//# sourceMappingURL=transaction.js.map
