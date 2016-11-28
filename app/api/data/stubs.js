// @flow
import moment from 'moment';
import faker from 'faker';
import type { walletType, accountType, transactionType } from './types';

// ==== Initial stub data for easier development =====

export const account: accountType = {
  profile: {
    name: 'Satoshi Nakamoto',
    email: 'satoshi@gmail.com',
    phoneNumber: '‎+810112714444',
    passwordHash: '961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506',
    passwordUpdateDate: '2015-11-20T10:18:06.286Z',
    languageLocale: 'en-US'
  }
};

export const wallets: Array<walletType> = [
  {
    address: '13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x',
    type: 'personal',
    currency: 'ada',
    amount: 19903750165.23,
    name: 'Main wallet',
    lastUsed: true,
  },
  {
    address: '1GrT8upXQCR4mzPpAeMfxqwZh2LF7PXsyn',
    type: 'personal',
    currency: 'ada',
    amount: 274912874.35,
    name: 'House rent',
    lastUsed: null,
  },
  {
    address: '1LYYdFD4RGLsYJT5tVQoLQEvfr7hJXHRuv',
    type: 'personal',
    currency: 'btc',
    amount: 0.0004924712,
    name: 'Mining',
    lastUsed: null,
  },
  {
    address: '1MKY9oAriyXvUKRBRsbrrT2JDHA8S9T2sf',
    type: 'personal',
    currency: 'ada',
    amount: 2500.00,
    name: 'Transporting',
    lastUsed: null,
  },
  {
    address: 'fd17fda6410ac905447840e33c1851a5',
    type: 'personal',
    currency: 'btc',
    amount: 0.02048244,
    name: 'Pocket money',
    lastUsed: null,
  }
];

let transactionsCount = 0;

const generateTransaction = (data: Object): transactionType => {
  transactionsCount += 1;
  let date = data.date;
  if (data.date == null) {
    date = moment().subtract(Math.floor(Math.random() * 10), 'days').toDate();
  }
  return Object.assign({}, {
    id: `t-id-${transactionsCount}`,
    description: faker.lorem.sentence(),
  }, data, { date });
};

const cardTransaction = (date: ?Date) => {
  const amount = -1 * ((Math.random() * 1000) + 1);
  return generateTransaction({
    amount,
    date,
    type: 'card',
    currency: '$',
    title: `Invoice to ${faker.company.companyName()}`,
    exchange: null,
    conversionRate: null,
    transactionId: null,
  });
};

const adaTransaction = (data: {
  amount: number,
  type: string,
  title: string,
  date: ?Date
}) => {
  const exchangeRate = (Math.random() * 1000) + 10;
  const { amount, type, title, date } = data;
  return generateTransaction({
    type,
    amount,
    title,
    date,
    currency: 'ada',
    exchange: `${amount.toFixed(2)} ADA for ${(amount / exchangeRate).toFixed(2)} USD`,
    conversionRate: `1 USD = ${exchangeRate.toFixed(2)} ADA`,
    transactionId: faker.finance.bitcoinAddress(),
  });
};

const adaExpend = (date: ?Date) => {
  const amount = -1 * ((Math.random() * 1000) + 1);
  return adaTransaction({
    amount,
    date,
    type: 'adaExpend',
    title: `Money to ${faker.name.firstName()}`
  });
};

const adaIncome = (date: ?Date) => {
  const amount = (Math.random() * 1000) + 1;
  return adaTransaction({
    amount,
    date,
    type: 'adaIncome',
    title: `Money from ${faker.name.firstName()}`,
  });
};

const exchange = (date: ?Date) => {
  const amount = (Math.random() * 1000) + 1;
  const exchangeRate = (Math.random() * 1000) + 10;
  return generateTransaction({
    amount,
    date,
    title: 'ADA to ETH',
    type: 'exchange',
    currency: 'ada',
    exchange: `${amount.toFixed(2)} ADA for ${(amount / exchangeRate).toFixed(2)} ETH`,
    conversionRate: `1 ETH = ${exchangeRate.toFixed(2)} ADA`,
    transactionId: faker.finance.bitcoinAddress(),
  });
};

const transactionTypes = [cardTransaction, adaExpend, adaIncome, exchange];

const generateRandomTransactions = (count: number) => {
  const transactions = [];
  for (let i = 0; i < count; i += 1) {
    const typeFactory = Math.floor(Math.random() * (transactionTypes.length - 1));
    transactions.push(transactionTypes[typeFactory]());
  }
  return transactions;
};

export const transactions = {
  '13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x': [
    cardTransaction(new Date()),
    adaExpend(new Date()),
    adaIncome(moment().subtract(1, 'days').toDate())
  ].concat(generateRandomTransactions(30)),
  '1GrT8upXQCR4mzPpAeMfxqwZh2LF7PXsyn': [
    cardTransaction(moment().subtract(1, 'days').toDate()),
    adaExpend(new Date()),
    adaIncome()
  ],
  '1LYYdFD4RGLsYJT5tVQoLQEvfr7hJXHRuv': [
    cardTransaction(moment().subtract(1, 'days').toDate()),
    exchange()
  ],
  '1MKY9oAriyXvUKRBRsbrrT2JDHA8S9T2sf': [
    cardTransaction(new Date()),
  ],
  fd17fda6410ac905447840e33c1851a5: [
    exchange(new Date())
  ]
};
