// @flow
import moment from 'moment';
import faker from 'faker';
import type { walletStruct, userStruct, transactionStruct } from './index';

// ==== Initial stub data for easier development =====

export const user: userStruct = {
  id: faker.random.uuid(),
  profile: {
    name: 'Satoshi Nakamoto',
    email: 'satoshi@gmail.com',
    phoneNumber: '‎+810112714444',
    passwordHash: '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',
    passwordUpdateDate: '2015-11-20T10:18:06.286Z',
    languageLocale: 'en-US'
  }
};

export const wallets: Array<walletStruct> = [
  {
    id: faker.random.uuid(),
    userId: user.id,
    address: faker.finance.bitcoinAddress(),
    type: 'personal',
    currency: 'ada',
    amount: 19903750165.23,
    name: 'Main wallet',
  },
  {
    id: faker.random.uuid(),
    userId: user.id,
    address: faker.finance.bitcoinAddress(),
    type: 'personal',
    currency: 'ada',
    amount: 274912874.35,
    name: 'House rent',
  },
  {
    id: faker.random.uuid(),
    userId: user.id,
    address: faker.finance.bitcoinAddress(),
    type: 'personal',
    currency: 'btc',
    amount: 0.0004924712,
    name: 'Mining',
  },
  {
    id: faker.random.uuid(),
    userId: user.id,
    address: faker.finance.bitcoinAddress(),
    type: 'personal',
    currency: 'ada',
    amount: 2500.00,
    name: 'Transporting',
  },
  {
    id: faker.random.uuid(),
    userId: user.id,
    address: faker.finance.bitcoinAddress(),
    type: 'personal',
    currency: 'btc',
    amount: 0.02048244,
    name: 'Pocket money',
  }
];

let transactionsCount = 0;

const generateTransaction = (data: Object): transactionStruct => {
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

const cardTransaction = (walletId: string, date: ?Date) => {
  const amount = -1 * ((Math.random() * 1000) + 1);
  return generateTransaction({
    walletId,
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
  walletId: string,
  amount: number,
  type: string,
  title: string,
  date: ?Date
}) => {
  const exchangeRate = (Math.random() * 1000) + 10;
  const { amount, type, title, date, walletId } = data;
  return generateTransaction({
    walletId,
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

const adaExpend = (walletId: string, date: ?Date) => {
  const amount = -1 * ((Math.random() * 1000) + 1);
  return adaTransaction({
    walletId,
    amount,
    date,
    type: 'adaExpend',
    title: `Money to ${faker.name.firstName()}`
  });
};

const adaIncome = (walletId: string, date: ?Date) => {
  const amount = (Math.random() * 1000) + 1;
  return adaTransaction({
    walletId,
    amount,
    date,
    type: 'adaIncome',
    title: `Money from ${faker.name.firstName()}`,
  });
};

const exchange = (walletId: string, date: ?Date) => {
  const amount = (Math.random() * 1000) + 1;
  const exchangeRate = (Math.random() * 1000) + 10;
  return generateTransaction({
    walletId,
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

const generateRandomTransactions = (walletId: string, count: number) => {
  const transactions = [];
  for (let i = 0; i < count; i += 1) {
    const typeFactory = Math.floor(Math.random() * (transactionTypes.length - 1));
    transactions.push(transactionTypes[typeFactory](walletId));
  }
  return transactions;
};

const firstWallet = wallets[0].id;
const secondWallet = wallets[1].id;
const thirdWallet = wallets[2].id;

export const transactions = generateRandomTransactions(firstWallet, 30).concat([
  cardTransaction(firstWallet, new Date()),
  adaExpend(firstWallet, new Date()),
  adaIncome(firstWallet, moment().subtract(1, 'days').toDate()),
  cardTransaction(secondWallet, moment().subtract(1, 'days').toDate()),
  adaExpend(secondWallet, new Date()),
  adaIncome(secondWallet),
  cardTransaction(thirdWallet, moment().subtract(1, 'days').toDate()),
  exchange(thirdWallet),
  cardTransaction(wallets[3].id, new Date()),
  exchange(wallets[4].id, new Date())
]);
