// @flow
import moment from 'moment';
import faker from 'faker';

let transactionsCount = 0;

type transactionType = {
  id: string,
  type: string,
  title: string,
  amount: number,
  currency: string,
  date: Date,
  description: string,
  exchange: ?string,
  conversionRate: ?string,
  transactionId: ?string,
};

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

const walletTransactions = {
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

export const loadWalletTransactions = (data: {
  address: string,
  searchTerm: string,
  limit: number
}) => new Promise((resolve) => {
  setTimeout(() => {
    const { address, searchTerm } = data;
    const regexp = new RegExp(searchTerm, 'i');
    let transactions = [];
    if (walletTransactions[address]) {
      transactions = (
        walletTransactions[address]
        .filter((t) => regexp.test(t.title)) // Filter by title search
        .sort((a, b) => { // Sort by date
          const aIsSmallerOrEqual = a.date < b.date ? 1 : 0;
          return a.date > b.date ? -1 : aIsSmallerOrEqual;
        })
        .slice(0, data.limit) // Limit number of results
      );
    }
    resolve({
      transactions,
      total: transactions.length
    });
  }, 1000);
});
