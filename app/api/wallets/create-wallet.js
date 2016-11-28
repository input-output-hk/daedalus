// @flow
import faker from 'faker';
import data from '../data';

export const createPersonalWallet = (request: {
  name: string,
  currency: string
}) => new Promise((resolve) => {
  const wallet = Object.assign({}, request, {
    type: 'personal',
    address: faker.finance.bitcoinAddress(),
    amount: 0,
  });
  data.wallets.push(wallet);
  data.transactions[wallet.address] = [];
  resolve(wallet);
});
