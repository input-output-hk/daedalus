// @flow

import faker from 'faker';
import { account, wallets, transactions } from './stubs';

export default {
  wallets,
  account,
  transactions,
  reset() {
    this.wallets = [];
    this.account = {};
  },
  createAccount() {
    this.account = {
      profile: {
        name: `${faker.name.firstName()} ${faker.name.lastName()}`,
        email: faker.internet.email(),
        phoneNumber: faker.phone.phoneNumber(),
        passwordUpdateDate: faker.date.past(),
        languageLocale: 'en-US'
      }
    };
    return this.account;
  },
  createWallet(customData: Object) {
    const wallet = Object.assign({}, {
      address: faker.finance.bitcoinAddress(),
      type: 'personal',
      currency: 'ada',
      amount: parseFloat(faker.finance.amount(), 10),
      name: faker.finance.accountName(),
      lastUsed: true
    }, customData);
    this.wallets.push(wallet);
    this.transactions[wallet.address] = [];
    return wallet;
  }
};
