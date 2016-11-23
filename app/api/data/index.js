// @flow

import faker from 'faker';
import wallets from '../data/wallets.json';
import account from '../data/account.json';

export default {
  wallets,
  account,
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
  createWallet() {
    const wallet = {
      address: faker.finance.bitcoinAddress(),
      type: 'personal',
      currency: 'ada',
      amount: faker.finance.amount(),
      name: faker.finance.accountName(),
      lastUsed: true
    };
    this.wallets.push(wallet);
    return wallet;
  }
};
