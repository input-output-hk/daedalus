// @flow
import faker from 'faker';
import { isString, isDate } from 'lodash';
import randomWords from 'random-words';
import _ from 'lodash';
import type {
  walletStruct,
  userStruct,
  transactionStruct,
  walletRecoveryPhraseStruct,
  getTransactionsRequest,
  updateUserProfileFieldRequest,
  loginRequest,
  getWalletRecoveryPhraseRequest
} from './index';

export default class StubRepository {

  user: userStruct;
  wallets: Array<walletStruct>;
  transactions: Array<transactionStruct>;

  constructor(
    user: userStruct, wallets: Array<walletStruct>, transactions: Array<transactionStruct>
  ) {
    this.user = user;
    this.wallets = wallets;
    this.transactions = transactions;
  }

  login(request: loginRequest) {
    return (
      request.email === this.user.profile.email &&
      request.passwordHash === this.user.profile.passwordHash
    );
  }

  findUser() {
    return this.user;
  }

  findWallets() {
    return this.wallets.map(wallet => _.omit(wallet, ['recoveryPhrase']));
  }

  findTransactions(request: getTransactionsRequest) {
    const { searchTerm } = request;
    const regexp = new RegExp(searchTerm, 'i');
    const filteredTransactions = this.transactions
      .filter((t) => t.walletId === request.walletId) // Filter by walletId
      .filter((t) => regexp.test(t.title)) // Filter by title search
      .sort((a, b) => { // Sort by date
        const aIsSmallerOrEqual = a.date < b.date ? 1 : 0;
        return a.date > b.date ? -1 : aIsSmallerOrEqual;
      });
    return {
      transactions: filteredTransactions.slice(0, request.limit), // Limit number of requests
      total: filteredTransactions.length
    };
  }

  reset() {
    this.user = null;
    this.wallets = [];
    this.transactions = [];
  }

  generateUser(customData: Object) {
    this.user = Object.assign({}, {
      id: faker.random.uuid(),
      profile: {
        name: `${faker.name.firstName()} ${faker.name.lastName()}`,
        email: 'satoshi@gmail.com',
        phoneNumber: faker.phone.phoneNumber(),
        passwordHash: '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',
        passwordUpdateDate: faker.date.past(),
        languageLocale: 'en-US'
      }
    }, customData);
    return this.user;
  }

  generateWallet(customData: Object) {
    const wallet: walletStruct = Object.assign({}, {
      id: faker.random.uuid(),
      userId: this.user.id,
      address: faker.finance.bitcoinAddress(),
      type: 'personal',
      currency: 'ada',
      amount: parseFloat(faker.finance.amount(), 10),
      name: faker.finance.accountName(),
      lastUsed: true,
      isBackupCompleted: false
    }, customData);
    this.wallets.push(Object.assign({}, wallet, { recoveryPhrase: randomWords(12) }));
    return wallet;
  }

  generateTransaction(data: Object) {
    if (isString(data.date)) data.date = new Date(data.date);
    if (!isDate(data.date)) data.date = new Date();
    const transaction: transactionStruct = Object.assign({}, {
      id: faker.finance.bitcoinAddress(),
      type: 'adaExpend',
    }, data);
    this.transactions.push(transaction);
    this.wallets.find(w => w.id === transaction.walletId).amount += transaction.amount;
    return transaction;
  }

  updateProfileField(request: updateUserProfileFieldRequest) {
    this.user.profile[request.field] = request.value;
    return true;
  }

  getWalletRecoveryPhrase(request: getWalletRecoveryPhraseRequest) {
    const { walletId } = request;
    const wallet = this.wallets.find(w => w.id === walletId);
    return wallet.recoveryPhrase;
  }

  setWalletBackupCompleted(walletId: string) {
    const wallet = this.wallets.find(w => w.id === walletId);
    wallet.isBackupCompleted = true;
  }
}
