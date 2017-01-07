// @flow
import type {
  getTransactionsRequest,
  createUserRequest,
  createWalletRequest,
  createTransactionRequest,
  updateUserProfileFieldRequest,
  loginRequest
} from './index';
import StubRepository from './StubRepository';
import { user, wallets, transactions } from './fixtures';
import environment from '../environment';
import WalletAddressValidator from 'wallet-address-validator';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import User from '../domain/User';
import Profile from '../domain/Profile';
import { isFunction } from 'lodash';

const fakeRequest = (result: any, requestTime: number = environment.FAKE_RESPONSE_TIME) => {
  let fakeRequestTime = requestTime;
  if (environment.isTest()) fakeRequestTime = 0;
  return new Promise((resolve) => {
    setTimeout(() => {
      if (isFunction(result)) result = result();
      resolve(result);
    }, fakeRequestTime);
  });
};

export default class StubApi {

  repository: StubRepository;

  constructor() {
    this.repository = new StubRepository(user, wallets, transactions);
  }

  login(request: loginRequest) {
    return fakeRequest(this.repository.login(request));
  }

  getUser() {
    const userData = this.repository.findUser();
    return fakeRequest(() => new User(userData.id, new Profile(userData.profile)));
  }

  getWallets(accountId: string) {
    return fakeRequest(this.repository.findWallets(accountId).map(w => new Wallet(w)));
  }

  getTransactions(request: getTransactionsRequest) {
    const result = this.repository.findTransactions(request);
    result.transactions = result.transactions.map(w => new WalletTransaction(w));
    return fakeRequest(result);
  }

  createUser(request: createUserRequest) {
    return fakeRequest(this.repository.generateUser(request));
  }

  createWallet(request: createWalletRequest) {
    return fakeRequest(new Wallet(this.repository.generateWallet(request)));
  }

  createTransaction(request: createTransactionRequest) {
    return fakeRequest(new WalletTransaction(
      this.repository.generateTransaction(Object.assign(request, { amount: -1 * request.amount }))
    ));
  }

  updateProfileField(request: updateUserProfileFieldRequest) {
    return fakeRequest(this.repository.updateProfileField(request), 0);
  }

  isValidAddress(currency: string, address: string) {
    return new Promise(resolve => resolve(WalletAddressValidator.validate(address, 'BTC')));
  }

  getTermsOfUse() {
    return fakeRequest(`
      <h1>Terms of use</h1>
      <p>
      First paragraph quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non
      numquam eius modi tempora incidunt ut labore et dolore some link aliquam quaerat voluptatem.
      </p>
      <p>
        Second paragraph ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut 
        aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse 
        quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?
      </p>
    `);
  }
}
