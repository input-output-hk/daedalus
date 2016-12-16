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

const fakeRequest = (result: any, requestTime: number = 1000) => {
  let fakeRequestTime = requestTime;
  if (environment.isTest()) fakeRequestTime = 0;
  return new Promise((resolve) => {
    setTimeout(() => { resolve(result); }, fakeRequestTime);
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
    return fakeRequest(this.repository.findUser());
  }

  getWallets(accountId: string) {
    return fakeRequest(this.repository.findWallets(accountId));
  }

  getTransactions(request: getTransactionsRequest) {
    return fakeRequest(this.repository.findTransactions(request));
  }

  createUser(request: createUserRequest) {
    return fakeRequest(this.repository.generateUser(request));
  }

  createWallet(request: createWalletRequest) {
    return fakeRequest(this.repository.generateWallet(request));
  }

  createTransaction(request: createTransactionRequest) {
    return fakeRequest(this.repository.generateTransaction(request, {}));
  }

  updateProfileField(request: updateUserProfileFieldRequest) {
    return fakeRequest(this.repository.updateProfileField(request), 0);
  }
}
