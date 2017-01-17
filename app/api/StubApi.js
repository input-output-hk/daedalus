// @flow
import { action } from 'mobx';
import type {
  getTransactionsRequest,
  createUserRequest,
  createWalletRequest,
  createTransactionRequest,
  updateUserProfileFieldRequest,
  loginRequest,
  getWalletRecoveryPhraseRequest
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

const fakeRequest = (name: string, result: any, requestTime: number = environment.FAKE_RESPONSE_TIME) => {
  let fakeRequestTime = requestTime;
  if (environment.isTest()) fakeRequestTime = 0;
  return new Promise((resolve) => {
    setTimeout(() => {
      if (isFunction(result)) result = result();
      console.debug(`StubApi::${name} resolving` , result);
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
    console.debug('StubApi::login called with' , request);
    return fakeRequest('login', this.repository.login(request));
  }

  getUser() {
    console.debug('StubApi::getUser called');
    const userData = this.repository.findUser();
    return fakeRequest('getUser', action(() => new User(userData.id, new Profile(userData.profile))));
  }

  getWallets(userId: string) {
    console.debug('StubApi::getWallets called with', userId);
    return fakeRequest('getWallets', action(() => {
      return this.repository.findWallets(userId).map(w => new Wallet(w));
    }));
  }

  getTransactions(request: getTransactionsRequest) {
    console.debug('StubApi::getTransactions called with', request);
    return fakeRequest('getTransactions', action(() => {
      const result = this.repository.findTransactions(request);
      result.transactions = result.transactions.map(w => new WalletTransaction(w));
      return result;
    }));
  }

  getWalletRecoveryPhrase(request: getWalletRecoveryPhraseRequest) {
    console.debug('StubApi::getWalletRecoveryPhrase called with', request);
    return fakeRequest('getWalletRecoveryPhraseRequest', action(() => {
      return this.repository.getWalletRecoveryPhrase(request);
    }));
  }

  setWalletBackupCompleted(walletId: string) {
    console.debug('StubApi::setWalletBackupCompleted called with', walletId);
    return fakeRequest('setWalletBackupCompleted', action(() => {
      return this.repository.setWalletBackupCompleted(walletId);
    }));
  }

  createUser(request: createUserRequest) {
    console.debug('StubApi::createUser called with', request);
    return fakeRequest('createUser', this.repository.generateUser(request));
  }

  createWallet(request: createWalletRequest) {
    console.debug('StubApi::createWallet called with', request);
    return fakeRequest('createWallet',  action(() => {
      return new Wallet(this.repository.generateWallet(request))
    }));
  }

  createTransaction(request: createTransactionRequest) {
    console.debug('StubApi::createTransaction called with', request);
    return fakeRequest('createTransaction',  action(() => {
      const data = this.repository.generateTransaction(
        Object.assign(request, { amount: -1 * request.amount })
      );
      return new WalletTransaction(data);
    }));
  }

  updateProfileField(request: updateUserProfileFieldRequest) {
    console.debug('StubApi::updateProfileField called with', request);
    return fakeRequest('updateProfileField', this.repository.updateProfileField(request), 0);
  }

  isValidAddress(currency: string, address: string) {
    return new Promise(resolve => {
      const result = WalletAddressValidator.validate(address, 'BTC');
      console.debug(`StubApi::isValidAddress resolving currency BTC, address ${address}:`, result);
      resolve(result);
    });
  }

  getTermsOfUse() {
    return fakeRequest('getTermsOfUse', `
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
