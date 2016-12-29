// @flow
import ClientApi from 'daedalus-client-api';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import type {
  createWalletRequest,
  getTransactionsRequest,
  createTransactionRequest,
} from './index';

const notYetImplemented = () => new Promise((resolve, reject) => reject(new Error('Api method not yet implemented')));

export default class CardanoClientApi {

  login() {
    // TODO: Implement when backend is ready for it
    return new Promise((resolve) => resolve(true));
  }

  getUser() {
    return notYetImplemented();
  }

  async getWallets() {
    const response = JSON.parse(await ClientApi.getWallets());
    return response.map(data => this._createWalletFromData(data));
  }

  async getTransactions(request: getTransactionsRequest) {
    const history = JSON.parse(await ClientApi.getHistory(request.walletId)());
    console.log(history);
    return new Promise((resolve) => resolve({
      transactions: history.map(data => this._createTransactionFromData(data)),
      total: history.length
    }));
  }

  createUser() {
    return notYetImplemented();
  }

  async createWallet(request: createWalletRequest) {
    const response = JSON.parse(await ClientApi.newWallet('personal')(request.currency)(request.name)());
    return this._createWalletFromData(response);
  }

  async createTransaction(request: createTransactionRequest) {
    const response = await ClientApi.send(request.sender)(request.receiver)(request.amount)();
    return this._createTransactionFromData(response);
  }

  updateProfileField() {
    return notYetImplemented();
  }

  _createWalletFromData(data) {
    return new Wallet({
      id: data.cwAddress,
      address: data.cwAddress,
      amount: data.cwAmount.getCoin,
      type: data.cwMeta.cwType,
      currency: data.cwMeta.cwCurrency,
      name: data.cwMeta.cwName,
    });
  }

  _createTransactionFromData(data) {
    return new WalletTransaction({
      id: data.ctId,
      type: data.ctType,
      title: 'TODO',
      currency: 'ada',
      amount: data.ctAmount.getCoin,
      date: new Date(),
    });
  }
}

