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
    return response.map(wallet => (
      new Wallet({
        id: wallet.cwAddress,
        address: wallet.cwAddress,
        amount: wallet.cwAmount.getCoin,
        type: wallet.cwMeta.cwType,
        currency: wallet.cwMeta.cwCurrency,
        name: wallet.cwMeta.cwName,
      })
    ));
  }

  async getTransactions(request: getTransactionsRequest) {
    const history = JSON.parse(await ClientApi.getHistory(request.walletId)());
    return new Promise((resolve) => resolve({
      transactions: history.map(t => (
        new WalletTransaction({
          id: t.ctId,
          type: t.ctType,
          title: 'TODO',
          currency: 'ada',
          amount: t.ctAmount.getCoin,
          date: new Date()
        })
      )),
      total: history.length
    }));
  }

  createUser() {
    return notYetImplemented();
  }

  createWallet(request: createWalletRequest) {
    return ClientApi.newWallet('personal')(request.currency)(request.name)()
      .then((wallet) => console.log(wallet));
  }

  async createTransaction(request: createTransactionRequest) {
    const response = await JSON.parse(await ClientApi.send(request.receiver)(request.sender)(request.amount)());
    console.log(response);
  }

  updateProfileField() {
    return notYetImplemented();
  }
}
