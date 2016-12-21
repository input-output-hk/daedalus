// @flow
import ClientApi from 'daedalus-client-api';

const notYetImplemented = () => new Promise((resolve, reject) => reject(new Error('Api method not yet implemented')));

export default class CardanoClientApi {

  constructor() {
    // TODO: thats just for a first test, remove later
    ClientApi.getWallets()
      .then((result) => console.log(result))
      .catch((error) => console.log(error));
  }

  login() {
    return notYetImplemented();
  }

  getUser() {
    return notYetImplemented();
  }

  getWallets() {
    return notYetImplemented();
  }

  getTransactions() {
    return notYetImplemented();
  }

  createUser() {
    return notYetImplemented();
  }

  createWallet() {
    return notYetImplemented();
  }

  createTransaction() {
    return notYetImplemented();
  }

  updateProfileField() {
    return notYetImplemented();
  }
}
