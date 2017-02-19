// @flow
import ClientApi from 'daedalus-client-api';
import { action } from 'mobx';
import { ipcRenderer } from 'electron';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import type {
  createWalletRequest,
  getTransactionsRequest,
  createTransactionRequest,
  walletRestoreRequest,
  redeemAdaRequest,
  importKeyRequest
} from './index';
import {
  // ApiMethodNotYetImplementedError,
  WalletAlreadyRestoredError,
  RedeemAdaError,
  WalletKeyImportError
} from './errors';

// const notYetImplemented = () => new Promise((_, reject) => {
//   reject(new ApiMethodNotYetImplementedError());
// });

export default class CardanoClientApi {

  notifyCallbacks = [];

  constructor() {
    ClientApi.notify(this._onNotify, this._onNotifyError);
  }

  notify(onSuccess, onError = () => {}) {
    this.notifyCallbacks.push({ message: onSuccess, error: onError });
  }

  reset() {
    this.notifyCallbacks = [];
  }

  async getWallets() {
    console.debug('CardanoClientApi::getWallets called');
    const response = await ClientApi.getWallets();
    return response.map(data => this._createWalletFromData(data));
  }

  async getTransactions(request: getTransactionsRequest) {
    const { walletId, searchTerm, skip, limit } = request;
    console.debug('CardanoClientApi::getTransactions called with', request);
    const history = await ClientApi.searchHistory(walletId, searchTerm, skip, limit);
    return new Promise((resolve) => resolve({
      transactions: history[0].map(data => this._createTransactionFromData(data, walletId)),
      total: history[1]
    }));
  }

  async createWallet(request: createWalletRequest) {
    console.debug('CardanoClientApi::createWallet called with', request);
    const response = await ClientApi.newWallet('CWTPersonal', 'ADA', request.name, request.mnemonic);
    return this._createWalletFromData(response);
  }

  async createTransaction(request: createTransactionRequest) {
    console.debug('CardanoClientApi::createTransaction called with', request);
    const { sender, receiver, amount, currency } = request;
    const description = 'no description provided';
    const title = 'no title provided';
    const response = await ClientApi.sendExtended(sender, receiver, amount, currency, title, description);
    return this._createTransactionFromData(response);
  }

  isValidAddress(currency: string, address: string) {
    return ClientApi.isValidAddress(currency, address);
  }

  isValidMnemonic(mnemonic: string) {
    return ClientApi.isValidMnemonic(mnemonic);
  }

  @action _createWalletFromData(data) {
    return new Wallet({
      id: data.cwAddress,
      address: data.cwAddress,
      amount: data.cwAmount.getCoin,
      type: data.cwMeta.cwType,
      currency: data.cwMeta.cwCurrency,
      name: data.cwMeta.cwName,
    });
  }

  @action _createTransactionFromData(data) {
    const isOutgoing = data.ctType.tag === 'CTOut';
    const coins = data.ctAmount.getCoin;
    let { ctmTitle, ctmDescription, ctmDate } = data.ctType.contents;
    if (!ctmTitle) ctmTitle = 'Incoming Money';
    return new WalletTransaction({
      id: data.ctId,
      title: ctmTitle,
      type: isOutgoing ? 'adaExpend' : 'adaIncome',
      currency: 'ada',
      amount: isOutgoing ? -1 * coins : coins,
      date: new Date(ctmDate * 1000),
      description: ctmDescription,
      numberOfConfirmations: data.ctConfirmations
    });
  }

  getWalletRecoveryPhrase() {
    return new Promise((resolve) => resolve(ClientApi.generateMnemonic().split(' ')));
  }

  async restoreWallet(request: walletRestoreRequest) {
    const { recoveryPhrase, walletName } = request;
    console.debug('CardanoClientApi::restoreWallet called with', request);
    try {
      const restoredWallet = await ClientApi.restoreWallet('CWTPersonal', 'ADA', walletName, recoveryPhrase);
      return this._createWalletFromData(restoredWallet);
    } catch (error) {
      console.error(error);
      // TODO: backend will return something different here, if multiple wallets
      // are restored from the key and if there are duplicate wallets we will get
      // some kind of error and present the user with message that some wallets
      // where not imported/restored if some where. if no wallets are imported
      // we will error out completely with throw block below
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      throw error;
    }
  }

  async importWalletFromKey(request: importKeyRequest) {
    console.debug('CardanoClientApi::importWalletFromKey called with', request);
    try {
      const importedWallet = await ClientApi.importKey(request.filePath);
      return this._createWalletFromData(importedWallet);
    } catch (error) {
      console.error(error);
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      throw new WalletKeyImportError();
    }
  }

  async redeemAda(request: redeemAdaRequest) {
    const { redemptionCode, walletId } = request;
    console.debug('CardanoClientApi::redeemAda called with', request);
    try {
      return await ClientApi.redeemADA(redemptionCode, walletId);
    } catch (error) {
      console.error(error);
      throw new RedeemAdaError();
    }
  }

  generateMnemonic() {
    return ClientApi.generateMnemonic().split(' ');
  }

  // PRIVATE

  _onNotify = (rawMessage) => {
    console.debug('CardanoClientApi::notify message: ', rawMessage);
    // TODO: "ConnectionClosed" messages are not JSON parsable … so we need to catch that case here!
    let message = rawMessage;
    if (message !== "ConnectionClosed") {
      message = JSON.parse(rawMessage);
    }
    this.notifyCallbacks.forEach(cb => cb.message(message));
  };

  _onNotifyError = (error) => {
    console.debug('CardanoClientApi::notify error: ', error);
    this.notifyCallbacks.forEach(cb => cb.error(error));
  };


  async nextUpdate() {
    console.debug('CardanoClientApi::nextUpdate called');
    let nextUpdate = null;
    try {
      nextUpdate = JSON.parse(await ClientApi.nextUpdate());
      console.debug('CardanoClientApi::nextUpdate returned', nextUpdate);
    } catch (error) {
      console.log(error);
      // TODO: Api is trowing an error when update is not available, handle other errors
    }
    return nextUpdate;
    // TODO: remove hardcoded response after node update is tested
    // nextUpdate = {
    //   cuiSoftwareVersion: {
    //     svAppName: {
    //       getApplicationName: "cardano"
    //     },
    //     svNumber: 1
    //   },
    //   cuiBlockVesion: {
    //     bvMajor: 0,
    //     bvMinor: 1,
    //     bvAlt: 0
    //   },
    //   cuiScriptVersion: 1,
    //   cuiImplicit: false,
    //   cuiVotesFor: 2,
    //   cuiVotesAgainst: 0,
    //   cuiPositiveStake: {
    //     getCoin: 66666
    //   },
    //   cuiNegativeStake: {
    //     getCoin: 0
    //   }
    // };
    if (nextUpdate && nextUpdate.cuiSoftwareVersion && nextUpdate.cuiSoftwareVersion.svNumber) {
      return { version: nextUpdate.cuiSoftwareVersion.svNumber};
    } else if (nextUpdate) {
      return { version: 'Unknown'};
    } else {
      return null;
    }
  }

  async applyUpdate() {
    await ClientApi.applyUpdate();
    ipcRenderer.send('kill-process');
  }
}
