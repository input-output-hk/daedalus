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
  GenericApiError,
  WalletAlreadyRestoredError,
  RedeemAdaError,
  WalletKeyImportError,
  NotEnoughMoneyToSendError
} from './errors';

// const notYetImplemented = () => new Promise((_, reject) => {
//   reject(new ApiMethodNotYetImplementedError());
// });

// Commented out helper code for testing async APIs
// (async () => {
//   const result = await ClientApi.nextUpdate();
//   console.log('nextUpdate', result);
// })();

// Commented out helper code for testing sync APIs
// (() => {
//   const result = ClientApi.isValidRedeemCode('HSoXEnt9X541uHvtzBpy8vKfTo1C9TkAX3wat2c6ikg=');
//   console.log('isValidRedeemCode', result);
// })();

export default class CardanoClientApi {

  notifyCallbacks = [];

  constructor() {
    ClientApi.notify(this._onNotify, this._onNotifyError);
  }

  notify(onSuccess: Function, onError: Function = () => {}) {
    this.notifyCallbacks.push({ message: onSuccess, error: onError });
  }

  reset() {
    this.notifyCallbacks = [];
  }

  async getWallets() {
    console.debug('CardanoClientApi::getWallets called');
    const response = await ClientApi.getWallets();
    return response.map(data => this._createWalletFromServerData(data));
  }

  async getTransactions(request: getTransactionsRequest) {
    const { walletId, searchTerm, skip, limit } = request;
    console.debug('CardanoClientApi::getTransactions called with', request);
    const history = await ClientApi.searchHistory(walletId, searchTerm, skip, limit);
    return new Promise((resolve) => resolve({
      transactions: history[0].map(data => this._createTransactionFromServerData(data, walletId)),
      total: history[1]
    }));
  }

  async createWallet(request: createWalletRequest) {
    console.debug('CardanoClientApi::createWallet called with', request);
    const response = await ClientApi.newWallet('CWTPersonal', 'ADA', request.name, request.mnemonic);
    return this._createWalletFromServerData(response);
  }

  async createTransaction(request: createTransactionRequest) {
    console.debug('CardanoClientApi::createTransaction called with', request);
    const { sender, receiver, amount, currency } = request;
    const description = 'no description provided';
    const title = 'no title provided';
    try {
      const response = await ClientApi.sendExtended(
        sender, receiver, amount, currency, title, description
      );
      return this._createTransactionFromServerData(response);
    } catch (error) {
      console.error(error);
      if (error.message.includes('Not enough money to send')) {
        throw new NotEnoughMoneyToSendError();
      }
      throw new GenericApiError();
    }
  }

  isValidAddress(currency: string, address: string): Promise<bool> {
    return ClientApi.isValidAddress(currency, address);
  }

  isValidMnemonic(mnemonic: string): Promise<bool> {
    return ClientApi.isValidMnemonic(mnemonic);
  }

  isValidRedemptionKey(mnemonic: string): Promise<bool> {
    return ClientApi.isValidRedeemCode(mnemonic);
  }

  @action _createWalletFromServerData(data: ServerWalletStruct) {
    return new Wallet({
      id: data.cwAddress,
      address: data.cwAddress,
      amount: data.cwAmount.getCoin,
      type: data.cwMeta.cwType,
      currency: data.cwMeta.cwCurrency,
      name: data.cwMeta.cwName,
      unit: data.cwMeta.cwUnit,
    });
  }

  @action _createTransactionFromServerData(data: ServerTransactionStruct) {
    const isOutgoing = data.ctType.tag === 'CTOut';
    const coins = data.ctAmount.getCoin;
    const { ctmTitle, ctmDescription, ctmDate } = data.ctType.contents;
    return new WalletTransaction({
      id: data.ctId,
      title: ctmTitle || isOutgoing ? 'Ada sent' : 'Ada received',
      type: isOutgoing ? 'adaExpend' : 'adaIncome',
      currency: 'ada',
      amount: isOutgoing ? -1 * coins : coins,
      date: new Date(ctmDate * 1000),
      description: ctmDescription || '',
      numberOfConfirmations: data.ctConfirmations,
      assuranceLevel: this._calculateAssuranceLevel(data.ctConfirmations),
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
      return this._createWalletFromServerData(restoredWallet);
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
      return this._createWalletFromServerData(importedWallet);
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
      const response: ServerWalletStruct = await ClientApi.redeemADA(redemptionCode, walletId);
      // TODO: Update response when it is implemented on the backend,
      // currently only wallet is returned
      return this._createWalletFromServerData(response);
    } catch (error) {
      console.error(error);
      throw new RedeemAdaError();
    }
  }

  generateMnemonic() {
    return ClientApi.generateMnemonic().split(' ');
  }

  // PRIVATE

  _calculateAssuranceLevel(numberOfConfirmations: number) {
    let assuranceLevel;
    if (numberOfConfirmations >= 33) {
      assuranceLevel = 'high';
    } else if (numberOfConfirmations >= 9) {
      assuranceLevel = 'medium';
    } else {
      assuranceLevel = 'low';
    }
    return assuranceLevel;
  }

  _onNotify = (rawMessage: string) => {
    console.debug('CardanoClientApi::notify message: ', rawMessage);
    // TODO: "ConnectionClosed" messages are not JSON parsable … so we need to catch that case here!
    let message = rawMessage;
    if (message !== 'ConnectionClosed') {
      message = JSON.parse(rawMessage);
    }
    this.notifyCallbacks.forEach(cb => cb.message(message));
  };

  _onNotifyError = (error: Error) => {
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
    // if (nextUpdate && nextUpdate.cuiSoftwareVersion && nextUpdate.cuiSoftwareVersion.svNumber) {
    //   return { version: nextUpdate.cuiSoftwareVersion.svNumber };
    // } else if (nextUpdate) {
    //   return { version: 'Unknown' };
    // }
    // return null;
  }

  async applyUpdate() {
    await ClientApi.applyUpdate();
    ipcRenderer.send('kill-process');
  }

  async getSyncProgress() {
    console.debug('CardanoClientApi::syncProgress called');
    const response = await ClientApi.syncProgress();
    console.log('CardanoClientApi::syncProgress response', response);
    const localDifficulty = response._spLocalCD.getChainDifficulty;
    // In some cases we dont get network difficulty & we need to wait for it from the notify API
    let networkDifficulty = null;
    if (response._spNetworkCD) networkDifficulty = response._spNetworkCD.getChainDifficulty;
    console.log({ localDifficulty, networkDifficulty });
    return { localDifficulty, networkDifficulty };
  }

  setUserLocale(locale: string) {
    return new Promise((resolve) => {
      // Fake async request here to make it more realistic
      setTimeout(() => resolve(locale), 100);
    });
  }

  setWalletUnit(walletId: string, unit: number) {
    return new Promise((resolve) => {
      // Fake async request here to make it more realistic
      setTimeout(() => resolve(unit), 100);
    });
  }

  testReset() {
    return ClientApi.testReset();
  }
}

type ServerCoinAmountStruct = {
  getCoin: number,
};

type ServerWalletStruct = {
  cwAddress: string,
  cwAmount: ServerCoinAmountStruct,
  cwMeta: {
    cwName: string,
    cwType: string,
    cwCurrency: string,
    cwUnit: number,
  },
}

type ServerTransactionStruct = {
  ctId: string,
  ctType: {
    tag: string,
    contents: {
      ctmDate: Date,
      ctmTitle: ?string,
      ctmDescription: ?string,
    }
  },
  ctAmount: ServerCoinAmountStruct,
  ctConfirmations: number,
}
