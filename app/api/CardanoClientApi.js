// @flow
import localStorage from 'electron-json-storage';
import ClientApi from 'daedalus-client-api';
import { action } from 'mobx';
import { ipcRenderer } from 'electron';
import Log from 'electron-log';
import BigNumber from 'bignumber.js';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import type {
  CreateWalletRequest,
  GetTransactionsRequest,
  CreateTransactionRequest,
  RestoreWalletRequest,
  UpdateWalletRequest,
  RedeemAdaRequest,
  ImportKeyRequest,
  DeleteWalletRequest,
  RedeemPaperVendedAdaRequest,
  ChangeWalletPasswordRequest,
  ChangeWalletPasswordResponse,
  SetWalletPasswordRequest,
  SetWalletPasswordResponse,
} from './index';
import {
  // ApiMethodNotYetImplementedError,
  GenericApiError,
  WalletAlreadyRestoredError,
  RedeemAdaError,
  WalletKeyImportError,
  NotEnoughMoneyToSendError,
} from './errors';
import type { AssuranceModeOption } from '../types/transactionAssuranceTypes';
import { LOVELACES_PER_ADA } from '../config/numbersConfig';

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

const getUserLocaleFromLocalStorage = () => new Promise((resolve, reject) => {
  localStorage.get('userLocale', (error, response) => {
    if (error) return reject(error);
    if (!response.locale) return resolve('');
    resolve(response.locale);
  });
});

const setUserLocaleInLocalStorage = (locale) => new Promise((resolve, reject) => {
  localStorage.set('userLocale', { locale }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

const unsetUserLocaleInLocalStorage = () => new Promise((resolve) => {
  localStorage.remove('userLocale', () => {
    resolve();
  });
});

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
    Log.debug('CardanoClientApi::getWallets called');
    try {
      const response = await ClientApi.getWallets();
      Log.debug('CardanoClientApi::getWallets success: ', JSON.stringify(response, null, 2));
      return response.map(data => _createWalletFromServerData(data));
    } catch (error) {
      Log.error('CardanoClientApi::getWallets error: ', error);
      throw new GenericApiError();
    }
  }

  async getTransactions(request: GetTransactionsRequest) {
    Log.debug('CardanoClientApi::searchHistory called: ', JSON.stringify(request, null, 2));
    const { walletId, searchTerm, skip, limit } = request;
    try {
      const history = await ClientApi.searchHistory(walletId, searchTerm, skip, limit);
      Log.debug('CardanoClientApi::searchHistory success: ', JSON.stringify(history, null, 2));
      return new Promise((resolve) => resolve({
        transactions: history[0].map(data => _createTransactionFromServerData(data, walletId)),
        total: history[1]
      }));
    } catch (error) {
      Log.error('CardanoClientApi::searchHistory error: ', error);
      throw new GenericApiError();
    }
  }

  async createWallet(request: CreateWalletRequest) {
    Log.debug('CardanoClientApi::createWallet called');
    try {
      const response = await ClientApi.newWallet('CWTPersonal', 'ADA', request.name, request.mnemonic);
      Log.debug('CardanoClientApi::createWallet success: ', JSON.stringify(response, null, 2));
      return _createWalletFromServerData(response);
    } catch (error) {
      Log.error('CardanoClientApi::createWallet error: ', error);
      throw new GenericApiError();
    }
  }

  async deleteWallet(request: DeleteWalletRequest) {
    Log.debug('CardanoClientApi::deleteWallet called: ', JSON.stringify(request, null, 2));
    try {
      await ClientApi.deleteWallet(request.walletId);
      Log.debug('CardanoClientApi::deleteWallet success: ', JSON.stringify(request, null, 2));
      return true;
    } catch (error) {
      Log.error('CardanoClientApi::deleteWallet error: ', error);
      throw new GenericApiError();
    }
  }

  async createTransaction(request: CreateTransactionRequest) {
    Log.debug('CardanoClientApi::createTransaction called: ', JSON.stringify(request, null, 2));
    const { sender, receiver, amount, currency } = request;
    const description = 'no description provided';
    const title = 'no title provided';
    try {
      const response = await ClientApi.sendExtended(
        sender, receiver, amount, currency, title, description
      );
      Log.debug('CardanoClientApi::createTransaction success: ', JSON.stringify(response, null, 2));
      return _createTransactionFromServerData(response);
    } catch (error) {
      Log.error('CardanoClientApi::createTransaction error: ', error);
      if (error.message.includes('Not enough money to send')) {
        throw new NotEnoughMoneyToSendError();
      }
      throw new GenericApiError();
    }
  }

  isValidAddress(currency: string, address: string): Promise<boolean> {
    return ClientApi.isValidAddress(currency, address);
  }

  isValidMnemonic(mnemonic: string): Promise<boolean> { // eslint-disable-line
    return ClientApi.isValidMnemonic(12, mnemonic);
  }

  isValidRedemptionKey(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidRedemptionKey(mnemonic);
  }

  isValidPaperVendRedemptionKey(redeemCode: string): Promise<boolean> {
    return ClientApi.isValidPaperVendRedemptionKey(redeemCode);
  }

  isValidRedemptionMnemonic(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidMnemonic(9, mnemonic);
  }

  getWalletRecoveryPhrase() {
    Log.debug('CardanoClientApi::getWalletRecoveryPhrase called');
    try {
      const response = new Promise((resolve) => resolve(ClientApi.generateMnemonic().split(' ')));
      Log.debug('CardanoClientApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Log.error('CardanoClientApi::getWalletRecoveryPhrase error: ', error);
      throw new GenericApiError();
    }
  }

  async restoreWallet(request: RestoreWalletRequest) {
    Log.debug('CardanoClientApi::restoreWallet called');
    const { recoveryPhrase, walletName } = request;
    try {
      const restoredWallet = await ClientApi.restoreWallet('CWTPersonal', 'ADA', walletName, recoveryPhrase);
      Log.debug('CardanoClientApi::restoreWallet success');
      return _createWalletFromServerData(restoredWallet);
    } catch (error) {
      Log.error('CardanoClientApi::restoreWallet error: ', error);
      // TODO: backend will return something different here, if multiple wallets
      // are restored from the key and if there are duplicate wallets we will get
      // some kind of error and present the user with message that some wallets
      // where not imported/restored if some where. if no wallets are imported
      // we will error out completely with throw block below
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      // We don't know what the problem was -> throw generic error
      throw new GenericApiError();
    }
  }

  async importWalletFromKey(request: ImportKeyRequest) {
    Log.debug('CardanoClientApi::importWalletFromKey called');
    try {
      const importedWallet = await ClientApi.importKey(request.filePath);
      Log.debug('CardanoClientApi::importWalletFromKey success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Log.error('CardanoClientApi::importWalletFromKey error: ', error);
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      throw new WalletKeyImportError();
    }
  }

  async redeemAda(request: RedeemAdaRequest) {
    Log.debug('CardanoClientApi::redeemAda called');
    const { redemptionCode, walletId } = request;
    try {
      const response: ServerWalletStruct = await ClientApi.redeemAda(redemptionCode, walletId);
      Log.debug('CardanoClientApi::redeemAda success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Log.error('CardanoClientApi::redeemAda error: ', error);
      throw new RedeemAdaError();
    }
  }

  async redeemPaperVendedAda(request: RedeemPaperVendedAdaRequest) {
    Log.debug('CardanoClientApi::redeemAdaPaperVend called');
    const { shieldedRedemptionKey, mnemonics, walletId } = request;
    try {
      const response: ServerWalletStruct =
        await ClientApi.redeemAdaPaperVend(shieldedRedemptionKey, mnemonics, walletId);
      Log.debug('CardanoClientApi::redeemAdaPaperVend success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Log.error('CardanoClientApi::redeemAdaPaperVend error: ', error);
      throw new RedeemAdaError();
    }
  }

  generateMnemonic() {
    Log.debug('CardanoClientApi::generateMnemonic called');
    try {
      const response = ClientApi.generateMnemonic().split(' ');
      Log.debug('CardanoClientApi::generateMnemonic success');
      return response;
    } catch (error) {
      Log.error('CardanoClientApi::generateMnemonic error: ', error);
      throw new GenericApiError();
    }
  }

  // PRIVATE

  _onNotify = (rawMessage: string) => {
    Log.debug('CardanoClientApi::notify message: ', rawMessage);
    // TODO: "ConnectionClosed" messages are not JSON parsable … so we need to catch that case here!
    let message = rawMessage;
    if (message !== 'ConnectionClosed') {
      message = JSON.parse(rawMessage);
    }
    this.notifyCallbacks.forEach(cb => cb.message(message));
  };

  _onNotifyError = (error: Error) => {
    Log.error('CardanoClientApi::notify error: ', error);
    this.notifyCallbacks.forEach(cb => cb.error(error));
  };

  async nextUpdate() {
    Log.debug('CardanoClientApi::nextUpdate called');
    let nextUpdate = null;
    try {
      nextUpdate = JSON.parse(await ClientApi.nextUpdate());
      Log.debug('CardanoClientApi::nextUpdate success: ', JSON.stringify(nextUpdate, null, 2));
    } catch (error) {
      Log.error('CardanoClientApi::nextUpdate error: ', error);
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
    Log.debug('CardanoClientApi::applyUpdate called');
    try {
      const response = await ClientApi.applyUpdate();
      Log.debug('CardanoClientApi::applyUpdate success: ', JSON.stringify(response, null, 2));
      ipcRenderer.send('kill-process');
    } catch (error) {
      Log.error('CardanoClientApi::applyUpdate error: ', error);
      throw new GenericApiError();
    }
  }

  async getSyncProgress() {
    Log.debug('CardanoClientApi::syncProgress called');
    try {
      const response = await ClientApi.syncProgress();
      Log.debug('CardanoClientApi::syncProgress success: ', JSON.stringify(response, null, 2));
      const localDifficulty = response._spLocalCD.getChainDifficulty;
      // In some cases we dont get network difficulty & we need to wait for it from the notify API
      let networkDifficulty = null;
      if (response._spNetworkCD) networkDifficulty = response._spNetworkCD.getChainDifficulty;
      return { localDifficulty, networkDifficulty };
    } catch (error) {
      Log.error('CardanoClientApi::syncProgress error: ', error);
      throw new GenericApiError();
    }
  }

  async setUserLocale(locale: string) {
    Log.debug('CardanoClientApi::updateLocale called: ', locale);
    try {
      await setUserLocaleInLocalStorage(locale);
      Log.debug('CardanoClientApi::updateLocale success: ', locale);
      return locale;
    } catch (error) {
      Log.error('CardanoClientApi::updateLocale error: ', error);
      throw new GenericApiError();
    }
  }

  async getUserLocale() {
    Log.debug('CardanoClientApi::getLocale called');
    try {
      const locale = await getUserLocaleFromLocalStorage();
      Log.debug('CardanoClientApi::getLocale success: ', locale);
      return locale;
    } catch (error) {
      Log.error('CardanoClientApi::getLocale error: ', error);
      throw new GenericApiError();
    }
  }

  async updateWallet(request: UpdateWalletRequest) {
    Log.debug('CardanoClientApi::updateWallet called: ', JSON.stringify(request, null, 2));
    const { walletId, type, currency, name, assurance } = request;
    try {
      const response = await ClientApi.updateWallet(walletId, type, currency, name, assurance, 0);
      Log.debug('CardanoClientApi::updateWallet success: ', JSON.stringify(response, null, 2));
      return response;
    } catch (error) {
      Log.error('CardanoClientApi::updateWallet error: ', error);
      throw new GenericApiError();
    }
  }

  // eslint-disable-next-line max-len
  changeWalletPassword(request: ChangeWalletPasswordRequest): Promise<ChangeWalletPasswordResponse> {
    return new Promise((resolve) => {
      // Fake async request here to make it more realistic
      setTimeout(() => resolve(request), 100);
    });
  }

  setWalletPassword(request: SetWalletPasswordRequest): Promise<SetWalletPasswordResponse> {
    return new Promise((resolve) => {
      // Fake async request here to make it more realistic
      setTimeout(() => resolve(request), 100);
    });
  }


  async testReset() {
    Log.debug('CardanoClientApi::testReset called');
    await unsetUserLocaleInLocalStorage(); // TODO: remove after saving locale to API is restored
    try {
      const response = await ClientApi.testReset();
      Log.debug('CardanoClientApi::testReset success: ', JSON.stringify(response, null, 2));
      return response;
    } catch (error) {
      Log.error('CardanoClientApi::testReset error: ', error);
      throw new GenericApiError();
    }
  }

  // PRIVATE

  _onNotify = (rawMessage: string) => {
    Log.debug('CardanoClientApi::notify message: ', rawMessage);
    // TODO: "ConnectionClosed" messages are not JSON parsable … so we need to catch that case here!
    let message = rawMessage;
    if (message !== 'ConnectionClosed') {
      message = JSON.parse(rawMessage);
    }
    this.notifyCallbacks.forEach(cb => cb.message(message));
  };

  _onNotifyError = (error: Error) => {
    Log.debug('CardanoClientApi::notify error: ', error);
    this.notifyCallbacks.forEach(cb => cb.error(error));
  };
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
    cwAssurance: AssuranceModeOption,
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

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action((data: ServerWalletStruct) => (
  new Wallet({
    id: data.cwAddress,
    address: data.cwAddress,
    amount: new BigNumber(data.cwAmount.getCoin).dividedBy(LOVELACES_PER_ADA),
    type: data.cwMeta.cwType,
    currency: data.cwMeta.cwCurrency,
    name: data.cwMeta.cwName,
    assurance: data.cwMeta.cwAssurance,
    hasPassword: true, // TODO: replace with real API response
    passwordUpdateDate: new Date('2017-02-01'), // TODO: replace with real API response
  })
));

const _createTransactionFromServerData = action((data: ServerTransactionStruct) => {
  const isOutgoing = data.ctType.tag === 'CTOut';
  const coins = data.ctAmount.getCoin;
  const { ctmTitle, ctmDescription, ctmDate } = data.ctType.contents;
  return new WalletTransaction({
    id: data.ctId,
    title: ctmTitle || isOutgoing ? 'Ada sent' : 'Ada received',
    type: isOutgoing ? 'adaExpend' : 'adaIncome',
    currency: 'ada',
    amount: new BigNumber(isOutgoing ? -1 * coins : coins).dividedBy(LOVELACES_PER_ADA),
    date: new Date(ctmDate * 1000),
    description: ctmDescription || '',
    numberOfConfirmations: data.ctConfirmations,
  });
});
