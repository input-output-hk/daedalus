// @flow
import localStorage from 'electron-json-storage';
import ClientApi from 'daedalus-client-api';
import type {
  ApiTransaction,
  // ApiAccount,
  ApiAccounts,
  ApiAddress,
  // ApiAddresses,
  ApiTransactions,
  ApiWallet,
  ApiWallets,
} from 'daedalus-client-api';
import { action } from 'mobx';
import { ipcRenderer } from 'electron';
import Log from 'electron-log';
import BigNumber from 'bignumber.js';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import WalletAddress from '../domain/WalletAddress';
import type {
  CreateWalletRequest,
  GetAddressesRequest,
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


// TODO: Remove after hd integraton is complete

// Get all accounts
// (async () => {
//   const accounts = await ClientApi.getAccounts();
//   console.log('accounts:', JSON.stringify(accounts, null, 2));
// })();

// Create account
// (async () => {
//   const account = await ClientApi.newAccount(
//     '1fCdJRF5Ht9yNvfLW9QYfoH3gBopLwKebhiVSuCwG1U96i8',
//     'Test',
//     'secret'
//   );
//   console.log('account:', JSON.stringify(account, null, 2));
// })();

// Create address
// (async () => {
//   const address = await ClientApi.newWAddress(
//     '1fCdJRF5Ht9yNvfLW9QYfoH3gBopLwKebhiVSuCwG1U96i8@1218575036'
//   );
//   console.log('addresses:', JSON.stringify(address, null, 2));
// })();

// Get wallet accounts
// (async () => {
//   const addresses = await ClientApi.getWalletAccounts(
//     '1fsHQP5N7sb9BsjTx7H5vwRrY86ioS7uHFNRf7Px271V6Ch'
//   );
//   console.log('addresses:', JSON.stringify(addresses, null, 2));
// })();
// TODO: ^^ Remove after hd integraton is complete


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

const unsetUserLocaleFromLocalStorage = () => new Promise((resolve) => {
  localStorage.remove('userLocale', () => {
    resolve();
  });
});

const getTermsOfUseAcceptanceFromLocalStorage = () => new Promise((resolve, reject) => {
  localStorage.get('termsOfUseAcceptance', (error, response) => {
    if (error) return reject(error);
    if (!response.accepted) return resolve(false);
    resolve(response.accepted);
  });
});

const setTermsOfUseAcceptanceInLocalStorage = () => new Promise((resolve, reject) => {
  localStorage.set('termsOfUseAcceptance', { accepted: true }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

const unsetTermsOfUseAcceptanceFromLocalStorage = () => new Promise((resolve) => {
  localStorage.remove('termsOfUseAcceptance', () => {
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
      const response: ApiWallets = await ClientApi.getWallets();
      Log.debug('CardanoClientApi::getWallets success: ', JSON.stringify(response, null, 2));
      console.log('getWallets', JSON.stringify(response, null, 2));
      const wallets = response.map(data => _createWalletFromServerData(data));
      return wallets;
    } catch (error) {
      Log.error('CardanoClientApi::getWallets error: ', error);
      throw new GenericApiError();
    }
  }

  async getAddresses(request: GetAddressesRequest) {
    Log.debug('CardanoClientApi::getAddresses called: ', JSON.stringify(request, null, 2));
    const { walletId } = request;
    try {
      const response: ApiAccounts = await ClientApi.getWalletAccounts(walletId);
      Log.debug('CardanoClientApi::getAddresses success: ', JSON.stringify(response, null, 2));
      console.log('getAddresses', JSON.stringify(response, null, 2));
      if (!response.length) {
        return new Promise((resolve) => resolve({ accountId: null, addresses: [] }));
      }

      // For now only the first account is used
      const firstAccount = response[0];
      const firstAccountId = firstAccount.caId;
      const firstAccountAddresses = firstAccount.caAddresses;

      return new Promise((resolve) => resolve({
        accountId: firstAccountId,
        addresses: firstAccountAddresses.map(data => _createAddressFromServerData(data)),
      }));
    } catch (error) {
      Log.error('CardanoClientApi::getAddresses error: ', error);
      throw new GenericApiError();
    }
  }

  async getTransactions(request: GetTransactionsRequest) {
    Log.debug('CardanoClientApi::searchHistory called: ', JSON.stringify(request, null, 2));
    const { accountId, /* walletId, */ searchTerm, skip, limit } = request;
    // searchHistory endpoint requires accountId (account.caId) and not walletId
    try {
      const history: ApiTransactions = await ClientApi.searchHistory(
        accountId, searchTerm, skip, limit
      );
      console.log('getTransactions', JSON.stringify(history, null, 2));
      Log.debug('CardanoClientApi::searchHistory success: ', JSON.stringify(history, null, 2));
      return new Promise((resolve) => resolve({
        transactions: history[0].map(data => _createTransactionFromServerData(data)),
        total: history[1]
      }));
    } catch (error) {
      Log.error('CardanoClientApi::searchHistory error: ', error);
      throw new GenericApiError();
    }
  }

  async createWallet(request: CreateWalletRequest) {
    // wallets are created WITHOUT an account!!!
    // after creation ClientApi.newAccount API call should be triggered
    Log.debug('CardanoClientApi::createWallet called');
    const { name, mnemonic, password } = request;
    const assurance = 'CWANormal';
    const unit = 0;
    try {
      // 1. create wallet
      const wallet: ApiWallet = await ClientApi.newWallet(
        name, assurance, unit, mnemonic, password || ''
      ); // empty string must be used if no password is set ^^
      console.log('wallet:', JSON.stringify(wallet, null, 2));
      Log.debug('CardanoClientApi::createWallet success: ', JSON.stringify(wallet, null, 2));

      // 2. create account
      const account = await ClientApi.newAccount(wallet.cwId, name, password || '');
      console.log('account:', JSON.stringify(account, null, 2));

      // 3. create (initial) wallet address
      const address = await ClientApi.newWAddress(account.caId, password || '');
      console.log('address:', JSON.stringify(address, null, 2));

      return _createWalletFromServerData(wallet);
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
    Log.debug('CardanoClientApi::createTransaction called');
    const { sender, receiver, amount, /* currency, */ password } = request;
    // sender must be set as accountId (account.caId) and not walletId
    const description = 'no description provided';
    const title = 'no title provided';
    try {
      const response: ApiTransaction = await ClientApi.newPaymentExtended(
        sender, receiver, amount, title, description, password || ''
      ); // empty string must be used if no password is set ^^
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

  isValidAddress(address: string): Promise<boolean> {
    return ClientApi.isValidAddress(address);
  }

  isValidMnemonic(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidMnemonic(12, mnemonic);
  }

  isValidRedemptionKey(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidRedemptionKey(mnemonic);
  }

  isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidPaperVendRedemptionKey(mnemonic);
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
    const { recoveryPhrase, walletName, walletPassword } = request;
    try {
      // 1. restore wallet
      const wallet: ApiWallet = await ClientApi.restoreWallet(
        walletName, 'CWANormal', 0, recoveryPhrase, walletPassword || ''
      ); // empty string must be used if no password is set ^^
      console.log('wallet:', JSON.stringify(wallet, null, 2));
      Log.debug('CardanoClientApi::restoreWallet success');

      // 2. create account
      const account = await ClientApi.newAccount(wallet.cwId, walletName, walletPassword || '');
      console.log('account:', JSON.stringify(account, null, 2));

      // 3. create (initial) wallet address
      const address = await ClientApi.newWAddress(account.caId, walletPassword || '');
      console.log('address:', JSON.stringify(address, null, 2));

      return _createWalletFromServerData(wallet);
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
      const importedWallet: ApiWallet = await ClientApi.importWallet(request.filePath, '');
      Log.debug('CardanoClientApi::importWalletFromKey success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Log.error('CardanoClientApi::importWalletFromKey error: ', error);
      console.log('ERROR', error);
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
      const response: ApiTransaction = await ClientApi.redeemAda(redemptionCode, walletId);
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
      const response: ApiTransaction = await ClientApi.redeemAdaPaperVend(
        shieldedRedemptionKey, mnemonics, walletId
      );
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
      Log.debug('CardanoClientApi::nextUpdate error: ', error);
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

  async setTermsOfUseAcceptance() {
    Log.debug('CardanoClientApi::setTermsOfUseAcceptance called');
    try {
      await setTermsOfUseAcceptanceInLocalStorage();
      Log.debug('CardanoClientApi::setTermsOfUseAcceptance success');
      return true;
    } catch (error) {
      Log.error('CardanoClientApi::setTermsOfUseAcceptance error: ', error);
      throw new GenericApiError();
    }
  }

  async getTermsOfUseAcceptance() {
    Log.debug('CardanoClientApi::getTermsOfUseAcceptance called');
    try {
      const acceptance = await getTermsOfUseAcceptanceFromLocalStorage();
      Log.debug('CardanoClientApi::getTermsOfUseAcceptance success: ', acceptance);
      return acceptance;
    } catch (error) {
      Log.error('CardanoClientApi::getTermsOfUseAcceptance error: ', error);
      throw new GenericApiError();
    }
  }

  async updateWallet(request: UpdateWalletRequest) {
    Log.debug('CardanoClientApi::updateWallet called: ', JSON.stringify(request, null, 2));
    const { walletId, type, currency, name, assurance } = request;
    try {
      const response: ApiWallet = await ClientApi.updateWallet(
        walletId, type, currency, name, assurance, 0
      );
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
    await unsetUserLocaleFromLocalStorage(); // TODO: remove after saving locale to API is restored
    await unsetTermsOfUseAcceptanceFromLocalStorage();
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


// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action((data: ApiWallet) => (
  new Wallet({
    id: data.cwId,
    amount: new BigNumber(data.cwAmount.getCCoin).dividedBy(LOVELACES_PER_ADA),
    name: data.cwMeta.cwName,
    assurance: data.cwMeta.cwAssurance,
    hasPassword: data.cwHasPassphrase,
    passwordUpdateDate: new Date(data.cwPassphraseLU * 1000),
  })
));

const _createAddressFromServerData = action((data: ApiAddress) => (
  new WalletAddress({
    id: data.cadId,
    amount: new BigNumber(data.cadAmount.getCCoin).dividedBy(LOVELACES_PER_ADA),
    isUsed: data.cadIsUsed,
  })
));

const _createTransactionFromServerData = action((data: ApiTransaction) => {
  const isOutgoing = 'CTOut'; // TODO: check how to determine ctType (data.ctType.tag === 'CTOut')
  const coins = data.ctAmount.getCCoin;
  const { ctmTitle, ctmDescription, ctmDate } = data.ctMeta;
  return new WalletTransaction({
    id: data.ctId,
    title: ctmTitle || isOutgoing ? 'Ada sent' : 'Ada received',
    type: isOutgoing ? 'adaExpend' : 'adaIncome',
    currency: 'ada',
    amount: new BigNumber(isOutgoing ? -1 * coins : coins).dividedBy(LOVELACES_PER_ADA),
    date: new Date(ctmDate * 1000),
    description: ctmDescription || '',
    numberOfConfirmations: data.ctConfirmations,
    addresses: {
      from: data.ctInputAddrs,
      to: data.ctOutputAddrs,
    },
  });
});
