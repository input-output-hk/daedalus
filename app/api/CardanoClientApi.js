// @flow
import ClientApi from 'daedalus-client-api';
import { action } from 'mobx';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import type {
  createWalletRequest,
  getTransactionsRequest,
  createTransactionRequest,
  walletRestoreRequest,
  redeemAdaRequest
} from './index';
import {
  // ApiMethodNotYetImplementedError,
  WalletAlreadyRestoredError,
  RedeemAdaError
} from './errors';

// const notYetImplemented = () => new Promise((_, reject) => {
//   reject(new ApiMethodNotYetImplementedError());
// });

export default class CardanoClientApi {

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
    const { sender, receiver, amount, currency, title } = request;
    let { description } = request;
    if (!description) description = 'no description provided';
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
      return await ClientApi.restoreWallet('CWTPersonal', 'ADA', walletName, recoveryPhrase);
    } catch (error) {
      console.error(error);
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      throw error;
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
}
