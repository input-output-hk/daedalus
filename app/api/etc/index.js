// @flow
import BigNumber from 'bignumber.js';
import { remote } from 'electron';
import { isAddress } from 'web3-utils/src/utils';
import { getEtcSyncProgress } from './getEtcSyncProgress';
import { Logger, stringifyData, stringifyError } from '../../utils/logging';
import {
  GenericApiError, IncorrectWalletPasswordError,
  WalletAlreadyRestoredError,
} from '../common';
import { mnemonicToSeedHex, quantityToBigNumber, unixTimestampToDate } from './lib/utils';
import {
  getEtcWalletData, setEtcWalletData, unsetEtcWalletData, updateEtcWalletData,
  initEtcWalletsDummyData,
} from './etcLocalStorage';
import { ETC_DEFAULT_GAS_PRICE, WEI_PER_ETC } from '../../config/numbersConfig';
import Wallet from '../../domain/Wallet';
import WalletTransaction, { transactionStates, transactionTypes } from '../../domain/WalletTransaction';

import { getEtcAccounts } from './getEtcAccounts';
import { getEtcAccountBalance } from './getEtcAccountBalance';
import { getEtcAccountRecoveryPhrase } from './getEtcAccountRecoveryPhrase';
import { createEtcAccount } from './createEtcAccount';
import { getEtcBlockByHash } from './getEtcBlock';
import { sendEtcTransaction } from './sendEtcTransaction';
import { deleteEtcAccount } from './deleteEtcAccount';
import { getEtcTransactionByHash } from './getEtcTransaction';
import { changeEtcAccountPassphrase } from './changeEtcAccountPassphrase';
import { getEtcEstimatedGas } from './getEtcEstimatedGas';
import { getEtcTransactions } from './getEtcTransactions';
import { getEtcBlockNumber } from './getEtcBlockNumber';
import { isValidMnemonic } from '../../../lib/decrypt';

import type { TransactionType } from '../../domain/WalletTransaction';
import type {
  GetSyncProgressResponse, GetWalletRecoveryPhraseResponse,
  GetTransactionsRequest, GetTransactionsResponse, GetWalletsResponse,
  CreateWalletRequest, CreateWalletResponse, UpdateWalletResponse,
  UpdateWalletPasswordRequest, UpdateWalletPasswordResponse,
  DeleteWalletRequest, DeleteWalletResponse,
  RestoreWalletRequest, RestoreWalletResponse,
  CreateTransactionResponse
} from '../common';
import type {
  EtcSyncProgress, EtcAccounts, EtcWalletBalance,
  EtcTransactions, EtcBlockNumber, EtcWalletId,
  EtcRecoveryPassphrase, EtcTxHash, EtcGas,
  EtcBlock, EtcTransaction,
} from './types';


// Load Dummy ETC Wallets into Local Storage
(async () => {
  await initEtcWalletsDummyData();
})();

/**
 * The ETC api layer that handles all requests to the
 * mantis client which is used as backend for ETC blockchain.
 */

const ca = remote.getGlobal('ca');

// export const ETC_API_HOST = 'ec2-52-30-28-57.eu-west-1.compute.amazonaws.com';
export const ETC_API_HOST = 'localhost';
export const ETC_API_PORT = 8546;

// ETC specific Request / Response params
export type ImportWalletResponse = Wallet;
export type UpdateWalletRequest = Wallet;

export type ImportWalletRequest = {
  name: string,
  privateKey: string,
  password: ?string,
};

export type CreateTransactionRequest = {
  from: string,
  to: string,
  value: BigNumber,
  password: string,
};

export type GetEstimatedGasPriceRequest = {
  ca: string,
  from: string,
  to: string,
  value: BigNumber,
  gasPrice: BigNumber,
};
export type GetEstimatedGasPriceResponse = Promise<BigNumber>;

export default class EtcApi {

  async getSyncProgress(): Promise<GetSyncProgressResponse> {
    Logger.debug('EtcApi::getSyncProgress called');
    try {
      const response: EtcSyncProgress = await getEtcSyncProgress({ ca });
      Logger.debug('EtcApi::getSyncProgress success: ' + stringifyData(response));
      return {
        localDifficulty: response ? parseInt(response.currentBlock, 16) : 100,
        networkDifficulty: response ? parseInt(response.highestBlock, 16) : 100,
      };
    } catch (error) {
      Logger.error('EtcApi::getSyncProgress error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getWallets = async (): Promise<GetWalletsResponse> => {
    Logger.debug('EtcApi::getWallets called');
    try {
      const accounts: EtcAccounts = await getEtcAccounts({ ca });
      Logger.debug('EtcApi::getWallets success: ' + stringifyData(accounts));
      return await Promise.all(accounts.map(async (id) => {
        const amount = await this.getAccountBalance(id);
        try {
          // use wallet data from local storage
          const walletData = await getEtcWalletData(id); // fetch wallet data from local storage
          const { name, assurance, hasPassword, passwordUpdateDate } = walletData;
          return new Wallet({ id, name, amount, assurance, hasPassword, passwordUpdateDate });
        } catch (error) {
          // there is no wallet data in local storage - use fallback data
          const fallbackWalletData = {
            id,
            name: 'Untitled Wallet (*)',
            assurance: 'CWANormal',
            hasPassword: true,
            passwordUpdateDate: new Date(),
          };
          const { name, assurance, hasPassword, passwordUpdateDate } = fallbackWalletData;
          return new Wallet({ id, name, amount, assurance, hasPassword, passwordUpdateDate });
        }
      }));
    } catch (error) {
      Logger.error('EtcApi::getWallets error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  async getAccountBalance(walletId: string): Promise<GetTransactionsResponse> {
    Logger.debug('EtcApi::getAccountBalance called');
    try {
      const status = 'latest';
      const response: EtcWalletBalance = await getEtcAccountBalance({
        ca, walletId, status,
      });
      Logger.debug('EtcApi::getAccountBalance success: ' + stringifyData(response));
      return quantityToBigNumber(response).dividedBy(WEI_PER_ETC);
    } catch (error) {
      Logger.error('EtcApi::getAccountBalance error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getTransactions = async (request: GetTransactionsRequest): Promise<GetTransactionsResponse> => {
    Logger.debug('EtcApi::getTransactions called: ' + stringifyData(request));
    try {
      const walletId = request.walletId;
      const mostRecentBlockNumber: EtcBlockNumber = await getEtcBlockNumber({ ca });
      const transactions: EtcTransactions = await getEtcTransactions({
        ca,
        walletId,
        fromBlock: Math.max(mostRecentBlockNumber - 10000, 0),
        toBlock: mostRecentBlockNumber,
      });
      Logger.debug('EtcApi::getTransactions success: ' + stringifyData(transactions));
      const receivedTxs = await Promise.all(
        transactions.received.map(async (tx: EtcTransaction) => (
          _createWalletTransactionFromServerData(transactionTypes.INCOME, tx)
        ))
      );
      const sentTxs = await Promise.all(
        transactions.sent.map(async (tx: EtcTransaction) => (
          _createWalletTransactionFromServerData(transactionTypes.EXPEND, tx)
        ))
      );
      const allTxs = receivedTxs.concat(sentTxs);
      return {
        transactions: allTxs,
        total: allTxs.length,
      };
    } catch (error) {
      Logger.error('EtcApi::getTransactions error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  async importWallet(request: ImportWalletRequest): Promise<ImportWalletResponse> {
    Logger.debug('EtcApi::importWallet called');
    const { name, privateKey, password } = request;
    try {
      const response: EtcWalletId = await createEtcAccount({
        ca, privateKey, password,
      });
      Logger.debug('EtcApi::importWallet success: ' + stringifyData(response));
      const id = response;
      const amount = quantityToBigNumber('0');
      const assurance = 'CWANormal';
      const hasPassword = password !== null;
      const passwordUpdateDate = hasPassword ? new Date() : null;
      await setEtcWalletData({
        id, name, assurance, hasPassword, passwordUpdateDate,
      });
      return new Wallet({ id, name, amount, assurance, hasPassword, passwordUpdateDate });
    } catch (error) {
      Logger.error('EtcApi::importWallet error: ' + stringifyError(error));
      throw error; // Error is handled in parent method (e.g. createWallet/restoreWallet)
    }
  }

  createWallet = async (request: CreateWalletRequest): Promise<CreateWalletResponse> => {
    Logger.debug('EtcApi::createWallet called');
    const { name, mnemonic, password } = request;
    const privateKey = mnemonicToSeedHex(mnemonic);
    try {
      const response: ImportWalletResponse = await this.importWallet({
        name, privateKey, password,
      });
      Logger.debug('EtcApi::createWallet success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('EtcApi::createWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getWalletRecoveryPhrase(): Promise<GetWalletRecoveryPhraseResponse> {
    Logger.debug('EtcApi::getWalletRecoveryPhrase called');
    try {
      const response: Promise<EtcRecoveryPassphrase> = new Promise(
        (resolve) => resolve(getEtcAccountRecoveryPhrase())
      );
      Logger.debug('EtcApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('EtcApi::getWalletRecoveryPhrase error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createTransaction(params: CreateTransactionRequest): CreateTransactionResponse {
    Logger.debug('EtcApi::createTransaction called');
    try {
      const senderAccount = params.from;
      const { from, to, value, password } = params;
      const txHash: EtcTxHash = await sendEtcTransaction({
        ca, from, to, value, password, gasPrice: ETC_DEFAULT_GAS_PRICE,
      });
      Logger.debug('EtcApi::createTransaction success: ' + stringifyData(txHash));
      return _createTransaction(senderAccount, txHash);
    } catch (error) {
      Logger.error('EtcApi::createTransaction error: ' + stringifyError(error));
      if (error.message.includes('Could not decrypt key with given passphrase')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  }

  async updateWallet(request: UpdateWalletRequest): Promise<UpdateWalletResponse> {
    Logger.debug('EtcApi::updateWallet called: ' + stringifyData(request));
    const { id, name, amount, assurance, hasPassword, passwordUpdateDate } = request;
    try {
      await setEtcWalletData({
        id, name, assurance, hasPassword, passwordUpdateDate,
      });
      Logger.debug('EtcApi::updateWallet success: ' + stringifyData(request));
      return new Wallet({ id, name, amount, assurance, hasPassword, passwordUpdateDate });
    } catch (error) {
      Logger.error('EtcApi::updateWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async updateWalletPassword(
    request: UpdateWalletPasswordRequest
  ): Promise<UpdateWalletPasswordResponse> {
    Logger.debug('EtcApi::updateWalletPassword called');
    const { walletId, oldPassword, newPassword } = request;
    try {
      await changeEtcAccountPassphrase({
        ca, walletId, oldPassword, newPassword,
      });
      Logger.debug('EtcApi::updateWalletPassword success');
      const hasPassword = newPassword !== null;
      const passwordUpdateDate = hasPassword ? new Date() : null;
      await updateEtcWalletData({
        id: walletId, hasPassword, passwordUpdateDate
      });
      return true;
    } catch (error) {
      Logger.error('EtcApi::updateWalletPassword error: ' + stringifyError(error));
      if (error.message.includes('Could not decrypt key with given passphrase')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  }

  async deleteWallet(request: DeleteWalletRequest): Promise<DeleteWalletResponse> {
    Logger.debug('EtcApi::deleteWallet called: ' + stringifyData(request));
    const { walletId } = request;
    try {
      await deleteEtcAccount({ ca, walletId });
      Logger.debug('EtcApi::deleteWallet success: ' + stringifyData(request));
      await unsetEtcWalletData(walletId); // remove wallet data from local storage
      return true;
    } catch (error) {
      Logger.error('EtcApi::deleteWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  restoreWallet = async (request: RestoreWalletRequest): Promise<RestoreWalletResponse> => {
    Logger.debug('EtcApi::restoreWallet called');
    const { recoveryPhrase: mnemonic, walletName: name, walletPassword: password } = request;
    const privateKey = mnemonicToSeedHex(mnemonic);
    try {
      const wallet: ImportWalletResponse = await this.importWallet({ name, privateKey, password });
      Logger.debug('EtcApi::restoreWallet success: ' + stringifyData(wallet));
      return wallet;
    } catch (error) {
      Logger.error('EtcApi::restoreWallet error: ' + stringifyError(error));
      if (error.message.includes('account already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      throw new GenericApiError();
    }
  }

  isValidMnemonic(mnemonic: string): Promise<boolean> {
    return isValidMnemonic(mnemonic, 12);
  }

  isValidAddress(address: string): Promise<boolean> {
    return Promise.resolve(isAddress(address));
  }

  async getEstimatedGasPriceResponse(
    request: GetEstimatedGasPriceRequest
  ): GetEstimatedGasPriceResponse {
    Logger.debug('EtcApi::getEstimatedGasPriceResponse called');
    try {
      const { from, to, value, gasPrice } = request;
      const estimatedGas: EtcGas = await getEtcEstimatedGas({
        ca, from, to, value, gasPrice,
      });
      Logger.debug('EtcApi::getEstimatedGasPriceResponse success: ' + stringifyData(estimatedGas));
      return quantityToBigNumber(estimatedGas).times(request.gasPrice).dividedBy(WEI_PER_ETC);
    } catch (error) {
      Logger.error('EtcApi::getEstimatedGasPriceResponse error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  testReset = async (): Promise<boolean> => {
    Logger.debug('EtcApi::testReset called');
    try {
      const accounts: EtcAccounts = await getEtcAccounts({ ca });
      await Promise.all(accounts.map(async (id) => this.deleteWallet({ walletId: id })));
      Logger.debug('EtcApi::testReset success');
      return true;
    } catch (error) {
      Logger.error('EtcApi::testReset error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletTransactionFromServerData = async (
  type: TransactionType, txData: EtcTransaction
): Promise<WalletTransaction> => {
  const { hash, blockHash, value, from, to, pending, } = txData;
  const txBlock: ?EtcBlock = blockHash ? await getEtcBlockByHash({
    ca, blockHash,
  }) : null;
  const blockDate = txBlock ? unixTimestampToDate(txBlock.timestamp) : new Date();
  return new WalletTransaction({
    id: hash,
    type,
    title: '',
    description: '',
    amount: quantityToBigNumber(value).dividedBy(WEI_PER_ETC),
    date: blockDate,
    numberOfConfirmations: 0,
    addresses: {
      from: [from],
      to: [to],
    },
    state: pending ? transactionStates.PENDING : transactionStates.OK,
  });
};

const _createTransaction = async (senderAccount: EtcWalletId, txHash: EtcTxHash) => {
  const txData: EtcTransaction = await getEtcTransactionByHash({
    ca, txHash,
  });
  const type = senderAccount === txData.from ? transactionTypes.EXPEND : transactionTypes.INCOME;
  return _createWalletTransactionFromServerData(type, txData);
};
