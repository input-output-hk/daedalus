// @flow
import bip39 from 'bip39';
import { getEtcSyncProgress } from './getEtcSyncProgress';
import { Logger, stringifyData, stringifyError } from '../../lib/logger';
import { GenericApiError } from '../common';
import { getEtcAccounts } from './getEtcAccounts';
import { getEtcAccountBalance } from './getEtcAccountBalance';
import { createEtcAccount } from './createEtcAccount';
import type { GetSyncProgressResponse } from '../common';
import type { GetEtcSyncProgressResponse } from './getEtcSyncProgress';
import type { GetEtcAccountsResponse } from './getEtcAccounts';
import type { GetEtcAccountBalanceResponse } from './getEtcAccountBalance';
import type { CreateEtcAccountResponse } from './createEtcAccount';

/**
 * The ETC api layer that handles all requests to the
 * mantis client which is used as backend for ETC blockchain.
 */

export const ETC_API_HOST = 'ec2-52-30-28-57.eu-west-1.compute.amazonaws.com';
export const ETC_API_PORT = 8546;

export type CreateWalletRequest = {
  name: string,
  mnemonic: string,
  password: ?string,
};

export default class EtcApi {

  async getSyncProgress(): Promise<GetSyncProgressResponse> {
    Logger.debug('EtcApi::getSyncProgress called');
    try {
      const response: GetEtcSyncProgressResponse = await getEtcSyncProgress();
      Logger.debug('EtcApi::getSyncProgress success: ' + stringifyData(response));
      return {
        localDifficulty: response.result ? parseInt(response.result.currentBlock, 16) : 100,
        networkDifficulty: response.result ? parseInt(response.result.highestBlock, 16) : 100,
      };
    } catch (error) {
      Logger.error('EtcApi::getSyncProgress error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getAccounts = async () => {
    Logger.debug('EtcApi::getAccounts called');
    try {
      const response: GetEtcAccountsResponse = await getEtcAccounts();
      Logger.debug('EtcApi::getAccounts success: ' + stringifyData(response));
      const accounts = response.result;
      return Promise.all(accounts.map(async (id) => ({
        id,
        balance: await this.getAccountBalance(id),
      })));
    } catch (error) {
      Logger.error('EtcApi::getAccounts error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  async getAccountBalance(accountId: string) {
    Logger.debug('EtcApi::getAccountBalance called');
    try {
      const response: GetEtcAccountBalanceResponse = await getEtcAccountBalance([accountId, 'latest']);
      Logger.debug('EtcApi::getAccountBalance success: ' + stringifyData(response));
      return parseInt(response.result, 16);
    } catch (error) {
      Logger.error('EtcApi::getAccountBalance error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createWallet(request: CreateWalletRequest) {
    Logger.debug('EtcApi::createWallet called');
    const { name, mnemonic, password } = request;
    const privateKey = bip39.mnemonicToSeedHex(mnemonic, password); // unencrypted private key (hex string)
    try {
      const response = await createEtcAccount(
        privateKey, password || '' // if password is not provided send empty string to the Api
      );
      Logger.debug('EtcApi::createWallet success');

      // save 'name' to the local storage and return it back as a part of newly created wallet
      // based on the 'password' presence return 'hasPassword' and 'passwordUpdateDate' for newly created wallet

      return response;
    } catch (error) {
      Logger.error('EtcApi::createWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

}
