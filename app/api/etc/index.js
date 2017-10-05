// @flow
import { getEtcSyncProgress } from './getEtcSyncProgress';
import { Logger, stringifyData, stringifyError } from '../../lib/logger';
import { GenericApiError } from '../errors';
import type { GetSyncProgressResponse } from '../index';
import type { GetEtcSyncProgressResponse } from './getEtcSyncProgress';

export const ETC_API_HOST = 'ec2-52-30-28-57.eu-west-1.compute.amazonaws.com';
export const ETC_API_PORT = 8546;

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

  // eslint-disable-next-line no-empty-function
  async getWallets() {}

}
