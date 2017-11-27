// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcBlockNumber } from './types';

export type GetEtcBlockNumberParams = {
  ca: string,
};

/**
 * Returns the number of most recent block.
 * @param ca
 * @returns {Promise<number>} integer of the current block number the client is on.
 */

export const getEtcBlockNumber = async (
  { ca }: GetEtcBlockNumberParams
): Promise<EtcBlockNumber> => {
  const response = await request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_blockNumber',
    params: []
  });
  return parseInt(response, 16);
};
