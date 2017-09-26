// @flow
import { request } from './lib/request';
import { ETC_API_PORT } from './index';

export type GetEtcSyncProgressResponse = {
  jsonrpc: string,
  id: number,
  result?: {
    startingBlock: string,
    currentBlock: string,
    highestBlock: string
  }
};

export const getEtcSyncProgress = (): Promise<GetEtcSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
  }, {
    jsonrpc: '2.0',
    method: 'eth_syncing',
  })
);
