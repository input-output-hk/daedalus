// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type GetEtcSyncProgressParams = {
  ca: string,
};

export type GetEtcSyncProgressResponse = false | {
  startingBlock: string,
  currentBlock: string,
  highestBlock: string
};

export const getEtcSyncProgress = (
  { ca }: GetEtcSyncProgressParams
): Promise<GetEtcSyncProgressResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_syncing',
  })
);
