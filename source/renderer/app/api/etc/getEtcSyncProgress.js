// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcSyncProgress } from './types';

export type GetEtcSyncProgressParams = {
  ca: string,
};

export const getEtcSyncProgress = (
  { ca }: GetEtcSyncProgressParams
): Promise<EtcSyncProgress> => (
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
