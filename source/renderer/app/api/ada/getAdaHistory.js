// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaHistoryParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: ?string,
  accountId: ?string,
  address: ?string,
  skip: number,
  limit: number,
};

export const getAdaHistory = (
  { apiParams, walletId, accountId, address, skip, limit }: GetAdaHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { walletId, accountId, address, skip, limit })
);
