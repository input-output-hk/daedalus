// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaAddressHistoryParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  { apiParams, accountId, address, skip, limit }: GetAdaAddressHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { accountId, address, skip, limit })
);
