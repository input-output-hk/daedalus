// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  { apiParams, walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { walletId, skip, limit })
);
