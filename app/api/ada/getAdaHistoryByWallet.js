// @flow
import type { ApiTransactions } from 'daedalus-client-api';
import { request } from './lib/request';

export type GetAdaHistoryByWalletParams = {
  ca: string,
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  { ca, walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, { walletId, skip, limit })
);
