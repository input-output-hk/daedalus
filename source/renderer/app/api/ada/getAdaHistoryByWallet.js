// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByWalletParams = {
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  config: RequestConfig,
  { walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: config.port,
    ca: config.ca,
  }, { walletId, skip, limit })
);
