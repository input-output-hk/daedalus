// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaHistoryByWalletParams = {
  ca: string,
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  { ca, walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: environment.WALLET_PORT,
    ca,
  }, { walletId, skip, limit })
);
