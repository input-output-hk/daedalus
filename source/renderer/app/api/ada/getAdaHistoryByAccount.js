// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaHistoryByAccountParams = {
  ca: string,
  accountId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByAccount = (
  { ca, accountId, skip, limit }: GetAdaHistoryByAccountParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: environment.WALLET_PORT,
    ca,
  }, { accountId, skip, limit })
);
