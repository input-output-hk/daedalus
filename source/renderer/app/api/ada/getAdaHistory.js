// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaHistoryParams = {
  ca: string,
  walletId: ?string,
  accountId: ?string,
  address: ?string,
  skip: number,
  limit: number,
};

export const getAdaHistory = (
  { ca, walletId, accountId, address, skip, limit }: GetAdaHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: environment.WALLET_PORT,
    ca,
  }, { walletId, accountId, address, skip, limit })
);
