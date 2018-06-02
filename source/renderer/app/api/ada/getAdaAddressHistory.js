// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaAddressHistoryParams = {
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  { accountId, address, skip, limit }: GetAdaAddressHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, { accountId, address, skip, limit })
);
