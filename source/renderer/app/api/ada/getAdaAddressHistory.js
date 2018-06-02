// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaAddressHistoryParams = {
  ca: string,
  port: number,
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  { ca, port, accountId, address, skip, limit }: GetAdaAddressHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port,
    ca,
  }, { accountId, address, skip, limit })
);
