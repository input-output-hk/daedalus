// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaAddressHistoryParams = {
  ca: string,
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  { ca, accountId, address, skip, limit }: GetAdaAddressHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: environment.WALLET_PORT,
    ca,
  }, { accountId, address, skip, limit })
);
