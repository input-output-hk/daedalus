// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/v1/request';

export type GetAdaHistoryByWalletParams = {
  ca: string,
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  { ca, walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => {

  skip;
  console.log('ca', ca);

  const v1Params = {
    walletId,
    page: 1,
    per_page: limit > 50 ? 50 : limit,
    accountIndex: 2147483648,
  };
  console.log('v1Params', v1Params);

  return (
    request({
      hostname: 'localhost',
      method: 'GET',
      path: '/api/v1/transactions',
      port: 8090,
      ca,
    }, v1Params)
  );
};

