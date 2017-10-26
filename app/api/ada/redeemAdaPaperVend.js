// @flow
import type { ApiTransaction } from 'daedalus-client-api';
import { request } from './lib/request';

export type RedeemAdaPaperVendQueryParams = {
  passphrase: ?string,
};

export type RedeemAdaPaperVendRawBodyParams = {
  redeemPaperVendedData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAdaPaperVend = (
  ca: string,
  pathParams: {},
  queryParams: RedeemAdaPaperVendQueryParams,
  rawBodyParams: RedeemAdaPaperVendRawBodyParams,
): Promise<ApiTransaction> => {
  const { redeemPaperVendedData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port: 8090,
    ca,
  }, queryParams, redeemPaperVendedData);
};
