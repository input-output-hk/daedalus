// @flow
import type { ApiTransaction } from 'daedalus-client-api';
import { request } from './lib/request';

export type RedeemAdaQueryParams = {
  passphrase: ?string,
};

export type RedeemAdaRawBodyParams = {
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
  ca: string,
  pathParams: {},
  queryParams: RedeemAdaQueryParams,
  rawBodyParams: RedeemAdaRawBodyParams,
): Promise<ApiTransaction> => {
  const { walletRedeemData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
    port: 8090,
    ca,
  }, queryParams, walletRedeemData);
};
