// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type UpdateAdaWalletPathParams = {
  walletId: string,
};

export type UpdateAdaWalletRawBodyParams = {
  walletMeta: {
    "cwName": string,
    "cwAssurance": string,
    "cwUnit": number,
  }
};

export const updateAdaWallet = (
  ca: string,
  pathParams: UpdateAdaWalletPathParams,
  queryParams: {},
  rawBodyParams: UpdateAdaWalletRawBodyParams,
): Promise<ApiWallet> => {
  const { walletId } = pathParams;
  const { walletMeta } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: 8090,
    ca,
  }, queryParams, walletMeta);
};
