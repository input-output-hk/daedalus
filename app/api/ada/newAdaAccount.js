// @flow
import type { ApiAccount } from 'daedalus-client-api';
import { request } from './lib/request';

export type NewAdaWalletQueryParams = {
  passphrase: ?string,
};

export type NewAdaWalletRawBodyParams = {
  accountInitData: {
    "caInitMeta": {
      "caName": string,
    },
    "caInitWId": string,
  }
};

export const newAdaAccount = (
  ca: string,
  pathParams: {},
  queryParams: NewAdaWalletQueryParams,
  rawBodyParams: NewAdaWalletRawBodyParams,
): Promise<ApiAccount> => {
  const { accountInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/accounts',
    port: 8090,
    ca,
  }, queryParams, accountInitData);
};
