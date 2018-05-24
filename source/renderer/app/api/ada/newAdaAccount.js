// @flow
import type { AdaAccount } from './types';
import { request } from './lib/request';

export type NewAdaAccountQueryParams = {
  passphrase: ?string,
};

export type NewAdaAccountRawBodyParams = {
  accountInitData: {
    caInitMeta: {
      caName: string,
    },
    caInitWId: string,
  }
};

export const newAdaAccount = (
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  pathParams: {},
  queryParams: NewAdaAccountQueryParams,
  rawBodyParams: NewAdaAccountRawBodyParams,
): Promise<AdaAccount> => {
  const { accountInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/accounts',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, queryParams, accountInitData);
};
