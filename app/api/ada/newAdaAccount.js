// @flow
import type { ApiAccount } from 'daedalus-client-api';
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
  ca: string,
  pathParams: {},
  queryParams: NewAdaAccountQueryParams,
  rawBodyParams: NewAdaAccountRawBodyParams,
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
