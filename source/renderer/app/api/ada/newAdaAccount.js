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
  ca: string,
  port: number,
  pathParams: {},
  queryParams: NewAdaAccountQueryParams,
  rawBodyParams: NewAdaAccountRawBodyParams,
): Promise<AdaAccount> => {
  const { accountInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/accounts',
    port,
    ca,
  }, queryParams, accountInitData);
};
