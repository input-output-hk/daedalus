// @flow
import type { AdaAccount, RequestConfig } from './types';
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
  config: RequestConfig,
  pathParams: {},
  queryParams: NewAdaAccountQueryParams,
  rawBodyParams: NewAdaAccountRawBodyParams,
): Promise<AdaAccount> => {
  const { accountInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/accounts',
    ...config,
  }, queryParams, accountInitData);
};
