// @flow
import type { AdaAccount } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
): Promise<AdaAccount> => {
  const { accountInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/accounts',
    port: environment.WALLET_PORT,
    ca,
  }, queryParams, accountInitData);
};
