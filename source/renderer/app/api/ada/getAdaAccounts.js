// @flow
import type { AdaAccounts, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaAccountsParams = {
  ca: Uint8Array,
};

export const getAdaAccounts = (
  config: RequestConfig
): Promise<AdaAccounts> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
  }, config))
);
