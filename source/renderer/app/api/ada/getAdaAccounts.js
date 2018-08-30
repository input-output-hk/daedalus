// @flow
import type { AdaAccounts, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaAccountsParams = {
  ca: Uint8Array,
};

export const getAdaAccounts = (
  config: RequestConfig
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: config.port,
    ca: config.ca,
  })
);
