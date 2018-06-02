// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';

export type GetAdaAccountsParams = {
};

export const getAdaAccounts = (
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
  })
);
