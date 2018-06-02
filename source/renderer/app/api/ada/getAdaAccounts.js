// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';

export type GetAdaAccountsParams = {
  ca: string,
  port: number,
};

export const getAdaAccounts = (
  { ca, port }: GetAdaAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port,
    ca,
  })
);
