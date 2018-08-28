// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaAccountsParams = {
  ca: string,
};

export const getAdaAccounts = (
  { ca }: GetAdaAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: environment.WALLET_PORT,
    ca,
  })
);
