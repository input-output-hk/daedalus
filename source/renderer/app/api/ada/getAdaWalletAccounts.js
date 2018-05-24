// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';


export type GetAdaWalletAccountsParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
};

export const getAdaWalletAccounts = (
  { apiParams, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { accountId: walletId })
);
