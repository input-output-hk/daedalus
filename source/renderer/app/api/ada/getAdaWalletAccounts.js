// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';


export type GetAdaWalletAccountsParams = {
  ca: string,
  walletId: string,
};

export const getAdaWalletAccounts = (
  { ca, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: environment.WALLET_PORT,
    ca,
  }, { accountId: walletId })
);
