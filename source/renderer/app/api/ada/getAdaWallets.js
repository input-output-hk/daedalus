// @flow
import type { AdaV1Wallets } from './types';
import { request } from './lib/v1/request';
import { MAX_ADA_WALLETS_COUNT } from '../../config/numbersConfig';
import environment from '../../../../common/environment';


export type GetAdaWalletParams = {
  ca: string,
};

export const getAdaWallets = (
  { ca }: GetAdaWalletParams
): Promise<AdaV1Wallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/wallets',
    port: environment.WALLET_PORT,
    ca,
  }, {
    per_page: MAX_ADA_WALLETS_COUNT, // 50 is the max per_page value
    sort_by: 'ASC[created_at]',
  })
);
