// @flow
import type { AdaAddress, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type NewAdaWalletAddressParams = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

export const newAdaWalletAddress = (
  config: RequestConfig,
  { spendingPassword, accountIndex, walletId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/addresses',
    ...config,
  }, {}, { spendingPassword, accountIndex, walletId })
);
