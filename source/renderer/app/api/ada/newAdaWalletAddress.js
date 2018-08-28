// @flow
import type { AdaAddress } from './types';
import { request } from './lib/v1/request';

export type NewAdaWalletAddressParams = {
  ca: string,
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

export const newAdaWalletAddress = (
  { ca, spendingPassword, accountIndex, walletId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/addresses',
    port: 8090,
    ca,
  }, {}, { spendingPassword, accountIndex, walletId })
);
