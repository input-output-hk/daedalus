// @flow
import type { AdaAddress, RequestConfig } from './types';
import { request } from '../../utils/request';

export type CreateAddressParams = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

export const createAddress = (
  config: RequestConfig,
  { spendingPassword, accountIndex, walletId }: CreateAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/addresses',
    ...config,
  }, {}, { spendingPassword, accountIndex, walletId })
);
