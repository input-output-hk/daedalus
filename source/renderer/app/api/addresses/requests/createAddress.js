// @flow
import type { RequestConfig } from '../../common/types';
import type { Address } from '../types';
import { request } from '../../utils/request';

export type CreateAddressParams = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

export const createAddress = (
  config: RequestConfig,
  { spendingPassword, accountIndex, walletId }: CreateAddressParams
): Promise<Address> => (
  request({
    method: 'POST',
    path: '/api/v1/addresses',
    ...config,
  }, {}, { spendingPassword, accountIndex, walletId })
);
