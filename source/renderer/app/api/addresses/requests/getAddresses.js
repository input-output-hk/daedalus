// @flow
import type { RequestConfig } from '../../common/types';
import type { Address } from '../types';
import { request } from '../../utils/request';

export const getAddresses = (
  config: RequestConfig,
  walletId: string
): Promise<Address[]> =>
  request({
    method: 'GET',
    path: `/wallets/${walletId}/addresses?state=used`,
    ...config,
  });
