import type { RequestConfig } from '../../common/types';
import type { Addresses, GetAddressesRequestQueryParams } from '../types';
import { request } from '../../utils/request';

export const getAddresses = (
  config: RequestConfig,
  walletId: string,
  queryParams?: GetAddressesRequestQueryParams
): Promise<Addresses> =>
  request(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/addresses`,
      ...config,
    },
    queryParams
  );
