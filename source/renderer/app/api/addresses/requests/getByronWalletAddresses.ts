import type { RequestConfig } from '../../common/types';
import type { Addresses, GetAddressesRequestQueryParams } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const getByronWalletAddresses = (
  config: RequestConfig,
  walletId: string,
  queryParams?: GetAddressesRequestQueryParams
): Promise<Addresses> =>
  request(
    {
      method: 'GET',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}/addresses`,
      ...config,
    },
    queryParams
  );
