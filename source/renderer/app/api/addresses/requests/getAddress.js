// @flow
import type { AdaAddress, RequestConfig } from './types';
import { request } from '../../utils/request';

export type GetAddressParams = {
  address: string,
};

export const getAddress = (
  config: RequestConfig,
  { address }: GetAddressParams
): Promise<AdaAddress> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/addresses/${encodedAddress}`,
    ...config,
  });
};
