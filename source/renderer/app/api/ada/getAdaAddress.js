// @flow
import type { AdaAddress, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type GetAdaAddressParams = {
  address: string,
};

export const getAdaAddress = (
  config: RequestConfig,
  { address }: GetAdaAddressParams
): Promise<AdaAddress> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/addresses/${encodedAddress}`,
    ...config,
  });
};
