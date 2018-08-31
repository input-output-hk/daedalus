// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export type IsValidAdaAddressParams = {
  address: string,
};

export const isValidAdaAddress = (
  config: RequestConfig,
  { address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${encodedAddress}`,
    ...config
  });
};
