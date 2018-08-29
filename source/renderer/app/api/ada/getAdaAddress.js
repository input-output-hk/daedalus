// @flow
import type { AdaAddress } from './types';
import { request } from './lib/v1/request';

export type GetAdaAddressParams = {
  ca: string,
  address: string,
};

export const getAdaAddress = (
  { ca, address }: GetAdaAddressParams
): Promise<AdaAddress> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/addresses/${encodedAddress}`,
    port: 8090,
    ca,
  });
};
