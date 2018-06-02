// @flow
import { request } from './lib/request';

export type IsValidAdaAddressParams = {
  address: string,
};

export const isValidAdaAddress = (
  { address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${encodedAddress}`,
  });
};
