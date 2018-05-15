// @flow
import { request } from './lib/request';

export type IsValidAdaAddressParams = {
  ca: string,
  port: number,
  address: string,
};

export const isValidAdaAddress = (
  { ca, port, address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${encodedAddress}`,
    port,
    ca,
  });
};
