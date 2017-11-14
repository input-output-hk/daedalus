// @flow
import { request } from './lib/request';

export type IsValidAdaAddressParams = {
  ca: string,
  address: string,
};

export const isValidAdaAddress = (
  { ca, address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const path = `/api/addresses/${address}`;
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: encodeURIComponent(path),
    port: 8090,
    ca,
  });
};
