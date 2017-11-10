// @flow
import { request } from './lib/request';

export type isValidAdaAddressPathParams = {
  address: string,
};

export const isValidAdaAddress = (
  ca: string, pathParams: isValidAdaAddressPathParams
): Promise<boolean> => {
  const { address } = pathParams;
  const path = `/api/addresses/${address}`;
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: encodeURIComponent(path),
    port: 8090,
    ca,
  });
};
