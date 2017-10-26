// @flow
import { request } from './lib/request';

export type isValidAdaAddressPathParams = {
  address: string,
};

export const isValidAdaAddress = (
  ca: string, pathParams: isValidAdaAddressPathParams
): Promise<boolean> => {
  const { address } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${address}`,
    port: 8090,
    ca,
  });
};
