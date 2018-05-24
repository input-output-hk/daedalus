// @flow
import { request } from './lib/request';

export type IsValidAdaAddressParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  address: string,
};

export const isValidAdaAddress = (
  { apiParams, address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${encodedAddress}`,
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  });
};
