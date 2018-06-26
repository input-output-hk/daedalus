// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type IsValidAdaAddressParams = {
  ca: string,
  address: string,
};

export const isValidAdaAddress = (
  { ca, address }: IsValidAdaAddressParams
): Promise<boolean> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/addresses/${encodedAddress}`,
    port: environment.WALLET_PORT,
    ca,
  });
};
