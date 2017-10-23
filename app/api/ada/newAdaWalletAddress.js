// @flow
import type { ApiAddress } from 'daedalus-client-api';
import { request } from './lib/request';

export type newAdaWalletAddressPathParams = {
  passphrase: string,
};

export const newAdaWalletAddress = (
  ca: string, pathParams: {}, queryParams: newAdaWalletAddressQueryParams
): Promise<ApiAddress> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/addresses`,
    port: 8090,
    ca,
  }, queryParams);
};
