// @flow
import type { ApiAddress } from 'daedalus-client-api';
import { request } from './lib/request';

export type NewAdaWalletAddressQueryParams = {
  passphrase: ?string,
};

export type NewAdaWalletAddressRawBodyParams = {
  accountId: string,
};

export const newAdaWalletAddress = (
  ca: string,
  pathParams: {},
  queryParams: NewAdaWalletAddressQueryParams,
  rawBodyParams: NewAdaWalletAddressRawBodyParams
): Promise<ApiAddress> => {
  const { accountId } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port: 8090,
    ca,
  }, queryParams, accountId);
};
