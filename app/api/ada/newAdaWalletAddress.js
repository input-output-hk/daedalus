// @flow
import type { ApiAddress } from 'daedalus-client-api';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  ca: string,
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  { ca, password, accountId }: NewAdaWalletAddressParams
): Promise<ApiAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port: 8090,
    ca,
  }, { passphrase: password }, accountId)
);

