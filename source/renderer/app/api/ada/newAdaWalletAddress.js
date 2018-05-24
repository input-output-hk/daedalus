// @flow
import type { AdaAddress } from './types';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  { apiParams, password, accountId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { passphrase: password }, accountId)
);

