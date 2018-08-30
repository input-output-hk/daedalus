// @flow
import type { AdaAddress, RequestConfig } from './types';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  config: RequestConfig,
  { password, accountId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port: config.port,
    ca: config.ca,
  }, { passphrase: password }, accountId)
);

