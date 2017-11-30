// @flow
import type { AdaAddress } from './types';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  ca: string,
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  { ca, password, accountId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port: 8090,
    ca,
  }, { passphrase: password }, accountId)
);

