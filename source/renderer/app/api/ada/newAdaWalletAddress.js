// @flow
import type { AdaAddress } from './types';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  ca: string,
  port: number,
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  { ca, port, password, accountId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
    port,
    ca,
  }, { passphrase: password }, accountId)
);

