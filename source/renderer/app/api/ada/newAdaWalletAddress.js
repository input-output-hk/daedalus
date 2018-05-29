// @flow
import type { AdaAddress } from './types';
import { request } from './lib/request';

export type NewAdaWalletAddressParams = {
  password: ?string,
  accountId: string,
};

export const newAdaWalletAddress = (
  { password, accountId }: NewAdaWalletAddressParams
): Promise<AdaAddress> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/addresses',
  }, { passphrase: password }, accountId)
);

