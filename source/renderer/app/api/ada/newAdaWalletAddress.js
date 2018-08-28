// @flow
import type { AdaAddress } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  }, { passphrase: password }, accountId)
);

