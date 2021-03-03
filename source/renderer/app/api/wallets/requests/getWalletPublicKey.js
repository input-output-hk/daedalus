// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { GetWalletPublicKeyRequest } from '../types';

export const getWalletPublicKey = (
  config: RequestConfig,
  { walletId, index, passphrase, extended }: GetWalletPublicKeyRequest
): Promise<string> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/keys/${index}`,
      ...config,
    },
    {},
    { passphrase, extended }
  );
