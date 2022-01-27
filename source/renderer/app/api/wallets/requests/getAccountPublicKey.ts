import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { GetAccountPublicKeyRequest } from '../types';

export const getAccountPublicKey = (
  config: RequestConfig,
  { walletId, index, passphrase, extended }: GetAccountPublicKeyRequest
): Promise<string> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/keys/${index}`,
      ...config,
    },
    {},
    {
      passphrase,
      format: extended ? 'extended' : 'non_extended',
    }
  );
