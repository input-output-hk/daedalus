import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { GetWalletPublicKeyRequest } from '../types';

export const getWalletPublicKey = (
  config: RequestConfig,
  { walletId, role, index }: GetWalletPublicKeyRequest
): Promise<string> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/keys/${role}/${index}`,
    ...config,
  });
