// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const getWalletPublicKey = (
  config: RequestConfig,
  { walletId }: { walletId: string }
): Promise<string> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/keys`,
    ...config,
  });
