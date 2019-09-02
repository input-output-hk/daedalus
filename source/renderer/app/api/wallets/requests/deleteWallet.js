// @flow
import type { RequestConfig } from '../../common/types';
import type { DeleteWalletRequest } from '../types';
import { request } from '../../utils/request';

export const deleteWallet = (
  config: RequestConfig,
  { walletId }: DeleteWalletRequest
): Promise<*> =>
  request({
    method: 'DELETE',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
