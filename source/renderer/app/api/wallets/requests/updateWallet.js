// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet, UpdateWalletRequest } from '../types';
import { request } from '../../utils/request';

export const updateWallet = (
  config: RequestConfig,
  { walletId, name }: UpdateWalletRequest
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}`,
      ...config,
    },
    {},
    { name }
  );
