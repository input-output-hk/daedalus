// @flow
import type { RequestConfig } from '../../common/types';
import type { GetWalletRequest, AdaWallet } from '../types';
import { request } from '../../utils/request';

export const getWallet = (
  config: RequestConfig,
  { walletId }: GetWalletRequest
): Promise<AdaWallet> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
