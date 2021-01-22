// @flow
import type { RequestConfig } from '../../common/types';
import type { GetWalletKeyParams } from '../types';
import { request } from '../../utils/request';

export const getWalletKey = (
  config: RequestConfig,
  { walletId, role, index }: GetWalletKeyParams
): Promise<string> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/keys/${role}/${index}`,
    ...config,
  });
