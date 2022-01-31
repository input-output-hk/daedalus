import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { ApiAssets, GetAssetsRequest } from '../types';
import { getRawWalletId, isLegacyWalletId } from '../../utils';

export const getAssets = (
  config: RequestConfig,
  { walletId }: GetAssetsRequest
): Promise<ApiAssets> =>
  request({
    method: 'GET',
    path: `/v2/${
      isLegacyWalletId(walletId) ? 'byron-wallets' : 'wallets'
    }/${getRawWalletId(walletId)}/assets`,
    ...config,
  });
