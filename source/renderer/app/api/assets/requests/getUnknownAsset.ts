import type { RequestConfig } from '../../common/types';
import type { GetUnknownAssetRequest } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId, isLegacyWalletId } from '../../utils';
import Asset from '../../../domains/Asset';

export const getUnknownAsset = (
  config: RequestConfig,
  { walletId, policyId }: GetUnknownAssetRequest
): Promise<Asset> =>
  request({
    method: 'GET',
    path: `/v2/${
      isLegacyWalletId(walletId) ? 'byron-wallets' : 'wallets'
    }/${getRawWalletId(walletId)}/assets/${policyId}`,
    ...config,
  });
