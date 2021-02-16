// @flow
import type { RequestConfig } from '../../common/types';
import type { Asset, GetAssetRequest } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId, isLegacyWalletId } from '../../utils';

export const getAsset = (
  config: RequestConfig,
  { walletId, policyId, assetName }: GetAssetRequest
): Promise<Asset> =>
  request({
    method: 'GET',
    path: `/v2/${
      isLegacyWalletId(walletId) ? 'byron-wallets' : 'wallets'
    }/${getRawWalletId(walletId)}/assets/${policyId}/${assetName}`,
    ...config,
  });
