// @flow
import type { RequestConfig } from '../../common/types';
import type { Asset, GetAssetRequest } from '../types';
import { request } from '../../utils/request';

export const getAsset = (
  config: RequestConfig,
  { walletId, policyId, assetName }: GetAssetRequest
): Promise<Asset> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/assets/${policyId}/${assetName}`,
    ...config,
  });
