// @flow
import type { RequestConfig } from '../../common/types';
import type { Asset, GetUnknownAssetRequest } from '../types';
import { request } from '../../utils/request';

export const getUnknownAsset = (
  config: RequestConfig,
  { walletId, policyId }: GetUnknownAssetRequest
): Promise<Asset> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/assets/${policyId}`,
    ...config,
  });
