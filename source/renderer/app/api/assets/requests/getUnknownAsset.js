// @flow
import type { RequestConfig } from '../../common/types';
import type { Asset, GetUnknownAssetRequest } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const getUnknownAsset = (
  config: RequestConfig,
  { walletId, policyId }: GetUnknownAssetRequest
): Promise<Asset> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${getRawWalletId(walletId)}/assets/${policyId}`,
    ...config,
  });
