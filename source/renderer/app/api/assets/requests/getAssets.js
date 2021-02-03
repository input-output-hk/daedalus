// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { Assets, GetAssetsRequest } from '../types';

export const getAssets = (
  config: RequestConfig,
  { walletId }: GetAssetsRequest
): Promise<Assets> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/assets`,
    ...config,
  });
