// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { Assets, GetAssetsRequest } from '../types';
import { getRawWalletId } from '../../utils';

export const getAssets = (
  config: RequestConfig,
  { walletId }: GetAssetsRequest
): Promise<Assets> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${getRawWalletId(walletId)}/assets`,
    ...config,
  });
