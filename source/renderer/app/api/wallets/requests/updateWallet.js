// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet, WalletAssuranceLevel } from '../types';
import { request } from '../../utils/request';

export type UpdateWalletParams = {
  walletId: string,
  assuranceLevel: WalletAssuranceLevel,
  name: string
};

export const updateWallet = (
  config: RequestConfig,
  { walletId, assuranceLevel, name }: UpdateWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  }, {}, { assuranceLevel, name })
);
