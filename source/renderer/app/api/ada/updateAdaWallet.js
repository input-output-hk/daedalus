// @flow
import type { AdaWallet, WalletAssuranceLevel, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type UpdateAdaWalletParams = {
  walletId: string,
  assuranceLevel: WalletAssuranceLevel,
  name: string
};

export const updateAdaWallet = (
  config: RequestConfig,
  { walletId, assuranceLevel, name }: UpdateAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  }, {}, { assuranceLevel, name })
);
