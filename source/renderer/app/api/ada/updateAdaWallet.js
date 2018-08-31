// @flow
import type { AdaV1Wallet, AdaV1Assurance, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type UpdateAdaWalletParams = {
  walletId: string,
  assuranceLevel: AdaV1Assurance,
  name: string
};

export const updateAdaWallet = (
  config: RequestConfig,
  { walletId, assuranceLevel, name }: UpdateAdaWalletParams
): Promise<AdaV1Wallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  }, {}, { assuranceLevel, name })
);
