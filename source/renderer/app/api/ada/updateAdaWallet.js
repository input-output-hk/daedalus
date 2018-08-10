// @flow
import type { AdaV1Wallet, AdaV1Assurance } from './types';
import { request } from './lib/v1/request';

export type UpdateAdaWalletParams = {
  ca: string,
  walletId: string,
  assuranceLevel: AdaV1Assurance,
  name: string
};

export const updateAdaWallet = (
  { ca, walletId, assuranceLevel, name }: UpdateAdaWalletParams
): Promise<AdaV1Wallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}`,
    port: 8090,
    ca,
  }, {}, { assuranceLevel, name })
);
