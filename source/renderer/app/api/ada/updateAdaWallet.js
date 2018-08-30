// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/request';

export type UpdateAdaWalletParams = {
  walletId: string,
  walletMeta: {
    cwName: string,
    cwAssurance: string,
    cwUnit: number,
  }
};

export const updateAdaWallet = (
  config: RequestConfig,
  { walletId, walletMeta }: UpdateAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: config.port,
    ca: config.ca,
  }, {}, walletMeta)
);
