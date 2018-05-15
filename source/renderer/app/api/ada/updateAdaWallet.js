// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type UpdateAdaWalletParams = {
  ca: string,
  port: number,
  walletId: string,
  walletMeta: {
    cwName: string,
    cwAssurance: string,
    cwUnit: number,
  }
};

export const updateAdaWallet = (
  { ca, port, walletId, walletMeta }: UpdateAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port,
    ca,
  }, {}, walletMeta)
);
