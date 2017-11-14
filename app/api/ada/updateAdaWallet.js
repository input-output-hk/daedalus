// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type UpdateAdaWalletParams = {
  ca: string,
  walletId: string,
  walletMeta: {
    cwName: string,
    cwAssurance: string,
    cwUnit: number,
  }
};

export const updateAdaWallet = (
  { ca, walletId, walletMeta }: UpdateAdaWalletParams
): Promise<ApiWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: 8090,
    ca,
  }, {}, walletMeta)
);
