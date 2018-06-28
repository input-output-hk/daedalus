// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: environment.WALLET_PORT,
    ca,
  }, {}, walletMeta)
);
